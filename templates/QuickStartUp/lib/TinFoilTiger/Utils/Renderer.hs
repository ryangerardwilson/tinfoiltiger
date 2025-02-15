--------------------------------------------------
-- File: lib/Utils/Renderer.hs
--------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TinFoilTiger.Utils.Renderer
  ( compile_template,
    render_template,
    render_template_substitutions,
  )
where

-- for strict Text operations

-- used in resource lookup
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedDir)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
-- import Debug.Trace (trace)
import Language.Haskell.TH.Syntax (Q, runIO)
import System.FilePath ((</>))

--------------------------------------------------------------------------------

-- | compile_template pre-renders a template (including wrapper/embed processing)
-- at compile time. All helper functions and resource maps are defined locally.

--------------------------------------------------------------------------------
compile_template :: FilePath -> Q TL.Text
compile_template fp = do
  -- All functions needed for compile_template are defined in this let block.
  let --------------------------------------------------------------------------------
      -- resourcesMap is built at compile time using embedDir.
      --------------------------------------------------------------------------------
      resourcesMap :: Map.Map FilePath TL.Text
      resourcesMap =
        Map.fromList
          $( [|
               map
                 (\(path, bs) -> (path, decodeUtf8 (BL.fromStrict bs)))
                 $(embedDir "resources")
               |]
           )

      --------------------------------------------------------------------------------
      -- loadResource does an in–memory lookup in resourcesMap.
      --------------------------------------------------------------------------------
      loadResource :: FilePath -> IO TL.Text
      loadResource fp' =
        case Map.lookup fp' resourcesMap of
          Just content -> return content
          Nothing -> error ("Resource not found (embedded): " ++ fp')

      --------------------------------------------------------------------------------
      -- The main function that renders a template.
      --------------------------------------------------------------------------------
      renderTemplate ::
        (FilePath -> IO TL.Text) -> -- loader
        FilePath -> -- template path
        [(TL.Text, TL.Text)] -> -- substitutions
        IO TL.Text
      renderTemplate loader templatePath subs = do
        baseTemplate <- loader templatePath
        -- trace ("[DEBUG] Loaded template: " ++ templatePath) (return ())
        -- trace ("[DEBUG] Template begins with: " ++ show (TL.take 100 baseTemplate)) (return ())
        composed1 <- processWrapper loader baseTemplate
        composed2 <- processEmbeds loader composed1
        let finalHtml = replaceVars composed2 subs
        -- trace ("[DEBUG] Final HTML after substitutions: " ++ show (TL.take 200 finalHtml)) (return ())
        return finalHtml

      --------------------------------------------------------------------------------
      -- replaceVars performs variable substitution for markers of the form {{KEY}}.
      --------------------------------------------------------------------------------
      replaceVars :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
      replaceVars content subs =
        foldl (\acc (k, v) -> TL.replace ("{{" <> k <> "}}") v acc) content subs

      --------------------------------------------------------------------------------
      -- processWrapper looks for a wrapper tag at the very beginning and,
      -- if found, loads the corresponding wrapper and replaces its <embed> placeholder.
      --------------------------------------------------------------------------------
      processWrapper :: (FilePath -> IO TL.Text) -> TL.Text -> IO TL.Text
      processWrapper loader content =
        case matchWrapper (TL.toStrict content) of
          Nothing -> do
            -- trace "[DEBUG] No wrapper tag found. Returning original template." (return ())
            return content
          Just (tagName, innerHtml) -> do
            -- trace ("[DEBUG] Found wrapper: " ++ T.unpack tagName) (return ())
            let wrapperFile = "components" </> "wrappers" </> T.unpack tagName <.> "html"
            wrapperContent <- loader wrapperFile
            -- trace ("[DEBUG] Loaded wrapper from: " ++ wrapperFile) (return ())
            -- trace ("[DEBUG] Inner HTML to embed: " ++ show (TL.take 100 innerHtml)) (return ())
            let replaced = replaceEmbedPlaceholder wrapperContent innerHtml
            -- trace "[DEBUG] After replacing embed placeholder in wrapper:" $
            --  trace (T.unpack $ T.take 100 $ TL.toStrict replaced) (return ())
            processEmbeds loader replaced
        where
          matchWrapper :: T.Text -> Maybe (T.Text, TL.Text)
          matchWrapper txt =
            let stripped = T.strip txt
             in if not ("<wrapper-" `T.isPrefixOf` stripped)
                  then Nothing
                  else case T.findIndex (== '>') stripped of
                    Nothing -> Nothing
                    Just i ->
                      let _openTag = T.take (i + 1) stripped -- e.g., "<wrapper-base-template>"
                          tagName = T.strip $ T.drop (T.length "<wrapper-") (T.take i stripped)
                          closingTag = "</wrapper-" <> tagName <> ">"
                       in if closingTag `T.isSuffixOf` stripped
                            then
                              let inner = T.strip $ T.drop (i + 1) (T.take (T.length stripped - T.length closingTag) stripped)
                               in Just (tagName, TL.fromStrict inner)
                            else Nothing

      --------------------------------------------------------------------------------
      -- processEmbeds recursively finds <embed-embedname /> tags and replaces them.
      --------------------------------------------------------------------------------
      processEmbeds :: (FilePath -> IO TL.Text) -> TL.Text -> IO TL.Text
      processEmbeds loader content = do
        let strictContent = TL.toStrict content
        case T.breakOn "<embed-" strictContent of
          (before, match)
            | T.null match -> do
                -- trace "[DEBUG] No embed tag found." (return ())
                return content
            | otherwise -> do
                let afterEmbed = T.drop (T.length "<embed-") match
                case T.findIndex (== '>') afterEmbed of
                  Nothing -> do
                    -- trace "[DEBUG] Malformed embed tag (no closing '>'). Returning content." (return ())
                    return content
                  Just idx -> do
                    let tagPart = T.take idx afterEmbed
                        restAfter = T.drop (idx + 1) afterEmbed
                        embedName = T.strip $ T.dropWhileEnd (\c -> c == '/' || c == ' ') tagPart
                    -- trace ("[DEBUG] Found embed tag for: " ++ T.unpack embedName) (return ())
                    let beforePart = before
                        embedFilePath = "components" </> "embeds" </> T.unpack embedName <.> "html"
                    embedContent <- loader embedFilePath
                    -- trace ("[DEBUG] Loaded embed from: " ++ embedFilePath) (return ())
                    processedEmbed <- processEmbeds loader embedContent -- process nested embeds
                    let replacement = TL.toStrict processedEmbed
                        newText = beforePart <> replacement <> restAfter
                    -- trace ("[DEBUG] After replacing embed, snippet: " ++ show (T.take 100 newText)) (return ())
                    processEmbeds loader (TL.fromStrict newText)

      --------------------------------------------------------------------------------
      -- replaceEmbedPlaceholder replaces the singular embed placeholder in a wrapper.
      --------------------------------------------------------------------------------
      replaceEmbedPlaceholder :: TL.Text -> TL.Text -> TL.Text
      replaceEmbedPlaceholder wrapper newContent =
        let find1 = "<embed />"
            find2 = "<embed/>"
         in if TL.isInfixOf find1 wrapper
              then TL.replace find1 newContent wrapper
              else
                if TL.isInfixOf find2 wrapper
                  then TL.replace find2 newContent wrapper
                  else wrapper

      --------------------------------------------------------------------------------
      -- Simple helper operator to append a file extension.
      --------------------------------------------------------------------------------

      (<.>) :: FilePath -> String -> FilePath
      filePath <.> ext = filePath ++ "." ++ ext

  -- Now actually call renderTemplate in the Q (compile–time) monad.
  t <- runIO $ renderTemplate loadResource fp []
  return t

--------------------------------------------------------------------------------

-- | Reads a template file from the resources directory without substitutions.

--------------------------------------------------------------------------------
render_template :: FilePath -> IO TL.Text
render_template templateName = do
  content <- BL.readFile ("resources" </> templateName)
  return (decodeUtf8 content)

-- Note: compile_template is assumed to be defined elsewhere in this module.
-- For example, it might look something like:
-- compile_template :: FilePath -> Q TL.Text
-- compile_template fp = ...

--------------------------------------------------------------------------------

-- | render_template_substitutions applies only variable substitutions to a pre–rendered template.
-- The helper function for performing substitutions is nested here.

--------------------------------------------------------------------------------
render_template_substitutions :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
render_template_substitutions preRendered subs =
  let replaceVars :: TL.Text -> [(TL.Text, TL.Text)] -> TL.Text
      replaceVars content subsList =
        foldl (\acc (k, v) -> TL.replace ("{{" <> k <> "}}") v acc) content subsList
   in replaceVars preRendered subs

--------------------------------------------------
