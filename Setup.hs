import Control.Monad (unless, when)
import Data.List (elemIndex)
import Distribution.Simple
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit
import System.Process

main :: IO ()
main = do
  args <- getArgs -- Get command-line arguments

  -- STEP I: Run compile.py in all cases
  putStrLn "[INFO] STEP I - Running compile.py..."
  exitCodeCompile <- system "python automations/compile.py"
  case exitCodeCompile of
    ExitSuccess -> putStrLn "[INFO] STEP I - Compiling complete."
    ExitFailure _ -> putStrLn "Failed to compile necessary Haskell files." >> exitWith exitCodeCompile

  -- Check if --publish flag is present
  when ("--publish" `elem` args) $ do
    putStrLn "[INFO] Optional: Updating version before publishing..."
    exitCodeUpdateVersion <- system "python automations/update_version.py"
    case exitCodeUpdateVersion of
      ExitSuccess -> putStrLn "[INFO] Version updated."
      ExitFailure _ -> putStrLn "Failed to update version." >> exitWith exitCodeUpdateVersion

  -- STEP II: Stack build and install
  putStrLn "[INFO] STEP II - Now building the Haskell project using Stack..."
  exitCodeBuild <- system "stack build"
  case exitCodeBuild of
    ExitSuccess -> do
      putStrLn "[INFO] STEP II - Build complete."
      exitCodeInstall <- system "stack install --verbosity=error"
      case exitCodeInstall of
        ExitSuccess -> putStrLn "[INFO] STEP II - Binary generated and installed."
        ExitFailure _ -> putStrLn "Stack install failed" >> exitWith exitCodeInstall
    ExitFailure _ -> putStrLn "Stack build failed" >> exitWith exitCodeBuild

  -- STEP III: Handle --test flag directly
  when ("--test" `elem` args) $ do
    -- Determine the template name following the --test flag.
    let maybeTestIndex = elemIndex "--test" args
        testTemplate = case maybeTestIndex >>= (\i -> if length args > i + 1 then Just (args !! (i + 1)) else Nothing) of
          Nothing -> "TestTemplate"
          Just tmpl -> tmpl
    putStrLn $ "[INFO] STEP III - Processing test setup with template: " ++ testTemplate
    let command = "./tinfoiltiger --new testApp --template " ++ testTemplate
    putStrLn $ "[INFO] Generating test scaffold via " ++ command
    exitCodeScaffold <- system command
    case exitCodeScaffold of
      ExitSuccess -> putStrLn "[INFO] STEP III Completed!"
      ExitFailure _ -> putStrLn "Failed to generate test scaffold." >> exitWith exitCodeScaffold

  -- Provide suggestion if the --test flag was not used
  unless ("--test" `elem` args) $
    putStrLn "Suggestion: Use the flag --test [TemplateName] to generate a test project scaffold."

  -- Check if --publish flag is present again for step 3
  if "--publish" `elem` args
    then do
      putStrLn "[INFO] STEP III - Running publish.py..."
      exitCodePublish <- system "python automations/publish.py"
      case exitCodePublish of
        ExitSuccess -> putStrLn "[INFO] STEP III - Publishing complete."
        ExitFailure _ -> putStrLn "Failed to publish." >> exitWith exitCodePublish
    else putStrLn "Suggestion: Use the flag --publish to run publish.py."
