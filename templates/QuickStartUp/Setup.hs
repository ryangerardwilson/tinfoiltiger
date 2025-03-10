import Control.Exception (SomeException, catch)
import System.Exit (exitFailure)
import System.Process (callProcess)

main :: IO ()
main = do
  putStrLn "[INFO] Compiling Tailwind CSS..."
  catch
    ( callProcess
        "tailwind"
        ["resources/main.css"]
    )
    ( \e -> do
        putStrLn $ "[ERROR] Tailwind CSS command failed: " ++ show (e :: SomeException)
        exitFailure
    )

  putStrLn "[INFO] Running stack build..."
  catch
    (callProcess "stack" ["build"])
    ( \e -> do
        putStrLn $ "[ERROR] Stack build failed: " ++ show (e :: SomeException)
        exitFailure
    )

  putStrLn "[INFO] Running stack run..."
  catch
    (callProcess "stack" ["run"])
    ( \e -> do
        putStrLn $ "[ERROR] Stack run failed: " ++ show (e :: SomeException)
        exitFailure
    )
