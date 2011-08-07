import Prelude hiding (catch)
import Control.Exception

import System.FilePath
import System.Exit
import System.Directory
import System.Posix.Directory

import System.Environment (getArgs)
import System.IO.Temp (withTempDirectory)
import System.Process hiding (runCommand)

data ModFile = Zip FilePath | Rar FilePath | SevenZip FilePath

parseFile :: FilePath -> ModFile
parseFile file = parse' $ takeExtension file
  where
    parse' ".zip" = Zip file
    parse' ".rar" = Rar file
    parse' ".7z"  = SevenZip file
    parse' ".001" = (addFile ".001" . parseFile . dropExtension) file
    parse' ext    = error $ "Unknown file type '" ++ ext ++ "'."

addFile :: String -> ModFile -> ModFile
addFile ext (SevenZip file) = SevenZip $ file ++ ext
addFile ext (Rar file) = Rar $ file ++ ext
addFile ext (Zip file) = Zip $ file ++ ext

modFilePath :: ModFile -> FilePath
modFilePath (Zip fp) = fp
modFilePath (Rar fp) = fp
modFilePath (SevenZip fp) = fp

main :: IO ()
main = do
  args <- getArgs
  path <- getWorkingDirectory
  let absolute x = joinPath [path, x]
  let files = map absolute args
  results <- mapM (catchString . repackInTemp path) files
  putStrLn $ showResults $ zip files results

catchString :: IO () -> IO String
catchString io = (io >> return "") `catch` handler
  where handler (SomeException e) = return $ show e

showResults :: [(FilePath, String)] -> String
showResults results
  | all (null . snd) results = "\nEverything repacked okay."
  | otherwise = unlines $ "" : "The following errors occurred:" : showAllErrors results
  where
    showAllErrors = map showError . filter (not . null . snd)
    showError (file, err) = "  " ++ file ++ ":\n    " ++ err

repackInTemp :: FilePath -> FilePath -> IO ()
repackInTemp base file = withTempDirectory base "repack." repackIn
  where repackIn path = changeWorkingDirectory path >> repack (parseFile file)

repack :: ModFile -> IO ()
repack file = do
  runCommand $ unpackCommand file
  fmap onlyPath (getDirectoryContents ".") >>= changeWorkingDirectory
  print $ packCommand $ outputFile file
  runCommand $ packCommand $ outputFile file

runCommand :: (String, [String]) -> IO ()
runCommand (cmd, args) = do
  code <- rawSystem cmd args
  case code of
    ExitSuccess -> return ()
    ExitFailure val -> error $ "Command '" ++ cmd ++ "' exited with status " ++ show val ++ "."

onlyPath :: [FilePath] -> FilePath
onlyPath = onlyPath' . filter notDots
  where
    notDots x = x /= "." && x /= ".."
    onlyPath' [x] = x
    onlyPath' xs = error $ "Archive contains " ++ show (length xs) ++ " paths, cannot repack."

unpackCommand :: ModFile -> (String, [String])
unpackCommand (Zip file) = ("unzip", [file])
unpackCommand (Rar file) = ("unrar", ["x", file])
unpackCommand (SevenZip file) = ("7zr", ["x", file])

packCommand :: ModFile -> (String, [String])
packCommand (SevenZip file) = ("7zr", ["a", file, "."])
packCommand _ = error "not expected"

outputFile :: ModFile -> ModFile
outputFile = SevenZip . (++ "-repack.7z") . dropExtensions . modFilePath
