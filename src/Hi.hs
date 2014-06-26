{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Hi
  (
    run
  , process
  ) where

import           Hi.Directory        (inDirectory)
import           Hi.FilePath         (rewritePath)
import qualified Hi.Git              as Git
import           Hi.Template         (readTemplates)
import           Hi.Types

import           Control.Applicative
import           Control.Monad
import           Data.List           (isSuffixOf)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Data.Text.Template  (Context, substitute)
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     (dropFileName)
import           System.Process      (system)

-- | Run 'hi'.
run :: Option -> IO ()
run option@(Option {repository}) = do
    putStrLn $ "Creating new project from repository: " ++ Git.expandUrl repository
    writeFiles =<< showFileList =<< process option <$> readTemplates repository
    postProcess option

-- |Write given 'Files' to filesystem.
writeFiles :: Files -> IO ()
writeFiles = mapM_ (uncurry write)
  where
    write :: FilePath -> String -> IO ()
    write path content = createDirectoryIfMissing True (dropFileName path) >> writeFile path content

-- | Show 'Files' to stdout.
showFileList :: Files -> IO Files
showFileList files = do
    mapM_ (showFile . fst) files
    return files
  where
    showFile :: FilePath -> IO ()
    showFile path = putStrLn $ "    " ++ green "create" ++ "  " ++ path
    green :: String -> String
    green x = "\x1b[32m" ++ x ++ "\x1b[0m"

-- |Process given 'Files' and return result. it does
--
-- 1. rewrite path
--
-- 2. substitute arguments
process :: Option -> Files -> Files
process Option {..} = map go
  where
    go (path, content) = if ".template" `isSuffixOf` path
                           then (rewritePath' path, substitute' content)
                           else (rewritePath' path, content)
    rewritePath'     = rewritePath packageName moduleName
    substitute' text = LT.unpack $ substitute (T.pack text) (context options)
    options          = [("packageName", packageName)
                       ,("moduleName", moduleName)
                       ,("author", author)
                       ,("email", email)
                       ,("year", year)
                       ,("repository", repository)
                       ]

-- | Return 'Context' obtained by given 'Options'
context :: [(String, String)] -> Context
context opts x = T.pack . fromJust $ lookup (T.unpack x) opts

postProcess :: Option -> IO ()
postProcess Option {initializeGitRepository, packageName} = do
    when initializeGitRepository $
      -- TODO This wont' work unless template has `package-name` as root dir.
      inDirectory packageName $
        void $ system "git init && git add . && git commit -m \"Initial commit\""
    return ()
