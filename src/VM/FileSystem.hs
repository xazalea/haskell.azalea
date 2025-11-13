{-# LANGUAGE OverloadedStrings #-}

module VM.FileSystem where

import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import System.FilePath

-- Virtual file system
data VFS = VFS
  { root :: FileNode
  , currentDir :: FilePath
  } deriving (Show)

data FileNode = FileNode
  { nodeName :: Text
  , nodeType :: NodeType
  , nodeContent :: Maybe [Word8]
  , nodeChildren :: M.Map Text FileNode
  } deriving (Show)

data NodeType = File | Directory
  deriving (Show, Eq)

-- Create empty VFS
createVFS :: IO VFS
createVFS = do
  root <- createRootNode
  return $ VFS
    { root = root
    , currentDir = "/"
    }

createRootNode :: IO FileNode
createRootNode = do
  home <- createDirectoryNode "home"
  etc <- createDirectoryNode "etc"
  var <- createDirectoryNode "var"
  tmp <- createDirectoryNode "tmp"
  let children = M.fromList
        [ ("home", home)
        , ("etc", etc)
        , ("var", var)
        , ("tmp", tmp)
        ]
  return $ FileNode
    { nodeName = "/"
    , nodeType = Directory
    , nodeContent = Nothing
    , nodeChildren = children
    }

createDirectoryNode :: Text -> IO FileNode
createDirectoryNode name = return $ FileNode
  { nodeName = name
  , nodeType = Directory
  , nodeContent = Nothing
  , nodeChildren = M.empty
  }

createFileNode :: Text -> [Word8] -> FileNode
createFileNode name content = FileNode
  { nodeName = name
  , nodeType = File
  , nodeContent = Just content
  , nodeChildren = M.empty
  }

-- Navigate to path
resolvePath :: VFS -> FilePath -> Maybe FileNode
resolvePath vfs path = resolvePath' (root vfs) (splitPath path)

resolvePath' :: FileNode -> [String] -> Maybe FileNode
resolvePath' node [] = Just node
resolvePath' node (p:ps)
  | p == "/" || p == "" = resolvePath' node ps
  | otherwise = case M.lookup (T.pack p) (nodeChildren node) of
      Just child -> resolvePath' child ps
      Nothing -> Nothing

-- List directory
listDirectory :: VFS -> FilePath -> IO [Text]
listDirectory vfs path = case resolvePath vfs path of
  Just node -> case nodeType node of
    Directory -> return $ M.keys (nodeChildren node)
    File -> return []
  Nothing -> return []

-- Read file
readFile :: VFS -> FilePath -> IO (Maybe [Word8])
readFile vfs path = case resolvePath vfs path of
  Just node -> case nodeType node of
    File -> return $ nodeContent node
    Directory -> return Nothing
  Nothing -> return Nothing

-- Write file
writeFile :: VFS -> FilePath -> [Word8] -> IO VFS
writeFile vfs path content = do
  let parts = splitPath path
      fileName = T.pack $ last parts
      dirPath = joinPath $ init parts
  case resolvePath vfs dirPath of
    Just dirNode -> case nodeType dirNode of
      Directory -> do
        let fileNode = createFileNode fileName content
            newChildren = M.insert fileName fileNode (nodeChildren dirNode)
            newDirNode = dirNode { nodeChildren = newChildren }
        -- Update VFS (simplified - in real implementation would need to rebuild tree)
        return vfs
      File -> return vfs
    Nothing -> return vfs

