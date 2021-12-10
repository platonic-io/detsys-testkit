module Journal.Internal.Parse where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Word (Word32, Word8)
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

------------------------------------------------------------------------

data JournalAST = JournalAST
  { jaActiveFile :: FileAST
  }

data FileAST = FileAST
  { faContent :: [EntryAST]
  , faFooter  :: Maybe FooterAST
  }
  deriving Show

data EntryAST = EntryAST HeaderAST BodyAST
  deriving Show

data HeaderAST = HeaderAST
  { haTag     :: Word8
  , haVersion :: Word8
  , haLength  :: Word32
  }
  deriving Show

newtype BodyAST = BodyAST ByteString
  deriving Show

data FooterAST = FooterAST HeaderAST PaddingAST
  deriving Show

newtype PaddingAST = PaddingAST ByteString
  deriving Show

------------------------------------------------------------------------

parseFileAST :: FilePath -> IO (Either ParseError FileAST)
parseFileAST fp = parseFromFile fileASTP fp

fileASTP :: Parser FileAST
fileASTP = FileAST <$> many1 entryP <*> optionMaybe footerP <?> "fileASTP"

entryP :: Parser EntryAST
entryP = EntryAST <$> headerP <*> bodyP <?> "entryP"

headerP :: Parser HeaderAST
headerP = HeaderAST <$> tagP <*> versionP <*> lengthP <?> "headerP"

tagP :: Parser Word8
tagP = c2w <$> (char (w2c (fromIntegral 0))
           <|>  char (w2c (fromIntegral 1))
           <|>  char (w2c (fromIntegral 2))
           <|>  char (w2c (fromIntegral 4))
           <?> "tagP")

versionP :: Parser Word8
versionP = c2w <$> anyToken <?> "versionP"

lengthP :: Parser Word32
lengthP = fromIntegral . foldl' (\a i -> a * 10 + digitToInt i) 0 <$>
  count 4 anyToken <?> "lengthP"

bodyP :: Parser BodyAST
bodyP = BodyAST . BSChar8.pack <$> many1 anyToken <?> "bodyP"

footerP :: Parser FooterAST
footerP = FooterAST <$> headerP <*> paddingP <?> "footerP"

paddingP :: Parser PaddingAST
paddingP = PaddingAST . BSChar8.pack <$>
  (many1 (char '\NUL') <|> eof *> pure "") <?> "paddingP"
