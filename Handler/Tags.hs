{-|
Utility functions for dealing with tags, including form fields, parsing functions.
-}
module Handler.Tags 
    (
     tagsField,
     tagsToText,
     writeTags
    ) where

import Import
import Data.Attoparsec.Text
import Data.Text (pack)
import Data.Char (toLower)
import Control.Monad
import Control.Applicative ((<|>))

-- | Default tag category name, must match the unique name of the tag category
-- in the backend.
defaultCategoryName :: String
defaultCategoryName = "default"

writeTags :: ImageId -> [(String,String)] -> YesodDB App ()
writeTags imageId listOfTags = 
  forM_ listOfTags $ \(categoryName, tagName) -> do
    let categoryText = pack categoryName
    mCategory <- getBy $ UniqueTagCategory categoryText
    -- insert tag category if necessary, get the key
    categoryId <- case mCategory of
      Nothing -> insert $ TagCategory categoryText
      Just (Entity key _) -> return key
    -- insert tag if necessary, get the key
    eTagId <- insertBy $ Tag (pack tagName) categoryId
    let tagId = case eTagId of
                  Left (Entity key _) -> key
                  Right key -> key
    -- links the image with the tag
    insert $ ImageTagLink imageId tagId

-- | Field for the tags text area, including parsing and checking of everything.
tagsField :: Field Handler [(String, String)]
tagsField = 
    checkMMap 
      (\t -> return $ parseTags $ unTextarea t) 
      (Textarea . tagsToText) 
      textareaField

-- | Converts categorized tags into the original input text. Necessary for
-- checkMMap for some reason.
tagsToText :: [(String,String)] -> Text
tagsToText tags = pack $ foldr catAndTagToString [] tags
  where catAndTagToString (cat,tag) rest =
            if cat == defaultCategoryName
            then tag ++ " " ++ rest
            else cat ++ ":" ++ tag ++ " " ++ rest

-- | Parse tags out of the text area input.
parseTags :: Text -> Either Text [(String,String)]
parseTags tagsText =
  let eTags = flip parseOnly tagsText $ many' $ do
                skipSpace
                tagAndCategoryParser <|> defaultCategoryTagParser in
  case eTags of
    Left errMessage -> Left $ pack errMessage
    Right tags -> Right tags
  

-- | Parser for a tag with a specified category.
tagAndCategoryParser :: Parser (String,String)
tagAndCategoryParser = do
  category <- many1 letter
  _ <- char ':'
  tag <- many1 letter
  return (map toLower category, map toLower tag)

-- | Parser for a tag with no specified category (get assigned to default).
defaultCategoryTagParser :: Parser (String, String)
defaultCategoryTagParser = do
  tag <- many1 letter
  return (map toLower defaultCategoryName, map toLower tag)