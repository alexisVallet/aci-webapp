{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Database.Esqueleto

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  images <- runDB $ select $
            from $ \image -> do
              orderBy [desc (image ^. ImageUploadTime)]
              return image
  defaultLayout $ do
    [whamlet|
     <h2> Recently uploaded
     <div .container>
       <div .row>
         $forall Entity imageId image <- images
           <div .col-sm-6 .col-md-3>
             <a href=@{ImageR $ imageId}>
               <div .thumbnail>
                 <img src=#{imageFilename image}>
    |]
