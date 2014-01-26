{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-|
Home page of the application. Displays recently uploaded images.
-}
module Handler.Home where

import Import
import Database.Esqueleto

-- | Handler for the home page of the application.
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
