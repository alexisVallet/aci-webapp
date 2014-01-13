{-|
Handler for the image upload page. Can only be accessed by a logged user.
-}
module Handler.Upload where

import Import

getUploadR :: Handler Html
getUploadR = do
  defaultLayout $ do
    [whamlet|
     <div .container>
       <div .page-header>
         <h1>
           Upload
    |]

postUploadR :: Handler Html
postUploadR = do
  defaultLayout $ do
    [whamlet|
     <div .container>
       <div .page-header>
         <h1>
           Upload
    |]