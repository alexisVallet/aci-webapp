{-|
Handler for the image upload page. Can only be accessed by a logged user.
-}
module Handler.Upload where

import Yesod.Form
import Text.Julius (rawJS)
import Control.Monad

import Import

uploadDirectory :: FilePath
uploadDirectory = "static/images"

uploadForm :: Html -> MForm Handler (FormResult (FileInfo, Text, Maybe Textarea), Widget)
uploadForm html = do
  (rfile, vfile) <- mreq fileField "Image" Nothing
  (rtitle, vtitle) <- mreq textField "Title" Nothing
  (rdescription, vdescription) <- mopt textareaField "Description" Nothing
  let vs = [vtitle, vdescription]
  return ((,,) <$> rfile <*> rtitle <*> rdescription, do
    -- Rendering image input with thumbnail as well as title, description fields
    addScript $ StaticR js_holder_js
    inputIds <- forM ([1..2] :: [Int]) $ const newIdent
    divIds <- forM ([1..2] :: [Int]) $ const newIdent
    let vAndIds = zip (zip vs inputIds) divIds
    forM_ vAndIds $ \((_,inputId),divId) -> toWidget $ do
      [julius|
        $("##{rawJS divId}>*").attr("id","#{rawJS inputId}");
        $("##{rawJS divId}>*").addClass("form-control");
      |]
    [whamlet|
      #{html}
      <div role=form>
        <div .fileinput .fileinput-new data-provides=fileinput>
          <div .fileinput-new .thumbnail style="width: 200px; height: 150px;">
            <img data-src="@{StaticR js_holder_js}/100%x100%">
          <div .fileinput-preview .fileinput-exists .thumbnail style="max-width: 200px; max-height: 150px;">
          <div>
            <span .btn .btn-default .btn-file>
              <span .fileinput-new> Select image
              <span .fileinput-exists> Change
              ^{fvInput vfile}
            <a href=#  .btn .btn-default .fileinput-exists data-dismiss=fileinput>
              Remove
        $forall ((v,inputId),divId) <- vAndIds
          <div .form-group>
             <label for=#{inputId}>#{fvLabel v}
             $maybe tt <- fvTooltip v
               <div .alert .alert-info>#{tt}
             <div ##{divId}>
               ^{fvInput v}
             $maybe err <- fvErrors v
               <div .alert .alert-danger>#{err}
    |])
  

getUploadR :: Handler Html
getUploadR = do
  ((_, widget), enctype) <- runFormPost uploadForm
  defaultLayout $ do
    [whamlet|
     <div .container>
       <div .page-header>
         <h1>
           Upload
       <div .row>
         <div .form-group>
           <form method=post enctype=#{enctype} #image-upload-form>
             ^{widget}
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