{-|
Handler for the image upload page. Can only be accessed by a logged user.
-}
module Handler.Upload where

import Yesod.Form
import Yesod.Auth
import Text.Julius (rawJS)
import Control.Monad
import Data.Text.Encoding (decodeUtf8)
import System.IO (openTempFileWithDefaultPermissions, hClose)
import Data.Time.Clock (getCurrentTime)

import Import
import Handler.Tags

-- | Directory where the images are uploaded relative to the root of the project.
uploadDirectory :: FilePath
uploadDirectory = "static/images" -- screw windows

-- | PNG mime content type converted to the Text datatype. Put at the top level
-- so it's automatically memoized.
typePngText :: Text
typePngText = decodeUtf8 typePng

-- | Form for image upload. Allows the user to select a png image, set a title and
-- an optional description of the image.
uploadForm :: Html -> MForm Handler (FormResult (FileInfo, Text, Maybe Textarea, [(String, String)]), Widget)
uploadForm html = do
  -- Custom file field with validation to only accept .png files.
  -- This is because our algorithm expects the background to be already
  -- removed, and we use transparency information for this.
  let
    isPng fileInfo = fileContentType fileInfo == typePngText
  (rfile, vfile) <- mreq (checkBool isPng ("The image must be a png file." :: Text) fileField) "Image" Nothing
  (rtitle, vtitle) <- mreq textField "Title" Nothing
  (rdescription, vdescription) <- mopt textareaField "Description" Nothing
  (rtags, vtags) <- mreq tagsField "Tags" Nothing
  let vs = [vtitle, vdescription, vtags]
  return ((,,,) <$> rfile <*> rtitle <*> rdescription <*> rtags, do
    -- Rendering image input with thumbnail as well as title, description fields
    addScript $ StaticR js_holder_js
    inputIds <- forM ([1..3] :: [Int]) $ const newIdent
    divIds <- forM ([1..3] :: [Int]) $ const newIdent
    let vAndIds = zip (zip vs inputIds) divIds
    forM_ vAndIds $ \((_,inputId),divId) -> toWidget $ do
      [julius|
        $("##{rawJS divId}>*").attr("id","#{rawJS inputId}");
        $("##{rawJS divId}>*").addClass("form-control");
      |]
    [whamlet|
      #{html}
      <div role=form>
        $maybe tt <- fvTooltip vfile
          <div .alert .alert-info>#{tt}
        $maybe err <- fvErrors vfile
          <div .alert .alert-danger>#{err}
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
             $maybe err <- fvErrors v
               <div .alert .alert-danger>#{err}
             <div ##{divId}>
               ^{fvInput v}
        <button type=submit .btn .btn-default>Submit
    |])

-- | Upload page handler, allows logged in users to upload images with titles
-- and descriptions.
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

-- | Handler for the result of the form submission.
postUploadR :: Handler Html
postUploadR = do
  ((result, _), _) <- runFormPost uploadForm
  case result of
    FormSuccess (file,title,description,listOfTags) -> do
      muser <- maybeAuth
      case muser of
        Nothing -> do
          setMessage "You must be logged in to upload images."
          redirect $ AuthR LoginR
        Just (Entity key _) -> do
          -- If successful and logged in, upload image file
          filename <- writeToServer file
          uploadTime <- liftIO $ getCurrentTime
          imageId <- runDB $ do
            imageId' <- insert (Image filename title description key uploadTime)
            -- Set the tags for the image.
            -- running all tag additions in a single transaction to avoid
            -- issues. Adds new tag categories and tags when necessary.
            writeTags imageId' listOfTags
            return imageId'
          setMessage "Image successfully uploaded"
          redirect $ ImageR imageId
      -- otherwise, display the error message and go back to the form        
    _ -> do
      setMessage "Upload failed for unknown reasons."
      redirect UploadR

-- | Writes the file to server under a unique file name.
writeToServer :: FileInfo -> Handler FilePath
writeToServer file = liftIO $ do
  -- Hijacking the temp file functions from the base package to generate a unique
  -- file name, although the file isn't temporary at all. Closing the handle right
  -- away.
  (path,handle) <-
      openTempFileWithDefaultPermissions uploadDirectory (unpack $ fileName file)
  hClose handle
  fileMove file path
  return path
