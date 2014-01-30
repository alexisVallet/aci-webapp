{-|
Handler for a single image, showing the image, title, description, tags, and allows
the uploader to modify everything.
-}
module Handler.Image where

import Text.Julius (rawJS)
import Yesod.Auth
import Database.Esqueleto
import Control.Arrow

import Import hiding ((==.), delete, (=.), update)
import Handler.Tags

modifyForm :: Image -> [(String,String)] -> Html -> MForm Handler (FormResult (Text, Maybe Textarea, [(String,String)]), Widget)
modifyForm image tags html = do
  (rtitle, vtitle) <- mreq textField "" (Just $ imageTitle image)
  (rdescription, vdescription) <- mopt textareaField "" (Just $ imageDescription image)
  (rtags, vtags) <- mreq tagsField "" (Just tags)
  return ((,,) <$> rtitle <*> rdescription <*> rtags, do
    master <- handlerToWidget getYesod
    viewClass <- newIdent
    editClass <- newIdent
    editButtonClass <- newIdent
    -- Deals with hiding and showing when going into or out of edit mode
    toWidget [julius|
      $(".#{rawJS editClass}").addClass("hidden");
      editMode = false;
      $(".#{rawJS editButtonClass}").on("click", function(){
        if (editMode) {
          editMode = false;
          $(".#{rawJS editClass}").addClass("hidden");
          $(".#{rawJS viewClass}").removeClass("hidden");
        } else {
          editMode = true;
          $(".#{rawJS editClass}").removeClass("hidden");
          $(".#{rawJS viewClass}").addClass("hidden");
        }
      });
      $(".#{rawJS editClass}>*").addClass("form-control");
    |]
    [whamlet|
      #{html}
      <div .container>
        <h1 .#{viewClass}>#{imageTitle image}
        <div .#{editClass} .form-group>
          ^{fvInput vtitle}
        <img src=#{relativeToAbsolute master $ imageFilename image} .img-thumbnail .img-responsive>
        $maybe description <- imageDescription image
          <p .#{viewClass}>#{description}
        <div .#{editClass} .form-group>
          ^{fvInput vdescription}
        <h2>Tags
        <p .#{viewClass}>
          #{tagsToText tags}
        <div .#{editClass} .form-group>
          ^{fvInput vtags}
        <p> Uploaded on #{show $ imageUploadTime image}
        <button type=button .btn .btn-default .#{editButtonClass} .#{viewClass}>
          Edit
        <button .#{editClass} type=submit .btn .btn-primary>Submit
        <button .#{editClass} .#{editButtonClass} type=button .btn .btn-danger>Cancel
    |])

-- | Shows the image, its title, description, tags, upload date, and allows the 
-- uploader to modify the image info and tags.
getImageR :: ImageId -> Handler Html
getImageR imageId = do
  mImage <- runDB $ get imageId
  case mImage of
    Nothing -> defaultLayout $ [whamlet|
      <p>This image doesn't exist!
    |]
    Just image -> do
      tags <- runDB $ 
              select $
              from $ \(imgTagLink `InnerJoin` tag `InnerJoin` cat) -> do
                      on (cat ^. TagCategoryId ==. tag ^. TagTagCategory)
                      on (tag ^. TagId ==. imgTagLink ^. ImageTagLinkTagId)
                      where_ (imgTagLink ^. ImageTagLinkImageId ==. val imageId)
                      return (cat,tag)
      let tagsStrings =
            map 
            (\(Entity _ cat, Entity _ tag) -> 
                 (unpack $ tagCategoryName cat, unpack $ tagName tag))
            tags
      ((_,modifyWidget),enctype) <- runFormPost $ modifyForm image tagsStrings
      defaultLayout [whamlet|
        <form method=post enctype=#{enctype} #image-modificcation-form>
          ^{modifyWidget}
      |]

postImageR :: ImageId -> Handler Html
postImageR imageId = do
  mImage <- runDB $ get imageId
  case mImage of
    Nothing -> redirect $ ImageR imageId
    Just image -> do
      tags <- fmap (map (unpack . tagCategoryName . entityVal ***
                         unpack . tagName . entityVal))
              $ runDB 
              $ select
              $ from $ \(imgTagLink `InnerJoin` tag `InnerJoin` cat) -> do
                on (tag ^. TagTagCategory ==. cat ^. TagCategoryId)
                on (imgTagLink ^. ImageTagLinkTagId ==. tag ^. TagId)
                where_ (imgTagLink ^. ImageTagLinkImageId ==. val imageId)
                return (cat, tag)
      ((result,_),_) <- runFormPost $ modifyForm image tags
      case result of
        FormSuccess (title, description, listOfTags) -> do
          muser <- maybeAuth
          case muser of
            Nothing -> do
              setMessage "You must be logged in to upload images."
              redirect $ AuthR LoginR
            Just (Entity _ _) -> do
              -- if successful and logged in, modify image and tags adequately
              runDB $ do
                update $ \img -> do
                  set img [ImageTitle =. val title
                          ,ImageDescription =. val description]
                  where_ (img ^. ImageId ==. val imageId)
                -- simply delete all tags associated to the image then runs
                -- the tag writing procedure again
                delete $ from $ \imgTagLink -> do
                  where_ (imgTagLink ^. ImageTagLinkImageId ==. val imageId)
                writeTags imageId listOfTags
              setMessage "Image successfully modified"
              redirect $ ImageR imageId
        _ -> do
          setMessage "Image modification failed for unknown reasons."
          redirect $ ImageR imageId