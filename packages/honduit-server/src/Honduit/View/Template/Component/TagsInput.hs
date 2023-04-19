module Honduit.View.Template.Component.TagsInput where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Lucid.Alpine.Extension as Lucid.Alpine

data Props = Props
  { tags :: [Text]
  , name :: Text
  , attributes :: [Lucid.Attribute]
  }

render :: (Monad m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    [ Lucid.classes_
      [ 
      ]
    , Lucid.Alpine.xData_ $ fold
      [ "{"
      , Text.intercalate ","
        [ "tags: " <> fold
          [ "["
          , Text.intercalate "," $ props.tags <&> \tag -> "'" <> tag <> "'"
          , "]"
          ]
        , "name: " <> "'" <> props.name <> "'"
        , "input: ''"
        ]
      , "}"
      ]
    ]
    do
    Lucid.template_
      [ Lucid.Alpine.xFor_ "tag in tags"
      ] do
      Lucid.input_
        ( [ Lucid.type_ "hidden"
          , Lucid.Alpine.xBind_ "name" "name"
          , Lucid.Alpine.xBind_ "value" "tag"
          ] `Lucid.concatAttributes`
          props.attributes
        )
    Lucid.input_
      [ Lucid.classes_
        [ "form-control"
        ]
      , Lucid.placeholder_ "Enter tags"
      , Lucid.Alpine.xModel_ [] "input"
      , Lucid.Alpine.xOn_ "keydown.enter.prevent" "if (!!input.trim() && !tags.includes(input.trim())) { tags.push(input.trim()); input = '' }"
      ]
    Lucid.div_
      [ Lucid.classes_
        [ "tag-list"
        ]
      ] do
      Lucid.template_
        [ Lucid.Alpine.xFor_ "tag in tags"
        , Lucid.Alpine.key_ "tag"
        ] do
        Lucid.span_
          [ Lucid.classes_
            [ "tag-default"
            , "tag-pill"
            ]
          ] do
          Lucid.i_
            [ Lucid.classes_
              [ "ion-close-round"
              ]
            , Lucid.Alpine.xOn_ "click" "tags = tags.filter(i => i !== tag)"
            ] do
            mempty
          Lucid.span_
            [ Lucid.Alpine.xText_ "tag"
            ] do
            mempty
