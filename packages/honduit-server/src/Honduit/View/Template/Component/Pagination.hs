module Honduit.View.Template.Component.Pagination where

import RIO
import qualified RIO.List as List
import qualified Lucid
import qualified Lucid.Svg
import qualified Lucid.Extension as Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Maybe as Maybe
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Paginator as Paginator

data Props = Props
  { metadata :: Type.PaginationMetadata
  , length :: Int
  , lookaround :: Int
  , toPath :: Type.PaginationMetadata -> Text
  , attributes :: [Lucid.Attribute]
  }

render :: (Monad m, Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.ul_
    ( [ Lucid.classes_
        [ "pagination"
        ]
      ] `Lucid.concatAttributes` props.attributes
    )
    do
      paginated <- lift do
        Paginator.paginate props.length props.lookaround props.metadata
      do
        void $ Maybe.runMaybeT do
          minimum <- Maybe.MaybeT $ pure do
            block <- List.headMaybe paginated
            List.headMaybe block
          let next = props.metadata { Type.page = props.metadata.page - 1 } :: Type.PaginationMetadata
          guard (next.page >= minimum)
          lift do
            Lucid.li_
              [ Lucid.classes_
                [ "page-item"
                ]
              ] do
              Lucid.button_
                [ Lucid.Htmx.hxGet_ $ props.toPath next
                , Lucid.classes_
                  [ "page-link"
                  , "relative"
                  , "inline-flex"
                  , "items-center"
                  , "rounded-l-md"
                  , "px-2"
                  , "py-2"
                  , "text-gray-400"
                  , "ring-1"
                  , "ring-inset"
                  , "ring-gray-300"
                  , "hover:bg-gray-50"
                  , "focus:z-20"
                  , "focus:outline-offset-0"
                  ] 
                ]
                do
                  Lucid.Svg.svg_
                    [ Lucid.Svg.viewBox_ "0 0 20 20"
                    , Lucid.Svg.fill_ "currentColor"
                    , Lucid.classes_
                      [ "h-5"
                      , "w-5"
                      ]
                    ]
                    do
                      Lucid.Svg.path_
                        [ Lucid.Svg.fill_rule_ "evenodd"
                        , Lucid.Svg.clip_rule_ "evenodd"
                        , Lucid.Svg.d_ "M12.79 5.23a.75.75 0 01-.02 1.06L8.832 10l3.938 3.71a.75.75 0 11-1.04 1.08l-4.5-4.25a.75.75 0 010-1.08l4.5-4.25a.75.75 0 011.06.02z"
                        ]
      do 
        fold $ List.intercalate
          [ Lucid.li_
              [ Lucid.classes_
                [ "page-item"
                ]
              ]
              do
              Lucid.span_
                [ Lucid.classes_
                  [ "page-link"
                  , "inline-flex"
                  , "items-center"
                  , "px-4"
                  , "py-2"
                  , "text-sm"
                  , "font-semibold"
                  , "text-gray-700"
                  , "ring-1"
                  , "ring-inset"
                  , "ring-gray-300"
                  , "focus:outline-offset-0"
                  ]
                ] do
                Lucid.toHtml ("..." :: String)
          ]
          ( paginated <&> fmap \slot -> do
              let next = props.metadata { Type.page = slot } :: Type.PaginationMetadata
              Lucid.li_
                [ Lucid.classes_ $ 
                  [ "page-item"
                  ] <> Maybe.catMaybes
                  [ do
                      guard $ props.metadata.page == slot
                      pure "active"
                  ]
                ] do
                Lucid.button_
                  [ Lucid.Htmx.hxGet_ $ props.toPath next
                  , Lucid.classes_ $
                    [ "page-link"
                    , "relative"
                    , "inline-flex"
                    , "items-center"
                    , "px-4"
                    , "py-2"
                    , "text-sm"
                    , "font-semibold"
                    , "text-gray-900"
                    , "ring-1"
                    , "ring-inset"
                    , "ring-gray-300"
                    , "hover:bg-gray-50"
                    , "focus:z-20"
                    , "focus:outline-offset-0"
                    ]
                  ]
                  do
                    Lucid.toHtml (show $ slot + 1)
          )
      do
        void $ Maybe.runMaybeT do
          maximum <- Maybe.MaybeT $ pure do
            block <- List.lastMaybe paginated
            List.lastMaybe block
          let next = props.metadata { Type.page = props.metadata.page + 1 } :: Type.PaginationMetadata
          guard (next.page <= maximum)
          lift do
            Lucid.li_
              [ Lucid.classes_
                [ "page-item"
                ]
              ] do
              Lucid.button_
                [ Lucid.Htmx.hxGet_ $ props.toPath next
                , Lucid.classes_
                  [ "page-link"
                  , "relative"
                  , "inline-flex"
                  , "items-center"
                  , "rounded-r-md"
                  , "px-2"
                  , "py-2"
                  , "text-gray-400"
                  , "ring-1"
                  , "ring-inset"
                  , "ring-gray-300"
                  , "hover:bg-gray-50"
                  , "focus:z-20"
                  , "focus:outline-offset-0"
                  ]
                ]
                do
                  Lucid.Svg.svg_
                    [ Lucid.Svg.viewBox_ "0 0 20 20"
                    , Lucid.Svg.fill_ "currentColor"
                    , Lucid.classes_
                      [ "h-5"
                      , "w-5"
                      ]
                    ]
                    do
                      Lucid.Svg.path_
                        [ Lucid.Svg.fill_rule_ "evenodd"
                        , Lucid.Svg.clip_rule_ "evenodd"
                        , Lucid.Svg.d_ "M7.21 14.77a.75.75 0 01.02-1.06L11.168 10 7.23 6.29a.75.75 0 111.04-1.08l4.5 4.25a.75.75 0 010 1.08l-4.5 4.25a.75.75 0 01-1.06-.02z"
                        ]

