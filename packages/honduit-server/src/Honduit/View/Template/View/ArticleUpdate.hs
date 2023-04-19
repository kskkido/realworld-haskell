module Honduit.View.Template.View.ArticleUpdate where

import RIO
import qualified Lucid
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.ArticleEditorForm as Capability.ArticleEditorForm
import qualified Honduit.View.Template.Component.ArticleEditorForm as ArticleEditorForm

data Props = Props
  { values :: Type.ArticleEditorForm
  , errors :: Type.ArticleEditorFormValidationRule
  }

render :: (Monad m, Capability.ArticleEditorForm.ArticleEditorForm m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_ [Lucid.classes_ ["editor-page"]] do
    Lucid.div_ [Lucid.classes_ ["container", "page"]] do
      Lucid.div_
        [ Lucid.classes_ ["row"]
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "col-md-10"
            , "offset-md-1"
            , "col-xs-12"
            ]
          ] do
          ArticleEditorForm.render $ ArticleEditorForm.Props
            { values = props.values
            , errors = props.errors
            , attributes =
              [ Lucid.id_ "article_create_form"
              ]
            }
