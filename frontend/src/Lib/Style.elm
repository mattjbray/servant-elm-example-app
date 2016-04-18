module Lib.Style (..) where

import Html
import Material.Style as Style


nestedGridStylesheet : Html.Html
nestedGridStylesheet =
  Style.stylesheet ".mdl-grid.mdl-grid--nested { padding: 0; margin: 0 -8px; }"


nestedGrid : Style.Style
nestedGrid =
  Style.cs "mdl-grid--nested"
