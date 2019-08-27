{- Graphics.Vega.VegaLite.Theme
Gregory W. Schwartz

Theme for professional publication-quality figures.
-}

{-# LANGUAGE OverloadedStrings #-}

module Theme
  ( theme
  ) where

-- Remote
import Graphics.Vega.VegaLite.Theme
import qualified Data.Text as T

-- Local

data Config = Config { configFontSize :: Double
                     , configFont :: T.Text
                     , configLabelFont :: T.Text
                     , configAxisColor :: T.Text
                     , configHeight :: Double
                     , configWidth :: Double
                     }

defaultConfig :: Config
defaultConfig = Config { configFontSize = 12
                       , configFont = "Arial"
                       , configLabelFont = "Arial"
                       , configAxisColor = "#000000"
                       , configHeight = 300
                       , configWidth = 400
                       }

theme :: Config -> [LabelledSpec] -> (VLProperty, VLSpec)
theme c = configure
        . configuration (View $ viewConfig c)
        . configuration (Legend $ legendConfig c)
        . configuration (TitleStyle $ titleConfig c)
        . configuration (Axis $ axisConfig c)
        . configuration (AxisX $ axisConfig c)
        . configuration (AxisY $ axisConfig c)

viewConfig :: Config -> [ViewConfig]
viewConfig c = [ ViewHeight (configHeight c)  -- 80 for publishing
               , ViewWidth (configWidth c)  -- 100 for publishing
               , StrokeOpacity 0  -- Despine
               ]

legendConfig :: Config -> [LegendConfig]
legendConfig c = [ LeLabelFontSize (configFontSize c)
                 , LeTitleFontSize (configFontSize c)
                 ]

titleConfig :: Config -> [TitleConfig]
titleConfig c = [ TFontSize (configFontSize c)
                , TFont (configFont c)
                , TColor "#000000"
                , TFontWeight Normal
                ]

axisConfig :: Config -> [AxisConfig]
axisConfig c = [ Grid False
               , DomainColor "#000000"
               , LabelFont (configLabelFont c)
               , LabelFontSize (configFontSize c)
               , LabelAngle 0
               , TickColor (configAxisColor c)
               , TitleFont (configFont c)
               , TitleFontSize (configFontSize c)
               , TitleFontWeight Normal
               ]
