{- Graphics.Vega.VegaLite.Theme
Gregory W. Schwartz

Theme for professional publication-quality figures.
-}

{-# LANGUAGE OverloadedStrings #-}

module Graphics.Vega.VegaLite.Theme
  ( theme
  , defaultConfig
  ) where

-- Remote
import Data.Maybe (catMaybes)
import Graphics.Vega.VegaLite
import qualified Data.Text as T

-- Local

data Config = Config { configFontSize :: Maybe Double
                     , configFont :: T.Text
                     , configLabelFont :: T.Text
                     , configAxisColor :: T.Text
                     , configHeight :: Maybe Double
                     , configWidth :: Maybe Double
                     }

defaultConfig :: Config
defaultConfig = Config { configFontSize = Nothing
                       , configFont = "Arial"
                       , configLabelFont = "Arial"
                       , configAxisColor = "#000000"
                       , configHeight = Nothing
                       , configWidth = Nothing
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
viewConfig c = catMaybes
                 [ fmap ViewHeight (configHeight c)  -- 80 for publishing
                 , fmap ViewWidth (configWidth c)  -- 100 for publishing
                 , Just $ ViewStrokeOpacity 0  -- Despine
                 ]

legendConfig :: Config -> [LegendConfig]
legendConfig c = catMaybes
                   [ fmap LeLabelFontSize (configFontSize c)
                   , fmap LeTitleFontSize (configFontSize c)
                   ]

titleConfig :: Config -> [TitleConfig]
titleConfig c = catMaybes
                  [ fmap TFontSize (configFontSize c)
                  , Just $ TFont (configFont c)
                  , Just $ TColor "#000000"
                  , Just $ TFontWeight Normal
                  ]

axisConfig :: Config -> [AxisConfig]
axisConfig c = catMaybes
                 [ Just $ Grid False
                 , Just $ DomainColor "#000000"
                 , Just $ LabelFont (configLabelFont c)
                 , fmap LabelFontSize (configFontSize c)
                 , Just $ LabelAngle 0
                 , Just $ TickColor (configAxisColor c)
                 , Just $ TitleFont (configFont c)
                 , fmap TitleFontSize (configFontSize c)
                 , Just $ TitleFontWeight Normal
                 ]
