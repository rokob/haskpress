{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.Haskpress 
  ( makeSlide
  , nextSlide
  , slideToHtml
  , genSlides
  , renderSlides
  , renderFromFile
  , renderFromFileToFile
  ) where

import qualified Data.ByteString.Lazy as L
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Monoid (mconcat)
import Control.Monad (mzero, liftM)
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Data.Text.Encoding (encodeUtf8)
import System.IO

data Slide = Slide Position Transform Content Id
data Transform = Transform Scale [Rotation]
data Rotation = Rotation Direction String
data Position = Position Int Int Int
data Direction = X | Y | Z deriving (Eq)
type Content = Html
type Scale = Int
type Id = Maybe String

instance FromJSON Slide where
  parseJSON (Object v) = Slide <$>
      (v .: "position") <*>
      (v .:? "transform" .!= defaultTransform) <*>
      liftM (preEscapedToHtml :: String -> Html) (v .: "content") <*>
      (v .:? "id")
  parseJSON _ = mzero

instance FromJSON Position where
  parseJSON (Object v) = Position <$>
    v .: "x" <*>
    v .: "y" <*>
    v .:? "z" .!= 0
  parseJSON _ = mzero

instance FromJSON Transform where
  parseJSON (Object v) = Transform <$>
    v .:? "scale"     .!= defaultScale <*>
    v .:? "rotations" .!= []
  parseJSON _ = mzero

instance FromJSON Rotation where
  parseJSON (Object v) = Rotation <$>
    v .: "direction" .!= Z <*>
    v .: "val"
  parseJSON _ = mzero

instance FromJSON Direction where
  parseJSON (String v) = do
    case v of
      "x" -> return X
      "y" -> return Y
      "z" -> return Z
      _   -> return Z
  parseJSON _ = mzero

defaultPosition :: Position
defaultPosition = Position 0 0 0

offsetPosition :: Position -> Direction -> Int -> Position
offsetPosition (Position x y z) X offset = Position (x + offset) y z
offsetPosition (Position x y z) Y offset = Position x (y + offset) z
offsetPosition (Position x y z) Z offset = Position x y (z + offset)

offsetPosition' :: Position -> Position -> Position
offsetPosition' (Position x y z) (Position x' y' z') = (Position (x+x') (y+y') (z+z'))

positionXY :: Int -> Int -> Position
positionXY x y = offsetPosition (offsetPosition defaultPosition X x) Y y

defaultScale :: Scale
defaultScale = 1

offsetScale :: Scale -> Float -> Scale
offsetScale s offset = round $ fromIntegral s * offset

defaultTransform :: Transform
defaultTransform = Transform defaultScale []

makeSlide :: Position -> Transform -> Content -> Id -> Slide
makeSlide p t c i = Slide p t c i

rotationDirection :: [Rotation] -> Direction -> AttributeValue
rotationDirection [] _ = toValue ("0" :: String)
rotationDirection ((Rotation d r):rs) d'
  | d == d' = toValue r
  | otherwise = rotationDirection rs d'

slideToHtml :: Slide -> Html
slideToHtml (Slide (Position x y z) (Transform scale rotations) content Nothing) =
  H.div ! class_ "step slide"
        ! dataAttribute "x" (toValue x) ! dataAttribute "y" (toValue y) ! dataAttribute "z" (toValue z)
        ! dataAttribute "rotate-x" (rotationDirection rotations X)
        ! dataAttribute "rotate-y" (rotationDirection rotations Y)
        ! dataAttribute "rotate-z" (rotationDirection rotations Z)
        ! dataAttribute "scale" (toValue scale) $ toHtml content
slideToHtml (Slide (Position x y z) (Transform scale rotations) content (Just idString)) =
  H.div ! class_ "step slide" ! A.id (toValue idString)
        ! dataAttribute "x" (toValue x) ! dataAttribute "y" (toValue y) ! dataAttribute "z" (toValue z)
        ! dataAttribute "rotate-x" (rotationDirection rotations X)
        ! dataAttribute "rotate-y" (rotationDirection rotations Y)
        ! dataAttribute "rotate-z" (rotationDirection rotations Z)
        ! dataAttribute "scale" (toValue scale) $ toHtml content

nextSlide :: Slide -> Position -> Float -> [Rotation] -> Content -> Id -> Slide
nextSlide (Slide p (Transform scale _) _ _) p' scale' rotations content id =
  makeSlide (offsetPosition' p p') (Transform (offsetScale scale scale') rotations) content id

combineSlides :: [Slide] -> Html
combineSlides ss = mconcat $ map slideToHtml ss

slidesFromFile :: String -> IO (Maybe [Slide])
slidesFromFile filename = do
  withFile filename ReadMode (\h -> do
    json <- L.hGetContents h
    let slides = decode json
    return slides)

genSlides :: [Slide] -> Html
genSlides ss = docTypeHtml $ do
  H.head $ do
   H.title $ "SLIDE!!!"
   genImpressCSS
  body ! class_ "impress-not-supported" $ do
    H.div ! class_ "fallback-message" $ do
      p $ "Your browser is old! Please get the latest version of Chrome, Safari, or Firefox"
    H.div ! A.id "impress" $ do
      combineSlides ss 
    genImpressScript

genImpressScript :: Html
genImpressScript = do
  script ! src "js/impress.js" $ ""
  script $ "impress().init();"

genImpressCSS :: Html
genImpressCSS = do
  link ! href "css/impress-demo.css" ! rel "stylesheet"

renderSlides :: Html -> L.ByteString
renderSlides = renderHtml

renderFromFile :: String -> IO L.ByteString
renderFromFile filename = do
  slides <- slidesFromFile filename
  case slides of
    Nothing      -> return ""
    Just slides' -> return (renderSlides . genSlides $ slides')

renderFromFileToFile :: String -> String -> IO ()
renderFromFileToFile infile outfile = do
  withFile infile ReadMode (\inHandle -> do
    json <- L.hGetContents inHandle
    let slides = decode json
    case slides of
      Nothing      -> return ()
      Just slides' -> withFile outfile WriteMode (\h -> do
        let htmlString = (renderSlides . genSlides $ slides')
        L.hPut h htmlString))
