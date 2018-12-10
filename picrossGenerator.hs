import Prelude as P
import Graphics.Image as I

loadImage path = do
   image <- readImageY VU path
   let resizeImage = (resize Bilinear Edge (20, 20) image)
   let thresholded = (thresholdWith (PixelY (<0.6)) resizeImage)
   displayImage thresholded