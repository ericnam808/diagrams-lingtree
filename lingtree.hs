import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Diagrams.Backend.Cairo.Text
import Diagrams.Prelude hiding (e,view)
import Graphics.UI.Gtk
import Graphics.Rendering.Pango
import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Text
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Data.Colour (withOpacity)
import System.IO.Unsafe

txtFont = "Monospace"

-- | Convenient constructor for leaves.
lf :: a -> Tree a
lf a = Node a []

-- get the extent/measurements of the string
extentFont :: String -> IO Double
extentFont str = do
    fontDesc    <- fontDescriptionNew
    fontDescriptionSetFamily fontDesc txtFont
--    fontDescriptionSetSize fontDesc 1.9
    fontMap     <- cairoFontMapGetDefault
    context     <- cairoCreateContext (Just fontMap)
--    contextSetFontDescription context fontDesc
--    fFamilies   <- contextListFamilies context
--    putStrLn    $ show fFamilies
    layout      <- layoutText context str
    (_,(PangoRectangle _ _ width _)) <- layoutGetExtents layout
    return width

-- render the tree with better spacing
renderLingTree :: Monoid' m
           => Double -> (String -> QDiagram b R2 m)
           -> (P2 -> P2 -> QDiagram b R2 m)
           -> Tree (String, P2) -> QDiagram b R2 m
renderLingTree rSep renderNode renderEdge = alignT . centerX . renderTreeR
  where
    renderTreeR (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (map renderTreeR cs)
      <> mconcat (map (renderEdge (ySep (-rSep) p') . ySep rSep . unp2 . snd . rootLabel) cs)
      where ySep n (x,y) = p2 (x,y+n)
            p' = unp2 p

example2 =
  renderLingTree 0.45 (\x -> ((<> rect (2.0 * (xFactor x) + 0.4) 1.0 # lcA (black `withOpacity` 1.0)) . (text # font txtFont # fontSize 0.65)) x)
            ((~~) #lw 0.03)
            (symmLayout' with { slHSep = 1.0, slVSep = 1.4, slWidth = extentX } t4) -- tree as last arg
    where
          --xFactor str = ((fromIntegral (length str)) * 0.38)/2.0 -- real extents contained here
          xFactor str = unsafePerformIO (extentFont str) * 0.12/6.0 -- Escaping IO as a hack for now
          extentX str = (-(xFactor str), xFactor str)


main :: IO ()
main = do
--  test <- extentFont "test"
--  putStrLn (show test)
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  set window [ windowDefaultWidth := 480
             , windowDefaultHeight := 240
             , containerBorderWidth := 8
             , containerChild := canvas ]
  canvas `on` exposeEvent $ (renderFigure canvas)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
  return()


--main = defaultMain example2

renderFigure :: DrawingArea -> EventM EExpose Bool
renderFigure canvas = do
   win <- eventWindow
   liftIO $ (defaultRender canvas example2)
   return True
   
t1 = Node "S" [Node "DP" (map lf ["the","angry","cat"]), Node "VP" [Node "V" (map lf ["bit","the","tiny","rat"])]]
t4 = Node "SSSSSSSS" [Node "DPPPPPP" (map lf ["asdfffff","gsdgah","fkdjlk"]), Node "SSSSSS" [Node "PPPPPPDP" (map lf ["asdfffff","gsdgah","fkdjlk"]), Node "VP" [Node "V" (map lf ["11111111111111s","rrrrrrrrrrrrrrrrrr","flkdajslkffoooooo","fdkljalkfjdlaskfjakljfdkjaskfjdk"])]], Node "VVVP" [Node "V34" (map lf ["11111111111111s","rrrrrrrrrrrrrrrrrr","flkdajslkffoooooo","fdkljalkfjdlaskfjakljfdkjaskfjdk"]), Node "S" [Node "DP" (map lf ["asdfffff","gsdgah","fkdjlk"]), Node "VP" [Node "V" (map lf ["11111111111111s","rrrrrrrrrrrrrrrrrr","flkdajslkffoooooo","fdkljalkfjdlaskfjakljfdkFFFFFFFFFFFFFFFFFjaskfjdk"])]]]]
