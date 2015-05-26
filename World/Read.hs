{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module World.Read where

import Codec.BMP
import Control.Arrow (second)
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Default
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.Functor.Monadic
import Data.Word
import Math.Geometry.Grid.Square
import System.Directory

import Agent.Wumpus
import Types
import World.Constants
import World.Utils

-- |An RGB pixel.
type Pixel = (Word8,Word8,Word8)

white = (255,255,255)
black = (0,0,0)
red = (255,0,0)
green = (0,255,0)

getRed :: Pixel -> Word8
getRed (r,_,_) = r

is :: Eq a => a -> a -> Bool
is = (==)

-- |Takes a world bitmap with bitmaps and reads a world.
--
--  The following files have to be present:
--
--  * topography.bmp - @#000000@ means an absent cell, @#FFFFFF@ means a present one.
--  * items.bmp - The value of the red channel (0-255) represents the amount of
--                meat, the value of the green channel: the amount of fruit,
--                the value of the blue channel: the amount of gold.
--  * entities.bmp - @#FF0000@ represents a Wumpus, @#00FF00@ an plant,
--                   @#000001@ to @#0000FF@ an agent, with the value of the
--                   blue channel being the agent's ID. The agent's mind can
--                   can be stored in an @agent_ID.txt@ file.
--                   @#646400@ represents a pit.
readWorld :: String -> IO World
readWorld dir = do
   topography <- M.fromList . map (second def) . filter (is white.snd) <$> readBitmap (dir ++ "/topography.bmp")
   items <- M.fromList <$> readBitmap (dir ++ "/items.bmp")
   entities <- M.fromList <$> readBitmap (dir ++ "/entities.bmp")
   agents <- M.fromList . catMaybes <$< mapM readAgent' [1..255]

   let -- create topography
       cd = M.foldrWithKey (\k v t -> t & ix k %~ addItem v) topography items
       -- add entities
       cd' = fst $ M.foldrWithKey (addEntity' agents) (cd,[1..]) entities
       -- add minds to the wumpuses
       cd'' = M.mapWithKey (\i c -> c & entity . _Just . _Wu . state %~ pullMessages world i) cd'

       --

       -- |Adds a bidirectional edge to m if its target @to@ is an existing cell.
       addEdge from to m = if M.member to topography then
            M.insert (to, getDirection to from) def
            $ M.insert (from, getDirection from to) def m
         else m

       -- |Adds edges to a cell's horizontal and vertical predecessor.
       addEdges i@(x,y) _ m = addEdge i (x-1,y) $ addEdge i (x,y-1) m

       -- |We go through all cells and try to add edges to their neighbors.
       --  Each cell only adds edges to its (at most) 2 predecessors: the
       --  cell directly to the left and the cell directly above.
       edges = M.foldrWithKey addEdges M.empty topography

       world = World (WD cSTART_TIME $ light' cSTART_TIME)
                     UnboundedSquareGrid
                     edges
                     cd'
                     (indexEntities cd')

   return (world & cellData .~ cd'')

   where
      addEntity' a k v (t, (n:ns)) = (t & ix k %~ addEntity a n k v, ns)

      readAgent' :: Show a => a -> IO (Maybe (a, Agent s))
      readAgent' i = readAgent (dir ++ "/agent" ++ show i ++ ".txt") >$> (>$> (i,))

      addItem (r,g,b) c = c & meat +~ fromIntegral r & fruit +~ fromIntegral g & gold +~ fromIntegral b

      addEntity :: M.Map Word8 (Agent SomeMind) -> Int -> CellInd -> Pixel -> CellData -> CellData
      addEntity _ n i (255,0,0) c = c & entity ?~ Wu (Wumpus (SM $ WumpusMind undefined i) (show n) cDEFAULT_WUMPUS_HEALTH cMAX_AGENT_STAMINA)
      addEntity _ _ _ (0,255,0) c = c & plant .~ Just cPLANT_MAX
      addEntity a _ _ (0,0,v) c| v > 0 = c & entity ?~ Ag (a M.! v)
      addEntity _ _ _ _ c = c

      indexEntities :: M.Map CellInd CellData -> M.Map EntityName CellInd
      indexEntities = M.foldrWithKey
         (\k cd -> if isJust (cd ^? entity)
                   then M.insert (cd ^. ju entity . name) k
                   else id) M.empty


readAgent :: String -> IO (Maybe (Agent s))
readAgent file = do
   ok <- doesFileExist file
   if ok then todo "readAgent mind"
   else return Nothing

-- |Reads a bitmap from file. The array's coordinates will be x,y.
readBitmap :: String -> IO [((Int,Int),Pixel)]
readBitmap = readBMP >=> (return.toArr.fromRight)
   where
      fromRight (Right r) = r

      height (InfoV5 i) = dib3Height $ dib4InfoV3 $ dib5InfoV4 i
      height (InfoV4 i) = dib3Height $ dib4InfoV3 i
      height (InfoV3 i) = dib3Height i

      width (InfoV5 i) = dib3Width $ dib4InfoV3 $ dib5InfoV4 i
      width (InfoV4 i) = dib3Width $ dib4InfoV3 i
      width (InfoV3 i) = dib3Width i

      toArr b = pixels 0 (w*h) (unpackBMPToRGBA32 b)
         where
            (w,h) = bmpDimensions b

            -- converts an index to an (x,y) coordinate
            toCoord :: Int -> (Int, Int)
            toCoord i = (i `mod` w, i `div` h)

            (!) = BS.index

            pixels _ 0 _ = []
            pixels i n xs = (toCoord i, (xs ! 0, xs ! 1, xs ! 2))
                            : pixels (i+1) (n-1) (BS.drop 4 xs)
