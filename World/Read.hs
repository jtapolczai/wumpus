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
import Data.Maybe (catMaybes)
import Data.Functor.Monadic
import Data.Word
import Math.Geometry.Grid.Square
import System.Directory
import System.IO

import Types
import World

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
readWorld :: forall s.String -> IO (World s)
readWorld dir = do
   topography <- M.fromList . map (second def) . filter (is white.snd) <$> readBitmap (dir ++ "/topography.bmp")
   items <- M.fromList <$> readBitmap (dir ++ "/items.bmp")
   entities <- M.fromList <$> readBitmap (dir ++ "/entities.bmp")
   agents <- M.fromList . catMaybes <$< mapM readAgent' [1..255]

   let cd = M.foldrWithKey (\k v t -> t & ix k %~ addItem v) topography items
       cd' = M.foldrWithKey (\k v t -> t & ix k %~ addEntity agents k v) cd entities

   return $ World (WD 0 Freezing)
                  UnboundedSquareGrid
                  undefined
                  cd'

   where
      readAgent' :: Show a => a -> IO (Maybe (a, Agent s))
      readAgent' i = readAgent (dir ++ "/agent" ++ show i ++ ".txt") >$> (>$> (i,))

      addItem (r,g,b) c = c & meat +~ fromIntegral r & fruit +~ fromIntegral g & gold +~ fromIntegral b

      addEntity :: M.Map Word8 (Agent s) -> CellInd -> Pixel -> CellData s -> CellData s
      addEntity _ i (255,0,0) c = c & entity .~ Wu (Wumpus (WumpusMind undefined i) 1 1)
      addEntity _ _ (0,255,0) c = c & plant .~ Just 1
      addEntity a i (0,0,v) c| v > 0 = c & entity .~ Ag (a M.! v)
      addEntity _ _ _ c = c


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
