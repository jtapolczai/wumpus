{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module World.Read where

import Codec.BMP
import Control.Arrow (second)
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Default
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Functor.Monadic
import qualified Data.Tree as T
import Data.Word
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import System.Directory

import Agent.Intelligent.Affect.Fragments
import Agent.Intelligent
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
   topography <- M.fromList . map (second def) . filter ((==) white.snd) <$> readBitmap (dir ++ "/topography.bmp")
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
                     (makeEntityIndex cd')

   return (world & cellData .~ cd'')

   where
      addEntity' a k v (t, (n:ns)) = (t & ix k %~ addEntity a n k v, ns)
      addEntity' _ _ _ _ = error "addEntity' called with empty list!"

      readAgent' :: Show a => a -> IO (Maybe (a, Agent s))
      readAgent' i = readAgent (dir ++ "/agent" ++ show i ++ ".txt") >$> (>$> (i,))

      addItem (r,g,b) c = c & meat +~ fromIntegral r & fruit +~ fromIntegral g & gold +~ fromIntegral b

      addEntity :: M.Map Word8 (Agent SomeMind) -> Int -> CellInd -> Pixel -> CellData -> CellData
      addEntity _ n i (255,0,0) c = c & entity ?~ Wu (Wumpus (SM $ WumpusMind undefined i) (show n) cDEFAULT_WUMPUS_HEALTH cMAX_AGENT_STAMINA)
      addEntity _ _ _ (0,255,0) c = c & plant .~ Just cPLANT_MAX
      addEntity a _ _ (0,0,v) c| v > 0 = c & entity ?~ Ag (a M.! v)
      addEntity _ _ _ _ c = c


readAgent :: String -> IO (Maybe (Agent s))
readAgent file = do
   ok <- doesFileExist file
   if ok then todo "readAgent mind"
   else return Nothing

readAgents :: String -> IO (M.Map Int (Agent SomeMind))
readAgents dir = readFile (dir ++ "/agents.txt")
                 >$> lines
                 >$> map (mkAgent . map trim . splitOn ";")
                 >$> M.fromList
  where
    mkAgent :: [String] -> (Int, Agent (SomeMind))
    mkAgent [num,a,f,e,c,s,symG,antG] = (read num, go)
      where
        go = defaultAgent $ AS
                num
                (M.fromList [(Anger, (0, personalityFragment Anger a)),
                             (Fear, (0, personalityFragment Fear f)),
                             (Enthusiasm, (0, personalityFragment Enthusiasm e)),
                             (Contentment, (0, personalityFragment Contentment c))])
                (M.empty, M.fromList [(Sympathy, sympathyFragment Sympathy s),
                                      (Trust, def),
                                      (Respect, def)])
                (T.Node (M.empty, M.empty) [])
                []
                []
                (M.fromList [])


    trim :: String -> String
    trim = takeWhile (not.isSpace) . dropWhile isSpace


-- |Creates a default Agent body from an agent
defaultAgent :: AgentState -> Agent SomeMind
defaultAgent as = Agent (as ^. name) North 1 1 (M.fromList [(Fruit,0), (Meat,0), (Gold,0)]) (SM as)

-- |Reads a bitmap from file. The array's coordinates will be x,y.
readBitmap :: String -> IO [((Int,Int),Pixel)]
readBitmap = readBMP >=> (return.toArr.fromRight)
   where
      fromRight (Right r) = r
      fromRight _ = error "readBitmap.fromRight called with Left!"

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
