{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module World.Read (
   readWorld,
   readAgents,
   defaultAgent,
   readBitmap,
   generateAgentLine,
   generateAgents,
   generateAgentsFile,
   ) where

import Codec.BMP
import Control.Arrow (second, (***))
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Default
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Functor.Monadic
import qualified Data.Tree as T
import Data.Word
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))

import Agent.Intelligent.Affect.Fragments
import Agent.Intelligent()
import Agent.Intelligent.Utils (choose)
import Agent.Wumpus
import Types
import World.Constants
import World.Utils

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "World.Read"

-- |An RGB pixel.
type Pixel = (Word8,Word8,Word8)

white = (255,255,255)

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
--  * agents.txt - a CSV, separated by ';', with 8 columns. The first is the
--                 agent's name (word8), the next 4 are weak/strong, standing
--                 for the four emotions (anger, fear, enthusiasm, contentment).
--                 the next one hostile/friendly (w.r.t sympathy), and the last two
--                 are the friendly and hostile gesture.
readWorld :: String -> IO (World, WorldMetaInfo)
readWorld dir = do
   topography <- M.fromList . map (second def) . filter ((==) white.snd) <$> readBitmap (dir ++ "/topography.bmp")
   items <- M.fromList <$> readBitmap (dir ++ "/items.bmp")
   entities <- M.fromList <$> readBitmap (dir ++ "/entities.bmp")
   agents <- readAgents dir

   logF traceM "topography"
   readBitmap (dir ++ "/topography.bmp") >>= (return. show) >>= logF traceM

   logF traceM "items"
   readBitmap (dir ++ "/items.bmp") >>= (return. show) >>= logF traceM

   logF traceM "entities"
   readBitmap (dir ++ "/entities.bmp") >>= (return. show) >>= logF traceM

   logF traceM "--------------------------------------------------"

   let -- create topography
       cd = M.foldrWithKey (\k v t -> t & ix k %~ addItem v) topography items
       -- add entities
       cd' = fst $ M.foldrWithKey (addEntity' agents) (cd,[1..]) entities
       -- add minds to the wumpuses
       cd'' = M.mapWithKey (\i c -> c & entity . _Just . _Wu . state %~ pullMessages world i) cd'

       -- |Adds a bidirectional edge to m if its target @to@ is an existing cell.
       addEdge from to m = if M.member to topography then
            M.insert (to,head $  getDirections to from) def
            $ M.insert (from, head $ getDirections from to) def m
         else m

       -- |Adds edges to a cell's horizontal and vertical predecessor.
       addEdges i@(x,y) _ m = addEdge i (x-1,y) $ addEdge i (x,y-1) m

       -- |We go through all cells and try to add edges to their neighbors.
       --  Each cell only adds edges to its (at most) 2 predecessors: the
       --  cell directly to the left and the cell directly above.
       edges = M.foldrWithKey addEdges M.empty topography

       entityIndex = makeEntityIndex cd'

       world = BaseWorld
          (WD cSTART_TIME $ light' cSTART_TIME)
          UnboundedSquareGrid
          edges
          cd'
          entityIndex
       
       -- the index of agent personalities, for the WorldMetaInfo object
       index = M.intersectionWith (const id) entityIndex
               $ M.fromList $ fmap (show *** snd) $ M.toList agents
       -- the finished world in which we give minds to the Wumpuses
       world' = world & cellData .~ cd''

   logF traceM (show world')

   logF traceM "intersected agent list:"
   logF traceM (show index)

   return (world', WMI index)

   where
      addEntity' a k v (t, (n:ns)) = (t & ix k %~ addEntity a n k v, ns)
      addEntity' _ _ _ _ = error "addEntity' called with empty list!"

      addItem (r,g,b) c = c & meat +~ fromIntegral r & fruit +~ fromIntegral g & gold +~ fromIntegral b

      -- adds and entity. Wumpuses will get undefined minds (to avoid an infinite regress!)
      addEntity :: M.Map Word8 (Agent SomeMind, a) -> Int -> CellInd -> Pixel -> CellData -> CellData
      addEntity _ n i (255,0,0) c = c & entity ?~ Wu (Wumpus (SM $ WumpusMind (error "tried to access undefined wumpusMind!") i) ("w" ++ show n) cDEFAULT_WUMPUS_HEALTH cMAX_AGENT_STAMINA)
      addEntity _ _ _ (0,255,0) c = c & plant .~ Just cPLANT_MAX
      addEntity a _ _ (0,0,v) c| v > 0 = c & entity ?~ Ag (fst (a M.! v))
      addEntity _ _ _ _ c = c


readAgents :: String -> IO (M.Map Word8 (Agent SomeMind, AgentIndex))
readAgents dir = readFile (dir ++ "/agents.txt")
                 >$> lines
                 >$> map (mkAgent . map trim . splitOn ";")
                 >$> M.fromList
   where
      mkAgent :: [String] -> (Word8, (Agent SomeMind, AgentIndex))
      mkAgent [num,a,f,e,c,s,symG,antG] = (read num, (go, index))
         where
            index = (psbcFragmentType a,
                     psbcFragmentType f,
                     psbcFragmentType e,
                     psbcFragmentType c,
                     sjsFragmentType s)

            go = let
               gest = (M.fromList [((Sympathy, Positive), symG),
                                  ((Sympathy, Negative), antG)])
               in
                  defaultAgent $ AS
                     num
                     (M.fromList [(Anger, (0, personalityFragment Anger a)),
                                  (Fear, (0, personalityFragment Fear f)),
                                  (Enthusiasm, (0, personalityFragment Enthusiasm e)),
                                  (Contentment, (0, personalityFragment Contentment c))])
                     (M.empty, M.fromList [(Sympathy, sympathyFragment Sympathy s gest),
                                           (Trust, def),
                                           (Respect, def)])
                     (T.Node (BaseWorld
                                (WD cSTART_TIME $ light' cSTART_TIME)
                                UnboundedSquareGrid
                                M.empty
                                M.empty
                                M.empty) [])
                     []
                     []
                     gest
                    
      mkAgent _ = error "mkAgent: incorrect number of fields!"


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

-- |Randomly generates a line that can be used in an agents-file in CSV-format.
generateAgentLine :: Word8 -> IO String
generateAgentLine w = do
   [a,f,e,c] <- sequence $ replicate 4 (choose ["weak", "strong"])
   s <- choose ["friendly", "hostile"]
   let symG = "love"
       antG = "hate"
   return $ intercalate ";" [show w, a,f,e,c,s,symG,antG]

-- |Creates a given number of agent-lines.
generateAgents :: Word8 -> IO [String]
generateAgents n = mapM generateAgentLine [1..n]

-- |Generates an agent-file with the given name and the given number of agents.
generateAgentsFile :: Word8 -> String -> IO ()
generateAgentsFile n fn = generateAgents n >>= writeFile fn . unlines
