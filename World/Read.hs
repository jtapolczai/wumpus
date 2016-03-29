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
   shuffle,
   ) where

import Prelude hiding (log)

import Codec.BMP
import Control.Arrow (first, second, (***))
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Default
import qualified Data.Foldable as F
import Data.Functor.Monadic
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map.Utils (removeKeys)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Tree as T
import Data.Word
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection(..))
import qualified System.Random as R

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
black = (0,0,0)

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
   entities <- M.fromList . filter ((/=) black.snd) <$> readBitmap (dir ++ "/entities.bmp")
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

   let wt = map (getFilters . view state)
            . mapMaybe (preview _Ag)
            . mapMaybe (view entity . snd)
            . M.toList
            . view cellData
            $ world'

   logF logM (show wt)

   return (world', WMI index)

   where
      addEntity' a k v (t, (n:ns)) = (t & ix k %~ addEntity a n k v, ns)
      addEntity' _ _ _ _ = error "addEntity' called with empty list!"

      addItem (r,g,b) c = c & meat +~ fromIntegral r & fruit +~ fromIntegral g & gold +~ fromIntegral b

      -- adds and entity. Wumpuses will get undefined minds (to avoid an infinite regress!)
      addEntity :: M.Map Word8 (Agent SomeMind, a) -> Int -> CellInd -> Pixel -> CellData -> CellData
      addEntity _ n i (255,0,0) c = c & entity ?~ Wu (Wumpus (SM $ WumpusMind (error "tried to access undefined wumpusMind!") i) ("w" ++ show n) cDEFAULT_WUMPUS_HEALTH cMAX_AGENT_STAMINA)
      addEntity _ _ _ (0,255,0) c = c & plant .~ Just cPLANT_MAX
      addEntity _ _ _ (100,100,0) c = c & pit .~ True
      addEntity a _ _ (0,0,v) c | v > 0 = c & entity ?~ Ag (fst (a M.! v))
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




-- Creating worlds
--------------------------------------------------------------------------------

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

-- |Creates a BMP given pixels and a default color.
makeBitmap :: M.Map (Int, Int) Pixel -> Pixel -> (Int, Int) -> BMP
makeBitmap foreground defColor (w,h) = packRGBA32ToBMP24 w h stream
   where
      background = M.fromList [((x,y), defColor) | y <- [0..h], x <- [0..w]]
      pixels = M.union foreground background

      pixelToWord8 :: Pixel -> [Word8]
      pixelToWord8 (r,g,b) = [r,g,b,0]

      stream :: BS.ByteString
      stream = BS.concat
               $ map (BS.pack . pixelToWord8 . snd)
               $ sortBy (comparing fst)
               $ M.toList pixels

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

-- |The combination of all possible agent personalities. 32 items.
--  The strings are combinations of the 5 personality fragments and the gestures
--  "love" and "hate".
allPersonalities :: [String]
allPersonalities = map (intercalate ";") pers
   where
      psbcFrag = ["w","s"]

      pers = [[a,f,e,c,h,"love","hate"] | a <- psbcFrag,
                                          f <- psbcFrag,
                                          e <- psbcFrag,
                                          c <- psbcFrag,
                                          h <- ["f", "h"]]


-- |Creates populations where each personality occurs N times.
--  The agent strings will be shuffled, consecutively numbered starting with 1,
--  and in the format of an agent file.
makePopulations :: Int -> IO [String]
makePopulations subPopSize = map mkString . zip agentNames <$> shuffle rawList
   where
      agentNames = [1..(length rawList)]
      rawList = concatMap (replicate subPopSize) allPersonalities
      mkString (n,pers) = show n ++ ";" ++ pers

-- |Randomly and uniformly selects elements from a collection
--  and applies a function to them. The update function will be applied N times,
--  where N is the size of the argument list. Each argument will only be used once.
--  
--  The function will be applied at most once to each element.
distributeElems
   :: Ord k
   => [a] -- ^List of arguments to the application function.
   -> (a -> v -> v) -- ^Function to apply to the selected element,
                    --  if it has been selected.
   -> M.Map k v -- ^Collection to which to apply the function.
   -> IO (M.Map k v, [k]) -- ^The modified map and the keys to
                            --  which the function was applied.
distributeElems args appF coll = do
   selectedKeys <- fst <$> takeRandomN (length args) (M.keys coll)
   let (_,coll') = F.foldl' f (args, coll) selectedKeys
       f (a:as, c) k = (as, M.adjust (appF a) k c)
       f ([], _) _ = error "distributeElems: bug!"
   return (coll', selectedKeys)

-- |Applies a function with a certain probability onto each element of a collection.
placeElems
   :: Ord k
   => (v -> v)
   -> Float
   -> M.Map k v
   -> IO (M.Map k v)
placeElems appF prob = mapM f
   where
      f x = do
         (r :: Float) <- R.randomRIO (0,1)
         return $ if r < prob then appF x else x   

-- |Shuffles an array. For every index k and element v in the array A
--  @P(v is put onto i) = 1 / length A@.
--
--  Not very efficient. Don't use this much.
shuffle :: [a] -> IO [a]
shuffle vec = view _1 <$> F.foldlM f ([], vec, length vec - 1) emptyVec
   where 
      emptyVec = replicate (length vec) undefined

      deleteAt :: Int -> [a] -> [a]
      deleteAt i xs = take i xs ++ drop (i+1) xs 

      f (ret, src, maxInd) _ = do
         i <- R.randomRIO (0,maxInd)
         return ((src !! i) : ret, deleteAt i src, maxInd - 1)

-- |Randomly selects N elements from a list. The first part is the list of
--  included elements, the second part is the list of excluded ones.
--
--  Not very efficient. Don't use this much.
takeRandomN :: Int -> [a] -> IO ([a], [a])
takeRandomN n xs = (\ys -> (take n ys, drop n ys)) <$> shuffle xs

-- |Takes a list of cells and populates the corresponding empty world with
--  agents, Wumpuses, plants, pits, and gold.
--
--  Note: don't write the resultant maps directly as bitmaps if the given
--  topology isn't rectangular. The output maps will have the same keys as the
--  input topology.
populateRandomWorld
   :: M.Map (Int, Int) a
   -> [Int] -- ^Agent names (numeric).
   -> Int -- ^Number of Wumpuses.
   -> Int -- ^Number of pits.
   -> Float -- ^Probability of placing gold on a cell; 0 to 1.
   -> Float -- ^Probability of placing a plant on a cell; 0 to 1.
   -> IO (M.Map (Int, Int) Pixel, M.Map (Int, Int) Pixel) -- ^The entity-map and item-map.
populateRandomWorld topology agents numWumpuses numPits goldProb plantProb = do
   let t1 = fmap (const (0,0,0)) topology
   (withAgents,   t2) <- remFrom t1 <$> distributeElems agents addAgent    t1
   (withWumpuses, t3) <- remFrom t2 <$> distributeElems wumpuses addWumpus t2
   (withPits,     t4) <- remFrom t3 <$> distributeElems pits addPit        t3
   (withPlants,   t5) <- remFrom t4 <$> distributeElems plants addPlant    t4
   withGold           <-                placeElems addGold goldProb        t5
   let entityMap = t1
                   `merge` withAgents
                   `merge` withWumpuses
                   `merge` withPits
                   `merge` withPlants
       itemMap = t1 `merge` withGold
   return (entityMap, itemMap)
   where
      wumpuses = replicate numWumpuses undefined
      pits = replicate numPits undefined
      plants = replicate (floor $ fromIntegral (M.size topology) * plantProb) undefined

      addAgent x _ = (0,0,fromIntegral x)
      addWumpus _ _ = (255,0,0)
      addPlant _ _ = (0,255,0)
      addPit _ _ = (100,100,0)
      addGold _ = (0,0,1)

      remFrom t (x,keys) = (x, removeKeys keys t)

      higherValue (0,0,0) r = r
      higherValue l _ = l

      infixl 5 `merge`
      merge = M.unionWith higherValue
