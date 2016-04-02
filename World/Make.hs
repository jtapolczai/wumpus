{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module World.Make (
   makeBitmap,
   generateAgentLine,
   generateAgents,
   generateAgentsFile,
   allPersonalities,
   makePopulations,
   shuffle,
   takeRandomN,
   distributeElems,
   placeElems,
   populateRandomWorld,
   makeRandomWorld,
   makeEvalWorld,
   ) where

import Prelude hiding (log)

import Codec.BMP
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import qualified Data.Foldable as F
import Data.List (intercalate, sortBy)
import qualified Data.Map as M
import Data.Map.Utils (removeKeys)
import Data.Ord (comparing)
import Data.Word
import System.FilePath
import qualified System.Random as R
import System.Random.Utils

import World.Read

import Debug.Trace.Wumpus

-- Module-specific logging function.
logF :: (String -> a) -> a
logF f = f "World.Make"

-- |An RGB pixel.
type Pixel = (Word8,Word8,Word8)

-- Creating worlds
--------------------------------------------------------------------------------

-- |Creates a BMP given pixels and a default color.
makeBitmap :: M.Map (Int, Int) Pixel -> Pixel -> (Int, Int) -> BMP
makeBitmap foreground defColor (w,h) =
   logF trace ("[makeBitmap] dimensions=" ++ show (w,h))
   $ logF trace ("[makeBitmap] expected number of pixels=" ++ show (w*h))
   $ logF trace ("[makeBitmap] number of bytes in stream=" ++ show (BS.length stream))
   $ logF trace ("[makeBitmap] number of bytes in stream=" ++ show (BS.length stream))
   $ logF trace ("[makeBitmap] effective number of pixels=" ++ show (BS.length stream `div` 4))
   $ logF trace ("[makeBitmap] actual number of pixels=" ++ show (M.size pixels))
   $ logF trace ("[makeBitmap] actual number of foreground pixels=" ++ show (M.size foreground))
   $ logF trace ("[makeBitmap] min X=" ++ show minX ++ ", min Y=" ++ show minY)
   $ logF trace ("[makeBitmap] max X=" ++ show maxX ++ ", max Y=" ++ show maxY)
   $ packRGBA32ToBMP24 w h stream
   where
      background = M.fromList [((x,y), defColor) | y <- [0..(h-1)], x <- [0..(w-1)]]
      pixels = M.union foreground background

      pixelToWord8 :: Pixel -> [Word8]
      pixelToWord8 (r,g,b) = [r,g,b,0]

      maxX = fst $ head $ reverse $ sortBy (comparing fst) $ map fst $ M.toList $ pixels
      maxY = snd $ head $ reverse $ sortBy (comparing snd) $ map fst $ M.toList $ pixels

      minX = fst $ head $ sortBy (comparing fst) $ map fst $ M.toList $ pixels
      minY = snd $ head $ sortBy (comparing snd) $ map fst $ M.toList $ pixels

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
      psbcFrag = ["weak","strong"]

      pers = [[a,f,e,c,h,"love","hate"] | a <- psbcFrag,
                                          f <- psbcFrag,
                                          e <- psbcFrag,
                                          c <- psbcFrag,
                                          h <- ["friendly", "hostile"]]


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

-- |Takes a topography.bmp and a population size and creates entities.bmp,
--  items.bmp and agents.txt in the same directory. The generated files are also
--  returned.
makeRandomWorld
   :: FilePath
   -> Int -- ^Size of each subpopulation.
   -> Int -- ^Number of Wumpuses.
   -> Int -- ^Number of pits.
   -> Float -- ^Probability of placing gold on a cell; 0 to 1.
   -> Float -- ^Probability to placing a plant on a cell; 0 to 1.
   -> IO (M.Map (Int, Int) Pixel, M.Map (Int, Int) Pixel, [String])
makeRandomWorld fp popSize numWumpuses numPits goldProb plantProb = do
   top <- filter ((white==) . snd) <$> readBitmap fp
   logF traceM "[makeRandomWorld] bitmap read."
   let dir = takeDirectory fp
       fromRight (Right r) = r
       fromRight (Left err) = error $ "makeRandomWorld.fromRight called with Left: " ++ show err
   (w,h) <- bmpDimensions . fromRight <$> readBMP fp
   logF traceM $ "[makeRandomWorld] bitmap dimensions: " ++ show (w,h)
   agents <- makePopulations popSize
   logF traceM "[makeRandomWorld] populations made."
   let (agentNames :: [Int]) = map (read . takeWhile isDigit) agents
   logF traceM $ "[makeRandomWorld] agent names: " ++ show agentNames
   (ents, items) <- populateRandomWorld
                       (M.fromList top)
                       agentNames
                       numWumpuses
                       numPits
                       goldProb
                       plantProb
   logF traceM "[makeRandomWorld] populateRandomWorld done."
   let ents' = makeBitmap ents (0,0,0) (w,h)
       items' = makeBitmap items (0,0,0) (w,h)
   logF traceM "[makeRandomWorld] writing BMPs..."
   writeBMP (dir </> "entities.bmp") ents'
   logF traceM "[makeRandomWorld] entities.bmp written."
   writeBMP (dir </> "items.bmp") items'
   logF traceM "[makeRandomWorld] items.bmp written."
   return (ents, items, agents)

-- |A wrapper for 'makeRandomWorld', with parameters set:
--  
--  1. size of each subpopulation = 7.
--  2. number of Wumpuses = 74.
--  3. number of pits = 70
--  4. probability of gold = 0.01
--  5. probability of plant = 0.05
makeEvalWorld :: FilePath -> IO (M.Map (Int, Int) Pixel, M.Map (Int, Int) Pixel, [String])
makeEvalWorld fp = makeRandomWorld fp 7 74 70 0.01 0.05

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
