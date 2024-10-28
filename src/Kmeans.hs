module Kmeans (
    initKmeans
) where

import System.Random
import Data.Ord
import System.Exit
import Pixel
import Data.List (minimumBy, find)

data Cluster = Cluster {
    i :: Int,
    centroid :: (Int, Int, Int),
    pixels :: [Pixel]
} deriving (Show)

getIndexCluster :: Cluster -> Int
getIndexCluster (Cluster j _ _) = j

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:xs) n | (n < 0) = Nothing
                 | (n > length (x:xs)) = Nothing
                 | otherwise = Just ((x:xs) !! n)

addAtBegin :: [a] -> a -> [a]
addAtBegin list newPixel = (newPixel: list)

getRandomNumbers :: Int -> Int -> [Int] -> [Pixel] -> IO([Int])
getRandomNumbers 0 _ nbsList _ = return nbsList
getRandomNumbers k maxI nbsList pixelsList = do
    n <- (randomRIO (0, maxI))
    r <- getUniqueRandomNb 0 maxI n nbsList pixelsList
    getRandomNumbers (k - 1) maxI (addAtBegin nbsList r) pixelsList

getUniqueRandomNb :: Int -> Int -> Int -> [Int] -> [Pixel] -> IO Int
getUniqueRandomNb a b randNb nbsList pixelsList
    | (find (== randNb) nbsList == Nothing &&
        safeNth pixelsList randNb /= Nothing)
        = return randNb :: IO Int
    | otherwise = do
        r <- (randomRIO (a, b))
        getUniqueRandomNb a b r nbsList pixelsList

getColor :: Pixel -> (Int, Int, Int)
getColor (Pixel (Just _, Just _) (Just r, Just g, Just b)) = (r, g, b)
getColor _ = error "getColor"

initClusters :: [Pixel] -> Int -> [Int] -> [Cluster]
initClusters list k (randNb:[]) =
    ((Cluster k (getColor (list !! randNb)) []) : [])
initClusters _ 0 _ = []
initClusters list k (randNb:ys) =
    ((Cluster k (getColor (list !! randNb)) [])
            : initClusters list (k - 1) (ys))
initClusters _ _ _ = error "init clusters"

getClosestClusterIndex :: [Cluster] -> Pixel -> Int
getClosestClusterIndex ((Cluster k c list):xs) pixel =
    getIndexCluster $
    minimumBy (comparing (getDistance (getColor pixel) . centroid))
    ((Cluster k c list):xs)
getClosestClusterIndex _ _ = error "getClosestClusterIndex"

assignPixelToCluster :: [Cluster] -> Int -> Pixel -> [Cluster]
assignPixelToCluster [] _ _ = []
assignPixelToCluster ((Cluster k c list):xs) n pixel
    | (n == k) = ((Cluster k c (addAtBegin list pixel)):xs)
    | otherwise = ((Cluster k c list) :
        assignPixelToCluster (xs) n pixel)

assignPixelsToClusters :: [Pixel] -> [Cluster] -> [Cluster]
assignPixelsToClusters [] clusters = clusters
assignPixelsToClusters (pixel:xs) clusters = assignPixelsToClusters (xs)
    (assignPixelToCluster clusters
    (getClosestClusterIndex clusters pixel) pixel)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

computeNewCentroid :: [(Int, Int, Int)] -> (Maybe Int, Maybe Int, Maybe Int)
computeNewCentroid coords = let
    (x, y, z) = unzip3 (coords)
    newX = safeDiv (sum x) (length coords)
    newY = safeDiv (sum y) (length coords)
    newZ = safeDiv (sum z) (length coords)
    in (newX, newY, newZ)

checkCentroid :: (Int, Int, Int) -> (Maybe Int, Maybe Int, Maybe Int) -> (Int, Int, Int)
checkCentroid _ (Just r, Just g, Just b) = (r, g, b)
checkCentroid old _ = old

updateCentroids :: [Cluster] -> [Cluster]
updateCentroids [] = []
updateCentroids ((Cluster k c list):xs) =
    ((Cluster k newCentroid list) : updateCentroids (xs))
    where
        newCentroid = checkCentroid c (computeNewCentroid
                        (fmap getColor list))

sqre :: Int -> Int
sqre nb = nb * nb

getDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
getDistance (r1, g1, b1) (r2, g2, b2) =
    sqrt (fromIntegral (sqre (r1 - r2) + sqre (g1 - g2) + sqre (b1 - b2)))

checkConvergence :: [Cluster] -> [Cluster] -> Float -> Bool
checkConvergence oldClusters newClusters limit =
    all (\(oldC, newC) -> distanceBetweenClusters
        oldC newC <= limit) (zip oldClusters newClusters)
    where
        distanceBetweenClusters :: Cluster -> Cluster -> Float
        distanceBetweenClusters oldCluster newCluster =
            let (x1, y1, z1) = centroid oldCluster
                (x2, y2, z2) = centroid newCluster
            in sqrt (fromIntegral (sqre (x2 - x1) +
            sqre (y2 - y1) + sqre (z2 - z1)))

resetClustersPixels :: [Cluster] -> [Cluster]
resetClustersPixels clusters = fmap
    (\(Cluster k c _) -> (Cluster k c [])) clusters

runKmeans :: [Pixel] -> [Cluster] -> [Cluster] -> Float -> IO ()
runKmeans list oldClusters newClusters l
    | ((checkConvergence oldClusters newClusters l) == True) =
        printClusters newClusters
    | otherwise =
        runKmeans list newClusters
        (updateCentroids (assignPixelsToClusters list resetClusters)) l
        where resetClusters = (resetClustersPixels newClusters)

printPixel :: Pixel -> IO ()
printPixel pixel = print pixel

printClusters :: [Cluster] -> IO ()
printClusters clusters = mapM_ printCluster clusters
    where
        printCluster :: Cluster -> IO ()
        printCluster (Cluster _ c list) =
            putStrLn "--" >> print c >> putStrLn "-"
            >> mapM_ printPixel list

initKmeans :: [Pixel] -> Int -> Float -> IO ()
initKmeans [] _ _ = exitWith (ExitFailure 84)
initKmeans (pixel:[]) k l = runKmeans (pixel:[])
        (initClusters (pixel:[]) k [0])
        (updateCentroids (assignPixelsToClusters (pixel:[])
        (initClusters (pixel:[]) k [0]))) l
initKmeans list k l = do
    randomNbs <- getRandomNumbers k ((length list) - 1) [] list
    runKmeans list (initClusters list k randomNbs)
        (updateCentroids (assignPixelsToClusters list
        (initClusters list k randomNbs))) l