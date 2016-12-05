
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Debug.Trace

validDigest1 :: String -> Bool
validDigest1 s = take 5 s == "00000"

validDigest2 :: String -> [(Int, Char)] -> Bool
validDigest2 s existing = (validDigest1 s) && pos < 8 && notFound
    where
        pos = digitToInt (s !! 5)
        notFound = not $ pos `elem` (map fst existing)

decrypt2' :: String -> Int -> Int -> [(Int, Char)] -> [(Int, Char)]
decrypt2' _ _ 0 keys = reverse keys
decrypt2' s index remaining keys
    | validDigest2 digest keys = traceShow digest $ decrypt2' s (succ index) (pred remaining) ((pos, key) : keys)
    | otherwise = decrypt2' s (succ index) remaining keys
    where
        md5input = s ++ show index
        digest = (show . md5 . C.pack) md5input
        (pos, key) = (digitToInt (digest !! 5), digest !! 6)

decrypt1' :: String -> Int -> Int -> [Char] -> [Char]
decrypt1' _ _ 0 keys = reverse keys
decrypt1' s index remaining keys
    | validDigest1 digest = traceShow digest $ decrypt1' s (succ index) (pred remaining) (digest !! 5 : keys)
    | otherwise = decrypt1' s (succ index) remaining keys
    where
        md5input = s ++ show index
        digest = (show . md5 . C.pack) md5input

decrypt1 :: String -> Int -> [Char]
decrypt1 s remaining = decrypt1' s 0 remaining []

decrypt2 :: String -> Int -> [(Int, Char)]
decrypt2 s remaining = decrypt2' s 0 remaining []

main :: IO ()
main = let input = "reyedfim" in
    print $ decrypt2 input 8
