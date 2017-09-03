module Main where
import System.Environment
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  case (foldMaybeNum [readMaybe s :: Maybe Double | s <- args]) of
    (Just n) -> roundMaybe n
    Nothing -> putStrLn "Those weren't numbers!"

foldMaybeNum :: (Num a, Eq a) => [Maybe a] -> Maybe a
foldMaybeNum x = foldr (\a b -> (+) <$> a <*> b) (Just 0) x

-- Decide whether to print the double as an int, or a double
roundMaybe :: Double -> IO ()
roundMaybe n =
  if n == fromInteger (round n) then
    (putStrLn . show) (round n :: Integer)
  else (putStrLn . show) n
