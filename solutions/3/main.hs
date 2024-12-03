import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (isPrefixOf)

extractMulResults :: String -> [Int]
extractMulResults s = extractMulResults' s True

extractMulResults' :: String -> Bool -> [Int]
extractMulResults' [] _ = []
extractMulResults' s@(c : cs) enabled
  | "don't()" `isPrefixOf` s = extractMulResults' cs False
  | "do()" `isPrefixOf` s = extractMulResults' cs True
  | "mul(" `isPrefixOf` s =
      case parseMul s of
        Just (result, rest) ->
          if enabled
            then result : extractMulResults' rest enabled
            else extractMulResults' rest enabled
        Nothing -> extractMulResults' cs enabled
  | otherwise = extractMulResults' cs enabled

parseMul :: String -> Maybe (Int, String)
parseMul s = do
  let prefix = "mul("
  guard (prefix `isPrefixOf` s)
  let afterMul = drop (length prefix) s
  let (num1Str, rest1) = span isDigit afterMul
  guard (not (null num1Str))
  guard (head rest1 == ',')
  let restAfterComma = tail rest1
  let (num2Str, rest2) = span isDigit restAfterComma
  guard (not (null num2Str))
  guard (head rest2 == ')')
  let remaining = tail rest2
  let product = read num1Str * read num2Str
  return (product, remaining)

part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let results = extractMulResults input
  print (sum results)

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let results = extractMulResults input
  print (sum results)