module Main (main) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8 (unpack)
import qualified Data.List as DL (intercalate)
import Data.Time.Calendar (fromGregorian, Day, diffDays)
import System.Process (callCommand)


reportYear :: String
reportYear = "2022"
treasURL :: String
treasURL= "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2022/all?field_tdr_date_value="
            ++ reportYear ++ "&type=daily_treasury_bill_rates&page&_format=csv"

data Bill = FourWk | EightWk | ThirteenWk | SeventeenWk | TwentySixWk | FiftyTwoWk deriving Eq
instance Show Bill where
  show FourWk = "4-Week "
  show EightWk = "8-Week "
  show ThirteenWk = "13-Week"
  show SeventeenWk = "17-Week"
  show TwentySixWk = "26-Week"
  show FiftyTwoWk = "52-Week"

columnList :: [(Bill, Int)]
columnList = [(FourWk, 1), (EightWk, 3), (ThirteenWk, 5), (SeventeenWk, 7), (TwentySixWk, 9), (FiftyTwoWk, 11)]

billSelection :: [Bill]
billSelection = [FourWk, EightWk, ThirteenWk, TwentySixWk, FiftyTwoWk]

type DateString = String

data Table a b = Table [(a,b)]
instance (Show a, Show b) => Show (Table a b) where
   show (Table lst) = concatMap (\(x, y) -> dropEndQuotes (show x) ++ "    " ++ show y ++ "\n") lst

dropEndQuotes :: String -> String
dropEndQuotes str = dropEndingQuote $ dropLeadingQuote str
  where dropLeadingQuote txt = if head txt == '"' then tail txt else txt
        dropEndingQuote txt = if last txt == '"' then init txt else txt

rateColumns :: [Int]
rateColumns = foldr (\wk lst -> (fromJust $ lookup wk columnList) : lst) [] billSelection



main :: IO ()
main = do
  realData <- isDataReal :: IO (Bool)
  csvData <- if realData then fmap L8.unpack $ simpleHttp treasURL
                         else readFile "testData.csv"
  (startDay, endDay) <- getInterval
  putStrLn ("Start day is " ++ show startDay ++ "     End day is " ++ show endDay)
  let dailyRates = selectData startDay endDay csvData
  putStrLn $ show (Table dailyRates)
  let (dNums, rates) = dayNumsAndRates endDay dailyRates
  putStrLn "\n\n\nThe day numbers are:  "
  putStrLn $ show dNums
  putStrLn "\n\n\nThe rates are:  "
  putStrLn $ show rates
  putStrLn "\n\n\n\n  Here are the least-squares fits ('d' = days from last data point in fit):"
  putStrLn $ analyzeRates (map fromIntegral dNums) billSelection rates 










isDataReal :: IO (Bool)
isDataReal = do
  putStrLn "Enter 'r' to download T-Bill rates from treasury, or 't' to use testData for rates"
  c <- getChar
  putStrLn ""
  case c of
    'r' -> return True
    't' -> return False
    _   -> putStrLn (c : " is not a valide entery. You must enter either 'r' or 't'\n") >> isDataReal

getInterval :: IO (Day, Day)
getInterval = do
  putStrLn "Input starting date for data ( m/d/y ):"
  startDay <- fmap textToDay getLine
  putStrLn "Input ending date for data ( m/d/y ):"
  endDay <- fmap textToDay getLine
  return  (startDay, endDay)



textToDay :: String -> Day
textToDay txt = if length threeStrings == 3 then fromGregorian y m d --Date {month = m, day = d, year = y}
                                             else error "Incorrect date format"
  where threeStrings = words $ map (\c -> if c == '/' then ' ' else c) txt 
        m = read (threeStrings !! 0) :: Int
        d = read (threeStrings !! 1) :: Int
        y = fourDigitYr $ read (threeStrings !! 2) :: Integer
        fourDigitYr yr = if  yr < 1000 then yr + 2000 else yr

selectData :: Day -> Day -> String  -> [(Day, [Double])]
selectData  startDay endDay allRates = filter inRange $ reverse $ map toDateRateTuple $ csvToList allRates  -- Collect data for bills specified by list 'weekSelec' defined before 'main'
  where csvToList = map wordsWithComma . drop 1 . lines 
        toDateRateTuple :: [String] -> (Day, [Double])
        toDateRateTuple [] = error "Empty line of date and rates"
        toDateRateTuple (date : rates) = (textToDay date, map read (select rates))
        select rs = foldr (\c selected -> (rs !! c) : selected) [] rateColumns
        inRange (day, _) = day >= startDay && day <= endDay 


dayNumsAndRates :: Day ->[(Day, [Double])] -> ([Integer], [[Double]])
dayNumsAndRates endDay rateTable = foldr addToLists ([], []) rateTable
  where addToLists (day, rates) (dayNums, rateList) = (diffDays day endDay : dayNums, rates : rateList)

analyzeRates :: [Double] -> [Bill] -> [[Double]] -> String
analyzeRates _ [] _ = "\n\n"                                        -- Create a table showing least squares fit for selected bills
analyzeRates dayNums weeks allRates = "\n" ++ show (head weeks) ++ " bill:  apr  = " ++ toNdecimal 4  m ++ " d + " ++ toNdecimal 2 b 
                                       ++ analyzeRates dayNums (tail weeks) moreRates
  where (m,b) = leastSq dayNums rates
        rates = map head allRates
        moreRates = map tail allRates



leastSq :: [Double] -> [Double] -> (Double, Double)
leastSq xVals yVals = (m, b)  -- Slope and intercept of least squares fit 
  where nPoints = fromIntegral $ length xVals
        xAve = sum xVals / nPoints
        yAve = sum yVals / nPoints
        ssxx = (sum $ map (**2) xVals) - nPoints * xAve**2
        ssxy = (sum $ zipWith (*) xVals yVals) - nPoints * xAve * yAve
        m = ssxy/ssxx
        b = ave yVals - m * ave xVals


fromJust :: Maybe a -> a
fromJust Nothing = error "Not a just value"
fromJust (Just x) = x

ave :: [Double] -> Double
ave xVals = sum xVals / (fromIntegral $ length xVals)

toNdecimal :: Int -> Double -> String
toNdecimal n dbl =  "0." ++ (replicate zeros '0') ++ (show $ round (dbl * scaleUp)) -- assuming dbl < 0, returns string showing n digits
  where scaleUp = 10^n
        zeros = min n $ ( ceiling $ negate $ logBase 10 dbl) - 1



wordsWithComma :: String -> [String]
wordsWithComma str
  |rest == "" = [newWord] 
  |otherwise = newWord : wordsWithComma (drop 1 rest)
  where rest = dropWhile (/= ',') str
        newWord = takeWhile (/= ',') str