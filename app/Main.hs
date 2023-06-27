module Main (main) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8 (unpack)
--import qualified Data.List as DL (intercalate)
import Data.List
import Data.Time.Calendar (fromGregorian, toGregorian, Day, diffDays)
--import System.Process (callCommand)


reportYear :: String
reportYear = "2023"
treasURL :: String
treasURL= "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2023/all?field_tdr_date_value="
            ++ reportYear ++ "&type=daily_treasury_bill_rates&page&_format=csv"

data Bill = FourWk | EightWk | ThirteenWk | SeventeenWk | TwentySixWk | FiftyTwoWk deriving Eq
instance Show Bill where
  show FourWk = "4-Week "
  show EightWk = "8-Week "
  show ThirteenWk = "13-Week"
  show SeventeenWk = "17-Week"
  show TwentySixWk = "26-Week"
  show FiftyTwoWk = "52-Week"

{--
billTime :: Bill -> Int
billTime bill = case bill of
  FourWk -> 4
  EightWk -> 8
  ThirteenWk -> 13
  SeventeenWk -> 17
  TwentySixWk -> 26
  FiftyTwoWk -> 52
--}

columnList :: [(Bill, Int)]
columnList = [(FourWk, 1), (EightWk, 3), (ThirteenWk, 5), (SeventeenWk, 7), (TwentySixWk, 9), (FiftyTwoWk, 11)]

billSelection :: [Bill]
billSelection = [EightWk] --[FourWk, EightWk, ThirteenWk, TwentySixWk, FiftyTwoWk]



newtype Table a b = Table [(a,b)]
instance (Show a, Show b) => Show (Table a b) where
   show (Table lst) = concatMap (\(x, y) -> dropEndQuotes (show x) ++ "    " ++ show y ++ "\n") lst

dropEndQuotes :: String -> String
dropEndQuotes str = dropEndingQuote $ dropLeadingQuote str
  where dropLeadingQuote txt = if head txt == '"' then tail txt else txt
        dropEndingQuote txt = if last txt == '"' then init txt else txt

rateColumns :: [Int]
rateColumns = map (\wk -> fromJust (lookup wk columnList)) billSelection


main :: IO ()
main = do
  realData <- isDataReal :: IO Bool
  csvData <- if realData then L8.unpack <$> simpleHttp treasURL
                         else readFile "testData.csv"

  (startDay, endDay) <- getInterval
  putStrLn ("Start day is " ++ show startDay ++ "     End day is " ++ show endDay)

  let dailyRates = selectData startDay endDay csvData
  putStrLn "\n\ndailyRates:"
  print (Table dailyRates)

  let (dNums, rates) = dayNumsAndRates endDay dailyRates

--  putStrLn "\n\n\n\n  Here are the least-squares fits ('d' = days from last data point in fit):"
  putStrLn "\n\nDebugging."
  putStrLn "\n rates:"
  print rates
  putStrLn "\nbillSelection:"
  print billSelection
  putStr "\nLeast squares of rates: "
  let ratesLeastSquares = analyzeRates dNums billSelection rates
  print ratesLeastSquares
--  print $ analyzeRates dNums billSelection rates
  putStrLn "\nLeast squares end points:\n"
  putStrLn $ leastSquaresEndPoints startDay endDay ratesLeastSquares
  writeFile "LsqEndpoints.txt" (leastSquaresEndPoints startDay endDay ratesLeastSquares)


--  putStrLn $ rateFitReport$ analyzeRates  dNums billSelection rates 
  
spacesToCommas :: String -> String
spacesToCommas = map (\c -> if c == ' ' then ',' else c)






isDataReal :: IO Bool
isDataReal = do
  putStrLn "Enter 'r' to download T-Bill rates from treasury, or 't' to use testData for rates"
  c <- fmap head getLine
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
        m = read (head threeStrings) :: Int
        d = read (threeStrings !! 1) :: Int
        y = fourDigitYr $ read (threeStrings !! 2) :: Integer
        fourDigitYr yr = if  yr < 1000 then yr + 2000 else yr

dayToText :: Day -> String
dayToText day = show m  ++ "/" ++ show d ++ "/" ++ show y 
  where (y,m,d) = toGregorian day

selectData :: Day -> Day -> String  -> [(Day, [Double])]
--selectData  startDay endDay allRates = filter inRange $ reverse $ map toDateRateTuple $ csvToList allRates  
selectData  startDay endDay allRates = reverse (filter inRange (map toDateRateTuple $ csvToList allRates))  -- Collect data for bills specified by list 'weekSelec' defined before 'main'
  where csvToList = map wordsWithComma . drop 1 . lines 
        toDateRateTuple :: [String] -> (Day, [Double])
        toDateRateTuple [] = error "Empty line of date and rates"
        toDateRateTuple (date : rates) = (textToDay date, map read (select rates))
        select rs = map (rs !!) rateColumns
        inRange (day, _) = day >= startDay && day <= endDay 


dayNumsAndRates :: Day ->[(Day, [Double])] -> ([Double], [[Double]])
dayNumsAndRates endDay = foldr addToLists ([], [])
  where addToLists (day, rates) (dayNums, rateList) = (fromIntegral (diffDays day endDay) : dayNums, rates : rateList)



analyzeRates :: [Double] -> [Bill] -> [[Double]] -> [(Bill, Double, Double)]  -- Create a table showing least squares fit for selected bills
analyzeRates dayNums weeks allRates = analyzeEachBill dayNums weeks allRates []


analyzeEachBill :: [Double] -> [Bill] -> [[Double]] -> [(Bill, Double, Double)] -> [(Bill, Double, Double)]
analyzeEachBill _ [] _ results = results
analyzeEachBill dayNums (week : remainWeeks) ratesList results = analyzeEachBill dayNums remainWeeks remainRates ((week, m, b) : results)
  where rates = map head ratesList
        remainRates = map tail ratesList
        (m,b) = leastSq dayNums rates


leastSquaresEndPoints :: Day -> Day -> [(Bill, Double, Double)] -> String
leastSquaresEndPoints startDay endDay ratesLeastSquares = dayToText startDay ++ ", " ++ numbersToCsv startValues ++ "\n"
                                                      ++  dayToText endDay ++ ", " ++ numbersToCsv  endValues
  where startValues :: [Double]
        startValues = foldr (  \(_, m, b) lines ->   ((m * fromIntegral ( diffDays startDay endDay)) + b) : lines  ) [] ratesLeastSquares
        endValues = map (\(_, _, b) -> b) ratesLeastSquares  -- assuming end is day 0 as in analysis
        numbersToCsv :: [Double] -> String
        numbersToCsv = intercalate  ", " . map show


{--
rateFitReport :: [(Bill, Double, Double)] -> String
rateFitReport fitResults = foldr addBillResult "" fitResults
  where addBillResult (bill, m, b) rpt = ( "\n" ++ show bill ++ ":  apr = " ++ toNdecimal 6 m ++ "d + " ++ toNdecimal 2 b ) ++ rpt
--  where addBillResult (bill, m, b) rpt = ( "\n" ++ show bill ++ ":  apr = " ++ show m ++ "d + " ++ show b ) ++ rpt
--}


         

leastSq :: [Double] -> [Double] -> (Double, Double)
leastSq xVals yVals = (m, b)  -- Slope and intercept of least squares fit 
  where nPoints = fromIntegral $ length xVals
        xAve = sum xVals / nPoints
        yAve = sum yVals / nPoints
        ssxx = sum (map (**2) xVals) - nPoints * xAve**2
        ssxy = sum (zipWith (*) xVals yVals) - nPoints * xAve * yAve
        m = ssxy/ssxx
        b = ave yVals - m * ave xVals



--aprToWeekPct apr weeks = apr * fromIntegral weeks * 7/ 365


fromJust :: Maybe a -> a
fromJust Nothing = error "Not a just value"
fromJust (Just x) = x

ave :: [Double] -> Double
ave xVals = sum xVals / fromIntegral (length xVals)



toNDecimal :: Int -> Double -> Double
toNDecimal n dbl = (fromIntegral $ round (10^n * dbl)) / 10^n



wordsWithComma :: String -> [String]
wordsWithComma str
  |rest == "" = [newWord] 
  |otherwise = newWord : wordsWithComma (drop 1 rest)
  where rest = dropWhile (/= ',') str
        newWord = takeWhile (/= ',') str
