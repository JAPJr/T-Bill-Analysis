module Main (main) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8 (unpack)
--import qualified Data.List as DL (intercalate)
import Data.Time.Calendar (fromGregorian, Day, diffDays)
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
billSelection = [FourWk, EightWk] --[FourWk, EightWk, ThirteenWk, TwentySixWk, FiftyTwoWk]



newtype Table a b = Table [(a,b)]
instance (Show a, Show b) => Show (Table a b) where
   show (Table lst) = concatMap (\(x, y) -> dropEndQuotes (show x) ++ "    " ++ show y ++ "\n") lst

dropEndQuotes :: String -> String
dropEndQuotes str = dropEndingQuote $ dropLeadingQuote str
  where dropLeadingQuote txt = if head txt == '"' then tail txt else txt
        dropEndingQuote txt = if last txt == '"' then init txt else txt

rateColumns :: [Int]
--rateColumns = foldr (\wk lst -> fromJust (lookup wk columnList) : lst) [] billSelection
rateColumns = map (\wk -> fromJust (lookup wk columnList)) billSelection


main :: IO ()
main = do
  realData <- isDataReal :: IO Bool
  csvData <- if realData then L8.unpack <$> simpleHttp treasURL
                         else readFile "testData.csv"
  (startDay, endDay) <- getInterval
  putStrLn ("Start day is " ++ show startDay ++ "     End day is " ++ show endDay)
  let dailyRates = selectData startDay endDay csvData
  print (Table dailyRates)
  let (dNums, rates) = dayNumsAndRates endDay dailyRates
  putStrLn "\n\n\nThe day numbers are:  "
  print dNums
  putStrLn "\n\n\nThe rates are:  "
  print rates
  putStrLn "\n\nFour week rates are:  "
  putStrLn $ spacesToCommas $ unwords $ map (show . head) rates
--  putStrLn "\n\n\n\n  Here are the least-squares fits ('d' = days from last data point in fit):"
  putStrLn "\n\nDebugging."
  putStrLn "\n rates:"
  print rates
  putStrLn "\nbillSelection:"
  print billSelection
  putStr "\nanalyzeRates: "
  print $ analyzeRates dNums billSelection rates

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

selectData :: Day -> Day -> String  -> [(Day, [Double])]
--selectData  startDay endDay allRates = filter inRange $ reverse $ map toDateRateTuple $ csvToList allRates  -- Collect data for bills specified by list 'weekSelec' defined before 'main'
selectData  startDay endDay allRates = reverse (filter inRange (map toDateRateTuple $ csvToList allRates))
  where csvToList = map wordsWithComma . drop 1 . lines 
        toDateRateTuple :: [String] -> (Day, [Double])
        toDateRateTuple [] = error "Empty line of date and rates"
        toDateRateTuple (date : rates) = (textToDay date, map read (select rates))
        select rs = foldr (\c selected -> (rs !! c) : selected) [] rateColumns
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

{--
analyzeRates :: [Double] -> [Bill] -> [[Double]] -> [Double] --[(Bill, [Double], [Bill])]  -- Create a table showing least squares fit for selected bills
analyzeRates dayNums weeks allRates = analyzeEachBill dayNums weeks allRates []

analyzeEachBill :: [Double] -> [Bill] -> [[Double]] -> [(Bill, [Double], [Bill])] -> [Double] --[(Bill, [Double], [Bill])]
analyzeEachBill dayNums (week : remainWeeks) ratesList results = rates
  where rates = map head ratesList
        remainRates = map tail ratesList
        (m,b) = leastSq dayNums rates
--}

{--
rateFitReport :: [(Bill, Double, Double)] -> String
rateFitReport fitResults = foldr addBillResult "" fitResults
  where addBillResult (bill, m, b) rpt = ( "\n" ++ show bill ++ ":  apr = " ++ toNdecimal 6 m ++ "d + " ++ toNdecimal 2 b ) ++ rpt
--  where addBillResult (bill, m, b) rpt = ( "\n" ++ show bill ++ ":  apr = " ++ show m ++ "d + " ++ show b ) ++ rpt
--}






-- ************************************ This section needs to be fixed to mesh with this program ***************************************
{--
aprDiffTable :: [(Bill, Double, Double)] ->  [(MatureTime, [(MatureTime, InterestRate)])]  -- Note Bill must be converted to Int
aprDiffTable billLeastSquares = reverse $ getNextSetOfDiffs billLeastSquares []
  where getNextSetOfDiffs remainingData diffList
          |(tail remainingData) == [] = diffList
          |otherwise                  = getNextSetOfDiffs (tail remainingData) ((aprDiffs remainingData) : diffList)

aprDiffs :: [(Bil, Double, Double)] -> (MatureTime, [(MatureTime, InterestRate)])
aprDiffs (billData : comparisonBillsData) = (getWeeks billData, comparisons)
  where getWeeks (bill, _, _) = billTime bill
        diffInterestWithLongerBill shortBillData (longWeeks, longIntRate, _) = aprWithReinvestment shortBillData longWeeks - longIntRate
        comparisons = foldr ( \compareTo comps -> (getWeeks compareTo, diffInterestWithLongerBill billData compareTo) : comps ) [] comparisonBillsData                                

aprWithReinvestment:: BillData -> Int -> Double
aprWithReinvestment (weeks, intRate, deltaRate) reinvestWeeks = 100 * ( factorForWholePeriods * factorForFracPeriods - 1.0) * 365.0 / fromIntegral (reinvestWeeks * 7)
  where  wholePeriods = fromIntegral (reinvestWeeks `div` weeks)
         fracPeriod = fromIntegral reinvestWeeks / fromIntegral weeks - wholePeriods 
         factorAtMaturity = 0.01 * intRate * ( fromIntegral weeks * 7.0 / 365.0 ) + 1.0   
         factorChangeAtMaturity = 0.01 * (7.0 * fromIntegral weeks * deltaRate ) *  ( fromIntegral weeks * 7.0 / 365.0 ) 
         factorForWholePeriods = foldr ( \n fact -> fact * (factorAtMaturity + n * factorChangeAtMaturity))  1 [0 .. wholePeriods -1] 
         factorForFracPeriods = ( (factorAtMaturity + wholePeriods * factorChangeAtMaturity - 1)* fracPeriod + 1)

--}
-- ************************************ End of section to be fixed **************************************************


{--
aprWithReinvest :: (Bill, Double, Double) -> Bill -> Double
aprWithReinvest (billShort, m, b) billLong = 100 * (factorForWholePeriods * factorForFracPeriod - 1.0) * 365.0 / fromIntegral (tLong * 7)
  where tShort = billTime billShort
        tLong = billTime billLong
        wholePeriods = fromIntegral (tLong `div` tShort) :: Double
        fracPeriod = (fromIntegral tLong) / (fromIntegral tShort) - wholePeriods
        factorAtMaturity = 0.01 * aprToWeekPct b tShort + 1.0
        factorChangeAtMaturity = 0.01 * aprToWeekPct (7.0 * (fromIntegral tShort) *  m) (fromIntegral tShort)
        factorForWholePeriods = foldr ( \n fact -> fact * (factorAtMaturity + n * factorChangeAtMaturity) ) 1.0 [0 .. wholePeriods - 1]
        factorForFracPeriod = (factorAtMaturity + wholePeriods * factorChangeAtMaturity - 1 ) * fracPeriod + 1
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

toNdecimal :: Int -> Double -> String
toNdecimal n dbl =  "0." ++ replicate zeros '0' ++ show (round (dbl * scaleUp)) -- assuming dbl < 0, returns string showing n digits
  where scaleUp = 10^n
        zeros = min n $  ceiling (negate $ logBase 10 dbl) - 1



wordsWithComma :: String -> [String]
wordsWithComma str
  |rest == "" = [newWord] 
  |otherwise = newWord : wordsWithComma (drop 1 rest)
  where rest = dropWhile (/= ',') str
        newWord = takeWhile (/= ',') str
