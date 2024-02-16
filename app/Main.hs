module Main (main) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L8 (unpack)
import Data.Time.Calendar (fromGregorian, toGregorian, Day, diffDays)
import qualified Text.PrettyPrint.Boxes as B
import Data.List (intercalate, sort)


reportYear = "2024" :: String
treasURL= "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2024/all?field_tdr_date_value="
            ++ reportYear ++ "&type=daily_treasury_bill_rates&page&_format=csv" :: String

data Bill = FourWk | EightWk | ThirteenWk | SeventeenWk | TwentySixWk | FiftyTwoWk deriving Eq
instance Show Bill where
  show FourWk = "4-Week"
  show EightWk = "8-Week"
  show ThirteenWk = "13-Week"
  show SeventeenWk = "17-Week"
  show TwentySixWk = "26-Week"
  show FiftyTwoWk = "52-Week"

--Where to find bill rates for investors i.e. not bank rates
columnList = [(FourWk, 2), (EightWk, 4), (ThirteenWk, 6), (SeventeenWk, 8), (TwentySixWk, 10), (FiftyTwoWk, 12)]
billSelection = [FourWk, EightWk, ThirteenWk, TwentySixWk, FiftyTwoWk]  --Bills to be Analyzed
rateColumns = map (\wk -> fromJust (lookup wk columnList)) billSelection

main :: IO ()
main = do
  -- Get raw data for analysis (test or Treasury)
  allData <- getTbillData -- :: [[String]]
  if False 
    then do putStrLn "\n\nallData:\n"
            showArray allData
    else return ()

  -- Get desired data and convert to numeric
  selectedBills <- chooseBills
  (startDay, endDay) <- getDateLimits
  let numericData = getAndConvert selectedBills startDay endDay allData -- :: [(Day, [Double])]
  if False 
    then do putStrLn "printListAsColumn numericData"
            printListAsColumn numericData
    else return ()

--Format data as csv and Save
  let csvData = numericDataToCsv selectedBills numericData
  if False
    then putStrLn ("\n\nSelected data as CSV\n" ++ csvData)
    else return ()
  writeFile "savedData.csv" $ numericDataToCsv selectedBills numericData

-- Perform least squares analysis
  putStrLn "The least squares fits are:"
  let lsq =  getLeastSquares numericData

-- Get and Save lsq endpoints for fit lines in plot
  let lsqGraphPoints = lsqEndPointsCsv startDay endDay selectedBills lsq 
  putStrLn "\n\nLeast squares endpoints"
  putStrLn lsqGraphPoints
  writeFile "lsq.csv" lsqGraphPoints

  

getTbillData :: IO ([[String]])
getTbillData = do
  putStrLn "\nChoose source of T-Bill data.  Enter 't' to download data from Treasury or 'f' to select a local file."
  dataSource <- getEitherOr 't' 'f'
  case dataSource of
    't' -> (drop 1 . csvToArray . L8.unpack) <$> simpleHttp treasURL
    'f' -> (drop 1 . csvToArray) <$> readFile "testData.csv"

chooseBills :: IO ([Bill])
chooseBills = do
  putStrLn "\nSelect bills to be analyzed by entering, in a single line with no separators, associated digits in table below."
  table "" ["", "Bill"] [["1", "4-Week"], ["2", "8-Week"], ["3", "13-Week"], ["4", "17-Week"], ["5", "26-Week"], ["6", "52-Week"]] 10
  input <- getLine
  let selection = map (\c -> read [c] :: Int) $ sort input -- order entry and convert to integers
      chosenBills = map (\n -> [FourWk, EightWk, ThirteenWk, SeventeenWk, TwentySixWk, FiftyTwoWk] !! (n-1)) selection
  putStr "You have chosen:  "
  print chosenBills
  return chosenBills    

getAndConvert :: [Bill] -> Day -> Day -> [[String]] -> [(Day,[Double])]
getAndConvert bills startDay endDay dataStringArray = filter dayInRange $ map convertLine dataStringArray
  where rateColumns = map (\b -> fromJust (lookup b columnList)) bills
        getRates aLine = map (read . (aLine !!)) rateColumns :: [Double]
        convertLine line = (textToDay (line !! 0), getRates line)
        dayInRange (day, _) = day >= startDay && day <= endDay

getDateLimits :: IO ((Day,Day))
getDateLimits = do
  putStrLn "\n\nEnter first date in analysis."
  startDate <- getLine        
  putStrLn "Enter last  date in analysis."
  endDate <- getLine
  return $ (textToDay startDate, textToDay endDate)

getEitherOr :: Char -> Char -> IO (Char)
getEitherOr c1 c2 = do 
  cIn <- fmap head getLine
  putStrLn ""
  if cIn == c1 || cIn == c2 then return cIn
                           else putStrLn ([cIn] ++ " is not valid.  Enter either  " ++ [c1] ++  ", or " ++ [c2]) >> getEitherOr c1 c2

getLeastSquares :: [(Day, [Double])] -> [(Double, Double)]
getLeastSquares theData = foldr (\i fits -> leastSq dayNums (getRateColumn i) : fits) [] [0 .. nRateColumns - 1] --leastSquares x = dayNums
  where lastDay = max (fst $ head theData) (fst $ last theData)
        dayNums = map (fromIntegral . negate . diffDays lastDay . fst) theData :: [Double]
        nRateColumns = length $ snd $ head theData
        getRateColumn n = map (( !!n).snd) theData

{--
printLeastSquares :: [Bill] -> [(Double, Double)] -> IO  ()
printLeastSquares theBills theFits = sequence_ $ foldr (\lsqResult lines -> makeLine lsqResult : lines)  [] billsAndFits
  where billsAndFits = zip theBills theFits 
        makeLine (bill, (m,b)) = putStrLn (show bill ++ "   r = " ++ showNDecimal 4 m ++ " d + " ++ showNDecimal 3 b)  
--}

printLeastSquares :: [Bill] -> [(Double, Double)] -> IO  ()
printLeastSquares theBills theFits = do
  putStrLn "\n\nLeast squares fits are:"
  sequence_ $ foldr (\lsqResult lines -> makeLine lsqResult : lines)  [] billsAndFits
  where billsAndFits = zip theBills theFits 
        makeLine (bill, (m,b)) = putStrLn (show bill ++ "   r = " ++ showNDecimal 4 m ++ " d + " ++ showNDecimal 3 b)  

numericDataToCsv :: [Bill] -> [(Day,[Double])] -> String
numericDataToCsv bills numericDat = intercalate "\n" $ billHeader ++ ratesStringLines
  where ratesStringLines = foldr addToStringLines [] numericDat
        addToStringLines (day, rates) lines  = intercalate ", " (dayToText day : map show rates)  : lines
        billHeader = [intercalate "," $ map show bills]

-- Working on this function
lsqEndPointsCsv :: Day -> Day -> [Bill] -> [(Double, Double)] -> String
lsqEndPointsCsv startDay endDay bills ratesLeastSquares = header ++ stringsToCsv resultsAsStrings
  where startValues :: [Double]
        startValues = foldr ( \( m, b) values ->   ((m * fromIntegral ( diffDays startDay endDay)) + b) : values  ) [] ratesLeastSquares
        endValues = map (\(_, b) -> b) ratesLeastSquares  -- assuming end is day 0 as in analysis
        resultsAsStrings = [(dayToText startDay) : (map (showNDecimal 4) startValues), (dayToText endDay) : (map (showNDecimal 4) endValues)]
        stringsToCsv = intercalate "\n" . map (intercalate ", ")
        header = intercalate ", " (map show bills) ++ "\n"


fromJust :: Maybe a -> a
fromJust Nothing = error "Not a just value"
fromJust (Just x) = x

table :: String -> [String] -> [[String]] -> Int -> IO ()
table title headings entries colWidth = B.printBox $ B.vsep 0 B.center1 [titleBox, headingsBox, entriesBox] 
  where tableEntry = B.alignHoriz B.right colWidth . B.text
        tableRow = B.hcat B.bottom . map tableEntry
        titleBox = B.text title
        headingsBox = B.hcat B.bottom $ map tableEntry headings
        entriesBox = B.vcat B.left $ map tableRow entries

printListAsColumn :: Show a => [a] -> IO ()
printListAsColumn [] = putStrLn ""
printListAsColumn (element:elements) = print element >> printListAsColumn elements

showArray :: [[String]] -> IO () --String
showArray a = putStrLn $ intercalate "\n" $ map (intercalate ", ") a 

csvToArray csv = map words $ lines $ commasToSpace csv
  where commasToSpace = map (\c -> if c == ',' then ' ' else c)

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

leastSq :: [Double] -> [Double] -> (Double, Double)
leastSq xVals yVals = (m, b)  -- Slope and intercept of least squares fit 
  where nPoints = fromIntegral $ length xVals
        xAve = sum xVals / nPoints
        yAve = sum yVals / nPoints
        ssxx = sum (map (**2) xVals) - nPoints * xAve**2
        ssxy = sum (zipWith (*) xVals yVals) - nPoints * xAve * yAve
        m = ssxy/ssxx
        b = ave yVals - m * ave xVals

ave :: [Double] -> Double
ave xVals = sum xVals / fromIntegral (length xVals)

showNDecimal :: Int -> Double -> String
showNDecimal n dbl = show $ fromIntegral (round (10^n * dbl) :: Integer) / 10^n
