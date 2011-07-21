import Text.Regex.PCRE ((=~))
import Data.List (partition)
import System (getArgs)

import Data.Map (Map)
import qualified Data.Map as Map

type RawLine = String
type Name = String
type ReferenceMap = Map Name [Column]

data Line = Unknown RawLine | End RawLine | Table Name RawLine [Column]
data Column = Column Name RawLine

instance Show (Line) where
  show (Unknown _) = "Unknown"
  show (End _)     = "End"
  show (Table name _ cols) = "Table :" ++ name ++ " " ++ show cols
instance Eq (Line) where
  (Table name1 _ _) == (Table name2 _ _) = name1 == name2
  (Unknown line1)   == (Unknown line2)   = line1 == line2
  (End     line1)   == (End     line2)   = line1 == line2
  _ == _ = False

instance Show (Column) where
  show (Column name _) = ':' : name
instance Eq (Column) where
  (Column name1 _) == (Column name2 _) = name1 == name2
instance Ord (Column) where
  compare (Column name1 _) (Column name2 _) = compare name1 name2


main :: IO ()
main = do
  args <- getArgs
  input1 <- readFile $ args !! 0
  input2 <- readFile $ args !! 1
  let refmap = mapTables $ parseFile input2
  let schema = map (maybeSortColumns refmap) $ parseFile input1
  putStr $ outputLines schema



parseFile :: String -> [Line]
parseFile = groupLines . map parseLine . lines

matchFull :: String -> String -> (String, String, String, [String])
matchFull pattern text = text =~ pattern

matchGroups :: (String, String, String, [String]) -> [String]
matchGroups (_, _, _, groups) = groups

findName :: RawLine -> Name
findName = head . matchGroups . matchFull "^[^\"]*\"([^\"]+)\""

parseLine :: RawLine -> Line
parseLine line
  | line =~ "^\\s*create_table " = Table (findName line) line []
  | line =~ "^\\s*end\\s*$" = End line
  | otherwise = Unknown line

isUnknown :: Line -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False

isTable :: Line -> Bool
isTable (Table _ _ _) = True
isTable _ = False

parseColumn :: RawLine -> Column
parseColumn line = Column (findName line) line

groupLines :: [Line] -> [Line]
groupLines [] = []
groupLines ((Table name line cols):xs) = Table name line (cols ++ moreCols) : groupLines after
  where
    (unknowns, after) = span isUnknown xs
    moreCols = map parseUnknown unknowns
    parseUnknown (Unknown x) = parseColumn x
    parseUnknown _ = error "unexpected"
groupLines (x:xs) = x : groupLines xs

sortColumns :: [Column] -> [Column] -> [Column]
sortColumns [] _  = []
sortColumns xs [] = xs
sortColumns (x1:xs1) all2@(x2:xs2)
  | x1 == x2      = x1   :  sortColumns xs1 xs2
  | x2 `elem` xs1 = isX2 ++ sortColumns (x1:isNotX2) xs2
  | otherwise     = x1   :  sortColumns xs1 all2
  where (isX2, isNotX2) = partition (==x2) xs1

maybeSortColumns :: ReferenceMap -> Line -> Line
maybeSortColumns refMap (Table name line cols) = Table name line (sortColumns cols refCols)
  where refCols = Map.findWithDefault [] name refMap
maybeSortColumns _ x = x

mapTables :: [Line] -> ReferenceMap
mapTables = Map.fromList . map assoc . filter isTable
  where
    assoc (Table name _ cols) = (name, cols)
    assoc _ = error "unexpected"

outputLine :: Line -> String
outputLine (Unknown line) = line
outputLine (End line)     = line
outputLine (Table _ line cols) = (unlines . init) items ++ last items
  where
    items = line : map outputColumn cols
    outputColumn (Column _ cline) = cline

outputLines :: [Line] -> String
outputLines = unlines . map outputLine
