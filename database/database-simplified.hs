import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)
import Text.Printf
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import System.IO

type Database = Map.Map String EntityPlastic
data EntityPlastic = EntityPlastic { productName :: String, tSofting :: Double, tensileStrength :: Double } deriving (Show)

instance Binary.Binary EntityPlastic where
  put (EntityPlastic productName tSofting tensileStrength) = do
    Binary.put (Data.Text.Encoding.encodeUtf8 (Data.Text.pack productName))
    Binary.put tSofting
    Binary.put tensileStrength
  get = do
    productNameBS <- Binary.get
    let productName = Data.Text.unpack (Data.Text.Encoding.decodeUtf8 productNameBS)
    tSofting <- Binary.get
    tensileStrength <- Binary.get
    return (EntityPlastic productName tSofting tensileStrength)

createDatabase :: Database
createDatabase = Map.empty

createEntity :: String -> Double -> Double -> EntityPlastic
createEntity productName tSofting tensileStrength = EntityPlastic { productName = productName, tSofting = tSofting, tensileStrength = tensileStrength }

addToDatabase :: String -> EntityPlastic -> Database -> Database
addToDatabase key value database = Map.insert key value database

getValueFromDatabase :: String -> Database -> Maybe EntityPlastic
getValueFromDatabase key database = Map.lookup key database

removeFromDatabase :: String -> Database -> Database
removeFromDatabase key database = Map.delete key database

saveDatabase :: FilePath -> Database -> IO ()
saveDatabase filePath database =
  withFile filePath WriteMode $ \handle -> do
    let serialized = Binary.encode database
    B.hPut handle serialized

loadDatabase :: FilePath -> IO (Maybe Database)
loadDatabase filePath =
  withFile filePath ReadMode $ \handle -> do
    content <- B.hGetContents handle
    case Binary.decodeOrFail content of
      Left _ -> return Nothing
      Right (_, _, result) -> return (Just result)

displayDatabaseFile :: FilePath -> IO ()
displayDatabaseFile filePath = do
  loadedDb <- loadDatabase filePath
  case loadedDb of
    Nothing -> putStrLn "Failed to load database\n"
    Just db -> displayDatabase db

displayDatabase :: Database -> IO ()
displayDatabase database = do
  putStrLn "╔═════════════╦════════════════════════════════════════════════════╦═══════════════╦════════════════╗"
  putStrLn "╠═════════════╬════════════════════════════════════════════════════╬═══════════════╬════════════════╣"
  putStrLn "║     Key     ║                    Product Name                    ║  Softing t°C  ║  Tensile MPa   ║"
  putStrLn "╠═════════════╬════════════════════════════════════════════════════╬═══════════════╬════════════════╣"
  mapM_ displayRecord (Map.toList database)
  printf   "╚═════════════╩════════════════════════════════════════════════════╩═══════════════╩════════════════╝\n"
  where
    displayRecord (key, EntityPlastic { productName = n, tSofting = t, tensileStrength = s}) = do
      printf   "║ %-11s ║ %-50s ║     %-3.2f    ║     %-3.2f      ║\n" key n t s
      putStrLn "╠═════════════╬════════════════════════════════════════════════════╬═══════════════╬════════════════╣"

addEntity :: FilePath -> IO ()
addEntity filePath = do
  putStrLn "How many entities do you want to add?"
  nStr <- getLine
  let n = readMaybe nStr :: Maybe Int
  case n of
    Nothing -> putStrLn "Invalid input"
    Just count -> do
      loadedDb <- loadDatabase filePath
      let initialDb = maybe createDatabase id loadedDb
      database <- addMultipleToDatabase count initialDb
      withFile filePath WriteMode $ \handle -> do
        let serialized = Binary.encode database
        B.hPut handle serialized
      putStrLn "Entities added successfully!\n"

addMultipleToDatabase :: Int -> Database -> IO Database
addMultipleToDatabase n database =
  if n == 0
    then return database
    else do
      putStrLn $ "Enter name, softing temperature and tensile for entity #" ++ show (n) ++ " separated by a space:"
      input <- getLine
      let parts = splitOn " " input
          (productNameParts, rest) = splitAt (length parts - 2) parts
          productNameStr = unwords productNameParts
          tSoftingStr = head rest
          tensileStrengthStr = last rest
          productName = productNameStr
          tSofting = maybe 0 id $ readMaybe tSoftingStr
          tensileStrength = maybe 0 id $ readMaybe tensileStrengthStr
          entityPlastic = createEntity productName tSofting tensileStrength
          key = "entity" ++ show n
          updatedDb = addToDatabase key entityPlastic database
      addMultipleToDatabase (n - 1) updatedDb

searchByName :: String -> Database -> [EntityPlastic]
searchByName name database = Map.elems $ Map.filter (\(EntityPlastic {productName = n}) -> n == name) database

searchByTSofting :: Double -> Database -> [EntityPlastic]
searchByTSofting t database = Map.elems $ Map.filter (\(EntityPlastic {tSofting = temp}) -> temp == t) database

searchByTensileStrength :: Double -> Database -> [EntityPlastic]
searchByTensileStrength s database = Map.elems $ Map.filter (\(EntityPlastic {tensileStrength = strength}) -> strength == s) database

displayEntities :: [EntityPlastic] -> IO ()
displayEntities entities = do
  putStrLn "╔════════════════════════════════════════════════════╦═══════════════╦════════════════╗"
  putStrLn "╠════════════════════════════════════════════════════╬═══════════════╬════════════════╣"
  putStrLn "║                    Product Name                    ║  Softing t°C  ║  Tensile MPa   ║"
  putStrLn "╠════════════════════════════════════════════════════╬═══════════════╬════════════════╣"
  mapM_ displayEntity entities
  printf   "╚════════════════════════════════════════════════════╩═══════════════╩════════════════╝\n"
  where
    displayEntity (EntityPlastic { productName = n, tSofting = t, tensileStrength = s }) = do
      printf   "║ %-50s ║     %-3.2f    ║     %-3.2f      ║\n" n t s
      putStrLn "╠══════════════════════════════════════════╬═══════════════╬════════════════╣"