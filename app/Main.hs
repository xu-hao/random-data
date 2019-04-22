module Main where

import PMD.HEALMapping
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Data.Csv
import Data.List (groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (unpack)
import Faker.Provider.Name (firstNameProvider, resolveNameText, lastNameProvider)
import Faker
import Faker.Internal
import Faker.Combinators
import Faker.Name
import System.Random
import Data.IORef
import Faker.Internet
import Faker.PhoneNumber
import qualified Faker.Lorem as Lorem
import Data.Char
import PMD.SQLGen
import Data.Time

firstName :: FakerSettings -> IO String
firstName settings =
  unpack <$> generateWithSettings settings (Fake (\settings -> randomUnresolvedVec settings firstNameProvider resolveNameText))

lastName :: FakerSettings -> IO String
lastName settings =
  unpack <$> generateWithSettings settings (Fake (\settings -> randomVec settings lastNameProvider))

maxDiff :: Integer
maxDiff = 40

generateWords :: FakerSettings -> String -> IO String
generateWords settings cell = do
  wordList <- map unpack <$> generateWithSettings settings (listOf (length (words cell)) Lorem.words)
  return (unwords wordList)

main :: IO ()
main = do
  [inputFile, tablesDir, outputDir] <- getArgs
  putStrLn ("input: " ++ inputFile)
  let settings = setNonDeterministic defaultFakerSettings
  idIORef <- newIORef 0
  withFile inputFile ReadMode $ \ h -> do
    contents <- BSL.hGetContents h
    case decodeByName contents  of
      Left err -> putStrLn err
      Right (header, rows) ->
        let rowsL = V.toList rows
            isSameTable a b = tableHeal a == tableHeal b
            tables = groupBy isSameTable rowsL        
        in
          mapM_ (\table -> do
                    let tableName = unpack (tableHeal (head table))
                    print tableName
                    let tableDataPath = tablesDir ++ "/" ++ tableName
                    let columnMap = M.fromList (map (\item -> (unpack (fieldNameHEAL item), item)) table)
                    withFile tableDataPath ReadMode $ \ tableDataHandle -> do
                      tableData <- BSL.hGetContents tableDataHandle
                      case decodeByName tableData of
                        Left err -> putStrLn err
                        Right (header, rows) -> do
                          let rowsL = V.toList rows :: [Map String String]
                          rows' <- mapM (\row -> do
                                      M.fromList <$> mapM (\x -> do
                                                                   let column = fst x
                                                                       cell = snd x in do
                                                                     cell' <- case M.lookup column columnMap of
                                                                       Nothing -> do
                                                                         putStrLn ("cannot find column " ++ column)
                                                                         return cell
                                                                       Just item ->
                                                                         let r = randomizationFeature item
                                                                         in
                                                                           case r of
                                                                             FirstName -> firstName settings
                                                                             LastName -> lastName settings
                                                                             Name -> unpack <$> generateWithSettings settings name
                                                                             Id -> return cell {- do
                                                                               i <- readIORef idIORef
                                                                               writeIORef idIORef (i + 1)
                                                                               return (show i) -}
                                                                             Email -> do
                                                                               domain <- unpack <$> generateWithSettings settings freeEmail
                                                                               username <- map toLower . unpack <$> generateWithSettings settings Lorem.words
                                                                               return (username ++ "@" ++ domain)
                                                                             PhoneNumber -> unpack <$> generateWithSettings settings formats
                                                                             LongTitle -> generateWords settings cell
                                                                              
                                                                             ShortTitle -> unpack <$> generateWithSettings settings Lorem.words
                                                                             None ->
                                                                               case dataType item of
                                                                                 SQLDate ->
                                                                                   case cell of
                                                                                     "" -> return cell
                                                                                     _ -> do
                                                                                       let timefromstring = parseTimeOrError False defaultTimeLocale "%F" cell :: Day
                                                                                       diff <- randomRIO (-maxDiff, maxDiff)
                                                                                       let timefromstring' = addDays diff timefromstring
                                                                                       let cell' = formatTime defaultTimeLocale "%F" timefromstring'
                                                                                       -- putStrLn ("date " ++ cell ++ " -> " ++ cell')
                                                                                       return cell'
                                                                                 SQLVarchar ->
                                                                                   generateWords settings cell
                                                                                 SQLBoolean -> do
                                                                                   bool <- randomIO
                                                                                   return (if bool
                                                                                     then "yes"
                                                                                     else "no")
                                                                                 SQLInteger -> do
                                                                                   diff <- randomRIO (-maxDiff, maxDiff)
                                                                                   return (show (maximum [0, diff + read cell]))
                                                                     return (column, cell')
                                                                  ) (M.toList row)
                                  ) rowsL
                          let outputTablePath = outputDir ++ "/" ++ tableName
                          BSL.writeFile outputTablePath (encodeByName header rows')
                ) tables
            
      
