import Text.Read
import Control.Monad

data BankAccount = BankAccount 
                   { balance :: Double
                   , bank :: Int
                   , accnr :: Int
                   , transactions :: [Transaction]
                   } deriving (Show)
                   
data Transaction = Transaction
                   { from :: Int
                   , to :: Int
                   , amount :: Double
                   } deriving (Show)

data Bank = Bank {uid :: Int} deriving (Show, Eq)

add :: Int -> [BankAccount] -> [BankAccount]
add = undefined

getInput :: IO (Maybe Transaction)
getInput = fmap (tuple . words) getLine

tuple :: [String] -> Maybe Transaction
tuple [from, to, amount] = fmap create from' <*> to' <*> amount'
                                    where
                                from' = readMaybe from :: Maybe Int
                                to' = readMaybe to :: Maybe Int
                                amount' = readMaybe amount :: Maybe Double
                                create a b c = Transaction a b c
tuple _ = Nothing

repl :: [BankAccount] -> IO ()
repl accounts = do
    action <- getInput
    print action
    repl accounts
    
main :: IO ()
main = repl []