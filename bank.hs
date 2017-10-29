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

help = "\nThese are your options: \n" ++
       "quit - exit the program, accounts will be lost\n" ++
       "help - display this menu\n" ++
       "add - create a new account\n"

add :: Int -> [BankAccount] -> [BankAccount]
add inc accounts = BankAccount 0 0 inc [] : accounts

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

repl :: [BankAccount] -> Int -> IO ()
repl accounts inc = do
    action <- getLine
    case action of
        "quit" -> putStrLn "\nGoodbye!"
        "help" -> putStrLn help >> repl accounts inc
        "add"  -> repl (add inc accounts) (inc+1)
        _      -> putStrLn "Not a valid choice, try \"help\".\n" >> repl accounts inc
 
main :: IO ()
main = repl [] 0
