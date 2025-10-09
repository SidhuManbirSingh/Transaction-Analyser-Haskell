import Data.List (sortBy, groupBy, intercalate)
import Data.Ord (comparing)
import Data.Monoid (Sum(..))
import Data.Function (on)
import Data.Foldable (foldl')
import Text.Printf (printf)

-- | Transaction Data Structures
-----------------------------------

-- 1. Define Transaction Record 
-- This was already provided in the question statement. 
data Transaction = Transaction
  { tDate :: String    -- Format YYYY-MM-DD for simplicity
  , tCategory :: String
  , tAmount :: Double
  , tNote :: String
  } deriving (Show, Eq)

-- data is a custom type keyword in Haskell to define a new data type.
-- Transaction is the name of the new data type. like classes in JAVA or other programming languages
-- deriving (Show, Eq) 
-- Show allows haskell to convert data to a string and print it like show transaction should work
-- Eq allows comparison of two Transaction objects using == and /= operators.

-- 2. Create sampleTransactions list 

sampleTransactions :: [Transaction]
sampleTransactions =
  [ Transaction "2025-10-01" "Electronics" 199.99 "Wireless headphones"
  , Transaction "2025-10-02" "Books"        25.50 "Haskell Programming Book"
  , Transaction "2025-10-03" "Electronics"   49.99 "USB-C Cable"
  , Transaction "2025-10-04" "Software"     12.00 "Monthly SaaS Subscription"
  , Transaction "2025-10-05" "Books"        15.00 "Sci-Fi Novel"
  , Transaction "2025-10-06" "Electronics"   9.99 "Screen Protector"
  ]

-- SampleTransactions holds a list of data values, and each value is of the Transaction data type that we defined earlier

-- | Transaction Filters
-----------------------------

-- 3. Implement filterByCategory :: String -> [Transaction] -> [Transaction]
-- Filters transactions where tCategory matches the given category string.
filterByCategory :: String -> [Transaction] -> [Transaction]
filterByCategory categoryName =
  filter (\t -> tCategory t == categoryName)

-- Little Explainer:
-- Here our function filterByCategory takes two parameters
-- 1. categoryName of type String
-- 2. A list of Transaction objects [Transaction]
-- and then returns a filtered list of Transaction objects [Transaction]


-- 4. Implement filterByMinAmount :: Double -> [Transaction] -> [Transaction]
-- Filters transactions where tAmount is greater than or equal to the minimum amount.
filterByMinAmount :: Double -> [Transaction] -> [Transaction]
filterByMinAmount minAmount =
  filter (\t -> tAmount t >= minAmount)

 -- Little Explainer:
-- Here our function filterByMinAmount takes two parameters
-- 1. minAmount of type Double
-- 2. A list of Transaction objects [Transaction]
-- and then returns a filtered list of Transaction objects [Transaction]



-- | 📊 Transaction Analysis
-----------------------------

-- 5. Implement totalByCategory :: [Transaction] -> [(String, Double)]
-- Calculates the total amount spent per category.
totalByCategory :: [Transaction] -> [(String, Double)]
totalByCategory transactions =
  -- a. Sort by category (required for groupBy to work correctly)
  let sorted = sortBy (comparing tCategory) transactions
      -- b. Group consecutive transactions with the same category
      grouped = groupBy ((==) `on` tCategory) sorted
      -- c. Map groups to (category, sum amounts)
      calculateTotal group =
        let category = tCategory (head group)
            total = foldl' (\acc t -> acc + tAmount t) 0.0 group
        in (category, total)
  in map calculateTotal grouped

-- Little Explainer:
-- Here our function totalByCategory takes one parameter
-- 1. A list of Transaction objects [Transaction]
-- and then returns a list of tuples [(String, Double)] 
-- where each tuple contains a category and the total amount spent in that category.


-- | 📄 Transaction Formatting
-----------------------------

-- 6. Implement formatTransactions :: [Transaction] -> String
-- Formats a list of transactions into a readable, padded string.
formatTransactions :: [Transaction] -> String
formatTransactions txns =
  -- a. Header: "Date     | Category | Amount | Note"
  let header = "Date       | Category    | Amount    | Note"
      -- Helper function to format a single transaction row
      formatRow t =
        -- Concat fields with padding (" " and "| ") using printf for alignment
        printf "%-10s | %-11s | %8.2f | %s"
          (tDate t) (tCategory t) (tAmount t) (tNote t)
      -- Map formatRow over the transactions
      rows = map formatRow txns
  -- b. Join with unlines
  in unlines (header : rows)

-- | 💻 Main Execution
----------------------

-- 7. Write main :: IO ()
main :: IO ()
main = do
  putStrLn "--- 🧾 E-commerce Transaction Analyzer ---"
  putStrLn "\n## All Sample Transactions"
  putStrLn (formatTransactions sampleTransactions)

  putStrLn "\n## Filtered: Electronics Category"
  -- Chain filters: Filter by Category
  let electronicsTxns = filterByCategory "Electronics" sampleTransactions
  putStrLn (formatTransactions electronicsTxns)

  putStrLn "\n## Filtered: Amount >= $50.00"
  -- Chain filters: Filter by Min Amount
  let highValueTxns = filterByMinAmount 50.00 sampleTransactions
  putStrLn (formatTransactions highValueTxns)

  putStrLn "\n## Filtered: Books AND Amount >= $20.00"
  -- Chain filters: Filter by Category, then by Min Amount
  let expensiveBooks =
        filterByMinAmount 20.00 $
        filterByCategory "Books" sampleTransactions
  putStrLn (formatTransactions expensiveBooks)

  putStrLn "\n## Total Spent By Category (All Transactions)"
  -- Print totals
  let totals = totalByCategory sampleTransactions
  mapM_ (\(cat, total) -> printf "  %-11s: $%.2f\n" cat total) totals