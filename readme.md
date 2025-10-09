# E-commerce Transaction Analyzer

A functional Haskell application for analyzing and filtering e-commerce transaction data, built to demonstrate core functional programming concepts including filtering, folding, grouping, and formatted output[web:11].

## Overview

This project implements a transaction management system that allows users to filter transactions by category and amount, calculate spending totals by category, and format transaction data for display[web:9]. The implementation emphasizes Haskell's functional programming paradigms with efficient list operations and strict evaluation strategies[web:8].

## Features

### Transaction Management
- **Custom Transaction Type**: Defines structured transaction records with date, category, amount, and notes
- **Sample Data**: Includes realistic sample transactions across multiple categories (Electronics, Books, Software)

### Filtering Operations
- **Category Filter**: Filter transactions by specific category name
- **Amount Filter**: Filter transactions meeting minimum amount thresholds
- **Composable Filters**: Combine multiple filters for complex queries (e.g., expensive books)

### Analysis Capabilities
- **Category Totals**: Calculate total spending per category using strict left-fold evaluation (`foldl'`)
- **Sorted Output**: Automatically groups and sorts transactions by category for analysis

### Formatted Output
- **Table Formatting**: Generates aligned, padded tables using `printf` for readable display
- **Consistent Layout**: Fixed-width columns for dates, categories, amounts, and notes

## Technical Highlights

### Performance Optimization
Uses `foldl'` (strict left fold) instead of lazy `foldl` to prevent stack overflow on large transaction lists, ensuring efficient memory usage during aggregation operations[memory:2].

### Functional Composition
Demonstrates function composition with the `$` operator and chaining filters to build complex queries from simple, reusable functions[web:8].

### Type Safety
Leverages Haskell's strong type system with custom data types deriving `Show` and `Eq` for automatic string conversion and equality comparisons[web:9].

## Requirements

- GHC (Glasgow Haskell Compiler) 8.0 or higher
- Standard Haskell libraries (included with GHC)

### Dependencies
```
Data.List        -- sortBy, groupBy, intercalate
Data.Ord         -- comparing
Data.Monoid      -- Sum
Data.Function    -- on
Data.Foldable    -- foldl'
Text.Printf      -- printf
```

## Installation

### Using GHC
```
ghc -o transaction-analyzer Main.hs
./transaction-analyzer
```

### Using Stack
```
stack ghc -- -o transaction-analyzer Main.hs
./transaction-analyzer
```

### Interactive Mode (GHCi)
```
ghci Main.hs
> main
```

## Usage

### Running the Application
Execute the compiled binary or run through GHCi to see demonstration output including all sample transactions, filtered views by category and amount, and spending totals[web:11].

### Code Examples

**Filter by Category:**
```
filterByCategory "Electronics" sampleTransactions
-- Returns only Electronics transactions
```

**Filter by Minimum Amount:**
```
filterByMinAmount 50.00 sampleTransactions
-- Returns transactions >= $50.00
```

**Combined Filters:**
```
filterByMinAmount 20.00 $ filterByCategory "Books" sampleTransactions
-- Returns Books transactions >= $20.00
```

**Calculate Category Totals:**
```
totalByCategory sampleTransactions
-- Returns [(String, Double)] tuples of (category, total)
```

## Project Structure

```
Main.hs              -- Complete implementation
├── Transaction      -- Custom data type definition
├── Filters          -- filterByCategory, filterByMinAmount
├── Analysis         -- totalByCategory
├── Formatting       -- formatTransactions
└── Main            -- Demonstration and output
```

## Learning Objectives

This project demonstrates understanding of:
- Custom algebraic data types with record syntax
- Higher-order functions (`filter`, `map`, `foldl'`)
- List processing with sorting and grouping
- Strict evaluation for performance
- Function composition and lambda expressions
- Type signatures and type safety
- String formatting with `printf`

## Sample Output

```
--- E-commerce Transaction Analyzer ---

## All Sample Transactions
Date       | Category    | Amount    | Note
2025-10-01 | Electronics |   199.99 | Wireless headphones
2025-10-02 | Books       |    25.50 | Haskell Programming Book
...

## Total Spent By Category (All Transactions)
  Books      : $40.50
  Electronics: $259.97
  Software   : $12.00
```

## Extending the Project

### Suggested Enhancements
- Add date range filtering
- Implement transaction persistence (file I/O)
- Calculate statistics (average, median spending)
- Add transaction editing and deletion
- Export reports to CSV format
- Implement tag-based filtering

## License

This project is available for educational purposes[web:11].

## Author

Created as a functional programming learning project demonstrating Haskell fundamentals[web:9].
```

This README follows best practices for Haskell documentation, providing clear installation instructions, usage examples, technical details about implementation choices, and suggestions for extending the project.

[1](https://blog.haskell.org/documentation-best-practices-in-2024/)
[2](https://sakshamsharma.com/2018/03/haskell-proj-struct/)
[3](https://www.freecodecamp.org/news/how-to-write-a-good-readme-file/)
[4](https://kowainik.github.io/posts/haddock-tips)
[5](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html)
[6](https://stackoverflow.com/questions/7947981/how-do-i-import-the-foldable-class-into-my-module)
[7](https://hackage.haskell.org/package/base/docs/Data-Foldable.html)
[8](https://xmonad.github.io/xmonad-docs/base-4.16.4.0/src/Data-Foldable.html)
[9](https://stackoverflow.com/questions/28118063/how-can-i-make-this-fold-more-generic)
[10](https://nikivazou.github.io/CMSC498V/lectures/MonoidsAndFoldables.html)
[11](https://www.reddit.com/r/haskell/comments/z7gadt/help_understanding_foldl/)
[12](https://news.ycombinator.com/item?id=36773022)
[13](https://www.reddit.com/r/haskell/comments/yr2grn/how_do_you_structure_haskell_projects/)
[14](https://www.reddit.com/r/haskell/comments/3zgork/is_there_any_hope_to_see_the_import_problem_solved/)
[15](https://www.reddit.com/r/haskell/comments/1j2xmi0/first_haskell_project_any_tips_best_practices/)
[16](https://haskell-haddock.readthedocs.io)
[17](https://stackoverflow.com/questions/37874974/best-practices-for-distributing-a-haskell-application-and-updating-it)
[18](https://github.com/haskell/cabal/issues/9214)
[19](https://moldstud.com/articles/p-a-comprehensive-guide-to-haskell-linting-best-practices-for-clean-code)
[20](https://www.youtube.com/watch?v=YYyMngA0HxM)