import System.Environment

main = do x <- getLine
          let y = read x
          return (y + 1)
