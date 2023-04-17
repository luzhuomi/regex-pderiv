module Main where


import System.Environment (getArgs) 
import System.IO

main :: IO ()
main = do 
    args <- getArgs
    case args of
        { [str, times, file ] -> 
            let t = (read times)::Int
            in do 
            {

                withFile file WriteMode ( \hdl -> 
                    do {
                        mapM_ (\_ -> hPutStr hdl str) [1..t]
                    }
                ) 
            }
        ; _ -> print "Usage: GenInput <str> <num_times_repeated> <file_path>"
        }