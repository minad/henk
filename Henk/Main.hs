---------------------------------------------------------------------
-- Henk 2000: Based on Pure Type Systems
-- by Jan-Willem Roorda
-- WWW: http://www.cs.uu.nl/~johanj/MSc/jwroorda
-- e-mail: jw@cs.uu.nl
---------------------------------------------------------------------

module Main where

import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import qualified Data.Text.Lazy as L
import Henk.HenkAS
import Henk.HenkParser(program,single_expr)
import Henk.HenkPP(expr2string)
import Henk.HenkInt(intmain,prog2DeltaRules)
import Henk.HenkTI(timain,tiexpr)
import Henk.HenkTC(tcmain,tcexpr)
import Henk.TypeSystems


welcome = "__   __ ______ __  __ __      ______________________________________________\n"++
          "||   || ||_ || ||  || ||/     Henk 2000: Based on Pure Type Systems     \n"++
          "||___|| ||- __ ||\\ || ||\\                                               \n"++
          "||---|| ||__|| || \\|| 2000    WWW: http://www.cs.uu.nl/~johanj/MSc/jwroorda\n"++
          "||   ||                       Report bugs to: jw@cs.uu.nl                \n"++
          "||   ||                       ______________________________________________\n\n"


typeSystem = cc

main :: IO ()
main = do{ putStr welcome
         ; putStr "Loading prelude.henk...\n\n"
         ; prelude_result <- parseFromFile program "prelude.henk"
         ; prelude_prog   <- case prelude_result of
                               Left  err   -> do{putStr (show(err)); return $ error ""}
                               Right prog  -> return prog
         ; prelude_rules  <- return $ prog2DeltaRules prelude_prog
         ; putStr $ "Type inferencing prelude.henk...\n"
         ; (prelude_ti_er,(prelude_prog,prelude_anns))  <- return $ timain [] prelude_prog
         ; putErrors prelude_ti_er ; putStr "\n"
         ; putStr $ "Type checking prelude.henk...\n"
         ; (prelude_tc_er,_) <- return $ tcmain prelude_rules typeSystem prelude_prog
         ; putErrors prelude_tc_er
         ; putStrLn   "\nEnter name of programfile <prog.henk>:"
         ; fnprog   <- getLine
         ; fnprog   <- if fnprog=="" then return "prog.henk" else return $ fnprog
         ; putStr   $ "\nParsing "++fnprog++"...\n\n"
         ; prog     <- parseFromFile program fnprog
         ; prog     <- case prog of
                        Left err -> do{ putStr (show(err)); return $ error ""}
                        Right d  -> return d
         ; prog_rules  <- return $ prog2DeltaRules prog
         ; putStr $ "Type inferencing...\n"
         ; (er,(prog,_)) <- return $ timain prelude_anns prog
         ; putErrors er
         ; putStr "\n"
         ; putStr $ "Type checking...\n"
         ; (er,_) <- return $ tcmain (prog_rules++prelude_rules) typeSystem prog
         ; putErrors er
         ; ev_expr  <- (intmain (prog_rules++prelude_rules) prog)
         ; putStr   $ ""
         }


putErrors xs = do {mapM (\s -> putStr $ s ++ "\n") (take 1 xs)
                  ;putStr $ "Numbers of errors: "++(show (length xs))++"\n"
                  ;return ()}

add_line_numbers :: String -> String
add_line_numbers s  =  (concat.(map (\(x,y) -> x++y)).(zip $ numbers l).(map (++"\n" ))) (lines s)
                       where
                       l = length (lines s)

numbers :: Int -> [String]
numbers l = [ (zeros i) ++ (show i) ++ ": " | i <- [1..]]
            where
            zeros i = take (length (show l) - (length (show i))) ['0' | j <- [0..]]

-- ti performs type inference on a single expression
ti :: String -> IO ()
ti s =
       do{ putStr "Parsing expression....\n"
         ; mex <- return $ parse single_expr "" (L.pack s)
         ; ex  <- case mex of
                Left  err -> do{putStr (show(err)); return $ error ""}
                Right ex  -> return ex
         ; putStr "Loading prelude.henk...\n\n"
         ; prelude_result <- parseFromFile program "prelude.henk"
         ; prelude_prog   <- case prelude_result of
                               Left  err   -> do{putStr (show(err)); return $ error ""}
                               Right prog  -> return prog
         ; prelude_rules  <- return $ prog2DeltaRules prelude_prog
         ; putStr $ "Type inferencing prelude.henk...\n"
         ; (prelude_ti_er,(prelude_prog,prelude_anns))  <- return $ timain [] prelude_prog
         ; putErrors prelude_ti_er ; putStr "\n"
         ; putStr $ "Type checking prelude.henk...\n"
         ; (prelude_tc_er,_) <- return $ tcmain prelude_rules typeSystem prelude_prog
         ; putErrors prelude_tc_er
         ; putStr   "\nEnter name of programfile <prog.henk>:"
         ; fnprog   <- return "" --getLine
         ; fnprog   <- if fnprog=="" then return "prog.henk" else return $ fnprog
         ; putStr   $ "\nParsing "++fnprog++"...\n\n"
         ; prog     <- parseFromFile program fnprog
         ; prog     <- case prog of
                        Left err -> do{ putStr (show(err)); return $ error ""}
                        Right d  -> return d
         ; prog_rules  <- return $ prog2DeltaRules prog
         ; putStr $ "Type inferencing program...\n"
         ; (er,(prog,prog_anns)) <- return $ timain prelude_anns prog
         ; putErrors er
         ; putStr "\n"
         ; putStr $ "Type checking program...\n"
         ; (er,_) <- return $ tcmain (prog_rules++prelude_rules) typeSystem prog
         ; putErrors er
         ; putStr "\n"
         ; putStr $ "Type inferencing expression...\n"
         ; (er,ex) <- return $ tiexpr (prelude_anns ++ prog_anns) prog ex
         ; putErrors er
         ; putStr "\n"
         ; putStr $ "Type checking expression...\n"
         ; (er,ext) <- return $ tcexpr (prog_rules++prelude_rules) typeSystem ex
         ; putErrors er
         ; putStr "\n"
         ; putStr $ (expr2string ex) ++ " has type: " ++(expr2string ext)
         }






