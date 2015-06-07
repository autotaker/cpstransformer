import Parser(term)
import TypeCheck(typeCheck,fromTerm)
import Text.Parsec
import PrettyPrint(pprintTerm)
import Text.PrettyPrint
import Syntax
import Control.Applicative
import Control.Monad

main :: IO ()
main = forever $ do
    l <- getLine
    case parse (term <* eof) "stdin" l of
        Left err -> print err
        Right t -> do
            putStrLn $ ("Parsed: "++) $ (render $ pprintTerm t)
            typeCheck t (putStrLn "TypeCheck : failed") $ \t' -> do
                putStrLn "TypeCheck : OK"
                let x = Symbol "x" (cpsSort $ getSort t')
                let z = Symbol "end" TO
                let k = Abst x (Var z)
                let t'' = cpsT t' k
                putStrLn $ ("CPS : " ++) $ render $ pprintTerm $ fromTerm t''
