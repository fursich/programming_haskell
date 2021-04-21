import Parser
import Control.Applicative
import Data.Char

comment :: Parser ()
comment = do symbol "--"
            many sat /= '\n'
            return ()

