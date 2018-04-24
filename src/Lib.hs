module Lib
    ( someFunc
    , someString
    , grid
    , languages
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someString :: String
someString = "someString"

grid = [  "_ _C _ _ _ _ _ _ _R_ _ _ "
           , " _ _SI_ _ _ _ _ _ _ _U_ _ "
           , " _ _HASKELL _ _ _ _B_"
           , " _ _A_ _A_ _ _ _ _S_ _Y "
           , " _ _R_ _ _B_ _ _C _ _ _ "
           , "_ _PHP_ _ _ _H_ _ _ _ "
           , "_ _ _ _S_LREP_ _ _ _ _ "
           , "_ _ _ _I_ _M_Y_ _L_  _ "
           , "_ _ _ _L_E_ _T_O _ _ _ "
           , "_ _ _ _ _ _ _ HB_ _ _ _  "
           , "_ _ _ _ _ _ _ O_ _ _ _ _  "
           , "_ _ _ _ _ _ CN_ _ _ _ _  " ]

languages = [ "BASIC"
                    , "COBOL"
                    , "CSHARP"
                    , "HASKELL"
                    , "LISP"
                    , "PHP"
                    , "PYTHON"
                    , "RUBY"
                    , "SCHEME"]
