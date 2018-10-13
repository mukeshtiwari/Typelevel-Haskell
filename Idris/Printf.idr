

 
{- Types can be treat in same way as the values. -}
returnType : Nat -> Type
returnType Z = Int
returnType (S Z) = String
returnType (S (S n)) = Int -> Int -> Int


{- We can return Int, String or function based on input value -}
valueType : (n : Nat) -> returnType n
valueType Z = 1
valueType (S Z) = "Hello World"
valueType (S (S n)) = \x => \y => x + y



printfType : List Char  -> Type
printfType [] = String
printfType ('%' :: 'd' :: rest)  = Int -> printfType rest
printfType ('%' :: 's' :: rest) = String -> printfType rest
printfType (c :: rest)  = printfType rest 


{- construct value level function -}
printfValue : (xs : List Char) -> String -> printfType xs
printfValue [] acc = acc
printfValue ('%' :: 'd' :: rest) acc = \x => printfValue rest (acc ++ show x)
printfValue ('%' :: 's' :: rest) acc = \x => printfValue rest (acc ++ x)
printfValue (c :: rest) acc = printfValue rest (acc ++ singleton c)


data Format = FInt Format
            | FString Format
            | FOther Char Format
            | FEnd

format : List Char -> Format
format ('%' :: 'd' :: cs ) = FInt ( format cs )
format ('%' :: 's' :: cs ) = FString ( format cs )
format ( c :: cs )         = FOther c ( format cs )
format []                  = FEnd

interpFormat : Format -> Type
interpFormat (FInt f)     = Int -> interpFormat f
interpFormat (FString f)  = String -> interpFormat f
interpFormat (FOther _ f) = interpFormat f
interpFormat FEnd            = String


formatString : String -> Format
formatString s = format (unpack s)


toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction (FInt f) a     = \i => toFunction f ( a ++ show i )
toFunction (FString f) a  = \s => toFunction f ( a ++ s )
toFunction (FOther c f) a = toFunction f ( a ++ singleton c )
toFunction FEnd a           = a 


printf : (s : String) -> interpFormat ( formatString s )
printf s = toFunction ( formatString s ) "" 

