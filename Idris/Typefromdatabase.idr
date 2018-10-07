%default total

data Tag =  A | B | C | Unknown | Unchecked

data Doutput : Tag -> Type where 
  Ca : Doutput A 
  Cb : Doutput B
  Cc : Doutput C
  Cu : Doutput Unknown
  Cs : String -> Doutput Unchecked  

{- Based on output from database you construct type -}
typelevel : (a : Doutput Unchecked) -> Type
typelevel (Cs "A") = Doutput A
typelevel (Cs "B") = Doutput B
typelevel (Cs "C") = Doutput C
typelevel (Cs s) = Doutput Unknown

{- Check value takes a type of Doutput Unchecked and transforms into one the known or unknown file format -}
checkValue : (a : Doutput Unchecked) -> typelevel a
checkValue (Cs "A") = Ca
checkValue (Cs "B") = Cb
checkValue (Cs "C") = Cc
checkValue (Cs s) = ?Cu {- I am leaving it as meta variable beacause Idris is refusing to simplify it -}

{- 

*Typefromdatabase> :r
Type checking ./Typefromdatabase.idr
Holes: Main.Cu1
*Typefromdatabase> ch
changeDir   check       checkValue  choice      choiceMap   chr
*Typefromdatabase> checkValue (Cs "A")
Ca : Doutput A
Holes: Main.Cu1
*Typefromdatabase> checkValue (Cs "B")
Cb : Doutput B
Holes: Main.Cu1
*Typefromdatabase> checkValue (Cs "C")
Cc : Doutput C
Holes: Main.Cu1
*Typefromdatabase> checkValue (Cs "Hello Wrold")
?Cu1 : Doutput Unknown
Holes: Main.Cu1
*Typefromdatabase>  -}


