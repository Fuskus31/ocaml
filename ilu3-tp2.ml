(* GRADE:  100% *)
let estZero_v1 n = match n with 0 -> "zero";;

let estZero_v2 n = match n with 
    0 -> "zero" |
    _ -> "nonZero";;

let voyelle x = match x with 
    'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true |
    _ -> false;;

let rang x = match x with 
    "lundi" -> 1 |
    "mardi" -> 2 |
    "mercredi" -> 3|
    "jeudi" -> 4 |
    "vendredi" -> 5 |
    "samedi" -> 6 |
    "dimanche" -> 7 |
    _ -> 0;;

let inf a b = let c = rang a in let d = rang b in (c <> 0 && d <> 0) && (c = d-1) || (c = 7 && d = 1);;
  

let jsem x = match x with
    1 -> "lundi" |
    2 -> "mardi" |
    3 -> "mercredi" |
    4 -> "jeudi" |
    5 -> "vendredi" |
    6 -> "samedi" | 
    7 -> "dimanche" |
    _ -> "jour inconnu";;


let jourSucc1 a = match a with 
    "lundi" -> "mardi" |
    "mardi" -> "mercredi" |
    "mercredi" -> "jeudi" |
    "jeudi" -> "vendredi" |
    "vendredi" -> "samedi" |
    "samedi" -> "dimanche" |
    "dimanche" -> "lundi" |
    _ -> "jour inconnu";;

let jourSucc2 a = let b = rang a in if b = 7 then jsem 1 else if b = 0 then jsem 0 else jsem (b + 1);; 

let jourSucc3 a = let b = rang a in if b = 0 then jsem 0 else jsem (b mod 7 + 1);;

let jourPred1 a = match a with 
    "lundi" -> "dimanche" |
    "mardi" -> "lundi" |
    "mercredi" -> "mardi" |
    "jeudi" -> "mercredi" |
    "vendredi" -> "jeudi" |
    "samedi" -> "vendredi" |
    "dimanche" -> "samedi" |
    _ -> "jour inconnu";;

let jourPred2 a = let b = rang a in if b = 0 then jsem 0 else if b = 1 then jsem 7 else jsem (b - 1);;

let jourPred3 a = let b = rang a in if b = 1 then jsem 7 else jsem (b - 1);;

let bissextile a = (a mod 4 = 0) && (a mod 100 <> 0) || (a mod 400 = 0);;

let nbjour a b = match a with 
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31 |
    4 | 6 | 9 | 11 -> 30 |
    2 -> if bissextile b then 29 else 28 |
    _ -> 0;;



