(* GRADE:  100% *)
let multiple_of n d = if (n mod d) = 0 then true else false;;


let integer_square_root n = int_of_float (sqrt (float_of_int n));;


let last_character chaine = let taille = String.length(chaine) in chaine.[taille-1];;


let string_of_bool boolean = string_of_bool boolean;; 


let pairwise_distinct (a, b, c, d) = a<>b && a<>c && a<>d && b<>c && b<>d;;


(*
: float * int * bool * string = (2., 5 , true, "salut")

: (char * int) * (char * int) * (char * int) = (('a', 1), ('b', 2), ('c', 3))

: int * int * int  = (1, 2, 3)

: int * (int * int) = (1, (2, 3))

: bool = false

: pas même type

: 'a -> 'b -> 'b -> 'a <fun>
*)


let e1 = (1, false), 1;;


let e2 = (fun f -> f), 'a';;


let f1 = fun a b -> (1, false);;


let f2 = fun f -> true;;


let f3 = fun f f -> 1;;


let f4 = fun (a, b) -> (b, a);;


let f5 a b c = let d = a c in b c d;;


let f6 (a, b, c) = b(c, a c);;


let f7 a b c d e = let (x, y) = a e in c ((b x y), (a(e) = a(d)));;


let f8 a b c d = if a = b && c = d then (a, c) else (b, d);;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


