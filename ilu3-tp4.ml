(* GRADE:  100% *)
let rec hanoi (source, temp, dest) n = 
  if n<=0 then []
  else hanoi (source, dest, temp) (n-1)
       @ (source, dest)::hanoi(temp, source, dest) (n-1);;


let rec map f l = match l with
  | [] -> []
  |x::l' -> (f x)::(map f l');;


let rec inserer x l = match l with
  | [] -> [x] 
  | y::l' -> if x<=y then x::l else y::(inserer x l');;


let rec triInsertion l = match l with
  | [] -> []
  | x::l' -> inserer x (triInsertion l');;


let rec partage l = match l with 
  | [] -> [], []
  | x::l' -> let l1, l2 = partage l' in 
      if List.length l1 <= List.length l2 then x::l1, l2 else l1, x::l2;;


let rec merge l1 l2 = match l1 with
  |[] -> l2
  | h::t -> match l2 with 
    |[] -> l1
    |h2::t2 -> if h<=h2 then h::merge t l2 else h2::merge l1 t2;;


let rec triFusion l = match l with
  | [] -> []
  | [x] -> [x]
  | _ -> let (l1, l2) = partage l in merge (triFusion l1) (triFusion l2);;


let fst (x, y) = x;;

let snd (x, y) = y;;

let rec estFonction l =
  match l with
  |[] -> true
  |[x] -> true 
  |h1::h2::t -> fst h1 <> fst h2 && estFonction (h1::t) && estFonction (h2::t);;

let rec image e l = match l with
  | []-> failwith "pas image"
  | x::l' -> if e=fst x then snd x else image e l';;

let rec imageEns le lc = match le with
  | []-> []
  | x::l' -> image x lc::imageEns l' lc;;


let rec estInjective l =
  match l with
  |[] -> true
  |[x] -> true 
  |h1::h2::t -> snd h1 <> snd h2 && estInjective (h1::t) && estInjective (h2::t);;


let rec isDef x liste =
  match liste with 
  | [] -> false
  |h::t -> x=fst h || isDef x t;;
             
let rec surcharge l1 l2 = 
  match l1 with
  | [] -> l2
  | h::t -> if (isDef (fst h) l2) then surcharge t l2
      else h::surcharge t l2;;


let rec composition l1 l2 =
  match l2 with
  | [] -> []
  | h::t -> if (isDef (snd h) l1) then (fst h,image (snd h) l1)::composition l1 t
      else composition l1 t ;;

let rec produit l1 l2 = 
  match (l1,l2) with 
  | [],_ -> []
  | _, [] -> []
  |(h::t),(hh::tt) -> ((fst h,fst hh),(image (fst h) l1,image (fst hh) l2))::produit [h] tt@produit t l2
