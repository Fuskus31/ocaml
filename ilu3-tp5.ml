(* GRADE:  100% *)
module ENV_LIST : tENV_LIST = struct
  type ('x, 'v) env = ('x * 'v) list
  let empty = []
  let rec get cle env = match env with
    |[] -> failwith "erreur vide"
    |(x, y)::l -> if x=cle then y else get cle l
    
  let rec put x y l = match l with
    |[] -> [x, y]
    |(a, b)::l' -> if a=x && b=y then l else (a,b)::put x y l'
end;;


module ENV_FUN : tENV_FUN = struct
  type ('x, 'v) env = 'x -> 'v
  let empty = fun x -> failwith "vide"
  let get x l = l x
  let put x y l = fun a -> if a=x then y else l a
end;;

let regle_if e = match e with
  | If(c,BConst(true),BConst(false)) -> c
  | If(c1,BConst(true),c2) -> Call(Or,c1,c2)
  | If(c1,c2,BConst(false)) -> Call(And,c1,c2)
  | _->e;;


let rec apply regle e = match e with
  | If(a,b,c) -> regle (If(apply regle a,apply regle b,apply regle c))
  | Let(s,a,b) -> regle (Let(s,apply regle a,apply regle b))
  | Call(s,a,b) -> regle (Call(s,apply regle a,apply regle b))
  | _ -> regle e;;
    


module EVAL : tEVAL = functor(E : tENV) -> struct
  type value = Vint of int | Vbool of bool
  let eval_op op v1 v2 = 
    match v1,v2 with
    | Vint(_),Vbool(_) -> failwith "type different"
    | Vbool(_),Vint(_) -> failwith "type different"
    | Vint(a),Vint(b) -> ( match op with
        | Add -> Vint(a+b)
        | Leq -> Vbool(a <= b)
        | _ -> failwith "mauvais operateur pour les types enonces"  )
    | Vbool(a),Vbool(b) -> ( match op with
        | And -> Vbool(a && b)
        | Or -> Vbool(a || b)
        | _ -> failwith "mauvais operateur pour les types enonces" ) 
  let rec eval env e = match e with
    | IConst x -> Vint(x)
    | BConst x -> Vbool(x)
    | Var x -> E.get x env
    | If(c1,c2,c3) -> if (eval env c1) = Vbool(true) then eval env c2 else eval env c3
    | Let(name,e1,e2) -> eval (E.put name (eval env e1) env) e2
    | Call(bin,e1,e2) -> eval_op bin (eval env e1) (eval env e2)
end
    
module TYPECHECK : tTYPECHECK = functor (E : tENV) -> struct
  let typeof_op op = match op with
    | And -> Bool,Bool,Bool
    | Or -> Bool,Bool,Bool
    | Add -> Int,Int,Int
    | Leq -> Int,Int,Bool
  let rec typeof env e = match e with
    | IConst x -> Int
    | BConst x -> Bool
    | Var x -> E.get x env
    | If(c1,c2,c3) -> let t = typeof env c2 in if t = (typeof env c3) then t 
        else failwith "expression mal typee"
    | Let(name,e1,e2) -> typeof (E.put name (typeof env e1) env) e2
    | Call(bin,e1,e2) -> let type_e1,type_e2,type_res = typeof_op bin in 
        if type_e1 = (typeof env e1) && type_e2 = (typeof env e2) then type_res 
        else failwith "expression mal typee"
end

















