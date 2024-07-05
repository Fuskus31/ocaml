(* GRADE:  100% *)
let rec premierCh n = if n<0 then failwith "premierCh" else
  if n<10 then n else premierCh (toutSaufDer n);;

let rec toutSaufPrem n = if n<0 then failwith "toutSaufPrem" else
  if n<10 then 0 else toutSaufPrem (toutSaufDer n) * 10 + dernierCh n;;

let rec estPalindrome n = if n<0 then estPalindrome (-n) else
    match n<10 with
      true -> true |
      _ -> (premierCh n = dernierCh n && estPalindrome(toutSaufPrem( premierCh n)));;

let rec nbOccs c n = if n<10 then if c = n then 1 else 0 else 
  if dernierCh n = c then 1 + nbOccs c (toutSaufDer n) else nbOccs c (toutSaufDer n);;

let rec iterer n f x = if n=0 then x else iterer (n-1) f (f x);;

let id f = f;;

let rec compose f g x = f (g x);;

let rec iterer2 n f = if n=0 then id else compose f (iterer2 (n-1) f);;

let rec itererBis f p x = if p x then x else itererBis f p (f x);;

let rec qqsoit n p = n<= 0 ||  p n && qqsoit (n-1) p;;

let rec fastpow n e = if e<0 then failwith "erreur" else
  if e=0 then 1 else if (e land 1)=0 then fastpow(n*n) (e asr 1)
  else n*(fastpow (n*n) ((e-1) asr 1));;

let rec ack (m, n) =  
  if m=0 then n+1 else if m>0 && n=0 then ack ((m-1), 1) else 
  if m>0 && n>0 then ack ((m-1), (ack (m, (n-1)))) else
    failwith "erreur"






