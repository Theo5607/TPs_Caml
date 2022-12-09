let fibo n =
  match n with
  | 0 | 1 -> n
  | _ ->
    let rec aux n prec1 prec2 =
      if n = 0 then prec1 + prec2
      else aux (n - 1) prec2 (prec1 + prec2)
    in aux (n - 2) 0 1
;;

let rec insert l x n =
  match l with
  | [] -> [x]
  | t::q -> 
    if n = 0 then x::l
    else t::(insert q x (n - 1))
;;

let rec insert_tout l x =
  let rec aux acc t=
    match acc with
    | 0 -> [x::t]
    | _ -> (insert t x acc)::(aux (acc - 1) t)
  in
  match l with
  | [] -> failwith "liste vide"
  | t::[] -> aux (List.length t) t
  | t::q -> (insert_tout q x) @ (aux (List.length t) t)
;;
  
let rec permute l =
  match l with
  | [] -> failwith "liste vide"
  | t::[] -> [[t]]
  | t::q -> insert_tout (permute q) t
;;

let rec partition q n =
  if q = n || n = 1 then 1
  else if n > q then 0
  else (partition (q - 1) (n - 1)) + (partition (q - n) n)
;;