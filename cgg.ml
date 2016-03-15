(* anagrams *)

(* tail recursive but could be better using array instead of list *)
let rec removeOne x ls2 acc =
  begin match ls2 with
  | [] -> None
  | y :: ls2' -> if x = y then
      Some (((*List.rev*) acc) @ ls2')
    else
      removeOne x ls2' (y :: acc)
  end;;

(* tail recursive but could be better using array instead of list *)
let rec ana ls1 ls2 =
  begin match ls1, ls2 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | x :: ls1' , _ ->
     begin match  (removeOne x ls2 [])  with
     | None -> false
     | Some ls2'' ->  ana ls1' ls2''
     end
  end;; 

ana ['a'; 'b'; 'c'; 'd'] ['b'; 'c'; 'd'; 'a'];;
ana ['b'; 'a'; 'd'] ['d'; 'a'; 'a'];;
ana ['a'; '1'; 'b'; '2'; 'c'; '3'] ['a'; 'b'; 'c'; '1'; '2'; '3'];;


(* stack it *)

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;

let _ = gcd (gcd 84 90) 120 = 6;;

let rec gcdList ls =
  match ls with
  | [] -> 0
  | x :: ls' ->
     List.fold_left (fun acc l -> gcd l acc) x ls';;

gcdList [84; 90; 120];;


(* the database tables are *)
type slot = {id : int; size : int}
type valet = {id : int; name : string}
type track = {index : int; who : valet; where: slot option; carCustomer : string }
  
