open Batteries

(* prob 1 *)
let () = print_endline "=== prob 1 ==="

let rec last list =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
;;

let () = print_endline @@ dump (last [ "a"; "b"; "c"; "d" ])
let () = print_endline @@ dump (last [])

(* prob 2 *)
let () = print_endline "=== prob 2 ==="

let rec last_two list =
  match list with
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

let () = print_endline @@ dump (last_two [ "a"; "b"; "c"; "d" ])
let () = print_endline @@ dump (last_two [ "a" ])

(* prob 3 *)
let () = print_endline "=== prob 3 ==="

let at idx list =
  let rec at' idx list =
    match list with
    | [] -> None
    | x :: tail -> if idx = 1 then Some x else at' (idx - 1) tail
  in
  at' idx list
;;

let () = print_endline @@ dump (at 3 [ "a"; "b"; "c"; "d"; "e" ])
let () = print_endline @@ dump (at 3 [ "a" ])

(* prob 4 *)
let () = print_endline "=== prob 4 ==="

let length list =
  let rec length' len list =
    match list with
    | [] -> len
    | _ :: tail -> length' (len + 1) tail
  in
  length' 0 list
;;

let () = print_endline @@ dump (length [ "a"; "b"; "c" ])
let () = print_endline @@ dump (length [])

(* prob 5 *)
let () = print_endline "=== prob 5 ==="

let rev list =
  let rec rev' acc list =
    match list with
    | [] -> acc
    | x :: tail -> rev' (x :: acc) tail
  in
  rev' [] list
;;

let () = print_endline @@ dump (rev [ "a"; "b"; "c" ])

(* prob 6 *)
let () = print_endline "=== prob 6 ==="

let is_palindrome list =
  let rec is_palindrome' list1 list2 =
    match list1, list2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | x :: tail1, y :: tail2 -> if x = y then is_palindrome' tail1 tail2 else false
  in
  is_palindrome' list (rev list)
;;

let () = print_endline @@ dump (is_palindrome [ "x"; "a"; "m"; "a"; "x" ])
let () = print_endline @@ dump (is_palindrome [ "a"; "b" ])

(* prob 7 *)
let () = print_endline "=== prob 7 ==="

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec flatten' acc list =
    match list with
    | [] -> acc
    | [ One x ] -> acc @ [ x ]
    | [ Many x ] -> flatten' acc x
    | One x :: tail -> flatten' (acc @ [ x ]) tail
    | Many x :: tail -> flatten' (flatten' acc x) tail
  in
  flatten' [] list
;;

let () =
  print_endline
  @@ dump (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
;;

(* prob 8 *)
let () = print_endline "=== prob 8 ==="

let compress list =
  let rec compress' acc list =
    match list with
    | [] -> acc
    | [ x ] -> acc @ [ x ]
    | x :: y :: tail ->
      if x = y then compress' acc (y :: tail) else compress' (acc @ [ x ]) (y :: tail)
  in
  compress' [] list
;;

let () =
  print_endline
  @@ dump
       (compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;

(* prob 9 *)
let () = print_endline "=== prob 9 ==="

let pack list =
  let rec pack' acc1 acc2 list =
    match list, acc2 with
    | [], [] -> acc1
    | [], _ -> acc1 @ [ acc2 ]
    | x :: tail, [] -> pack' acc1 [ x ] tail
    | x :: tail, y :: _ ->
      if x = y then pack' acc1 (x :: acc2) tail else pack' (acc1 @ [ acc2 ]) [ x ] tail
  in
  pack' [] [] list
;;

let () =
  print_endline
  @@ dump
       (pack
          [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ])
;;

(* prob 10 *)
let () = print_endline "=== prob 10 ==="

let encode list =
  let rec encode' acc chr nr list =
    match list with
    | [] -> if nr = 0 then acc else acc @ [ nr, chr ]
    | x :: tail ->
      if nr = 0
      then encode' acc x 1 tail
      else if x = chr
      then encode' acc chr (nr + 1) tail
      else encode' (acc @ [ nr, chr ]) x 1 tail
  in
  encode' [] "" 0 list
;;

let () =
  print_endline
  @@ dump
       (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;

(* prob 11 *)
let () = print_endline "=== prob 11 ==="

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
  let rec encode' acc chr nr list =
    match list with
    | [] ->
      if nr = 0
      then acc
      else if nr = 1
      then acc @ [ One chr ]
      else acc @ [ Many (nr, chr) ]
    | x :: tail ->
      if nr = 0
      then encode' acc x 1 tail
      else if x = chr
      then encode' acc chr (nr + 1) tail
      else if nr = 1
      then encode' (acc @ [ One chr ]) x 1 tail
      else encode' (acc @ [ Many (nr, chr) ]) x 1 tail
  in
  encode' [] "" 0 list
;;

let () =
  print_endline
  @@ dump
       (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;

(* prob 12 *)
let () = print_endline "=== prob 12 ==="

let decode list =
  let rec decode' acc list =
    match list with
    | [] -> acc
    | x :: tail ->
      (match x with
       | One y -> decode' (acc @ [ y ]) tail
       | Many (nr, y) ->
         if nr > 0
         then decode' (acc @ [ y ]) (Many (nr - 1, y) :: tail)
         else decode' acc tail)
  in
  decode' [] list
;;

let () =
  print_endline
  @@ dump
  @@ decode
       [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

(* prob 13 *)
let () = print_endline "=== prob 13 ==="

let encode list =
  let rec encode' acc chr nr list =
    match list with
    | [] ->
      if nr = 0
      then acc
      else if nr = 1
      then acc @ [ One chr ]
      else acc @ [ Many (nr, chr) ]
    | x :: tail ->
      if nr = 0
      then encode' acc x 1 tail
      else if x = chr
      then encode' acc chr (nr + 1) tail
      else if nr = 1
      then encode' (acc @ [ One chr ]) x 1 tail
      else encode' (acc @ [ Many (nr, chr) ]) x 1 tail
  in
  encode' [] "" 0 list
;;

let () =
  print_endline
  @@ dump
       (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;

(* prob 14 *)
let () = print_endline "=== prob 14 ==="

let duplicate list =
  let rec duplicate' acc list =
    match list with
    | [] -> acc
    | x :: tail -> duplicate' (acc @ [ x; x ]) tail
  in
  duplicate' [] list
;;

let () = print_endline @@ dump (duplicate [ "a"; "b"; "c"; "c"; "d" ])

(* prob 15 *)
let () = print_endline "=== prob 15 ==="

let replicate list times =
  let rec repeat acc el times =
    match times with
    | 0 -> acc
    | _ -> repeat (acc @ [ el ]) el (times - 1)
  in
  let rec replicate' list =
    match list with
    | [] -> []
    | x :: tail -> repeat [] x times @ replicate' tail
  in
  replicate' list
;;

let () = print_endline @@ dump (replicate [ "a"; "b"; "c" ] 3)

(* prob 16 *)
let () = print_endline "=== prob 16 ==="

let drop list nth =
  let rec drop' list n =
    match list with
    | [] -> []
    | x :: tail -> if n = 1 then drop' tail nth else x :: drop' tail (n - 1)
  in
  drop' list nth
;;

let () =
  print_endline @@ dump (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)
;;

(* prob 17 *)
let () = print_endline "=== prob 17 ==="

let split list idx =
  let rec split' list n acc1 acc2 =
    match list with
    | [] -> acc1, acc2
    | x :: tail ->
      if n < idx
      then split' tail (n + 1) (acc1 @ [ x ]) acc2
      else split' tail (n + 1) acc1 (acc2 @ [ x ])
  in
  split' list 0 [] []
;;

let x, y = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
let () = print_endline @@ dump x
let () = print_endline @@ dump y
let x, y = split [ "a"; "b"; "c"; "d" ] 5
let () = print_endline @@ dump x
let () = print_endline @@ dump y

(* prob 18 *)
let () = print_endline "=== prob 18 ==="

let slice list st en =
  let rec slice' list idx =
    match list with
    | [] -> []
    | x :: tail ->
      if idx < st
      then slice' tail (idx + 1)
      else if idx > en
      then []
      else x :: slice' tail (idx + 1)
  in
  slice' list 0
;;

let () =
  print_endline @@ dump (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6)
;;

(* prob 19 *)
let () = print_endline "=== prob 19 ==="

let rotate list n =
  let len = length list in
  let n' = if n < 0 then (n mod len) + len else n in
  let a, b = split list n' in
  b @ a
;;

let () = print_endline @@ dump (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)
let () = print_endline @@ dump (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2))

(* prob 20 *)
let () = print_endline "=== prob 20 ==="

let remove_at idx list =
  let rec remove_at' idx list acc =
    match list with
    | [] -> acc
    | x :: tail -> if idx = 0 then acc @ tail else remove_at' (idx - 1) tail (acc @ [ x ])
  in
  remove_at' idx list []
;;

let () = print_endline @@ dump (remove_at 1 [ "a"; "b"; "c"; "d" ])

(* prob 21 *)
let () = print_endline "=== prob 21 ==="

let insert_at value idx list =
  let rec insert_at' value idx list acc =
    match idx, list with
    | 0, _ -> acc @ [ value ] @ list
    | _, [] -> acc @ [ value ]
    | _, x :: tail -> insert_at' value (idx - 1) tail (acc @ [ x ])
  in
  insert_at' value idx list []
;;

let () = print_endline @@ dump (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])
let () = print_endline @@ dump (insert_at "alfa" 3 [ "a"; "b"; "c"; "d" ])
let () = print_endline @@ dump (insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ])

(* prob 22 *)
let () = print_endline "=== prob 22 ==="

let range st en =
  let rec range' st en acc =
    if st == en
    then st :: acc
    else if st < en
    then range' st (en - 1) (en :: acc)
    else range' st (en + 1) (en :: acc)
  in
  range' st en []
;;

let () = print_endline @@ dump (range 4 9)
let () = print_endline @@ dump (range 9 4)
let () = print_endline @@ dump (range 5 5)

(* prob 23 *)
let () = print_endline "=== prob 23 ==="

let rand_select list k =
  let rec extract list n acc =
    match list with
    | [] -> raise Not_found
    | x :: tail -> if n = 0 then x, acc @ tail else extract tail (n - 1) (acc @ [ x ])
  in
  let extract_rand list len acc = extract list (Random.int len) acc in
  let rec rand_select' list k len acc =
    if k = 0
    then acc
    else (
      let picked, rest = extract_rand list len [] in
      rand_select' rest (k - 1) (len - 1) (acc @ [ picked ]))
  in
  rand_select' list k (length list) []
;;

let () = print_endline @@ dump (rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3)

(* prob 24 *)
let () = print_endline "=== prob 24 ==="
let lotto_select n m = rand_select (range 1 m) n
let () = print_endline @@ dump (lotto_select 6 49)
