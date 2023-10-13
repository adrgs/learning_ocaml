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
