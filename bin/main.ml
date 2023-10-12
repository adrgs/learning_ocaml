(* prob 1 *)
let () = print_endline "=== prob 1 ==="

let rec last list =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
;;

let f a =
  match a with
  | None -> print_endline "None"
  | Some x -> print_endline x
;;

let () = f (last [ "a"; "b"; "c"; "d" ])
let () = f (last [])

(* prob 2 *)
let () = print_endline "=== prob 2 ==="

let rec last_two list =
  match list with
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

let f list =
  match last_two list with
  | Some (_x, _y) ->
    let () = print_endline _x in
    let () = print_endline _y in
    ()
  | None -> print_endline "None"
;;

let () = f [ "a"; "b"; "c"; "d" ]
let () = f [ "a" ]

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

let f idx list =
  match at idx list with
  | Some x -> print_endline x
  | None -> print_endline "None"
;;

let () = f 3 [ "a"; "b"; "c"; "d"; "e" ]
let () = f 3 [ "a" ]

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

let f list = print_endline (string_of_int (length list))
let () = f [ "a"; "b"; "c" ]
let () = f []

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

let f list =
  let rec f' list =
    match list with
    | [] -> ()
    | x :: tail ->
      let () = print_endline x in
      f' tail
  in
  f' list
;;

let () = f (rev [ "a"; "b"; "c" ])

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

let f list = print_endline (string_of_bool (is_palindrome list))
let () = f [ "x"; "a"; "m"; "a"; "x" ]
let () = f [ "a"; "b" ]

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

let f list =
  let rec f' list =
    match list with
    | [] -> ()
    | x :: tail ->
      let () = print_endline x in
      f' tail
  in
  f' list
;;

let () = f (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])

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

let f list =
  let rec f' list =
    match list with
    | [] -> ()
    | x :: tail ->
      let () = print_endline x in
      f' tail
  in
  f' list
;;

let () =
  f (compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
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

let f list =
  let rec f' list =
    match list with
    | [] -> ()
    | x :: tail ->
      let () = print_endline (String.concat "" x) in
      f' tail
  in
  f' list
;;

let () =
  f (pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ])
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

let f list =
  let rec f' list =
    match list with
    | [] -> ()
    | (nr, chr) :: tail ->
      let () = print_endline (string_of_int nr ^ chr) in
      f' tail
  in
  f' list
;;

let () =
  f (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;
