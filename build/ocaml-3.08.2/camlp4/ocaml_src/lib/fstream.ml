(* camlp4r *)
(* This file has been generated by program: do not edit! *)
(* Copyright 2001 INRIA *)

type 'a t = { count : int; data : 'a data Lazy.t }
and 'a data =
    Nil
  | Cons of 'a * 'a t
  | App of 'a t * 'a t
;;

let from f =
  let rec loop i =
    {count = 0;
     data =
       lazy
         (match f i with
            Some x -> Cons (x, loop (i + 1))
          | None -> Nil)}
  in
  loop 0
;;

let rec next s =
  let count = s.count + 1 in
  match Lazy.force s.data with
    Nil -> None
  | Cons (a, s) -> Some (a, {count = count; data = s.data})
  | App (s1, s2) ->
      match next s1 with
        Some (a, s1) -> Some (a, {count = count; data = lazy (App (s1, s2))})
      | None ->
          match next s2 with
            Some (a, s2) -> Some (a, {count = count; data = s2.data})
          | None -> None
;;

let empty s =
  match next s with
    Some _ -> None
  | None -> Some ((), s)
;;

let nil = {count = 0; data = lazy Nil};;
let cons a s = Cons (a, s);;
let app s1 s2 = App (s1, s2);;
let flazy f = {count = 0; data = Lazy.lazy_from_fun f};;

let of_list l =
  List.fold_right (fun x s -> flazy (fun () -> cons x s)) l nil
;;

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)
;;

let of_channel ic =
  from
    (fun _ ->
       try Some (input_char ic) with
         End_of_file -> None)
;;

let iter f =
  let rec do_rec strm =
    match next strm with
      Some (a, strm) -> let _ = f a in do_rec strm
    | None -> ()
  in
  do_rec
;;

let count s = s.count;;

let count_unfrozen s =
  let rec loop cnt s =
    if Lazy.lazy_is_val s.data then
      match Lazy.force s.data with
        Cons (_, s) -> loop (cnt + 1) s
      | _ -> cnt
    else cnt
  in
  loop 0 s
;;
