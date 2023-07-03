open Log_calls.Stdout

type 'a c = 'a * char [@@deriving show]
type t = char [@@deriving show]

module T = struct type i = int [@@deriving show] end

let%log f ?s ~(i : T.i) (c : t c) (t2 : int * int) (_ : 'a) =
  let%log h () = () in
  h ();
  ignore (i, c, s, ignore t2)

and g ?(x : (int * int) option) (y : bool option option * int) = ignore (x, y)

let () =
  f ~s:"Y" ~i:2 ('b', 'c') (1, 2) 3;
  g ~x:(1, 2) (Some (Some true), 3)
