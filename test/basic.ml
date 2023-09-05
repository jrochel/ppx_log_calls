open Log_calls.Stdout

type 'a c = 'a * char [@@deriving show]
type t = char [@@deriving show]

module T = struct type i = int [@@deriving show] end

let%log f ?s ~(i : T.i) (c : t c) (t2 : int * int) (_ : 'a) : int * int =
  ignore (i, c, s);
  t2

let%log g ?(x : (int * int) option) (y : bool option option * int) =
  ignore (x, y)

let () =
  ignore @@ f ~s:"Y" ~i:2 ('b', 'c') (1, 2) 3;
  g ~x:(1, 2) (Some (Some true), 3)
