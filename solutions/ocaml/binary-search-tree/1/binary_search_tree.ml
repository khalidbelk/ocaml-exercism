open Base

type bst =
  | Empty
  | Node of {
    data: int;
    left: bst;
    right: bst;
  }

let empty : bst = Empty

let value t =
  match t with
    | Empty -> Error "Empty tree"
    | Node {data; left; right} -> Ok (data)

let left t =
  match t with
  | Empty -> Error "Empty tree"
  | Node {data; left; right} -> Ok(left)

let right t =
  match t with
  | Empty -> Error "Empty tree"
  | Node {data; left; right} -> Ok(right)

let rec insert nb tree =
  match tree with
  | Empty -> Node { data = nb; left = Empty; right = Empty}
  | Node { data; left; right } ->
    match nb with
      | nb when nb <= data -> Node {data; left = insert nb left; right}    (* insert in left subtree *) 
      | nb when nb > data -> Node {data; left; right = insert nb right }  (* insert in right subtree *) 
      (* | _ -> tree                            value is the same, so no need to duplicate*)

let rec to_list t =
  match t with
    | Empty -> []
    | Node {data; left; right} ->
      (to_list left) @ [data] @ (to_list right)