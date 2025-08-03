type robot = string ref

let used_names = Hashtbl.create 1000

let generate_name () : string =
  let chars = String.init 2 (fun _ -> Char.chr (65 + Random.int 26)) in
  let digits = string_of_int (100 + Random.int 900) in
  chars ^ digits

let rec new_name () : string =
  let name = generate_name () in
  if Hashtbl.mem used_names name then
    new_name ()
  else
    (Hashtbl.add used_names name (); name)

let new_robot () =
  ref (new_name ())

let name (r: robot) : string = 
  !r      (* dereference to get string value of r *)

let reset (r: robot) : unit =
  let robot_name = !r in
  Hashtbl.remove used_names robot_name;
  r := new_name ()     (* reassign to r a new generated name*)