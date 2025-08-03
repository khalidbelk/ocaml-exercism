open Base

module Int_map = Map.M(Int)
type school = string list Int_map.t

let empty_school = Map.empty (module Int)

let add (student: string) (grade: int) (school: school) : school =
    Map.update school grade ~f:(function
      | None -> [student]
      | Some students -> student :: students
    )

let grade (grade: int) (school: school) : string list =
  match Map.find school grade with
  | None -> []
  | Some students -> students

let sorted school : school =
  school |> Map.map ~f:(fun students -> List.sort ~compare:String.compare students)

let roster school : string list =
  school |> sorted |> Map.data |> List.concat (* sort and extract all the Map's values as a list + concat them *)