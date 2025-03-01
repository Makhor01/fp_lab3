open Interpolation
open Utils
open Printf

type runner = {
  step : float;
  points_stream : (float * float) Seq.t;
  interpolation_types : interpolation list;
  last_interpolated_x : (string * float option) list;
}

let print_points points =
  let x_values = List.map fst points in
  let y_values = List.map snd points in
  List.iter (printf "%.2f\t") x_values;
  print_newline ();
  List.iter (printf "%.2f\t") y_values;
  print_newline ()

let read_point () =
  print_endline "Введите точку (X Y через пробел):";
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [x; y] -> (float_of_string x, float_of_string y)
  | _ -> failwith "Некорректный ввод!"


let generate_x_values x_0 x step =
  let rec aux current =
    if current >= x then Seq.Nil
    else
      let next = current +. step in
      if next > x then Seq.Cons (current, fun () -> Seq.Nil)
      else Seq.Cons (current, fun () -> aux next)
  in
  fun () -> aux x_0

let perform_interpolation points interpolation_types step (last_interpolated : (string * float option) list) : (string * float option) list =
  let sorted_types = List.sort (fun a b -> compare a.window_size b.window_size) interpolation_types in

  List.map (fun interpolation ->
    let relevant_points =
      if interpolation.window_size = 2 then
        List.rev points |> take 2 |> List.rev
      else
        take interpolation.window_size points
    in
    if List.length relevant_points >= interpolation.window_size then
      let end_point = List.hd (List.rev relevant_points) in
      let x_1 = List.assoc_opt interpolation.name last_interpolated in
      let x_0 =
        match x_1 with
        | None -> fst (List.hd relevant_points)
        | Some (Some x) -> x +. step
        | Some None -> fst (List.hd relevant_points)
      in
      if x_0 < fst end_point then
        let result =
          generate_x_values x_0 (fst end_point) step
          |> Seq.map (fun x -> (x, interpolation.interpolate relevant_points x))
          |> List.of_seq
        in
        print_endline interpolation.name;
        print_points result;
        (interpolation.name, Some (fst (List.hd (List.rev result))))
      else
        (interpolation.name, match x_1 with Some x -> x | None -> None)
    else
      (interpolation.name, None)
  ) sorted_types

let rec upd_run runner =
  let points = List.of_seq runner.points_stream in

  let last_interpolated =
    if List.length points >= 2 then
      perform_interpolation points runner.interpolation_types runner.step runner.last_interpolated_x
    else
      runner.last_interpolated_x
  in

  let new_point = read_point () in
  let updated_points = Seq.append runner.points_stream (Seq.return new_point) in

  let required_points = List.fold_left (fun acc interpolation -> max acc interpolation.window_size) 0 runner.interpolation_types in
  let updated_points =
    if Seq.length updated_points > required_points then Seq.drop 1 updated_points else updated_points
  in

  upd_run { runner with
    points_stream = updated_points;
    last_interpolated_x = last_interpolated
  }

let init_run runner =
  let min_points = 2 in
  let rec read_initial_points n acc =
    if n = 0 then acc
    else
      let new_point = read_point () in
      let updated_acc = Seq.append acc (Seq.return new_point) in
      read_initial_points (n - 1) updated_acc
  in
  let initial_points = read_initial_points min_points Seq.empty in
  upd_run { runner with
    points_stream = initial_points;
    last_interpolated_x = List.map (fun i -> (i.name, None)) runner.interpolation_types
  }