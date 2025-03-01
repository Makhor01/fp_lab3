open Interpolation

let parse_arguments args =
  let rec aux args current_step interpolation_types =
    match args with
    | [] -> {
        Runner.step = current_step;
        points_stream = Seq.empty;
        interpolation_types;
        last_interpolated_x = List.map (fun i -> (i.name, None)) interpolation_types;
      }
    | "-m" :: "linear" :: rest -> aux rest current_step (linear_interpolation :: interpolation_types)
    | "-m" :: "lagrange" :: rest -> aux rest current_step (lagrange_interpolation :: interpolation_types)
    | "-m" :: "both" :: rest -> aux rest current_step (linear_interpolation :: lagrange_interpolation :: interpolation_types)
    | "-s" :: step_value :: rest -> aux rest (float_of_string step_value) interpolation_types
    | _ :: rest -> aux rest current_step interpolation_types
  in
  aux args 1.0 []