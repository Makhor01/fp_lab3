open INTERP.Runner
open INTERP.Args

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let app = parse_arguments args in
  init_run app