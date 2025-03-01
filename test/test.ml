open Alcotest
open INTERP.Interpolation
open INTERP.Runner

let test_linear () =
  let points = [ (0., 0.); (2., 4.) ] in
  let x = 1. in
  let y = linear_interpolate points x in
  check (float 0.01) "linear interpolation" 2. y

let test_lagrange () =
  let points = [ (0., 0.); (1., 1.); (2., 4.) ] in
  let x = 1.5 in
  let y = lagrange_interpolate points x in
  check (float 0.01) "lagrange interpolation" 2.25 y

let test_generate() =
  let x_min = 0. in
  let x_max = 2.5 in
  let step = 1. in
  let result = generate_x_values x_min x_max step |> List.of_seq in
  check (list (float 0.01)) "generate x values" [ 0.; 1.; 2. ] result

let () =
  let interpolation_tests = [
    test_case "Linear interpolation" `Quick test_linear;
    test_case "Lagrange interpolation" `Quick test_lagrange;
    test_case "Generate x values" `Quick test_generate;
  ] in
  run "Interpolation Tests" [
    "Interpolation", interpolation_tests;
  ]
