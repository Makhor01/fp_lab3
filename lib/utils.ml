let take n lst =
  let rec aux n lst acc =
    match lst, n with
    | _, 0 -> List.rev acc
    | [], _ -> List.rev acc
    | x :: xs, _ -> aux (n - 1) xs (x :: acc)
  in
  aux n lst []