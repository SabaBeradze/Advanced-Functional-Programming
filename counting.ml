
open Thread
open Event




let spawn_counter n c =
  let rec count i =
    let _ = sync (receive c) in
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
    if i < n then
      (sync (send c true);
      count (i+1))
    else
      (sync (send c false))
  in
  create count 0

let run_counters m n =
  let channels = List.init m (fun _ -> new_channel ()) in
  let counters = List.map (spawn_counter n) channels in
  let rec run = function [] -> ()
    | c::cs -> sync (send c true);
      let chans = if sync (receive c) then cs @ [c] else cs in
      run chans
  in
  run channels;
  List.iter join counters;
  print_newline ()

let _ = run_counters 10 1000

