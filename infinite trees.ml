let rec layer_tree r = LNode (r, (fun () -> layer_tree (r+1)), (fun () -> layer_tree (r+1)))

let rec interval_tree (l, r) =
  LNode ((l, r), (fun () -> interval_tree (l, (l +. r) /. 2.)), (fun () -> interval_tree ((l +. r) /. 2., r)))

let rational_tree () =
  let rec impl (n,d) = LNode ((n,d), (fun () -> impl (n,d+1)), (fun () -> impl (n+1, d)))
  in impl (0, 0)

let rec top n t =
  if n <= 0 then Empty else
  let LNode (v, lf, rf) = t in
  Node (v, top (n-1) (lf ()), top (n-1) (rf ()))

let rec map f t =
  let LNode (v, lf, rf) = t in
  LNode (f v, (fun () -> map f (lf ())), (fun () -> map f (rf ())))

let find f t =
  let rec bfs queue =
    match queue with [] -> failwith "unreachable"
    | q::qs -> let LNode (v, lf, rf) = q in if f v then q else bfs (List.rev (rf () :: lf () :: List.rev qs))
  in
  bfs [t]