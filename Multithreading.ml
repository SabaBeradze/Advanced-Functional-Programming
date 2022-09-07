#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;


module Future = struct
  type 'a msg = Result of 'a | Ex of exn
  type 'a t = 'a msg channel



  let create f a =
    let c = new_channel () in
    let task () =
      let r = try Result (f a) with e -> Ex e in
      sync (send c r)
    in
    let _ = Thread.create task () in
    c



  let get c =
    match sync (receive c) with
    | Result r -> r
    | Ex e -> raise e




  let then_ f c =
    let c' = new_channel () in
    let task () =
      let r = match sync (receive c) with
      | Result r -> Result (f r)
      | Ex e -> Ex e
      in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'



  let when_any cs =
    let c' = new_channel () in
    let task () =
      let r = select (List.map receive cs) in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'










    
 