open Drumaderian

let modal = Html.getById "modal"


let () =
  create
    ~into:modal
    ~width:720
    ~height:416
    ()
    
let _ = Loop.forever ~delay:(0.0001) (fun () ->
    let r = Printf.sprintf "%d-%d" (Mouse.x()) (Mouse.y()) in
    log (js_string r)
  )
