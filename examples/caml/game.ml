open Drumaderian

let modal = Html.getById "modal"


let () =
  create
    ~into:modal
    ~width:720
    ~height:416
    ()
    

let _ = Loop.forever (fun () ->
    if Keyboard.(trigger space)  then
      log (js_string (Printf.sprintf "%d-%d" (Mouse.x ()) (Mouse.y ())))
  )
