open Drumaderian

let modal = Html.getById "modal"


let () =
  create
    ~into:modal
    ~width:720
    ~height:416
    ()
    

let _ = Loop.forever (fun () ->
    if Keyboard.repeat 65 then log "yo"
  )
