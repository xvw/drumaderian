open Drumaderian

let modal = Html.getById "modal"
let _ = Canvas.createIn modal 800 420
let _ = Canvas.webgl_initialize (Some Color.(gl black))

