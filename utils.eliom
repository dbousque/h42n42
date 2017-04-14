

[%%client

open Eliom_content.Html.D
open Eliom_content.Html
open Lwt

let elt_to_dom_elt = To_dom.of_element

let random_rotation () =
	Random.int 360

let random_forward_time () =
	Unix.gettimeofday () +. 3.0 +. Random.float 2.0

]