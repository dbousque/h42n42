

[%%client

open BestioleType

let update_rotation bestiole new_rot =
	bestiole.rotation <- new_rot mod 360 ;
	let rotation = "rotate(" ^ (string_of_int (bestiole.rotation + 90)) ^ "deg)" in
	bestiole.dom_elt##.style##.transform := Js.string rotation

let get_elt_coords elt =
	let coords = elt##getBoundingClientRect in
	(coords##.left, coords##.top)

let get_bestiole_size bestiole =
	bestiole.size

let get_bestiole_relative_coords x y =
	let cont_x, cont_y = get_elt_coords (Utils.elt_to_dom_elt ~%(Page.bestiole_container)) in
	(x -. cont_x, y -. cont_y)

let get_bestiole_absolute_coords x y =
	let cont_x, cont_y = get_elt_coords (Utils.elt_to_dom_elt ~%(Page.bestiole_container)) in
	(cont_x +. x, cont_y +. y)

let actual_move_bestiole bestiole x_float y_float =
	let body = Utils.elt_to_dom_elt ~%(Page.body_html) in
	let x = Js.string (string_of_float x_float ^ "px") in
	let y = Js.string (string_of_float (y_float +. float_of_int body##.scrollTop) ^ "px") in
	bestiole.dom_elt##.style##.left := x ;
	bestiole.dom_elt##.style##.top := y ;
	let x, y = get_bestiole_relative_coords x_float y_float in
	bestiole.x <- x ;
	bestiole.y <- y ;
	bestiole.updated_at <- Some (Unix.gettimeofday ())

let move_bestiole bestiole new_x new_y =
	let _limit_by_borders value low high =
		if value < low then low
		else if value > high then high
		else value
	in
	let x, y = get_bestiole_relative_coords new_x new_y in
	let offset = get_bestiole_size bestiole in
	let x = _limit_by_borders x 0.0 (float_of_int Config.board_width -. offset) in
	let y = _limit_by_borders y 0.0 (float_of_int Config.board_height -. offset) in
	let x, y = get_bestiole_absolute_coords x y in
	actual_move_bestiole bestiole x y

let move_bestiole_bounce bestiole x y =
	let b_size = get_bestiole_size bestiole in
	( if x <= 0.0 then
		update_rotation bestiole (180 + (360 - bestiole.rotation))
	) ;
	( if x +. b_size >= float_of_int Config.board_width then
		update_rotation bestiole (180 - bestiole.rotation)
	) ;
	( if y <= 0.0 then
		update_rotation bestiole (360 - bestiole.rotation)
	) ;
	( if y +. b_size >= float_of_int Config.board_height then
		update_rotation bestiole (360 - bestiole.rotation)
	) ;
	let x, y = get_bestiole_absolute_coords x y in
	move_bestiole bestiole x y

]