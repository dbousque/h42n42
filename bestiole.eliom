

[%%client

open Eliom_content.Html.D
open Eliom_content.Html
open Lwt
open BestioleType

let is_ill bestiole =
	match bestiole.state with
	| StdIll false -> false
	| StdIll true -> true
	| Beserk -> true
	| Naughty -> true

let cure_bestiole bestiole =
	if bestiole.state = Beserk then (
		bestiole.size <- float_of_int (Config.get_val "bestiole-size") ;
		let size_str = (Config.get_val "bestiole-size") |> string_of_int |> Js.string in
			bestiole.dom_elt##setAttribute (Js.string "width") size_str
	) ;
	bestiole.state <- StdIll false ;
	bestiole.dom_elt##setAttribute (Js.string "src") (Js.string "./images/bestiole_sane.png") ;
	bestiole.got_infected_at <- None ;
	bestiole.full_size_at <- None

let make_bestiole_ill bestiole =
	let n = Random.int 10 in
	bestiole.got_infected_at <- Some (Unix.gettimeofday ()) ;
	let img_src = ( match n with
		| 0 -> bestiole.state <- Beserk ; bestiole.full_size_at <- Some (Unix.gettimeofday () +. 7.0) ; "./images/bestiole_beserk.png"
		| 1 -> bestiole.state <- Naughty ; "./images/bestiole_naughty.png"
		| _ -> bestiole.state <- StdIll true ; "./images/bestiole_ill.png"
	) in
	bestiole.dom_elt##setAttribute (Js.string "src") (Js.string img_src)

let kill_bestiole bestiole =
	bestiole.dead <- true ;
	let container = Utils.elt_to_dom_elt ~%(Page.bestiole_container) in
	Dom.removeChild container bestiole.dom_elt ;
	Lwt.return ()

let update_speed bestiole =
	let multiplier = match bestiole.state with
		| StdIll true -> 0.85
		| StdIll false -> 1.0
		| Beserk -> 0.85
		| Naughty -> 1.15
	in
	let time_speedup = (Unix.gettimeofday () -. bestiole.start_time) /. 120.0 in
	let time_speedup = 1.0 +. time_speedup in
	bestiole.speed <- float_of_int (Config.get_val "bestiole-speed") *. time_speedup *. multiplier 

let update_size bestiole =
	if bestiole.state <> Beserk then ()
	else (
		match bestiole.full_size_at with
		| Some x -> (
			let current_time = Unix.gettimeofday () in
				let multiplier = ( if current_time >= x then
						4.0
					else
						1.0 +. ((current_time +. 7.0 -. x) /. 7.0 *. 2.0)
				) in
				let multiplier = if multiplier > 4.0 then 4.0 else multiplier in
				bestiole.size <- float_of_int (Config.get_val "bestiole-size") *. multiplier ;
				let size_str = bestiole.size |> string_of_float |> Js.string in
				bestiole.dom_elt##setAttribute (Js.string "width") size_str
			)
		| _ -> ()
 	)

let next_coords bestiole =
	let deg_to_rad deg =
		0.01745329252 *. float_of_int deg
	in
	match bestiole.updated_at with
	| None -> (bestiole.x, bestiole.y)
	| Some updated_at -> (
		let nb_pixels_decal = bestiole.speed *. (Unix.gettimeofday () -. updated_at) in
		let x = nb_pixels_decal *. cos (deg_to_rad bestiole.rotation) in
		let y = nb_pixels_decal *. sin (deg_to_rad bestiole.rotation) in
		(bestiole.x +. x, bestiole.y +. y)
	)

let change_rotation_if_ok bestiole =
	match bestiole.state with
	| Naughty -> ()
	| _ -> (
		if Unix.gettimeofday () >= bestiole.change_rotation_at then (
			BestioleUtils.update_rotation bestiole (Utils.random_rotation ()) ;
			bestiole.change_rotation_at <- Utils.random_forward_time ()
		)
	)

let make_ill_if_ok bestiole =
	if not (is_ill bestiole) && bestiole.y <= float_of_int Config.extremes_height then
		make_bestiole_ill bestiole

let rec bestiole_thread bestiole =
	let living_time_after_infection = Config.get_val "living-time-after-infection" in
	let living_time_after_infection = float_of_int living_time_after_infection in
	match bestiole.got_infected_at with
	| Some time when Unix.gettimeofday () -. time >= living_time_after_infection ->
			kill_bestiole bestiole
	| _ -> (
		Lwt_js.sleep 0.01 >>= fun () ->
			match bestiole.currently_dragged with
			| true -> ( bestiole.updated_at <- Some (Unix.gettimeofday ()) ;
						bestiole_thread bestiole )
			| false -> (
				change_rotation_if_ok bestiole ;
				update_speed bestiole ;
				update_size bestiole ;
				let x, y = next_coords bestiole in
				BestioleUtils.move_bestiole_bounce bestiole x y ;
				make_ill_if_ok bestiole ;
				bestiole_thread bestiole
		)
	)

let handle_bestiole_dragend bestiole =
	let bottom_y = bestiole.y +. BestioleUtils.get_bestiole_size bestiole in
	let hospital_start = Config.board_height - Config.extremes_height in
	let hospital_start = float_of_int hospital_start in
	if bottom_y >= hospital_start then cure_bestiole bestiole ;
	BestioleUtils.update_rotation bestiole (Utils.random_rotation ()) ;
	bestiole.change_rotation_at <- Utils.random_forward_time ()

let make_bestiole ?fadein:(fadein=false) start_time dragging_handler =
	let image = img
		~alt:""
		~a:[
			a_width (Config.get_val "bestiole-size") ;
			a_class ["bestiole"] ;
			a_style (if fadein then "animation: fadein 2s;" else "")
		]
		~src:(make_uri (Eliom_service.static_dir ()) ["images" ; "bestiole_sane.png"]) ()
	in
	let bestiole = {
		elt = image ;
		dom_elt = Utils.elt_to_dom_elt image ;
		x = 0.0 ;
		y = 0.0 ;
		size = float_of_int (Config.get_val "bestiole-size") ;
		rotation = 0 ;
		start_time = start_time ;
		speed = float_of_int (Config.get_val "bestiole-speed") ;
		state = StdIll false ;
		change_rotation_at = Utils.random_forward_time () ;
		updated_at = None ;
		dead = false ;
		got_infected_at = None ;
		full_size_at = None ;
		currently_dragged = false
	} in
	let rand_x = Random.float (float_of_int Config.board_width) in
	let rand_y = Random.float (float_of_int Config.board_height) in
	let rand_x, rand_y = BestioleUtils.get_bestiole_absolute_coords rand_x rand_y in
	BestioleUtils.update_rotation bestiole (Utils.random_rotation ()) ;
	BestioleUtils.move_bestiole bestiole rand_x rand_y ;
	Dragging.make_draggable dragging_handler bestiole ;
	bestiole

let make_bestioles ?fadein:(fadein=false) start_time dragging_handler n =
	let rec _make_bestioles acc = function
		| 0 -> acc
		| n -> _make_bestioles (make_bestiole ~fadein:fadein start_time dragging_handler::acc) (n - 1)
	in
	_make_bestioles [] n

let make_bestioles_and_attach start_time dragging_handler parent fadein n =
	let bestioles = make_bestioles ~fadein:fadein start_time dragging_handler n in
	List.iter (fun b -> Dom.appendChild parent b.dom_elt) bestioles ;
	bestioles

]