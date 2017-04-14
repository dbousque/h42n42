

open Eliom_content.Html.D
open Eliom_content.Html
open Lwt

let wave_images =
	let _make_wave_image cls =
		img
			~alt:""
			~a:[a_class ["wave" ; cls]]
			~src:(make_uri (Eliom_service.static_dir ()) ["images" ; "wave.png"]) ()
	in
	let rec _make_wave_images acc current_height =
		let new_image = _make_wave_image "wave2" in
		if current_height + 22 > Config.extremes_height then acc
		else _make_wave_images (new_image::acc) (current_height + 21)
	in
	let first_image = _make_wave_image "wave1" in
	first_image::_make_wave_images [] 25

let river =
	div ~a:[
		a_class ["extreme" ; "river"] ;
		a_style ("height:" ^ string_of_int Config.extremes_height ^ "px;")
	] wave_images

let hospital_images =
	let _make_hospital_image () =
		img
			~alt:""
			~a:[a_class ["center-vertical" ; "hospital-image"]]
			~src:(make_uri (Eliom_service.static_dir ()) ["images" ; "hospital.png"]) ()
	in
	let nb_hospital_images =
		if Config.extremes_height > 305 then 1
		else if Config.extremes_height > 120 then 2
		else 3
	in
	let rec _make_hospital_images = function
		| 0 -> []
		| n -> (_make_hospital_image ())::(_make_hospital_images (n - 1))
	in
	_make_hospital_images nb_hospital_images

let hospital =
	div ~a:[
		a_class ["extreme" ; "hospital"] ;
		a_style ("height:" ^ string_of_int Config.extremes_height ^ "px;" ^
							"margin-top:" ^ string_of_int (Config.board_height - Config.extremes_height * 2) ^ "px;")
	] [
		div ~a:[a_class ["center-vertical" ; "center-horizontal"]]
			hospital_images
	]

let bestiole_container =
	div ~a:[
		a_class ["bestiole-container"] ;
		a_style ("width:" ^ string_of_int Config.board_width ^ "px;" ^
							"height:" ^ string_of_int Config.board_height ^ "px;")
	] [
		river ;
		hospital
	]

let you_lost =
	div ~a:[
		a_class ["you-lost-container"] ;
		a_style (
		"margin-left:" ^ string_of_int ((Config.board_width + 40 - 400) / 2) ^ "px;" ^
		"margin-top:" ^ string_of_int ((Config.board_height + 40 - 175) / 2) ^ "px;"
		)
	] [
		div ~a:[a_class ["you-lost-container-child"]] [
			p [pcdata "You lost"]
		]
	]

let board_container =
	div ~a:[
		a_class ["board-container"] ;
		a_style ("width:" ^ string_of_int (Config.board_width + 40) ^ "px;" ^
							"height:" ^ string_of_int (Config.board_height + 40) ^ "px;")
	] [
		you_lost ;
		div ~a:[
			a_class ["hide-border" ; "top-bottom-border"]
		] [] ;
		div ~a:[
			a_class ["hide-border" ; "left-right-border"] ;
			a_style ("height:" ^ string_of_int Config.board_height ^ "px;")
		] [] ;
		bestiole_container ;
		div ~a:[
			a_class ["hide-border" ; "left-right-border"] ;
			a_style ("height:" ^ string_of_int Config.board_height ^ "px;")
		] [] ;
		div ~a:[
			a_class ["hide-border" ; "top-bottom-border"] ;
			a_style ("margin-top:" ^ string_of_int Config.board_height ^ "px;")
		] []
	]

let first_input_parent =
	div ~a:[a_class ["input-parent"]] [
		pcdata "Bestiole size : ";
		span ~a:[a_class ["range-field rangeparent"] ; a_title "fst-parent"] [
			Form.input ~a:[a_id "bestiole-size"] ~input_type:`Range Form.string
		] ;
		pcdata "Bestiole speed : ";
		span ~a:[a_class ["range-field rangeparent"] ; a_title "fst-parent"] [
			Form.input ~a:[a_id "bestiole-speed"] ~input_type:`Range Form.string
		] ;
	]

let second_input_parent =
	div ~a:[a_class ["input-parent"]] [
		pcdata "Starting nb bestioles : ";
		span ~a:[a_class ["range-field rangeparent"] ; a_title "snd-parent"] [
			Form.input ~a:[a_id "starting-nb-bestioles"] ~input_type:`Range Form.string
		] ;
		pcdata "New bestiole every n second : ";
		span ~a:[a_class ["range-field rangeparent"] ; a_title "snd-parent"] [
			Form.input ~a:[a_id "new-bestiole-every"] ~input_type:`Range Form.string
		] ;
	]

let start_button =
	div ~a:[a_class ["btn start-game"]] [pcdata "Start game"]

let third_input_parent =
	div ~a:[a_class ["input-parent"]] [
		pcdata "Ill surviving time : ";
		span ~a:[a_class ["range-field rangeparent"] ; a_title "thd-parent"] [
			Form.input ~a:[a_id "living-time-after-infection"] ~input_type:`Range Form.string
		] ;
		start_button
	]

let form = 
	div ~a:[a_class ["all-input-parent"]] [
		first_input_parent ;
		second_input_parent ;
		third_input_parent
	]

let body_html =
	(body [
		h1 [pcdata "H42N42"] ;
		p ~a:[a_class ["centertext"]] [pcdata "dbousque"] ;
		p ~a:[a_class ["centertext" ; "description"]] [pcdata "Keep bestioles from touching the river, it is contaminated. Bring ill bestioles to the hospital."] ;
		board_container ;
		form
	])

let make () =
	html
		(head (title (pcdata "H42N42"))
			[css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["css" ; "h42n42.css"]) () ;
			css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["css" ; "materialize.min.css"]) () ;
			js_script ~uri:(make_uri (Eliom_service.static_dir ()) ["js" ; "jquery.min.js"]) () ;
			js_script ~uri:(make_uri (Eliom_service.static_dir ()) ["js" ; "materialize.min.js"]) () ;
			js_script ~uri:(make_uri (Eliom_service.static_dir ()) ["js" ; "setvals.js"]) ()
			])
		body_html