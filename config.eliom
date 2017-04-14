

[%%shared

let board_width = 1100
let board_height = 800
let extremes_height = 71

let vals = Hashtbl.create ~random:true 10

let set_std_vals () =
	Hashtbl.add vals "bestiole-size" (50, 30, 100) ;
	Hashtbl.add vals "bestiole-speed" (100, 30, 300) ;
	Hashtbl.add vals "starting-nb-bestioles" (3, 1, 20) ;
	Hashtbl.add vals "new-bestiole-every" (10, 3, 30) ;
	Hashtbl.add vals "living-time-after-infection" (7, 1, 20)

let set_val name value =
	let current_value, mini, maxi = Hashtbl.find vals name in
	Hashtbl.replace vals name (value, mini, maxi)

let get_val name =
	let value, _, _ = Hashtbl.find vals name in
	value

]