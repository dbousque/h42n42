

[%%shared

module type Leaf = sig
	type t
	val get_coords : t -> (float * float)
	val get_size : t -> (float * float)
end

module type Quadtree = sig
	type t
	type tree
	type leaftype
	type leafinfo
	val make : float -> float -> t
	val add : t -> leaftype -> t
	val collision_pred : t -> leaftype -> (leaftype -> leaftype -> bool) -> bool
end

module type MakeSig =
	functor (Leaftype : Leaf) ->
		Quadtree with type leaftype = Leaftype.t

module Make : MakeSig =
	functor (Leaftype : Leaf) ->
		struct
			type leaftype = Leaftype.t
			type leafinfo = {
				t: leaftype ;
				x: float ;
				y: float
			}
			type tree = Node of (tree * tree * tree * tree * leafinfo list) | Leaf of leafinfo list
			type t = (float * float * tree)

			let make width height =
				(width, height, Leaf [])

			let get_rect_nb width height (x, y) =
				let nb_x = if x <= width /. 2.0 then 0 else 1 in
				let nb_y = if y <= height /. 2.0 then 0 else 2 in
				nb_x + nb_y

			let get_all_rects width height (x1, y1) (x2, y2) =
				let rect1 = get_rect_nb width height (x1, y1) in
				let rect2 = get_rect_nb width height (x2, y2) in
				if rect1 = rect2 then [rect1]
				else (
					let maxi = max rect1 rect2 in
					let mini = min rect1 rect2 in
					let rects = [rect1 ; rect2] in
					let rects = if mini = 0 && maxi = 3 then 1 :: 2 :: rects else rects in
					let rects = if mini = 1 && maxi = 2 then 0 :: 3 :: rects else rects in
					rects
				)

			let get_new_leaf half_width half_height leaf rect_nb =
				let sub_x = if rect_nb mod 2 = 1 then half_width else 0.0 in
				let sub_y = if rect_nb >= 2 then half_height else 0.0 in
				{
					t = leaf.t ;
					x = leaf.x -. sub_x ;
					y = leaf.y -. sub_y
				}

			let get_leaf_coords leaf =
				let leaf_width, leaf_height = Leaftype.get_size leaf.t in
				let leaf_max_x, leaf_max_y = leaf.x +. leaf_width, leaf.y +. leaf_height in
				(leaf.x, leaf.y, leaf_max_x, leaf_max_y)

			let rec fill_tree width height leaf tl tr bl br lst =
				let half_width, half_height = width /. 2.0, height /. 2.0 in
				let leaf_x, leaf_y, leaf_max_x, leaf_max_y = get_leaf_coords leaf in
				let tl_rect_nb = get_rect_nb width height (leaf_x, leaf_y) in
				let br_rect_nb = get_rect_nb width height (leaf_max_x, leaf_max_y) in
				if tl_rect_nb <> br_rect_nb then
					(tl, tr, bl, br, leaf :: lst)
				else (
					let leaf = get_new_leaf half_width half_height leaf tl_rect_nb in
					match tl_rect_nb with
					| 0 -> (let tl = add2 half_width half_height tl leaf in
							(tl, tr, bl, br, lst))
					| 1 -> (let tr = add2 half_width half_height tr leaf in
							(tl, tr, bl, br, lst))
					| 2 -> (let bl = add2 half_width half_height bl leaf in
							(tl, tr, bl, br, lst))
					| _ -> (let br = add2 half_width half_height br leaf in
							(tl, tr, bl, br, lst))
				)

			and add2 width height node leaf =
				let rec explode_leaf_to_node tl tr bl br lst = function
					| [] -> Node (tl, tr, bl, br, lst)
					| hd :: rest -> (
							let tl, tr, bl, br, lst = fill_tree width height hd tl tr bl br lst in
							explode_leaf_to_node tl tr bl br lst rest
						)
				in
				match node with
				| Leaf lst -> (
						if List.length lst < 5 then
							Leaf (leaf :: lst)
						else
							explode_leaf_to_node (Leaf []) (Leaf []) (Leaf []) (Leaf []) [] (leaf :: lst)
					)
				| Node (tl, tr, bl, br, lst) -> Node (fill_tree width height leaf tl tr bl br lst)

			let add (width, height, node) leaf =
				let leaf_x, leaf_y = Leaftype.get_coords leaf in
				let leaf = {
					t = leaf ;
					x = leaf_x ;
					y = leaf_y
				} in
				(width, height, add2 width height node leaf)

			let rec has_collision_pred width height node leaf pred =
				let rec _investigate_subnodes tl tr bl br = function
					| [] -> false
					| hd :: rest -> (
							let half_width, half_height = width /. 2.0, height /. 2.0 in
							let leaf = get_new_leaf half_width half_height leaf hd in
							let node = match hd with | 0 -> tl | 1 -> tr | 2 -> bl | _ -> br in
							if has_collision_pred half_width half_height node leaf pred then
								true
							else
								_investigate_subnodes tl tr bl br rest
						)
				in
				match node with
				| Leaf lst -> List.exists pred lst
				| Node (tl, tr, bl, br, lst) -> (
						if List.exists pred lst then true
						else (
							let x, y, max_x, max_y = get_leaf_coords leaf in
							let rects = get_all_rects width height (x, y) (max_x, max_y) in
							_investigate_subnodes tl tr bl br rects
						)
					)

			let collision_pred tree leaf pred =
				let _real_collide l1 l2 =
					let l1_x, l1_y = Leaftype.get_coords l1 in
					let l1_width, l1_height = Leaftype.get_size l1 in
					let l1_max_x, l1_max_y = l1_x +. l1_width, l1_y +. l1_height in
					let l2_x, l2_y = Leaftype.get_coords l2 in
					let l2_width, l2_height = Leaftype.get_size l2 in
					let l2_max_x, l2_max_y = l2_x +. l2_width, l2_y +. l2_height in
					if (l1_x <= l2_max_x && l2_x <= l1_max_x
						&& l1_y <= l2_max_y && l2_y <= l1_max_y) then true
					else false
				in
				let leaf_x, leaf_y = Leaftype.get_coords leaf in
				let leaf = {
					t = leaf ;
					x = leaf_x ;
					y = leaf_y
				} in
				let width, height, node = tree in
				let pred = (fun x -> _real_collide x.t leaf.t && pred x.t leaf.t) in
				has_collision_pred width height node leaf pred
		end

]