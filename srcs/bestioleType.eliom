

[%%client

open Eliom_content.Html.D
open Eliom_content.Html
open Lwt

type bestiole_state =
	| StdIll of bool
	| Beserk
	| Naughty

type bestiole = {
	elt:						Html_types.img D.elt ;
	dom_elt:					Dom_html.element Js.t ;
	mutable x:					float ;
	mutable y:					float ;
	mutable size:				float ;
	mutable rotation:			int ;
	start_time:					float ;
	mutable speed:				float ;
	mutable state:				bestiole_state ;
	mutable change_rotation_at:	float ;
	mutable updated_at:			float option ;
	mutable dead:				bool ;
	mutable got_infected_at:	float option ;
	mutable full_size_at:		float option ;
	mutable currently_dragged:  bool
}

]