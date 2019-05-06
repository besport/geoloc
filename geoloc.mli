open Googlemaps
open Js_of_ocaml

exception NoLocation of string

type path = Polyline.t

(** Fix a zoom level where new markers and window aren't visible **)
val visible_zoom_level :
  int ->
  ?markers:Marker.t list ->
  ?windows:InfoWindow.t list ->
  Map.t ->
  MapsEventListener.t

(** Convert coords to latitude longitude point **)
val latlng_of_coords : float * float -> LatLng.t

(** Convert latitude longitude point to coords **)
val coords_of_latlng : LatLng.t -> float * float

val make_my_position_marker : ?title:string -> unit -> Marker.t

(** Change the icon for the "my position" marker **)
val set_my_position_icon : my_position:Marker.t -> string -> unit

(** Creates a map from a center (coords), a zoom and
    the HTML element which will contain the map (js_of_ocaml element) **)
val create_map :
  ?mapoptions:Googlemaps.MapOptions.t -> float * float -> int
  -> Dom_html.element Js.t -> Map.t

(** Function getting current "my position" coordinates **)
val get_my_position : ?timeout:float -> unit -> (float*float) Lwt.t

(** Show "my position" marker on the given map
    And updates it every interval seconds.
    This is the equivalent of HTML5's watchPosition **)
val show_my_position :
  ?interval:float -> my_position:Marker.t -> Map.t -> unit Lwt.t

(** Hide "my position" marker on the current map
    Stops tracking my_position **)
val hide_my_position :
  ?show_my_position_th:unit Lwt.t -> my_position:Marker.t -> unit

(** Takes a boolean to check whether the marker has to be clickable
    or not, draggable or visible. Function also takes a title for
    the marker, its position and the map to draw it on **)
val create_simple_marker :
  ?clickable:bool ->
  ?draggable:bool ->
  ?title:string ->
  ?visible:bool ->
  LatLng.t ->
  Map.t ->
  Marker.t

(** Takes a boolean to check whether the marker has to be clickable
    or not, draggable or visible. Function also takes a title for
    the marker, its position and the map to draw it on
    The string is the url of the icon for th enew marker **)
val create_icon_marker :
  ?clickable:bool ->
  ?draggable:bool ->
  ?title:string ->
  ?visible:bool ->
  ?scaled_size:Size.t ->
  ?size:Size.t ->
  string ->
  LatLng.t ->
  Map.t ->
  Marker.t

(** Hide the given marker **)
val hide_marker : Marker.t -> unit

(** Show the given marker **)
val show_marker : Marker.t -> unit

(* Path *)
(** Creates a new empty path **)
val create_path :
  ?color:string -> ?weight:float -> ?visible:bool -> Map.t -> path

(** Start tracking my position. Every timeout seconds, the current
    position is added to the path given if more than min_distance
    meters has been done with the alst known position **)
val start_tracking :
  path ->
  ?interval:float ->
  ?min_distance:float ->
  unit ->
  unit Lwt.t

(** Stop the tracking of the current path **)
val stop_tracking : tracking_th:unit Lwt.t -> unit

(** Add a checkpoint (LatLng point) to the given path **)
val add_latlng : path -> LatLng.t -> int

(** Add checkpoints (LatLng point list) to the given path **)
val add_latlng_list : path -> LatLng.t list -> int

(** Add a checkpoint (coordinates) to the given path **)
val add_coords : path -> float * float -> int

(** Add checkpoints (coordinates list) to the given path **)
val add_coords_list : path -> (float * float) list -> int

(** Create a fresh new path from a LatLng point list **)
val path_of_latlngs :
  ?color:string ->
  ?weight:float ->
  ?visible:bool ->
  LatLng.t list  ->
  Map.t ->
  path

(** Get the latlng points from a path **)
val latlngs_of_path : path -> LatLng.t list

(** Create a fresh new path from coordinates point list **)
val path_of_coords :
  ?color:string ->
  ?weight:float ->
  ?visible:bool ->
  (float*float) list  ->
  Map.t ->
  path

(** Get the coords points from a path **)
val coords_of_path : path -> (float*float) list

(* Geocoding *)
(** Get a human readable address from a LatLng point **)
val address_of_latlng : LatLng.t -> (string list) Lwt.t

(** Get a human readable address from coordinates **)
val address_of_coords : (float * float) -> (string list) Lwt.t

(** Get LatLng point from a human readable address **)
val latlng_of_address : string -> (LatLng.t option) Lwt.t

(* Spot *)
(** Creates and add a new marker to the given map
    text is the text to add to the spot
    animation can be BOUNCE or DROP (see Google Map API)
    icon is the icon of the marker
 **)
val add_marker_spot :
  ?text:string ->
  ?animation:animation ->
  ?icon:string ->
  LatLng.t ->
  Map.t ->
  Marker.t

(** Creates and add many spots given by the LatLng point list
    It's possible to give a text and an icon for each of them
 **)
val add_marker_spots :
  ?text_l:(string list) ->
  ?animation:animation ->
  ?icon_l:(string list) ->
  LatLng.t list ->
  Map.t ->
  Marker.t list

(** Creates and add a spot represented by a window which content
    is the give string (can be HTML code or simple string) **)
val add_window_spot : string -> LatLng.t -> Map.t -> InfoWindow.t

(** Changes the content of the given window **)
val set_content : InfoWindow.t -> string -> unit

(** Creates and add many window spots defined by the string list and
    the LatLng list **)
val add_window_spots: string list -> LatLng.t list -> Map.t -> InfoWindow.t list

(** Add a green marker for the user **)
val add_marker_user :
  ?icon:string ->
  picture_url:string ->
  name:string ->
  LatLng.t ->
  Map.t ->
  Marker.t

(** Add a window with the given picture (url) and name **)
val add_user_window : string -> string -> LatLng.t -> Map.t -> InfoWindow.t

(** Add many users window at the same time **)
val add_users_window :
  string list ->
  string list ->
  LatLng.t list ->
  Map.t ->
  InfoWindow.t list

(** Same than previous functions but the position are given with
    coords (float*float) list **)
val add_users_from_coords :
  string list ->
  string list ->
  (float*float) list ->
  Map.t ->
  InfoWindow.t list

(** Close a window, impossible to reopen it after this call **)
val close_window : InfoWindow.t -> unit
