open Maps

exception NoLocation of string

type path = Polyline.t

let visible_zoom_level zoom ?(markers=[]) ?(windows=[]) map =
  let hide_show _ =
    let map_zoom = Map.get_zoom map in
    List.iter (fun x -> Marker.set_visible x (map_zoom >= zoom)) markers;
    List.iter
    (fun x ->
       if map_zoom >= zoom
       then InfoWindow.open' x ()
       else InfoWindow.close x)
    windows
  in
  Event.add_listener (Map.t_to_js map) "zoom_changed" hide_show

let latlng_of_coords = function
  | x,y -> LatLng.new_lat_lng x y

let coords_of_latlng ll =
  (LatLng.lat ll), (LatLng.lng ll)

(** Value containing "my position" **)
let my_position =
  let opts = MarkerOptions.create
      ~draggable:false
      ~clickable:true
      ~title:"Ma position"
      ~visible:false
      ~z_index:17.
      ()
  in
  Marker.new_marker ~opts ()

let set_my_position_icon url =
  Marker.set_icon my_position (Icon.create ~url ())

(** Function taking a string representing the id of the div
    containing the map **)
let create_map (lat,lng) zoom elt =
  let center = LatLng.new_lat_lng lat lng in
  let opts = MapOptions.create ~center ~zoom () in
  let elt = Converter.Element.t_of_dom elt in
  Map.new_map elt ~opts ()

let get_my_position () =
  let at,au = Lwt.wait () in
  if (Geolocation.is_supported ()) then
    let geo = Geolocation.geolocation in
    let options = Geolocation.empty_position_options() in
    let () = options##.enableHighAccuracy := true in
    let f_success pos =
      let coords = pos##.coords in
      let latitude = coords##.latitude in
      let longitude = coords##.longitude in
      Lwt.wakeup au (latitude,longitude)
    in
    let f_error err =
      let code = err##.code in
      if code = err##._TIMEOUT then
        Lwt.wakeup_exn au (raise (NoLocation("Timeout")))
      else
        Lwt.wakeup_exn au (raise (NoLocation("Unknwown")))
    in
    let () = geo##getCurrentPosition
        (Js.wrap_callback f_success)
        (Js.wrap_callback f_error)
        options in
    at
  else
    raise (NoLocation("Geolocation not supported"))


(* My position *)
let showing = ref false
let lock = Lwt_mutex.create ()

(** Function taking 1 parameter : the map **)
let show_my_position ?(timeout=3.) map =
  let rec aux () =
    let%lwt (lat,lng) = get_my_position () in
    let latlng = LatLng.new_lat_lng lat lng in
    let () = Marker.set_position my_position latlng in
    let%lwt () = Lwt_mutex.lock lock in
    let%lwt () = Lwt_js.sleep timeout in
    if !showing
    then
      let () = Lwt_mutex.unlock lock in
      aux ()
    else
      Lwt.return (Lwt_mutex.unlock lock)
  in
  let%lwt () = Lwt_mutex.lock lock in
  if !showing
  then Lwt.return (Lwt_mutex.unlock lock)
  else let () = showing := true in
       let () = Marker.set_map my_position (Some(map)) in
       let () = Marker.set_visible my_position true in
       let () = Lwt_mutex.unlock lock in
       aux ()

let hide_my_position () =
  let%lwt () = Lwt_mutex.lock lock in
  let () = showing := false in
  let () = Marker.set_visible my_position false in
  let () = Lwt_mutex.unlock lock in
  Lwt.return ()

(* Marker *)
let create_simple_marker
    ?(clickable=true)
    ?(draggable=false)
    ?(title="")
    ?(visible=true)
    latlng
    map =
  let opts = MarkerOptions.create
      ~clickable ~draggable ~title ~visible ~position:latlng ~map () in
  Marker.new_marker ~opts ()

let create_icon_marker
    ?(clickable=true)
    ?(draggable=false)
    ?(title="")
    ?(visible=true)
    url
    latlng
    map =
  let marker =
    create_simple_marker ~clickable ~draggable ~title
                         ~visible:false latlng map in
  let icon = Icon.create ~url () in
  Marker.set_icon marker icon;
  Marker.set_visible marker true;
  marker

let hide_marker marker =
  Marker.set_visible marker false

(* Path *)
let create_path ?(color="#000000") ?(weight=5.) ?(visible=true) map  =
  let opts = PolylineOptions.create
      ~stroke_color:color
      ~stroke_weight:weight
      ~visible
      ~map
      ~draggable:false
      ~editable:false () in
  Polyline.new_polyline ~opts ()

let add_latlng path latlng =
  let arr = Polyline.get_path path in
  MVCArray.push arr (LatLng.t_to_js latlng)

let add_latlng_list path latlngs =
  List.fold_left
    (fun size latlng -> add_latlng path latlng)
    0
    latlngs

let add_coords path (lat,lng) =
  let latlng = LatLng.new_lat_lng lat lng in
  add_latlng path latlng

let add_coords_list path list =
  let latlngs = List.map latlng_of_coords list in
  add_latlng_list path latlngs

let path_of_coords ?(color="#000000") ?(weight=15.) ?(visible=true) list map  =
  let path = create_path ~color ~weight ~visible map in
  let _ = add_coords_list path list in
  path

let path_of_latlngs ?(color="#000000") ?(weight=15.) ?(visible=true) list map  =
  let path = create_path ~color ~weight ~visible map in
  let _ = add_latlng_list path list in
  path

let latlngs_of_path path =
  let mvc = Polyline.get_path path in
  let liste = MVCArray.get_array mvc in
  let latlngs = List.map (LatLng.t_of_js) liste in
  latlngs


let coords_of_path path =
  let latlngs = latlngs_of_path path in
  List.map coords_of_latlng latlngs



let is_tracking = ref false
let track_lock = Lwt_mutex.create ()

let start_tracking path ?(timeout=3.) ?(min_distance=0.) () =
  let%lwt () = Lwt_mutex.lock track_lock in
  if !is_tracking
  then Lwt.return (Lwt_mutex.unlock track_lock)
  else
    let rec callback () =
      let%lwt coords = get_my_position () in
      let coords_l = latlng_of_coords coords in
      let arr = Polyline.get_path path in
      let size = MVCArray.get_length arr in
      let dist =
        if size > 0
        then let last = MVCArray.get_at arr (size-1) in
          let last = LatLng.t_of_js last in
          Spherical.compute_distance_between
            coords_l last ()
        else 0. in
      let () =
        if dist >= min_distance
        then ignore (add_coords path coords)
        else () in
      let%lwt () = Lwt_js.sleep timeout in
      if !is_tracking
      then callback ()
      else Lwt.return ()
    in
    let () = is_tracking := true in
    let () = Lwt_mutex.unlock track_lock in
    callback ()

let stop_tracking path =
  let%lwt () = Lwt_mutex.lock track_lock in
  let () = is_tracking := false in
  Lwt.return (Lwt_mutex.unlock track_lock)

(* Geocoding *)
let geocoder () = Geocoder.new_geocoder ()

let address_of_latlng latlng =
  let at, au = Lwt.wait () in
  let callback' result_l (status:geocoder_status) =
    match status with
    | Ok ->
      let addr_list =
        List.map GeocoderResult.formatted_address result_l in
      Lwt.wakeup au (addr_list)
    | _ -> Lwt.wakeup au []
  in
  let geo = geocoder () in
  let request = GeocoderRequest.create ~location:latlng () in
  let () = Geocoder.geocode geo request callback' in
  at

let address_of_coords (lat,lng) =
  let latlng = LatLng.new_lat_lng lat lng in
  address_of_latlng latlng

let latlng_of_address addr =
  let at, au = Lwt.wait () in
  let callback' result_l (status:geocoder_status) =
    match status with
    | Ok ->
      if List.length result_l = 0
      then Lwt.wakeup au None
      else
        let result = List.hd result_l in
        let geometry = GeocoderResult.geometry result in
        Lwt.wakeup au (Some(GeocoderGeometry.location geometry))
    | _ -> Lwt.wakeup au None
  in
  let geo = geocoder () in
  let request = GeocoderRequest.create ~address:addr () in
  let _ = Geocoder.geocode geo request callback' in
  at

let add_marker_spot ?text ?animation ?icon latlng map =
  let opts = MarkerOptions.create
               ~draggable:false
               ?animation
               ~position:latlng
               ~map
               ?title:text
               () in
  let () =
    match icon with
    | None -> ()
    | Some s ->
      let icn = Icon.create ~url:s () in
      MarkerOptions.set_icon opts icn in
  let () =
    match text with
    | None -> ()
    | Some s ->
      let label = MarkerLabel.create ~text:s () in
      MarkerOptions.set_label opts label in
  Marker.new_marker ~opts ()

(* Spots *)
let add_marker_spots ?text_l ?animation ?icon_l latlng_l map =
  let a_of_opt =
    function
    | None -> []
    | Some l -> l in
  let texts = a_of_opt text_l in
  let icons = a_of_opt icon_l in
  let make ?text ?icon latlng =
    add_marker_spot ?text ?animation latlng map in
  let error () = raise (Invalid_argument("Lists have different size")) in
  let rec map3 buf = function
    | [], [], [] -> List.rev buf
    | _, _, [] -> error ()
    | [], [], ll::tail -> map3 ((make ll)::buf) ([], [], tail)
    | text::l1, [], ll::l3 -> map3 ((make ~text ll)::buf) (l1, [], l3)
    | [], icon::l1, ll::l3 -> map3 ((make ~icon ll)::buf) ([], l1, l3)
    | text::l1, icon::l2, ll::l3 ->
      map3 ((make ~text ~icon ll)::buf) (l1, l2, l3)
  in
  map3 [] (texts, icons, latlng_l)

let add_window_spot content latlng map =
  let opts = InfoWindowOptions.create
      ~position:latlng
      ~content
      () in
  let infowin = InfoWindow.new_info_window ~opts () in
  let () = InfoWindow.open' infowin ~map () in
  infowin

let set_content win c =
  InfoWindow.set_content win c

let add_window_spots content_l pos_l map =
  List.map2
    (fun content pos ->
       add_window_spot content pos map)
    content_l pos_l

let add_user_window url name latlng map =
  let content =
    "<img src=\""^url^"\" alt=\""^name^"\" />
      <p>"^name^"</p>" in
  add_window_spot content latlng map

let add_users_window url_l name_l latlng_l map =
  let make url name latlng =
    add_user_window url name latlng map in
  let error () = raise (Invalid_argument("Lists have different size")) in
  let rec map3 buf = function
    | [], [], [] -> List.rev buf
    | _, _, [] -> error ()
    | [], [], ll::tail -> map3 ((make "" "" ll)::buf) ([], [], tail)
    | url::l1, [], ll::l3  -> map3 ((make url "" ll)::buf) (l1, [], l3)
    | [], name::l1, ll::l3 -> map3 ((make "" name ll)::buf) ([], l1, l3)
    | url::l1, name::l2, ll::l3 ->
      map3 ((make url name ll)::buf) (l1, l2, l3)
  in
  map3 [] (url_l, name_l, latlng_l)

let add_users_from_coords url_l name_l coords_l map =
  let liste = List.map latlng_of_coords coords_l in
  add_users_window url_l name_l liste map

let close_window infowindow =
  InfoWindow.close infowindow
