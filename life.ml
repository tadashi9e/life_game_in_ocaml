module Position =
  struct
    type t = int * int
    let equal (x0,y0) (x1, y1) =
      match Stdlib.compare x0 x1 with
        0 -> if Stdlib.compare y0 y1 = 0 then
          true else false
      | c -> false
    let hash (x, y) = x * 1023 + y
  end

module PositionSet = Hashset.Make(Position)

(*
   Append some monadic syntaxes to List.
   Thanks @htsign san.

   see https://qiita.com/tadashi9e/items/07d94efb879fb6323244#comment-2e3f4d486ffee5f2f93b
 *)
module List = struct
  include Stdlib.List
  
  let (let*) xs f = List.map f xs |> List.flatten
  let return x = [x]
end

(*
   Function create_position_set_of:
 Get PositionSet as given position list.

 Arguments: position_list, (int * int) list.
 Returns: PositionSet.t
 *)
let create_position_set_of position_list =
  List.fold_left
    (fun s p -> PositionSet.add s p; s)
    (PositionSet.create (List.length position_list))
    position_list

(*
   Function print_position_set:
 Print a PositionSet contents.

 Arguments: PositionSet.t
 Returns: nothing.
 *)
let print_position_set set =
  PositionSet.iter (fun (x, y) ->
    print_string "(" ; print_int x ; print_string "," ;
    print_int y ; print_string ") ")
    set

(*
   Function territory_list_of:
 Returns territory position list for given position.

 Arguments: (x, y), int * int.
 Returns: (int * int) list.
 *)
let territory_list_of (x, y) =
  let range a b = List.init (b - a) (fun i -> i + a) in
  List.(
    let* i = range (-1) 2 in
    let* j = range (-1) 2 in
    return (x + i, y + j)
  )

(*
   Function territory_set_of:
 Returns territory set of given position set.

 Arguments: set, PositionSet.t
 Returns: PositionSet.t
 *)
let territory_set_of set =
  (* Add territory of position to set *)
  let add_territory_of position set2 =
    List.fold_left
      (fun s p -> PositionSet.add s p; s)
      set2
      (territory_list_of position)
  in
  (* Get all territory of position set *)
  PositionSet.fold
    add_territory_of
    set
    (PositionSet.create (PositionSet.cardinal set))
(*
   Function is_next_alive:
 Return true if next cell status is "Alive", returns false if not.

 Arguments: position, int * int.
            set, PositionSet.t
 Returns: bool.
 *)
let is_next_alive position set =
  (* Get neighbours list of (x, y) *)
  let neighbours_list_of (x, y) =
    List.filter
      (fun (x0, y0) -> x0 != x || y0 != y)
      (territory_list_of (x, y))
  in
  (* Count neighbour alive cells *)
  let count_neighbours position set =
    List.fold_left
      (fun c p -> if PositionSet.mem set p then (c + 1) else c)
      0
      (neighbours_list_of position)
  in
  (* Determine next step alive/death *)
  match count_neighbours position set with
    3 -> true
  | 2 -> PositionSet.mem set position
  | _ -> false
(*
   Function life:
 Returns next step "Alive" cell set from current "Alive" cell set.

 Arguments: alives, PositionSet.t
 Returns: PositionSet.t
 *)
let life alives =
  let territory_set = territory_set_of alives in
  let territory_list =
    PositionSet.fold (fun p s -> p :: s) territory_set []
  in List.fold_left
    (fun s p -> if is_next_alive p alives then (PositionSet.add s p; s) else s)
    (PositionSet.create (PositionSet.cardinal territory_set))
    territory_list

module G = Graphics

(*
   Function update_display:
 Draw alive cells into graphics.
 "pixel_size" is cell size in graphics, and
 (x0, y0) is lower left corner origin.

 Arguments: pixel_size, int.
            (x0, y0), int * int.
            set, PositionSet.t
 Returns: nothing.
 *)
let update_display pixel_size (x0, y0) set =
  G.set_color (G.rgb 0 0 0);
  G.fill_rect 0 0 (G.size_x ()) (G.size_y ());
  G.set_color (G.rgb 255 255 255);
  PositionSet.fold
    (fun (x, y) () -> G.fill_rect
        (x0 + x * pixel_size) (y0 + y * pixel_size) pixel_size pixel_size)
    set ();
  ()
