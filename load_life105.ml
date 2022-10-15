(*
   Function input_line_crlf:
 CRLF acceptable input_line.

 Arguments: f, in_channel.
 Returns: string
 *)
let input_line_crlf chan =
  let rec foreach_char s =
    let c = input_char chan in
    match Char.code(c) with
      0x0a -> s
    | 0x0d -> foreach_char s
    | _ -> foreach_char (s ^ (String.make 1 c))
  in
  foreach_char ""
  
(*
   Function load105_from:
 Load LIFE1.05 format file.

 Arguments: (x, y), int * int.
            file_name, string.
 Returns: ('a * 'b) list
 *)
let load_life105_from (x, y) file_name =
  (* Parse line string *)
  let parse_105_line (x, y) line points =
    let rec line_points_of_offset dx lst =
      if dx < String.length line then
        match String.get line dx with
          '#' -> lst
        | '*' -> (x + dx, y) :: line_points_of_offset (dx + 1) lst
        | _ -> line_points_of_offset (dx + 1) lst
      else
        lst
    in
    line_points_of_offset 0 points
  in
  (* Parse line string list *)
  let parse_105_lines (x, y) lines =
    let rec points_of_offset dy lst = function
        [] -> lst
      | line::rest ->
        points_of_offset (dy + 1) (parse_105_line (x, y + dy) line lst) rest
    in
    points_of_offset 0 [] lines
  in
  (* Read file into line string list *)
  let lines = ref [] in
  let chan = open_in file_name in (
    try
      while true do
        let line = input_line_crlf chan in
        print_endline line;
        lines := line :: !lines
      done;
    with End_of_file ->
      close_in chan);
  (* Returns "Alive" cells *)
  parse_105_lines (x, y) !lines
