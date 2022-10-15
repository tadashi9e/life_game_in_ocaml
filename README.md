# Game of Life on OCaml

Game of Life based on OCaml Hashset.

## Files

This file:

- README.md

OCaml source files:

- load_file105.ml
- life.ml

Sample Life 1.05 pattern files:

- acorn.life
- glide_ne.life
- glide_nw.life

## Requirements

- graphics.cma
- hashset.cma

## Example

Execute utop command as following:

~~~sh
utop -I $(ocamlfind query graphics) graphics.cma -I $(ocamlfind query hashset) hashset.cma
~~~

And type following in utop:

~~~ocaml
#use "load_life105.ml";;
#use "life.ml";;

G.open_graph " 300x300" ;;

let s0 = create_position_set_of (load_life105_from (0, 0) "glider_ne.life") in
let s = ref s0 in
for step = 1 to 200 do
  let s2 = life !s in
  update_display 5 (0, 0) s2;
  s := s2;
  Unix.sleepf 0.01;
done
;;
~~~

## Example

Execute utop command as following:

~~~sh
utop -I $(ocamlfind query graphics) graphics.cma -I $(ocamlfind query hashset) hashset.cma
~~~

And type following in utop:

~~~ocaml
#use "load_life105.ml";;
#use "life.ml";;

G.open_graph " 300x300" ;;

let s0 = create_position_set_of (load_life105_from (200, 200) "acorn.life") in
let s = ref s0 in
for step = 1 to 1000 do
  let s2 = life !s in
  update_display 1 (0, 0) s2;
  s := s2;
  Unix.sleepf 0.0001;
done
;;
~~~