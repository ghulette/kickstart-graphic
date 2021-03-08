let set_color =
  let rgb r g b = Graphics.rgb (r * 16) (g * 16) (b * 16) in
  function
  | '\x00' -> Graphics.set_color (rgb 0xF 0xF 0xF)
  | '\x01' -> Graphics.set_color (rgb 0x0 0x0 0x0)
  | '\x02' -> Graphics.set_color (rgb 0x7 0x7 0xC)
  | '\x03' -> Graphics.set_color (rgb 0xB 0xB 0xB)
  | _ -> failwith "unexpected color index"

let jump x y = Graphics.moveto (Char.code x) (Char.code y)
let line_to x y = Graphics.lineto (Char.code x) (Char.code y)
let fill x y =
  Printf.printf "unimplemented fill at %d, %d\n" (Char.code x) (Char.code y)

type mode = Jump | Line | Fill

let rec interp ch mode =
  let b1 = input_char ch in
  let b2 = input_char ch in
  match mode, b1, b2 with
  | _, '\xFF', '\xFF' -> ()
  | _, '\xFF', col -> set_color col; interp ch Jump
  | _, '\xFE', col -> set_color col; interp ch Fill
  | Jump, x, y -> jump x y; interp ch Line
  | Line, x, y -> line_to x y; interp ch Line
  | Fill, x, y -> fill x y; interp ch Fill

let interp_channel ch =
  try interp ch Jump
  with End_of_file -> failwith "unexpected EOF"

let () =
  let open Graphics in
  open_graph "";
  set_window_title "Kickstart";
  resize_window 320 200;
  interp_channel stdin;
  loop_at_exit [Key_pressed] (fun st -> if st.keypressed then raise Exit)
