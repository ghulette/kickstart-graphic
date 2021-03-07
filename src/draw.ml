let draw_polyline _ _ = ()
let flood_fill _ _ = ()

let () =
  Graphics.open_graph "";
  (* Graphics.set_window_title "Amiga Kickstart";
  Graphics.resize_window 320 200; *)
  let done_ = ref false in
  while !done_ do
    Graphics.clear_graph ();
    Graphics.synchronize ();
    let status = Graphics.(wait_next_event [Key_pressed]) in
    if status.Graphics.keypressed then begin
      done_ := true;
      print_endline "exiting"
    end
  done
