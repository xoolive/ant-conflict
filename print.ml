open Types
open Constants

(* All printing functions *)

let print_state e=
  match e with
      E0         -> Printf.printf "E0 "
    | E1 (_,_)   -> Printf.printf "E1 "
    | E2 (_,_,_) -> Printf.printf "E2 "
    | END        -> Printf.printf "END "

let print_trails trail file =
  try
    TrailMap.iter (
      fun (tcur,state) next ->
        List.iter (
          fun {pos = pos; pherom = q; tcur = t; state = s} ->
            let l = TrailMap.find (t,s) trail in
            if List.exists (fun a -> (a.Types.state = Types.END)) l
            then
              Printf.fprintf file "%f %f %f\n" pos.x pos.y q) next; flush file
    ) trail
  with
    x -> ()

let print_ant fp edge =
  let cost = ref 0.0 in
  List.iter ( fun (i,j,m,n) -> Printf.fprintf fp "(%d,%d)/(%d,%d) " i j m n;
                               cost := !cost +. 1.0) (List.rev edge);
  Printf.fprintf fp "%f\n" !cost;;

let print_convergence threshold max_surv overall =
  let t = int_of_float (Unix.time () -. !f_cpu_time) in
  Printf.fprintf conv "%d\t%d\t%d\t%f\n" t threshold max_surv overall ;
  flush conv

(* Send to gnuplot *)
let plot_ant ants gnuplot na nb_iter cost =

  let file    = open_out ("log/trajectories_"^(string_of_int nb_iter)) in
  let seconds = int_of_float (Unix.time () -. !f_cpu_time) in
  Printf.fprintf file "# set title \" %d iterations in %d seconds (%d)\" \n"
    nb_iter seconds (int_of_float cost);

  for np=0 to !nb_app-1 do
    List.iter (fun (node,_) ->
                 Printf.fprintf file "%f %f\n" node.pos.x node.pos.y)
      ants.(np).(na).edge;
    Printf.fprintf file "\n";
  done;
  close_out file;
  Printf.fprintf gnuplot
    ("plot [-4000:4000][-4000:4000] 'log/trajectories_%d' u 1:2 w l\n") nb_iter;
  flush gnuplot



