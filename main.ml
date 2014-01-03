open Ant
open Print
open Constants

(* Random seed *)
let _ = Random.self_init ()

(* Ant colony optimisation algorithm *)
let aco  =
  let surv = ref 0 in
  for nb = 0 to nb_max do

    move_ants ants trails;

    (* Constraint relaxation *)
    let threshold =
      if !surv < !nb_app/4 then 4 else
      if !surv < !nb_app/3 then 3 else
      if !surv < !nb_app/2 then 2 else
      if !surv < !nb_app then 1 else
      0 in
    detect_conflicts ants threshold;

    (* Threshold should only be decreasing *)
    let (k_opt, min_cost, max_surv) = stats ants in
    surv := max !surv max_surv;

    (* Keep track of the convergence *)
    print_convergence threshold max_surv min_cost;

    (* Send to gnuplot *)
    plot_ant ants gnuplot k_opt nb min_cost;

    (* Update trails *)
    update_trails ants trails;

    (* Reinit ants *)
    reinit ants;

  done
