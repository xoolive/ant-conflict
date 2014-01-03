open Types
open Constants

(* Basic functions *)

let dist_point2 {x = x1; y = y1} {x = x2; y = y2} =
  ((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2))

let dist_point a b = sqrt (dist_point2 a b)

let move {x = x; y = y} cap =
  let ix = (cos cap) *. speed *. (float timestep) /. 60.0
  and iy = (sin cap) *. speed *. (float timestep) /. 60.0 in
  {x = x +. ix; y = y +. iy}

(* Heading between two points *)
let compute_heading {x = x1 ; y = y1} {x = x2; y = y2} =
  atan2 (y2-.y1) (x2-.x1)

(* Evaluate the cost of a path *)
let evaluate path =
  List.fold_right ( fun (node,new_node) s ->
                      s +. (match  new_node.state with
                              E0         -> 0.
                            | E1 (_,_)   -> 2.
                            | E2 (_,_,_) -> 1.
                            | _          -> 0.) ) path 1.

(* In an array filled of nodes with cumulated probabilities, choose the node
 * with the closest probability to input parameter p *)


(* Initialise problem *)

let final_pos   = Array.create !nb_app {x = 0.; y = 0.}
let initial_pos = Array.create !nb_app {x = 0.; y = 0.}

(* All aircraft are placed around a circle, heading to the center *)
let initial_node = Array.init !nb_app (
  fun i ->
    (* 360 deg divided by the number of planes *)
    let angle        = (float i) *. (2. *. pi) /. (float !nb_app) in
    let pos          = {x = 0. -. radius *. cos angle;
                        y = 0. -. radius *. sin angle} in
    initial_pos.(i) <- pos;
    (* Symmetric *)
    final_pos.(i)   <- {x = radius *. cos angle;
                        y = radius *. sin angle};
    { tcur = 0; pos = pos; heading = angle; state = E0;
      (* Initialise pheromons *)
      pherom = (1. +. 6. *. (ftmax +. 1.)); }
)

(* We create a table of nb_ants for each of the nb_app aircraft *)
let ants = Array.init !nb_app
             (fun i -> Array.init nb_ants
                         (fun j -> {node = initial_node.(i); edge = []}))

(* Unroll all nodes departing from current node *)

let unroll {tcur = t; pos = p; heading = h; state = s; pherom = q} a tmax =
  (* unless timeout *)
  if t >= tmax then [] else (* last point *)
    if (dist_point p final_pos.(a)) < epsilon then
      [ {tcur = t+1; pos = p; heading = h; state = END; pherom = q} ]
    else (* regular case *)
      match s with
        END -> []
        | E0 ->
            let ft = float (tmax - t - 1) in
            { tcur = t + 1; pos = (move p h); heading = h; state = E0;
              (* either straight or turning according to 6 different angles *)
              pherom = (1. +. 6. *. (ft+.1.)) }::(
                List.map 
                  ( fun alpha -> {tcur    = t+1; pos    = (move p (h +. alpha));
                                  heading = h;   state  = E1 (t+1, alpha);
                                  pherom  = (ft +. 1.)}
                  ) [ppio6; ppio9; ppio18; mpio6; mpio9; mpio18]
              )
        | E1 (t0, alpha) ->
            let ft = float (tmax - t - 1) in
            let new_h = (compute_heading p final_pos.(a)) in
            [ (* keep flying away *)
              { tcur    = t+1; pos   = (move p (h +. alpha));
                heading = h;   state = E1 (t0, alpha); pherom = (ft -. 1.)};
              (* back on track *)
              { tcur    = t+1;   pos   = (move p new_h);
                heading = new_h; state = E2 (t0, alpha, t+1); pherom = 1.}
            ]
        | E2 (t0, alpha, t1)->
            [ (* all straight back *)
              { tcur    = t+1; pos   = (move p h);
                heading = h;   state = E2 (t0,alpha,t1); pherom = q}
            ]

(* Create the whole set of possible paths for each aircraft *)

let create_trail aircraft =
  let trail    = ref (TrailMap.empty)
  and node_deb = initial_node.(aircraft) in
  let rec next node trail =
    try (
      if node.state = END then raise End
      else
        let nodes_next = unroll node aircraft tmax in
        List.iter (fun x -> next x trail) nodes_next;
        trail := TrailMap.add (node.tcur, node.state) nodes_next !trail
    ) with End -> () in
  next node_deb trail;
  !trail

let trails = Array.init !nb_app (fun i -> create_trail i)

(* How ants move on the graph *)

let move_ants ants trails =

  for np=0 to !nb_app-1 do
    for na=0 to nb_ants-1 do
      try (
        for t = 0 to tmax - 1 do (* à chaque instant *)
          let node = ants.(np).(na).node in
          (* Choose next node *)
          let newnode =
            begin
              let next = try
                TrailMap.find (node.tcur, node.state) trails.(np)
              with _ -> []
              and liste = ref [] and sum = ref 0. in

              (* Read nodes and pheromones in order to normalise *)
              List.iter (fun {tcur=t; pos=pos; state=s; heading=h; pherom=q} ->
                           let p  = q ** alpha in
                           sum   := !sum +. p;
                           liste := (t,pos,s,h,p)::(!liste)) next;

              (* Fill a proper structure in order to pick up the next node *)
              let tab = Array.init (List.length !liste)
                          (fun _ -> {tcur = 0; pos = {x=0.; y=0.}; state = E0;
                                     heading = 0.; pherom = 0.})
              and k = ref 0 in

              List.iter (
                fun (t, pos, s, h, q) ->
                  let npn = q /. !sum in (* normalized pheromones *)
                  let cq =
                    if !k=0 then npn
                    else let z = tab.(!k-1).pherom in npn +. z in
                  tab.(!k) <-
                  {tcur=t; pos=pos; state=s; heading=h; pherom=cq};
                  incr k
              ) !liste;

              (* At last, pick next node *)
              let find_in_tab p tab = try
                Array.iter (fun node -> if p < node.pherom then
                              raise (Out node)) tab;
                failwith "find_in_tab"
              with
                Out node -> node in
              let r = Random.float 1.0 in
              find_in_tab r tab
            end in

          (* Now, move the ant *)
          ants.(np).(na).edge <- (node,newnode)::ants.(np).(na).edge;
          ants.(np).(na).node <- newnode;

          if newnode.state=END then raise Finished
        done;
        ants.(np).(na).edge <- []; (* Stop upon reaching tmax *)
      )
      with
        Finished -> ()
    done;
  done

(* Update all trails *)

let update_trails ants trails =
  (* Evaporation *)
  Array.iteri ( fun np a -> (
    trails.(np) <- TrailMap.map (
      fun l -> List.map
                 ( fun { tcur=t; pos=p; heading=h; state=s; pherom=q } ->
                     { tcur=t; pos=p; heading=h; state=s;
                       pherom = max (rho *. q) incmin } ) l) a)) trails;

  for na=0 to nb_ants-1 do
    (* Compute the number of survivors *)
    let nb = ref 0 in
    Array.iter (fun a -> if (a.(na).edge) <> [] then incr nb) ants;

    (* Update pheromone trails for each ant *)
    for np = !nb_app - 1 downto 0 do
      let edge = ants.(np).(na).edge in
      (* Pheromones to add to each edge of the path *)
      let dq = (float !nb) *. inc0 /. (evaluate edge) /. (float !nb_app) in
      List.iter
        ( fun (node, newnode) ->
            let next = TrailMap.find (node.tcur,node.state) trails.(np) in
            (* Update each edge *)
            trails.(np) <-
            TrailMap.add (node.tcur,node.state)
              (
                List.map
                  (fun {tcur = t; pos = p; heading=h; state = s; pherom = q}->
                     {
                       tcur = t; pos = p; heading = h; state = s;
                       pherom = q +.
                       (if (newnode.tcur,newnode.state) = (t,s) then dq else 0.)
                     }
                  ) next
              ) trails.(np) ) edge
    done
  done

(* Recreate the ant population *)

let reinit ants =
  Array.iteri (fun np a ->
                 Array.iter ( fun ant ->
                                ant.node <- initial_node.(np);
                                ant.edge <- []) a) ants

(* Conflict detection *)

let conflict (nodei, _) (nodej, _) =
  if (dist_point2 nodei.pos nodej.pos) < separation2 then raise Conflict

let detect_conflicts ants threshold =
  for na=0 to nb_ants-1 do
    let conflicts = Array.create !nb_app 0 in (* array for conflicts *)
    (* count all conflicts *)
    for i = 0 to !nb_app-2 do
      let edge_i = ants.(i).(na).edge in
      if edge_i <> [] then
        for j = i+1 to !nb_app-1 do
          let edge_j = ants.(j).(na).edge in
          try
            List.iter2 (fun n1 n2 -> conflict n1 n2)
              (List.rev edge_i) (List.rev edge_j)
          with
            Conflict ->
              conflicts.(i) <- conflicts.(i) + 1;
              conflicts.(j) <- conflicts.(j) + 1;
          | _ -> ()
        done
    done;

    (* kill ants creating too many conflicts *)
    Array.iteri (fun i a ->
                   if conflicts.(i) > threshold then (a.(na).edge <- [])) ants;
  done

(* Keep track of convergence *)

let stats ants =

  (* k_opt is the min cost among the max number of survivors *)
  let min_cost = ref (1.0/.0.0) and max_surv = ref 0 and k_opt = ref nb_ants in

  for na = 0 to nb_ants - 1 do
    let cost   = ref 0. in (* the delay *)
    let surv   = ref 0  in (* the number of survivors *)

    for np = 0 to !nb_app-1 do
      if (ants.(np).(na).edge) <> [] then (
        incr surv;
        cost:= !cost +. evaluate ants.(np).(na).edge;
      )
    done;

    if !surv > !max_surv then (
      max_surv := !surv;
      min_cost := (1.0/.0.0)
    );

    if ((!surv= !max_surv) && (!cost < !min_cost)) then
      (min_cost := !cost;
       k_opt := na)
  done;

  (!k_opt, !min_cost, !max_surv)

