
(* PI and derivates *)

let pi         = 2. *. acos(0.)
let ppio6      = pi /. 6.
let ppio9      = pi /. 9.
let ppio18     = pi /. 18.
let mpio6      = (0. -. ppio6)
let mpio9      = (0. -. ppio9)
let mpio18     = (0. -. ppio18)

(* Problem constants *)

(* Lateral separation *)
let separation  = (6. *. 60.)
let separation2 = separation *. separation

(* Timestep *)
let timestep   = 60

(* Aircraft speed *)
let speed      = 480.0

(* Size of the circle *)
let radius     = 3500.

(* Distance an aircraft can fly between two time steps *)
let epsilon    = ( (float timestep) *. speed /. 60.)

(* Time between each step *)
let stepmin    = 2. *. radius /. speed *. 60. /. (float timestep)
let tmax       = truncate (1.5*.stepmin)
let ftmax      = float tmax

(* Number of aircraft *)
let nb_app     = ref 30

(* Number of ants *)
let nb_ants    = 20 * !nb_app

(* Number of iterations *)
let nb_max     = 10000

(* Ant colony optimisation parameters *)
let alpha      = 1.

(* Evaporation rate *)
let rho        = 0.95

(* Minimal amount of pheremones *)
let incmin     = 0.1

(* Amount of pheromones *)
let inc0       = ftmax

(* Origin of time *)
let f_cpu_time = ref (Unix.time ())

(* Plot with gnuplot *)
let gnuplot    = Unix.open_process_out "gnuplot"

(* Create files for logging *)
let _    = try ( Unix.mkdir "log" 0o775 ) with _ -> ()
let conv = open_out ("log/convergence_"^(Unix.gethostname ()))
let _    = Printf.fprintf conv "# t\tthreshold\tsurvivors\tcost\n"; flush conv
