
type position = {
  x : float;
  y : float
}

type state_aircraft =
  | E0
  | E1 of int*float
  | E2 of int*float*int
  | END

type node = {
  tcur    : int;
  pos     : position;
  heading : float;
  state   : state_aircraft;
  mutable pherom  : float;
}

type ant = {
  mutable node : node;
  mutable edge : (node*node) list
}

module TrailMap =
  Map.Make( struct type t = (int * state_aircraft)
              let compare = compare end )

exception Out of node
exception End
exception Finished
exception Conflict


