open Sum_types

type config =
  { maxprof : int
  ; static : string list
  ; dynamic : string list
  ; types : ty_dec list }
