open Sum_types

type config =
  { maxprof : int
  ; functions : string list
  ; static : string list
  ; dynamic : string list
  ; types : ty_dec list }
