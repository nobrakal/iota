%{ (* -*- tuareg -*- *)
   open Program

   let rec sept_init_end xs =
     match xs with
     | [] -> assert false
     | [x] -> [],x
     | x::xs ->
        let ys,e = sept_init_end xs in
        x::ys,e
%}

(*** TOKENS ***)

%token EOF

(* Surrounding tokens *)
%token LPAREN "(" RPAREN ")"

(* Logic *)
%token PLUS LOR LAND
%token HAS LINK
%token NOT
%token EQ

%token ARROW SEMICOLON ";" COMMA ","

%token LET
%token IN

%token ENSURE MAINTAIN

%token FORALL EXISTS

(* Typed tokens *)
%token<string> LowerId UpperId

(*** PRIORITY ***)
%left LOR
%left LAND

%start<string Program.parsed_program> program

%%

(*** RULES ***)

program:
| vars=list(letdef) IN safe=separated_list(SEMICOLON,safe) ensure=option(ensure) maintain=option(maintain) EOF
  { let ensure = Option.value ~default:[] ensure in
    let maintain = Option.value ~default:[] maintain in {vars;safe;ensure;maintain} }

ensure:
| ENSURE x=separated_list(SEMICOLON,general) { x }

maintain:
| MAINTAIN x=separated_list(SEMICOLON,general) { x }

letdef:
| LET x=LowerId xs=list(LowerId) EQ y=safe { Def (x,xs,y) }

safe:
| x=safe_atom { x }
| x=safe_strong { x }
| x=safe_atom LAND y=safe { Pand (x,y) }
| x=safe_atom LOR y=safe { Por (x,y) }

(* A safe without a proper inclusion of formula *)
safe_wf:
| x=safe_atom_wf { x }
| x=safe_strong { x }
| x=safe_atom_wf LAND y=safe { Pand (x,y) }
| x=safe_atom_wf LOR y=safe { Por (x,y) }

safe_strong:
| FORALL x=LowerId y=formula_atom ARROW z=safe { Forall (x,y,z) }
| EXISTS x=LowerId y=formula_atom LAND z=safe { Exists (x,y,z) }
| x=safe_apply { x }

safe_apply:
| x=safe_atom y=safe_atom { Apply (x,y) }
| x=safe_apply y=safe_atom { Apply (x,y) }

safe_atom:
| x=formula_atom { Leaf x }
| "(" x=safe_wf ")" { x }
| x=LowerId { Var x }

safe_atom_wf:
| "(" x=safe_wf ")" { x }
| x=LowerId { Var x }

formula:
| x=formula_atom { x }
| NOT x=formula_atom { Unop (Not,x) }
| x=formula LAND y=formula { Binop (And,x,y) }
| x=formula LOR y=formula { Binop (Or,x,y) }

formula_atom :
| x=lit { Lit x }
| "(" x=formula ")" { x }

lit:
| p=option(PLUS) x=dyn { (Option.is_some p,x) }

dyn:
| HAS "(" x=LowerId ")" { Has x }
| LINK "(" x=LowerId "," y=LowerId ")" { Link (x,y) }
| p=UpperId "(" x=LowerId ")" { Other (p,x) }

general:
| xs=separated_nonempty_list(ARROW,formula) { let xs,x = sept_init_end xs in General (xs,x) }
