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
| vars=list(letdef) safe=separated_list(SEMICOLON,safe) ensure=option(ensure) maintain=option(maintain) EOF
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
| FORALL x=LowerId y=formula_atom z=safe { Forall (x,y,z) }
| EXISTS x=LowerId y=formula_atom z=safe { Exists (x,y,z) }
| x=safe_atom LAND y=safe_atom { Pand (x,y) }
| x=safe_atom LOR y=safe_atom { Por (x,y) }

safe_atom:
| x=formula_atom { Leaf x }
| x=LowerId { Var x }
      (* | "(" x=safe ")" { x } *)

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
