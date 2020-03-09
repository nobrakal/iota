%{ (* -*- tuareg -*- *)
   open Program
   open Config

   let list_of_option x = Option.value ~default:[] x
%}

(*** TOKENS ***)

%token EOF

(* Surrounding tokens *)
%token LPAREN "(" RPAREN ")"

(* Logic *)
%token PLUS LOR LAND
%token PARENT HAS LINK TLINK EQQ
%token NOT
%token EQ
%token LCHEVRON "<" RCHEVRON ">"

%token ARROW SEMICOLON ";" COMMA ","
%token BIGARROW
%token DOT "."

%token LET
%token IN

%token ENSURE MAINTAIN

%token FORALL EXISTS

%token TYPE GUARD OF

(* Config tokens *)
%token MAXPROF STATIC DYNAMIC ABOUT

(* Typed tokens *)
%token<string> LowerId UpperId
%token<int> Int

(*** PRIORITY ***)
%left LOR
%left LAND

%right ARROW

%start<string Program.parsed_program> program
%start<Config.config> config
%%

(*** RULES ***)

program:
| vars=option(vars) safe=separated_list(SEMICOLON,safe) ensure=option(ensure) maintain=option(maintain) EOF
  { let vars = list_of_option vars in
    let ensure = list_of_option ensure in
    let maintain = list_of_option maintain in {vars;safe;ensure;maintain} }

vars:
| vars=nonempty_list(letdef) IN { vars }

ensure:
| ENSURE x=separated_list(SEMICOLON,general) { x }

maintain:
| MAINTAIN x=separated_list(SEMICOLON,general) { x }

letdef:
| LET x=LowerId xs=list(LowerId) EQ y=safe { Def (x,xs,y) }

safe:
| x=safe_formula { Formula x }
| FORALL x=LowerId y=guard ARROW z=safe { Quantif (Forall,x,y,z) }
| EXISTS x=LowerId y=guard LAND z=safe { Quantif (Exists,x,y,z) }
| x=safe_apply { x }

safe_apply:
| x=LowerId y=safe_atom { Apply (Var x,y) }
| x=safe_apply y=safe_atom { Apply (x,y) }

safe_atom:
| x=lit { Leaf x }
| "(" x=safe ")" { x }
| x=LowerId { Var x }

let safe_formula := mk_formula(safe_formula_lit)

safe_formula_lit:
| x=safe_atom { Lit x }

let formula := mk_formula(formula_atom)

formula_atom :
| x=lit { Lit x }
| "(" x=formula ")" { x }

lit:
| p=option(PLUS) x=dyn { (Option.is_some p,x) }

dyn:
| HAS "(" x=term ")" { Has x }
| g=guard { Bin g }
| p=UpperId "(" x=term ")" { Other (p,x) }

guard:
| EQQ "(" x=term "," y=term ")" { (B Eq,x,y) }
| LINK "(" x=term "," y=term ")" { (B Link,x,y) }
| TLINK "<" s1=LowerId "," s2=LowerId ">" "(" x=term "," y=term ")" { (TLink (s1,s2),x,y) }

term:
| x=LowerId { V x }
| PARENT "<" s1=LowerId "," s2=LowerId ">" "(" x=term ")" { Parent (s1,s2,x) }
| x=term "." y=LowerId { Func (y,x) }

general:
| xs=separated_list(ARROW,guard) BIGARROW x=formula { General (xs,x) }

let mk_formula(atom) :=
  | x=atom; { x }
  | NOT; "("; x=mk_formula(atom); ")"; { Not x }
  | x=mk_formula(atom); LAND; y=mk_formula(atom); { Binop (And,x,y) }
  | x=mk_formula(atom); LOR; y=mk_formula(atom); { Binop (Or,x,y) }
  | x=mk_formula(atom); ARROW; y=mk_formula(atom); { Binop (Or,Not x,y) }

(* Configuration *)
config:
| maxprof=econfig(MAXPROF,Int) types = list(type_elem) predicates=list(predicate) EOF
 {
   {maxprof;predicates;types}
 }

let econfig(keyword,value) :=
  | LET; keyword; EQ; x=value; { x }

let type_elem :=
  | TYPE; x=LowerId; {(x,[])}
  | TYPE; x=LowerId; EQ; xs=separated_nonempty_list(GUARD,accessor); {(x,xs)}

accessor: x=LowerId OF y=LowerId {(x,y)}

predicate:
| STATIC  x=UpperId ABOUT y=LowerId {(false,x,y)}
| DYNAMIC x=UpperId ABOUT y=LowerId {(true,x,y)}
