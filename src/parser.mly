%token EOL EOF
%token IMPORT
%token TRUE FALSE
%token EQUALS
%token COMMA
%token LSB RSB LCB RCB
%token <int> INT
%token <string> IDENT
%token <string> STRING

%start main
%type <Types.expr list> main

%%

main:
  | expr_list EOF {List.rev $1}
;

expr_list:
  | expr_list expr {$2 :: $1}
  |                {[]}
;

expr:
  | IMPORT STRING           {`Import $2}
  | IDENT EQUALS value      {`Bind  ($1, $3)}
  | IDENT LCB expr_list RCB {`Group ($1, $3)}
;

value_list:
  | value_list COMMA value {$3 :: $1}
  | value                  {[$1]}
  |                        {[]}
;

value:
  | TRUE   {`Bool true}
  | FALSE  {`Bool false}
  | INT    {`Int  $1}
  | STRING {`String $1}
  | LSB value_list RSB {`List (List.rev $2)}
;

%%
