(** Common tokens *)
%token <string> Ident
%token <string> Var
%token <string> CapIdent
%token <string> String
%token <string> Comment
%token <int> Int

%token DOT
%token LBRACE RBRACE
%token LPAR RPAR
%token STAR
%token COLON
%token SEMICOLON
%token COMMA
%token RIGHTARROW

%token CHAIN OPERATION FOREACH
%token IN OUT
%token SET

%token EOF


%start <int> foo
%%

foo: { 1 }
