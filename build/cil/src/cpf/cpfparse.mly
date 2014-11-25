%{
    open Cpfannotation

%}

%token <string> FSYM VSYM VAR UNINTERP NUM
%token COMMA LPAREN RPAREN ARROW DEREF DOT END 

%start cpf_annotation
%type <Cpfannotation.cpf_annotation> cpf_annotation

%right DEREF
%left ARROW DOT
%left ELIST

%%

cpf_annotation:
  | cpf_annotation_aux END         { $1 }

cpf_annotation_aux:
  | cs_cpf_exp_list                { CpfCSAnn $1 }
  | cpf_exp_list                   { CpfELAnn $1 }

cs_cpf_exp_list:
  | cpf_exp COMMA cs_cpf_exp_list_aux { $1 :: $3 }

cs_cpf_exp_list_aux:
  | cpf_exp                           { [ $1 ] }
  | cpf_exp COMMA cs_cpf_exp_list_aux { $1 :: $3 }

cpf_exp_list:
  | cpf_exp                           { [ $1 ] }
  | cpf_exp cpf_exp_list %prec ELIST  { $1 :: $2 }

cpf_exp:
  | DEREF cpf_exp                     { CpfDerefExp $2 }
  | LPAREN cpf_annotation_aux RPAREN  { CpfParenExp $2 }
  | cpf_exp DOT cpf_exp               { CpfDotOffset ($1,$3) }
  | cpf_exp ARROW cpf_exp             { CpfArrowOffset ($1,$3) }
  | VAR                               { CpfVarExp $1 }
  | FSYM                              { CpfFSymExp $1 }
  | VSYM                              { CpfVSymExp $1 }
  | UNINTERP                          { CpfUninterp $1 }
  | NUM                               { CpfNum $1 }

