%{
	int bad_input = 1;
%}
/*** Definitions ***/ 

VALUE 			[0-9]|[1-9][0-9]*
ID				[a-zA-Z][a-zA-Z0-9]*
STRING 			["](.*?)["]
COMMENT			";;".*
OP_PLUS			"+"
OP_MINUS		"-"
OP_DIV			"/"
OP_MULT			"*"
OP_OP 			"\("
OP_CP			"\)"
OP_DBLMULT		"**"
OP_OC 			"“"
OP_CC 			"”"
OP_COMMA		","
KW_AND 			"and"
KW_OR 			"or"
KW_NOT  		"not"
KW_EQUAL  		"equal"
KW_LESS 		"less"
KW_NIL 			"nil"
KW_LIST  		"list"
KW_APPEND  		"append"
KW_CONCAT 		"concat"
KW_SET 			"set"
KW_DEFFUN  		"deffun"
KW_FOR  		"for"
KW_IF  			"if"
KW_EXIT  		"exit"
KW_LOAD 		"load"
KW_DISP 		"disp"
KW_TRUE 		"true"
KW_FALSE	 	"false"
NEWLINE			"n"

/*** Rule Section ***/
%%

(\r\n|\r|\n) {return 0;}
{KW_AND}	{printf("KW_AND\n");}
{KW_OR}		{printf("KW_OR\n");}
{KW_NOT}	{printf("KW_NOT\n");}
{KW_EQUAL}	{printf("KW_EQUAL\n");}
{KW_LESS}	{printf("KW_LESS\n");}
{KW_NIL}	{printf("KW_NIL\n");}
{KW_LIST}	{printf("KW_LIST\n");}
{KW_APPEND}	{printf("KW_APPEND\n");}
{KW_CONCAT}	{printf("KW_CONCAT\n");}
{KW_SET}	{printf("KW_SET\n");}
{KW_DEFFUN}	{printf("KW_DEFFUN\n");}
{KW_FOR}	{printf("KW_FOR\n");}
{KW_IF}		{printf("KW_IF\n");}
{KW_EXIT}	{printf("KW_EXIT\n");}
{KW_LOAD}	{printf("KW_LOAD\n");}
{KW_DISP}	{printf("KW_DISP\n");}
{KW_TRUE}	{printf("KW_TRUE\n");}
{KW_FALSE}	{printf("KW_FALSE\n");}

{OP_PLUS}		{printf("OP_PLUS\n");}
{OP_MINUS}		{printf("OP_MINUS\n");}
{OP_DIV}		{printf("OP_DIV\n");}
{OP_MULT}		{printf("OP_MULT\n");}
{OP_CP}			{printf("OP_CP\n");}
{OP_OP}			{printf("OP_OP\n");}
{OP_DBLMULT}	{printf("OP_DBLMULT\n");}
{OP_OC} 		{printf("OP_OC\n");}
{OP_CC} 		{printf("OP_CC\n");}
{OP_COMMA}		{printf("OP_COMMA\n");}
{STRING}		{printf("STRING\n");}
[[:space:]]+ 		/* ignore space */
{COMMENT}		{printf("COMMENT\n");}


[[:alpha:]][[:alnum:]]*   {printf("IDENTIFIER\n");}
[[1-9][:digit:]]+         {printf("VALUE\n");}
{VALUE}					  {printf("VALUE\n");}

	
[a-zA-Z]*[,+-/*]	 	  	  {printf("SYNTAX ERROR\n"); return 0;} 
[,][A-zA-Z0-9+-/*,]*	 	  {printf("SYNTAX ERROR\n"); return 0;}
[+][A-zA-Z0-9,-/*+]*	 	  {printf("SYNTAX ERROR\n"); return 0;}	
[-][A-zA-Z0-9+,/*-]*	 	  {printf("SYNTAX ERROR\n"); return 0;}	
[/][A-zA-Z0-9+-,*/]*	 	  {printf("SYNTAX ERROR\n"); return 0;}
[*][A-zA-Z0-9+-,/*]*	 	  {printf("SYNTAX ERROR\n"); return 0;}
[[:digit:]]+[[:alpha:]]  	  {printf("SYNTAX ERROR\n"); return 0;}
[[:digit:]]+[,+-/*]   	 	  {printf("SYNTAX ERROR\n"); return 0;}
[0][0-9]*				 	  {printf("SYNTAX ERROR\n"); return 0;}                       	
.		 				 	  {printf("SYNTAX ERROR\n"); return 0;}	
%%

int yywrap(){}
int main(int argc, char **argv){
	++argv, --argc;

	if(argc > 0){ 
		yyin = fopen(argv[0], "r");
		yylex();
	}
	else{	
		yyin = stdin;

		printf("Interpreter shell>>> \n");
		while(1) {
			yylex();
		} 
	}

	
	
	return 0;
}
