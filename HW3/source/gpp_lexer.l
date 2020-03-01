%{
	#include "y.tab.h"
	extern YYSTYPE yylval;
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
{KW_AND}	{return KW_AND;}
{KW_OR}		{return KW_OR;}
{KW_NOT}	{return KW_NOT;}
{KW_EQUAL}	{return KW_EQUAL;}
{KW_LESS}	{return KW_LESS;}
{KW_NIL}	{return KW_NIL;}
{KW_LIST}	{return KW_LIST;}
{KW_APPEND}	{return KW_APPEND;}
{KW_CONCAT}	{return KW_CONCAT;}
{KW_SET}	{return KW_SET;}
{KW_DEFFUN}	{return KW_DEFFUN;}
{KW_FOR}	{return KW_FOR;}
{KW_IF}		{return KW_IF;}
{KW_EXIT}	{return KW_EXIT;}
{KW_LOAD}	{return KW_LOAD;}
{KW_DISP}	{return KW_DISP;}
{KW_TRUE}	{return KW_TRUE;}
{KW_FALSE}	{return KW_FALSE;}

{OP_PLUS}		{return OP_PLUS;}
{OP_MINUS}		{return OP_MINUS;}
{OP_DIV}		{return OP_DIV;}
{OP_MULT}		{return OP_MULT;}
{OP_CP}			{return OP_CP;}
{OP_OP}			{return OP_OP;}
{OP_DBLMULT}	{return OP_DBLMULT;}
{OP_OC} 		{return OP_OC;}
{OP_CC} 		{return OP_CC;}
{OP_COMMA}		{return OP_COMMA;}
{STRING}		{return STRING;}
[[:space:]]+ 		/* ignore space */
{COMMENT}		{return COMMENT;}


[[:alpha:]][[:alnum:]]*   {strcpy(yylval.id, yytext); return ID;}
[[1-9][:digit:]]+         {yylval.ival = atoi(yytext); return VALUE;}
{VALUE}					  {yylval.ival = atoi(yytext); return VALUE;}

	
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
/*
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
*/