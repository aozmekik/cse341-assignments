%{
#include <stdio.h>
#include <string.h>
#include "gpp_interpreter.h"

void print_arr(int*);
int* concat_arr(int*, int*);
int* append_arr(int*, int);
int pow_func(int, int);

%}

/* To compile all:

yacc -d gpp_interpreter.y
lex gpp_lexer.l
cc -g lex.yy.c y.tab.c -ll

*/ 

%union{
    int ival;
    int *ivals;
    char id[20];
}

%start INPUT
%token STRING COMMENT OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS
KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT
KW_LOAD KW_DISP KW_TRUE KW_FALSE NEWLINE


%token <ival> VALUE
%token <id> ID


%type <ival> INPUT
%type <ival> EXPI
%type <ival> EXPB
%type <ivals> VALUES
%type <ivals> EXPLISTI
%type <ivals> LISTVALUE

%%

INPUT: 
    EXPI {printf("SYNTAX OK. \nResult = %d\n", $1);}
    |
    EXPLISTI {printf("SYNTAX OK. \nResult = "); print_arr($1);}
    ;

/** You can uncomment this for testing.
INPUT:  
    EXPI {printf("SYNTAX OK. \nResult = %d\n", $1);}
    |
    EXPLISTI {printf("SYNTAX OK. \nResult = "); print_arr($1);}
    |
    EXPB {printf("Result : %s\n", $1 == 1 ? "true" : "false");}
    |
    INPUT EXPI {printf("SYNTAX OK. \nResult = %d\n", $2);}
    |
    INPUT EXPLISTI {printf("SYNTAX OK. \nResult = "); print_arr($2);}
    |
    INPUT EXPB {printf("Result : %s\n", $2 == 1 ? "true" : "false");}
    ;
**/

/* Some of the CFGs of EXPI is not included here. Reason of this is,
 * there were inconsistencies between lexer tokens and parser definitions
 * part. And since we are inheriting the lexer from previous homework, I 
 * picked the lexer definitions correct and didn't include the other parts in CFG.
 * And also they were not reasonable to add to EXPI, because while, for or etc. 
 * returning something does not make sense.
 */
EXPI:
    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;} /* (+ EXPI EXPI) */
    |
    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;} /* (- EXPI EXPI) */
    |
    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;} /* (* EXPI EXPI) */
    |
    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;} /* (/ EXPI EXPI) */
    |
    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = pow_func($3, $4);}
    |
    ID {$$ = get_entry($1);}
    |
    VALUE {$$ = $1;}
    |
    OP_OP KW_SET ID EXPI OP_CP {$$ = $4; put_entry($3, $4);}/* (set Id EXPI) */
    |
    OP_OP KW_IF EXPB EXPI OP_CP {$$ = (1 == $3) ? $4: 0;} /* (if EXPB EXPI) */
    |
    OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (1 == $3) ? $4 : 0; } /* (for EXPB EXPI)*/
    |
    OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = (1 == $3) ? $4: $5;}
     /* (if EXPB EXPI EXPI) */
    |
    OP_OP KW_DISP EXPI OP_CP { $$ = $3; printf("Print: %d\n", $3);} 
    ;

EXPB:
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}   /* (and EXPB EXPB) */
    |
    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}    /* (or EXPB EXPB) */
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}      /* (not EXPB) */
    |
    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}  /* (equal EXPB EXPB) */
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);}  /* (equal EXPI EXPI) */
    |
    OP_OP KW_LESS EXPI EXPI OP_CP { $$ = $3 < $4; } /* (less EXPI EXPI) */
    |
    KW_TRUE  { $$ = 1; }   /* true */
    |
    KW_FALSE   { $$ = 0; } /* false */
    | 
    OP_OP KW_DISP EXPB OP_CP { $$ = $3; printf("Print: %s\n", ($3 ? "true":"false"));}
    ;

EXPLISTI:
    OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$$ = concat_arr($3, $4);}
    |
    OP_OP KW_APPEND EXPI EXPLISTI OP_CP {$$ = append_arr($4, $3);}
    |
    OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
    |
    LISTVALUE  {$$ = $1;}
    |
    OP_OP KW_DISP LISTVALUE OP_CP { $$ = $3; printf("Print: "); print_arr($3);}
    ;


/* The reason I didn't use symbol '‘' here explained in the gpp_interpreter.lisp file */

LISTVALUE:  /* LISTVALUE -> ( VALUES ) | () | nil */
    OP_OP VALUES OP_CP {$$ = $2;}
    |
    OP_OP OP_CP { $$= NULL; }
    |
    KW_NIL { $$ = NULL;}
    ;

VALUES: /* VALUES -> VALUES IntegerValue | IntegerValue */
    VALUES VALUE  {$$ = append_arr($1, $2);}
    |
    VALUE {$$ = NULL; $$ = append_arr($$, $1);}
    ;


%%

/* For printing error messages */
int yyerror(char *s) {
    fprintf(stderr, "SYNTAX ERROR. \n");
    return 0;
}

void print_arr(int *arr){

    printf("( ");
    for(int i=0;arr[i]!=-1; ++i)
        printf("%d ", arr[i]);
    printf(")\n");

}

// will be used both in init and append situations.
int* append_arr(int *arr, int num){ // build if not exist, or append the num.
    if(arr == NULL){ // create new arr.
        arr = (int *)malloc(sizeof(int)*2);
        arr[0] = num;
        arr[1] = -1;
    } 
    else{ // expand the old arr.
        int *temp = arr;
        int size = 0;

        while(*temp != -1){
            ++temp;
            ++size;
        }

        temp = arr;
        arr = (int*)(malloc(sizeof(int)*(size+2)));

        int i=0;
        for(i;i<size;++i)
            arr[i] = temp[i]; // copy old array.
        arr[i] = num; // add the item.
        arr[i+1] = -1; // new size.
        free(temp);
    } 

    return arr;     
}

int* concat_arr(int *arr1, int *arr2){
    int size1=0, size2=0;

    int *temp = arr1;
    while(*temp != -1){
        size1++;
        temp++;
    }
    
    printf("%d\n", size1);

    temp = arr2;
    while(*temp != -1){
        size2++;
        temp++;
    }

    printf("%d\n", size1+size2);

    temp = (int *) malloc(sizeof(int) * (size1 + size2) + 2);

    int i=0;
    for(i;i<size1;++i) // copy arr1.
        temp[i] = arr1[i];
    
    int j=0;
    for(j;j<size2; ++j) // copy arr2.
        temp[i++] = arr2[j]; 

    temp[i] = -1;
    free(arr1);
    free(arr2);
    return temp;
}

int pow_func(int b, int pow) {
    if (pow != 0)
        return (b * pow_func(b, pow - 1));
    else
        return 1;
}

int main(int argc, char **argv)
{

    ++argv, --argc;
    init_table();

    
    //if (argc > 1 && argv[1])
    //    yy_scan_string(argv[1]);

    while(1){
        yyparse();
    }

    return 0;
}

