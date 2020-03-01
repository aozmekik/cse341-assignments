#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yyerror (char *s);
int yylex();

#define total_entry 20

struct entry {
	char id[20]; // key.
	int value;
};

struct symbol_table {
	struct entry table[total_entry];
	int valid_entry;
};

struct symbol_table *symtab;

void init_table(){
	symtab = (struct symbol_table*) malloc(sizeof(struct symbol_table));
	symtab->valid_entry = 0;
}

void put_entry(char id[20], int value){
	struct entry *e;
	e = (struct entry*) malloc(sizeof(struct entry));
	e->value = value;
	strcpy(e->id, id);

	int i=0;
	for(i;i<symtab->valid_entry; ++i){
		if(strcmp(e->id, symtab->table[i].id) == 0){ // if found, change.
			symtab->table[i].value = e->value; 
			return;
		}
	}


	// if not found, put.
	strcpy(symtab->table[i].id, e->id); 
	symtab->table[i].value = e->value; 
	++(symtab->valid_entry);
	free(e);
}

int get_entry(char id[20]){

	int i=0;


	for(i;i<symtab->valid_entry; ++i){
		if(strcmp(id, symtab->table[i].id) == 0) // if found, change.
			return symtab->table[i].value;
	}

	printf("Symbol not found in the symbol table!\n Exiting...");
	exit(-1);
}

