/**
 **  ibis.c: Basic IBIS-2 file utilities
 **      Uses new IBIS-2 Subroutine Library.
 **/

#include "vicmain_c"
#include "ibis_local.h"
#include <string.h>

static char *subcmd[]={
	"gen",
	"list",
	"copy",
	"catenate",
	"group",
	"xxxx"
};

typedef enum {
	cmdGEN=0,
	cmdLIST,
	cmdCOPY,
	cmdCATENATE,
	cmdGROUP,
	cmdLAST
} cmdType;

/* Global variable - Num Cols (dim) for Graphics-1 files */
int gr1dim; 

void main44(void)
{
    int count,def,cmd;
    char command[20],*str;
    
    /* get the global parm */
    zvp( "gr1dim", &gr1dim, &def);
    
    /* determine subcommand */
    zvparm("_SUBCMD",command,&count,&def,0,0);
    for (str=command;*str;str++) *str = tolower(*str);
    for (cmd=cmdGEN;cmd<=cmdLAST;cmd++)
    	if (!strcmp(command,subcmd[cmd])) break;

    switch (cmd)
    {
        case cmdGEN :      new_file();  break;
		   		
        case cmdLIST :     list_file(); break;
		   		
        case cmdCOPY :     copy_file(); break;
	
        case cmdCATENATE : catenate_files(); break;
	
        case cmdGROUP :    modify_group(); break;
	
	/* This should never happen: */   		
	default: zvmessage("Unknown Subcommand"," "); zabend();
    }
}


void pre_format(int ibis)
{
	int i,status;
	int intcols[40],a4cols[40],num_intcol,num_a4col;
	
	zvp( "intcols", intcols, &num_intcol );
        if (!intcols[0]) num_intcol=0;
	for (i=0;i<num_intcol;i++)
	{
		status=IBISColumnSet(ibis,
			ICOLUMN_FORMAT,IFMT_FULL,intcols[i]);
		if (status!=1) IBISSignal(ibis,status,1);
	}
	zvp( "a4cols", a4cols, &num_a4col );
        if (!a4cols[0]) num_a4col=0;
	for (i=0;i<num_a4col;i++)
	{
		status=IBISColumnSet(ibis,ICOLUMN_FORMAT,"A4",a4cols[i]);
		if (status!=1) IBISSignal(ibis,status,1);
		status=IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"A5",a4cols[i]);
		if (status!=1) IBISSignal(ibis,status,1);
	}
}

char *new_format_string(nc,deffmt,str)
int nc;
char *deffmt;
char *str;
{
    char *fmt=(char *)0,*ptr;
    int i;
    
    if (!deffmt) deffmt=IFMT_REAL;
    
    if (str) fmt=str;
    else
    {
	    fmt = (char *)calloc(1L, IFMT_SIZE * nc);
	    if (!fmt) return fmt;
    }
    
    for (i=0,ptr=fmt;i<nc;i++,ptr+=IFMT_SIZE)
    	strcpy(ptr,deffmt);
    
    return fmt;
}


