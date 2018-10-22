#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-GROUP                             ***
 ***                                                                  ***
 ************************************************************************/ 

modify_group()
{
	int outunit;
	int status,def,count;
	int ibis;
	int cols[20],ncols;
	int *colptr=(int*)0;
	char type[10];
	char name[33];
	char expr[80];
	char *exprptr=(char *)0;
	
	status=zvunit( &outunit, "inp", 1, NULL);
	if (status!=1) zvsignal( outunit, status, 1);
	
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(outunit, &ibis, IMODE_UPDATE, gr1dim, 0, 0, 0);
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	zvp("type", type, &def );
	zvp("name", name, &def );
	
	if (zvptst("create"))
	{
		zvparm("cols",cols,&ncols,&def,0,0);
		if (!def && ncols && cols[0]) colptr=cols;
		zvparm("expr",expr,&count,&def,0,0);
		if (!def && expr[0] && expr[0]!=' ') exprptr = expr;
		
		count=IBISGroupNew( ibis, type, name, colptr, ncols , exprptr);
		if (count<0) IBISSignal( ibis, count, 0);
	}
	else
	{
		status=IBISGroupDelete(ibis, type, name );
		if (status!=1) IBISSignalU( outunit, status, 1);
	}
	
	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( outunit, status, 1);
}


