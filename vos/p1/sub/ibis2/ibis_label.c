#include "ibis.h"
#include "errdefs.h"
#include <string.h>

/* IBIS Label Manipulation Routines */

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

int IBISLabelRemove( unit )
int unit;	/* input: VICAR file unit */
{
	int status=1;
	int maxlen,nelts;
	char key[MAX_GRP_NAME+1];
	char format[10];
	_ibis_current_module="IBISLabelRemove";


	/* Position file pointer at beginning of IBIS Property */
        status = zlinfo(unit,"property","property",format,&maxlen,
               &nelts,"property","ibis",NULL);

	if (status != 1) return 1; /* no such property */
	
	/* zap those sick little monkies! */       
	while (1)
	{
	    /* Get next keyword */
           status = zlninfo(unit,key,format,&maxlen,&nelts,NULL);
           if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
                (strcmp(key,"PROPERTY") == 0)) break;
	   else if (status != 1) break;
	
	   status=zldel( unit, "property",key, "property","ibis",NULL);
	   if (status != 1) break;
	}

	/* now zap the IBIS label */
	status=zldel( unit, "property","property", "property","ibis",NULL);

	
	return status;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2_(ibis_label_remove, IBIS_LABEL_REMOVE) ( int *unit , int *status)
#if 0
int *unit;	/* input: VICAR file unit */
int *status;
#endif
{
   *status = IBISLabelRemove( *unit );  
   return;
}

