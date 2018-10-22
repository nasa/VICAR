#include "ibis.h"
#include <string.h>
#include <stdio.h>

/* Take the status of a previous XIBIS subroutine call and print	*/
/* the appropriate error message.  Abort the program if			*/
/* requested to.							*/


/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/


int IBISSignalU( unit, status, abend_flag )
int unit;
int status;
int abend_flag;
{
   static char msgbuf[200];

   static struct
   {
      char *key;
      char *text;
   } xi_messages[] =
   {
     {"MEMFAIL","Error allocating Memory; program error"},
     {"NOTIBIS","File unit is not an IBIS file"},
     {"ALRDYOPN","IBIS File is already opened"},
     {"RDONLY","IBIS File is opened read-only"},
     {"INVALMOD","Invalid mode for opening IBIS file"},
     {"INVALPAR","Invalid IBIS parameter"},
     {"NOTFOUND","IBIS Group not found"},
     {"LIMEXCD","Column Limit Exceeded"},
     {"INVALCPAR","Invalid Column Parameter"},
     {"NOTOPEN","IBIS File is not open"},
     {"OLDIBIS","Unsupported in old IBIS Files"},
     {"NOSUCHCOL","No such column exists"},
     {"LOCKCOL","Column is currently locked by a record"},
     {"INVALNAM","Group name has invalid characters"},
     {"MODFORMAT","Can't modify a FORMAT group"},
     {"INVALTYP","Invalid group TYPE for this routine"},
     {"GRPEXISTS","This group already exists"},
     {"EMPTYGRP","Defined group is empty (no columns)"},
     {"CANTTRANS","Unable to translate ASCII<->numeric"},
     {"INVALFMT","Invalid FORMAT specified"},
     {"LENGTHREQ","String length required"},
     {"LASTROW","Attempted to access past last row of file"},
     {"IMAGEDAT","File Contains Image Data; Can\'t extend"},
     {"NSNOTSET","You must set the pixel NS value before FORMAT"},
     {"NCREQD","The NC (dimension) value is required"},
     {0,0}		/* Terminator entry */
   };
   
   	if (status==1) return 0; /* not an error */
   	
	if (status < IBIS_BASE && status > IBIS_LAST )
	{
		char err_key[18];
		int msgcode=IBIS_BASE-status-1;
		
      		sprintf(msgbuf, "Exception in %s", _ibis_current_module);
   		zvmessage(msgbuf, "IBIS-GENERR");
		strcpy(err_key,"IBIS-");
		strcat(err_key,xi_messages[msgcode].key);
		
		zvmessage(xi_messages[msgcode].text, err_key);
		if (abend_flag) zabend();
	}
	else /* this is a VICAR signal */
	{
      		sprintf(msgbuf, "Exception in %s Reported by VICAR",
      				 _ibis_current_module);
   		zvmessage(msgbuf, "IBIS-VICMSG");
		zvsignal( unit, status, abend_flag );
	}
	return 0;
}

int IBISSignal( ibis_id, status, abend_flag )
int ibis_id;
int status;
int abend_flag;
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
 	return IBISSignalU( ibis->unit, status, abend_flag );
}


/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2_(ibis_signal, IBIS_SIGNAL) 
(
  int *ibis_id,	/* In: unit number of file whose operation is being checked */
  int *status,	/* In: status being checked */
  int *abend_flag /* In: if TRUE, call abend() */
)
{
   IBISSignal(*ibis_id, *status, *abend_flag);

   return;
}

void FTN_NAME2_(ibis_signal_u, IBIS_SIGNAL_U)
(
  int *unit,	/* In: unit number of file whose operation is being checked */
  int *status,	/* In: status being checked */
  int *abend_flag /* In: if TRUE, call abend() */
)
{
   IBISSignalU(*unit, *status, *abend_flag);

   return;
}
