
#include <stdio.h>		/* Standard C I/O Include File          */
#include <math.h>		/* Map Projection Include File  */
#include "vicmain_c"
#include "mp_routines.h"

/****************************************

TEST PROGRAM TMP_LABEL.C for MP Routines

(taken from program by E. Hauber of DLR)

****************************************/

void main44()
{
	int	status,count;
	int	inunit,outunit;
	int	inlines,insampl;
	int	sll,i,num;
	int	buf_step;
	int	read;
	int	type[mpNUMBER_OF_KEYWORDS], class[mpNUMBER_OF_KEYWORDS];

	unsigned char	buffer[10000];
	char		keylist[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
	char		cval[133],msg[133];
	double		dval,zval;

	MP mp_obj;			/* Declaration of MP pointer */

/*************************************************************
	Assign a unit number to the input file:
*************************************************************/
	status = zveaction( "AU", "System error" );

	status = zvunit(&inunit,"INP",1,0);

/****************************
	Open the input file :
****************************/

	status = zvopen(inunit,"OP","READ","U_FORMAT","BYTE",0);
	zvmessage(" ",0);	

/********************************************************************
	Retrieve the number of lines and samples in the input file :
********************************************************************/

	status = zvget(inunit,"nl",&inlines,"ns",&insampl,0);

/******************************************************************
	Map Label processing begins: "mpInit"
******************************************************************/

	status = mpInit(&mp_obj);	 /* Create MP data object */
	ABENDif( status<mpSUCCESS );

	zvmessage("Mp object has been opened",0);

/*********************************************
	MP label is being read
*********************************************/

	status = mpLabelRead(mp_obj,inunit);
	ABENDif( status<mpSUCCESS );

	zvmessage("Old map label has been read.",0);

/****************************************************************************
	Now we check the mp_obj:
	- We use the routine "mpGetKeywords" to derive all the keywords from
	the 'mp_obj', ie. the number of keywords is returned into "num"
	and the names of the keywords are returned into "keylist".
	- We use the routine "mpGetValues" to get the values of the keywords
	found in the 'mp_obj' with "mpGetKeywords".
****************************************************************************/

	status = mpGetKeywords( mp_obj, keylist, &num, type, class);
	ABENDif( status<mpSUCCESS );


	for (i=0; i<num; i++) {

		if (type[i] == mpCHAR) {
		status = mpGetValues( mp_obj, keylist[i], cval, "");
		strcpy( msg, keylist[i]);
		strcat( msg, " = ");
		strcat( msg, cval);
		zvmessage( msg, "");

		}
		else if (type[i] == mpDBLE) {

		status = mpGetValues( mp_obj, keylist[i], &dval, "");
		strcpy( msg, keylist[i]);
		strcat( msg, " = ");
		sprintf( cval, " %10.3e", dval);
		strcat( msg, cval);
		zvmessage( msg, "");
		}
	}

	status=zvunit(&outunit,"OUT",1,0);
	status=zvopen(outunit,"OP","WRITE","OPEN_ACT","SA","U_FORMAT",
	"BYTE","O_FORMAT","BYTE","U_NL",inlines,"U_NS",insampl,0);

	status = mpLabelWrite( mp_obj, outunit, "HISTORY");
	ABENDif( status<mpSUCCESS );

	status = mpLabelWrite( mp_obj, outunit, "PROPERTY");
	ABENDif( status<mpSUCCESS );


/********************************************************
	-- Loop through all image lines
	-- All lines are read in from the input image
	-- All lines are written out to the output image
********************************************************/

	for( i=1; i <= inlines; i++)
		{
			sll=1+i-1;
			buf_step = (i-1)* insampl;

			status = zvread(inunit,buffer,"samp",1,
				      "line",sll,"nsamps",insampl,0);

			if (status == 0)
			{
				free(buffer);
				break;
			}

			status = zvwrit(outunit,buffer,"samp",1,
				      "nsamps",insampl,0);

			if (status == 0)
			{
				free(buffer);
				break;
			}

		}

	status = zvclose(outunit,0);
	zvmessage("Output image has been closed",0);


/*********************************************
	MP object is free again
*********************************************/

	status = mpFree(mp_obj);
	zvmessage("Mp object has been closed",0);

/*********************************************
	Images are closed after processing
*********************************************/

	status = zvclose(inunit,0);
	zvmessage("Input image has been closed",0);

}
