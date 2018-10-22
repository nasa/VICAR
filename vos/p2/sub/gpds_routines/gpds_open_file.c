				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/

/*

VICAR ROUTINE GPDS_OPEN_FILE

PURPOSE

GPDS_OPEN_FILE is a MIPS PDS utility to open a PDS standard file.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44 ()
	{

	AGGREGATE *label_ptr;
	char file_name[40];
	int status_flag;
	...

	label_ptr = gpds_open_file( file_name, &status_flag );

	...
	
	}

INPUT

	file_name  	(character string)

	This is a complete pathname for the PDS standard, PDS labeled file 
	that is to be read or searched.

RETURN

	label_ptr 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values. If status flag is negative, there
	is a failure with opening the designated file and a NULL is returned
	for label_ptr.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the opening of the
	file for reading or writing of the PDS label. The following are the
	meanings of the returned values:

	0	successful opening for reading of PDS labeled file
	-1	failure in returning label pointer
	
BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August, 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.

*/
/*************************************************************************

C Callable Version

*************************************************************************/

AGGREGATE gpds_open_file( file_name, status )
char *file_name;
int  *status;
{
AGGREGATE label_ptr;
int label_status;

/* Initialize status flag 		*/
label_status = PDS_SUCCESS;

/* Perform PDS label library setups 	*/
lab_setup();

/* Read label of file and return ODL tree label pointer 	*/
label_ptr = lab_read_label_or_template( file_name );

/* Perform error checking */
if ( 	label_ptr == NULL ||
	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	*status = -1;
else
	*status = 0;

return label_ptr;
}
