				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/


/*

VICAR ROUTINE GPDS_CLOSE_FILE

PURPOSE

GPDS_CLOSE_FILE is a MIPS PDS utility to close a PDS standard file for
label reading and writing.

CALLING SEQUENCE


	#include "gpdsroutines.h"

	main44()
	{
	AGGREGATE label_ptr;
	char file_name[40];
	int status_flag;
	...

	label_ptr = gpds_open_file( file_name, &status_flag );

	...

	gpds_close_file( label_ptr );

	...
	
	}

INPUT

	label_pointer 	(AGGREGATE data type)

	This is the pointer to the PDS specific ODL tree which represents
	the label in the file. This value is passed on to other GPDS routines
	that read or write PDS label values.

BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:		None

*/
/*************************************************************************

C Callable Version

*************************************************************************/
void gpds_close_file( label_ptr )
AGGREGATE label_ptr;
{
/* 

Deallocate memory of ODL tree	

*/

if ( label_ptr != NULL )
	label_ptr = lab_remove_label_or_template( label_ptr );

lab_exit();
}
