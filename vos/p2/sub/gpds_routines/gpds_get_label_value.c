				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "pdsdef.h"		/* PDS Label Library Include Files	*/
#include "label.h"		
#include "gpds.h"		/* VICAR/PDS Include File		*/
#include <string.h>

/*
VICAR ROUTINE GPDS_GET_LABEL_VALUE

PURPOSE

GPDS_GET_LABEL_VALUE is a MIPS PDS utility to retrieve a value of a keyword
in a PDS labeled file.

CALLING SEQUENCE

	#include "gpdsroutines.h"

	main44()
	{

	AGGREGATE *label_ptr;
	char object_name[40];
	char keyword_name[40];
	char *value;
	int element_of_value;
	int status_flag;
	...

	value = gpds_get_label_value( label_ptr, object_name, keyword_name, 
			element_of_value, &status_flag );

	...
	
	}

INPUT

	label_ptr		(AGGREGATE type)

	This is a pointer to the root of the PDS label. It is returned by the
	routine GPDS_OPEN_FILE.

	object_name  		(character string)

	This is the PDS label object name (exact spelling including underscores)
	in which the PDS keyword value is to be found.

	keyword_name  		(character string)

	This is the PDS keyword name (exact spelling including underscores)
	for which a value is to be returned.

	element_of_value	(integer)

	This is the index or position of a value in a multivalued keyword
	in the PDS label (e.g. ASSOCIATED_PRODUCT_ID = { 03R00321, 03B00322 })
	In this example, the value of 03B00322 of the keyword would be 
	referenced by element_of_value set equal to "2", corresponding to the
	second element of the multivalued keyword. 

RETURN

	value 		(character string) 

	This is the pointer to a character string list containing the value
	of a PDS keyword in the PDS label.

OUTPUT

	status_flag 	(integer)

	This is an indicator of the success or failure of the returning the
	value of a PDS keyword. The following are the meanings of the 
	returned values:

	0	successful returning of pointer to value
	-1	failure in returning pointer to value, failure in 
		finding object or parameter;

	
BACKGROUND

This routine is based on the PDS Label Library of the PDS Toolbox, version 5.2
dated September 27, 1991. Refer to PDS Label Library User's Guide, D-8922
for reference to software called in this source.

HISTORY

Programmer: 		Justin McNeill, Jr.
Cognizant programmer:	Justin McNeill, Jr.
Written:		August 1993
Revisions:

Aug. 10, 98  ...RRP... Made it ANSI compatiable so it can be compiled on
                       HP.

*/
/*************************************************************************

C Callable Version

*************************************************************************/
char *gpds_get_label_value( label_ptr, object, keyword, element, status )
AGGREGATE label_ptr;
char *object;
char *keyword;
int element;
int *status;
{
int 	label_status;
char 	*value;
static char	blank[2];

strcpy(blank," ");

/* Get a single value from PDS label	*/
value = lab_get_value( label_ptr, object, 0, 1, keyword, 0, element,
	FALSE, &label_status );

/* Perform error checking		*/
if ( 	label_status == PDS_ERROR ||
	label_status == PDS_MULTIPLE_OBJECTS ||
	label_status == PDS_MULTIPLE_PARMS )
	{
	*status = -1;
	return blank;
	}
else
	{
	*status = 0;
	return value;
	}
}
