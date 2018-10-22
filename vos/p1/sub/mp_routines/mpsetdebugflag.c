				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"

int mp_debug;			/* MP debug flag declaration		*/

/*

SUBROUTINE      		mpSetDebugFlag 

Purpose				To set a global value to print internal
				calculations of the MP routines to standard
				output.

Function			Sets mp_debug global external variable to TRUE.

Libraries and subroutines
required to run routine:	none




Arguments:
	
	Name		Type		In/Out		Description
	
	Flag		int		Input		Value to which the
							mp_debug flag should
							be set.  Valid values
							are TRUE (1) or 
							FALSE (0).

	Status		int		Output 		Status flag for routine.
							Return values are
							mpSUCCESS or mpFAILURE.


Date:				May 1994

History:			Original */
   

int mpSetDebugFlag( int flag )
{
int status;
extern int mp_debug;

if( flag == TRUE || flag == FALSE )
	{
	mp_debug = flag;
	status = mpSUCCESS;
	}
else
	status = mpFAILURE;

return status;
}
