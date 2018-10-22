				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"
/*

SUBROUTINE      		mpFree

Purpose				To free memory for a map projection data object.

Function			Deallocates memory for map projection data 
				object using free().

Background and References:	MIPS Map Projection Software Users' Guide, 
				JPL, D-11810, J. McNeill, May 1994.

Programming Language:		ANSI C


Date:				October 1993
History:			Original

*/




int mpFree( MP mp_obj )
{
/* Deallocate memory for pointer to structure MP_OBJECT			*/
free( mp_obj );

return mpSUCCESS;
}
