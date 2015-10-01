#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

/* Deletes the virtual address space associated with an array file */

int free_mapped_sect(arraystate)
struct arraystate *arraystate;
{
   char *inadr[2];

   inadr[0] = arraystate->start;	/* starting address of array file */
   inadr[1] = inadr[0] + arraystate->size - 1;		/* ending address */

   return sys$deltva(inadr, 0, 0);

}
