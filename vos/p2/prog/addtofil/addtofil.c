#include <stdio.h>
#include <string.h>
#include "vicmain_c"

/*  Revision History                                             */
/*    11-96  SXP   Converted from DCL to C for portability.	 */
/*     1-97  SXP   Programmer note:  This version crashes mysteriously
                   on our current version of VMS.  I noticed in RESET.COM,
                   SVH could not get VMS to behave.  Maybe this is a
                   similar problem.  I left this module using DCL for VMS.  */
void main44(void)
{
#define MAXR1    256            /* buffer length for file name.     */
#define MAXR2    2*MAXR1        /* buffer length for full file name */   

    FILE *unit;	                /* C I/O control block	            */
    int stat;	                /* status return	            */
    int count;			/* COUNT from zvp	       	    */
    char str[MAXR1];            /* buffer for string.               */
    char file_name[MAXR1];	/* name of file to be appended to.  */
    char full_name[MAXR2];	/* full file path name		    */
/*=========================================================================*/

    zvp("STRING1",str,&count);             /* get string to be appended */ 

    zvp("INPUT",file_name,&count);	   /* get name of input		*/
    zvfilename(file_name,full_name,MAXR2);

    unit = fopen(full_name,"a");
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }

    stat = fprintf (unit, "%s\n", str); 
    if (stat <= 0)
    {
 	zvmessage(" Write (update) error on input","");
	zabend();
    }
    fclose(unit);
}
