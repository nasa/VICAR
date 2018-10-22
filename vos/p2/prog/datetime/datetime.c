#include "vicmain_c"
#include  <math.h>
#include <string.h>

/**********************************************************************/
/* PROGRAM: datetime                                                  */
/**********************************************************************/
/*                                                                    */
/*   Program "datetime" is a VICAR applications program which types   */
/*   current local date and time.                                     */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/* HISTORY:                                                           */
/*                                                                    */
/*   Converted to 'C' and ported to UNIX  by CRI          March 94    */
/*   Prepared in Fortran-77 for MIPL by Steve Pohorsky    Sept  84    */
/*                                                                    */
/*   Jul. 9, 1998 ..T.Huang .. Modified to output a 4-digit year.     */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*                                                                    */
/*   Input args : None                                                */
/*                                                                    */
/*   Output args: None                                                */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/* MAIN PROGRAM                                                       */
/*                                                                    */
/**********************************************************************/


void main44()
{
    long time(), t;

    char  *date_time, *ctime();
    char  aday[3], amonth[4], ayear[5], adate[8];
    char  line[80];

    zifmessage("DATETIME version 09-Jul-1998");

    /* Get current time */
    t = time(0);

    /* Convert to Local in ASCII String  */
    date_time = ctime(&t); 

    /* Gather date and time pieces */
    strncpy(ayear, (date_time + 20), 4);
    strncpy(amonth, (date_time + 4), 3);
    strncpy(aday, (date_time + 8), 2);
    strncpy(adate, (date_time +11), 8);

    sprintf(line, "\nThe date and time are: %.2s-%.3s-%.4s %.8s\0\n",
            aday,amonth,ayear,adate);

    zvmessage(line, "");
}
