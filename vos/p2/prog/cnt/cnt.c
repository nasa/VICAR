#include <stdio.h>
#include <string.h>
#include "taeconf.inp"
#include "vicmain_c"
#include "parblk.inc"
#include "pgminc.inc"

/*  Revision History                                                    */
/*    2-87   SP   Added code to handle  NO RECORDS in SRCH file.        */
/*    6-94   SVH  Ported to UNIX - Steve Hwan				*/
/*    2-96   FFM  Made TOTAL & NXTFIL as optional keywords to obsolete  */
/*           procedure count                                            */

void main44(void)
{

#define MAXR1       80          /* buffer length for first record.      */
#define MAX_LEN     80


    int count,def;		/* COUNT, DEF for XVPARM		*/
    struct PARBLK out_parb;	/* Local parameter block for XQ calls	*/
    int next_file;		/* Next file to be retrieved		*/
    int stat,i;			/* Return status ind, increment var	*/
    int total;			/* Total number of files in list	*/
    int maxlen;			/* dummy parameter to satisfy zvparm	*/
    char filename[40];	        /* Name of ASCII file to be read	*/
    char new_name[44];	        /* filename with unique extension	*/
    char output_name[MAX_LEN];	/* Name of file for output		*/
    char msg[80];		/* Message buffer for zvmessage		*/
    char rec1[MAXR1];           /* buffer for first record in file.     */
    FILE *unit;			/* C I/O unit control block		*/

    zvparm("INPUT",filename,&count,&def,1,0);
    zvfilename(filename,new_name,43);

    unit = fopen(new_name,"r+");	/* Open the file		*/
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }


 /*   Load first record into rec1 buffer.                                */

    for (i=0; (rec1[i] = getc(unit) ) != '\n' && i < MAXR1-1; ++i)
         ;

    rewind(unit);

    if ( strncmp(rec1,"NO RECORDS",10) == 0 )
    {
       next_file = 0;                   /* case of no records found by SRCH */
       total     = 0;
    }
    else
    {
    stat = fscanf(unit,"NEXT FILE = %5d",&next_file);
    if (stat != 1)
    {
	zvmessage(" Error reading number of next file","");
	zvmessage(" First line of input file must be of the form:","");
	zvmessage(" NEXT FILE = file_number","");
	zabend();
    }

    if (next_file <= 0) next_file = 1;	/* Protect from user error	*/

    for (total = 0; 1; total++)
    {
	stat = fscanf(unit,"%s",output_name);
	if (stat == EOF) break;
	if (stat != 1)
	{
	    zvmessage(" Read error on input","");
	    zabend();
	}
    }
    }
    sprintf(msg," File %d out of %d total",next_file,total);
    zvmessage(msg,"");
	
    q_init(&out_parb,500,P_ABORT);	/* Initialize a local par block	*/

/* output variables TOTAL, and NXTFIL to vblock				*/
    q_intg(&out_parb,"TOTAL",1,&total,P_ADD);
    q_intg(&out_parb,"NXTFIL",1,&next_file,P_ADD);

    zvq_out(&out_parb);		/* Output vblock to TM		*/

    fclose(unit);
}

make_file_process_specific(old_name,new_name)

    char old_name[];		/* User supplied file name		*/
    char new_name[];		/* Output: new process specific name	*/

{
    strcpy(new_name,old_name);

    return;
}
