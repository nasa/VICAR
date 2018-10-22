#include <stdio.h>
#include <string.h>
#include "vicmain_c"

/*  Revision History                                                    */
/*    8-86   SP   changed fscanf to sscanf and changed                  */
/*                format specifier from "%5d" to "10%d"                 */
/*                to allow users to modify the first record in file.    */
/*                added code to check for first record having a         */
/*                different length than expected and to handle this.    */
/*    6-94   SVH  Ported to UNIX - Steve Hwan				*/

void main44(void)
{
#define MAXR1    80             /* buffer length for first record.      */
#define LENGTH1  17             /* number of chars in first record      */
   
    FILE *unit, *ounit;	        /* C I/O control block			*/
    int file_number,stat;	/* NEXT FILE number and status return	*/
    int count;			/* COUNT from zvp			*/
    int i,j, length;              /* loop variable, first record length   */
    char c;
    char rec1[MAXR1];           /* buffer for first record in file.     */
    int rec_tmp[MAXR1];           /* buffer for first record in file.     */
    char file_name[41];		/* name of file to reset		*/
    char new_name[45];		/* process specifiec file_name		*/
    long off_dst,off_src;	/* used for fseek */
    int c_buf_size, c_buf_indx;

    zvp("INPUT",file_name,&count);	/* get name of input		*/
    zvfilename(file_name,new_name,43);

    unit = fopen(new_name,"r+");
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }

/*   Load first record into rec1 buffer.                                */

    for (i=0; (rec1[i] = getc(unit) ) != '\n' && i < MAXR1-1; ++i)
         ;
    length = i;
    /* Start source offset at character right after the first \n. */
    off_src=i+1;

    stat = sscanf(rec1, "NEXT FILE = %10d", &file_number);
    if (stat != 1)
    {
	zvmessage(" Error reading number of next file","");
	zvmessage(" First line of input file must be of the form:","");
	zvmessage(" NEXT FILE = file_number","");
	zabend();
    }

    zvp("NEXTFILE",&file_number,&count);	/* Get next file number	*/

    if ( length >= LENGTH1 )      /* If no change in record length just */
    {                             /* change first record.               */
      rewind(unit);				/* Write it out		*/
      sprintf(rec1,"NEXT FILE = ");
      for(i=0; i< (length-LENGTH1); i++)
        strcat(rec1," ");
      sprintf(&rec1[12+length-LENGTH1],"%5d\n",file_number);
      stat = fprintf(unit,rec1);
      if (stat != length+1)
      {
 	zvmessage(" Write (update) error on input","");
	zabend();
      }
      fclose(unit);
    } else { 
/* sorry guys.  I tried to get VMS to use the same code as UNIX, but */
/* it just refused, and i tried 3 different approaches, so we're stuck */
/* with 2 seperate routines. */
#if VMS_OS
      /* open up another file and just copy into it - the reason this */
      /* works is because VMS just opens up another version. */
      ounit = fopen(new_name,"w");
      if (ounit == NULL)
      {
	zvmessage(" File open error.  Check specification.","");
	zabend();
      }
      sprintf(rec1,"NEXT FILE = %5d\n",file_number);

      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], ounit );          /* write out first record. */

      while ( (c=getc(unit)) != EOF )
        putc( c, ounit);                 /* copy the rest of the file. */

      fclose(unit);
      fclose(ounit);

#else
      /* determine size needed and fill circular buffer */
      c_buf_size = LENGTH1-length;
      c_buf_indx = 0;
      for(j=0; (j<c_buf_size) && (rec_tmp[c_buf_indx] = getc(unit)) != EOF;
		j++) {
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_src++;
      }
      /* off_scr is now sitting at the first read element (0) which will */
      /* be the next written over. */

      rewind(unit);
      /* write 1st record */
      sprintf(rec1,"NEXT FILE = %5d\n",file_number);
      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], unit );          /* write out first record. */
      off_dst = strlen(rec1);

      /* copy over remainder of file through circular buffer */
      /* until we hit end of file */
      fseek(unit,off_src,0);
      while ( (rec_tmp[c_buf_indx]=getc(unit)) != EOF )
      {
        fseek(unit,off_dst,0);
        /* write out oldest element */
        putc(rec_tmp[ (c_buf_indx+1) % (c_buf_size+1)] , unit);
        off_dst++;
        off_src++;
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        fseek(unit,off_src,0);
      }

      /* wrap up the file */
      c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
      for(j=0; j<c_buf_size ; j++) {
        putc(rec_tmp[c_buf_indx],unit);
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_dst++;
      }

      fclose(unit);
#endif
    }

}
