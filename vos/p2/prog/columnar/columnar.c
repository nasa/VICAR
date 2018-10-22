/* Columnar takes in 2 ascii files of records and concatenates respective
   records side by side separated by 1 space.  If SKIP1 is set then one
   column (through 1st tab) of data is skipped from 2nd file. */
/* Revision History */
/* 7-97 RRD Combined modules columnar, columns, and columns2 */

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    255              /* buffer length for file name      */
#define MAXR2    2*MAXR1          /* buffer length for full file name */
#define BLEN     32767            /* buffer length for text           */
#define NUMFILES 2                /* number of input files            */

   int   count, def;              /* used to get parameters           */
   char  *buf_ptr, *buf_ptr2;     /* text pointers                    */
   FILE  *outfile;                /* out file                         */
   FILE  *infiles[NUMFILES];      /* array of infiles                 */
   char  inbufs[NUMFILES][BLEN];  /* array of text buffers            */
   char  outbuf[NUMFILES*BLEN];   /* output text buffer               */
   char  inames[NUMFILES][MAXR1]; /* array of input files             */
   char  oname[MAXR1];            /* output file name                 */
   char  full_out[MAXR2+1];       /* full file path name              */
   char  full_in[NUMFILES][MAXR2+1]; /* full file path names          */
   int   i, status, exit, zvpt;
/*  ================================================================= */

/* Retrieve and open user specified filenames */

   zvparm("IN", inames, &count, &def, NUMFILES, MAXR1);
   for (i=0;i<NUMFILES;++i) {
     zvfilename(inames[i],full_in[i],MAXR2);     
     infiles[i] = fopen(full_in[i], "r");
     if (infiles[i] == NULL)
       file_error(inames[i], "OPENING");
   }

   zvp("OUT", oname, &count);
   zvfilename(oname,full_out,MAXR2);
   outfile = fopen(full_out, "w");
   if (outfile == NULL)
     file_error(oname, "OPENING");

/* Read infiles into buffers and write to outfile */

   exit = 0;
   zvpt = zvptst("SKIP1");
   while (1) {
     outbuf[0]='\0';
     for (i=0;i<NUMFILES;++i) {
       if (fgets(inbufs[i], sizeof(inbufs[i]), infiles[i]) == NULL) {
	 exit = 1;
	 break;
       }
       buf_ptr = inbufs[i];
       if (zvpt && i != 0) {
	 buf_ptr2 = strchr(inbufs[i], '\t');
         if (buf_ptr2 != NULL)
	   buf_ptr = buf_ptr2;
	   ++buf_ptr;
       }
       if (i != NUMFILES-1) {
	 buf_ptr2 = strchr(inbufs[i], '\n');
	 if (buf_ptr2 != NULL)
	   *buf_ptr2 = ' ';
	 else {
	   buf_ptr2 = strchr(inbufs[i], '\0');
	   *buf_ptr2 = ' ';
	   ++buf_ptr2;
	   *buf_ptr2 = '\0';
	 }
       }
       strcat(outbuf, buf_ptr);
     }
     if (exit) break;
     status = fprintf(outfile,"%s",outbuf);
     if (status==0)
       file_error(oname, "WRITING");
   }


   for (i=0;i<NUMFILES;++i) {
     status= fclose(infiles[i]);
     if (status!=0) 
       file_error(inames[i], "CLOSING");
   }
   status= fclose(outfile);
   if (status!=0)
     file_error(oname, "CLOSING");
   return;
}


/* handles file errors */
file_error(fname, msg)
char fname[], msg[];
{
  char buf[MAXR1+25];

  sprintf(buf, "ERROR %s TEXT FILE '%s'", msg, fname);
  zmabend(buf);
  return;
}
