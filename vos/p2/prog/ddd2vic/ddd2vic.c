/*******************************************************************************

ddd2vic  -- 

Purpose: to convert ddd formatted MGS filesto VICAR format

20dec00 -bam- original version

******************************************************************************/

#include <math.h>
#include "vicmain_c"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

FILE *fp,*fopen();              /* for file work */

int i_unit,o_unit;              /* for VICAR */
int status;

char message[80];               /* for user information output */
  
int error;                      /* return error flag */
int hstart,hend;                /* start and end of header data */

/***************************************************************************/
void main44(void)
{
  char verdat[11];              /* VERSION_DATE */
  int nl, ns;	        	/* actual dimensions */
   
  /* inform user of Program version & update VERSION_DATE for label */
  zvmessage("   ","");
  zvmessage("*** ddd2vic version 20-Dec-2000 ***","");
  zvmessage("   ","");
  strcpy( verdat, "2000-12-20");

  process_parms(&nl, &ns);      /* get nl,ns + data file names */
  if ( error == 1 ) return;     /* error handling files */

  process_data(nl,ns);          /* read and write ddd data */

  fclose(fp);
  status = zvclose(o_unit,NULL);

  return;
}


/****************************************************************************/
/* process user input parameters                                            */
/****************************************************************************/
process_parms(nl,ns)
  int *nl, *ns;	        	/* actual dimensions */
{
  int i,l,s;
  char file[100];

  error = 0;                    /* return error flag */

  zvp("INP",file,&i);	 	/* Get input file name	*/
  fp = fopen(file,"r");         /* open the input file */
  if ( fp == 0 ) 
      {
      sprintf ( message, "\nError opening input file\n\n " );
      zvmessage (message," ");
      error = 1;
      return 0;
      }

  find(&l, &s);  /* get input nl, and ns from ddd header */
  *nl = l;
  *ns = s;
  if ( error == 1 ) 
      {
      sprintf ( message, "\nError finding # of lines and samples\n\n " );
      zvmessage (message," ");
      error = 1;
      return 0;
      }

 
  zvp("OUT",file,&i);
  status = zvunit(&o_unit,"OUT",1,NULL);
  if ( status != 1 ) goto bad;
  status = zvopen(o_unit,"OP","WRITE","U_FORMAT","BYTE","O_FORMAT",
	"BYTE","U_NL",*nl,"U_NS",*ns,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  if ( status != 1 ) goto bad;
  return 0;

/* some type of problem with the output file */
bad: sprintf ( message, "\nError handling output file\n\n " );
     zvmessage (message," ");
     error = 1;
     return 0;
}


/****************************************************************************/
/* Find the number of lines and samples                                     */
/****************************************************************************/
find(l, s)
  int *l,*s;	        	/* actual dimensions */
{
 int i,j,k;
 int len;
 char header[1024];              /* ddd header */
 int nl,ns;

 len = 1024;
 status = fread(header,1,len,fp);

/* grab all the pertinent data from the header first */
 for ( i=0; i<len; i++ )
     {
     if ( header[i] == 'd' ) /* found the start of the header */
         {
         hstart = i;
         j = i;
         for ( k=j; k<len; k++ )
             {
             if ( header[k] == '0' ) /* found the end of the header */
                 {
                 hend = k - 1;
                 goto next;
                 } 
             }       
         }
     }

/* find the # of samples */
next:
 for ( i=0; i<len; i++ )
      {
      if (strncmp(&header[i],"cross",5)==0 ) /* find the samples */
          {          
          sscanf(&header[i+6], "%d", &ns);
          break;
          }
      }

/* find the # of lines */
 for ( i=0; i<len; i++ )
      {
      if (strncmp(&header[i],"down",4)==0 ) /* find the lines */
          {          
          sscanf(&header[i+5], "%d", &nl);
          break;
          }
      }
  *l = nl;
  *s = ns;
  return 0;
}


/****************************************************************************/
/* process data                                                             */
/****************************************************************************/
process_data(nl,ns)
  int nl,ns;	        	/* actual dimensions */
{
  int i;
  int lines;
  unsigned char *buf;           /* buffer for the data */
  int i_status, o_status;

/* allocate enough room for a line of data */
  buf = (unsigned char *)malloc(sizeof(char) * ns );  

  for ( i=0; i<nl; i++ ) /* read and write the data */
      {
      i_status = fread(buf,1,ns,fp);
      if ( i_status == EOF ) /* check for end of file */
          {
          lines = i;
          goto end;
          }      
      if ( i_status != ns ) /* check for end of file */
          {
          sprintf ( message, "\nError reading input\n\n " );
          zvmessage (message," ");
          sprintf ( message, "\nProgram terminated\n\n " );
          zvmessage (message," ");
          lines = i;
          goto end;
          }      
      o_status = zvwrit (o_unit, buf, NULL); /* and write it out */
      lines = i;
      }

end:
  status = zladd(o_unit,"SYSTEM","NL",&lines,"FORMAT","INT",NULL);
  return 0;
}
