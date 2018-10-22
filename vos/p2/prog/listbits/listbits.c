/* VICAR PROGRAM LISTBITS -- lists the bits in an image area */
#include "vicmain_c"

void main44(void)
{
int i,j,count;
int iunit,sl,ss,nl,ns,nli,nsi,line,samp;
unsigned char ibuf[10000];	/* Input image line */
int space,blank=32;
unsigned char pbuf[4][8];
char zbuf[80];
 
/* Log LISTBITS startup message */
zifmessage ("LISTBITS version 02-May-94");

/* Set default error handling action */
zveaction ("SA", "");

/* Assign unit number for the file about to be read */ 
zvunit (&iunit,"INP",1,NULL);

/* Open input file using assigned unit number */
zvopen (iunit,NULL);

/* Request image size */
zvsize (&sl,&ss,&nl,&ns,&nli,&nsi);

/* Indicate whether a keyword was specified */
space  = zvptst("SPACE");

/* Loop through the file one line at a time, format and print results */
for (line=sl; line<sl+nl; line++) {

   /* Read a single line from operator specified input-image file */
   zvread(iunit,ibuf,"LINE",line,"SAMP",ss,"NSAMPS",ns, NULL);

   sprintf (zbuf, "Line=%10d", line);
   zvmessage (zbuf, "");

   for (j=0; j<8; j++){
      for (i=0; i<4; i++) {
         pbuf[i][j]= blank;
      }
   }

   for (j=0; j<sizeof(zbuf); j++) { 
      zbuf[j] = blank;
   }

   zbuf[sizeof(zbuf)-1] = '\0';

   /* Loop through the number of samples for the line being processed */
   for (i=0; i<ns; i+=4)	/* print the bits 4 bytes at a time */
       {

       /* Set 'samp' to starting sample number */
       samp = ss + i;

       /* Convert integer 'samp' to decimal ASCII and store in zbuf[0..3] */
       sprintf (zbuf, "%3d", samp);

       /* Clear zbuf 'NULL' character for next line to be printed */
       count = 3;
       zbuf[count] = ' ';

       /* Convert hexadecimal array ibuf[] into ASCII and store in zbuf[] */
       count = 4;
       zhexcon(&ibuf[i],&zbuf[4],&count);		/* hex number */

       if (i > ns-4) {			/* If on last four bytes... */
           for (j=0; j<8; j++) zbuf[j+2*(ns-i)+5] = ' '; /*blank*/
       }

       for (j=0; j<4; j++) {
	   listbyte(&ibuf[i+j],&pbuf[j][0]);
           zmve (1,8,&pbuf[j][0],&zbuf[17*j+13],1,2);
       }
       zvmessage (zbuf,""); 
       if (space) zvmessage ("0",""); 
       }
   }
zvmessage ("LISTBITS task completed","");
zvclose (iunit,NULL);
return;
}

/* LISTBYTE():
   Inspect each byte in the input buffer. Chech each bit position by checking
   magnitude of byte (modulo 128, 64, 32, 16, 8, 4, 2 & 0).  If byte value is 
   greater than or equal to checked value then reduce the value of the byte
   by the magnitude of the bit position being checked and return an ASCII '1',
   else return an ASCII '0' */ 

listbyte(ibuf,pbuf)
unsigned char *ibuf;
unsigned char *pbuf;
{
   int ival;
   unsigned char *qbuf;

   qbuf = pbuf;

   ival = *ibuf;

   /* If highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 128) {
      *qbuf=49;
      ival = ival - 128;
   } else {
      *qbuf=48;    
   }

   /* Bump output buffer pointer */
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 64) {
      *qbuf=49;
      ival = ival - 64;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 32) {
      *qbuf=49;
      ival = ival - 32;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 16) {
      *qbuf=49;
      ival = ival - 16;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 8) {
      *qbuf=49;
      ival = ival - 8;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 4) {
      *qbuf=49;
      ival = ival - 4;
   } else {
      *qbuf=48;    
   }
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 2) {
      *qbuf=49;
      ival = ival - 2;
   } else {
      *qbuf=48;    
   } 
   qbuf++;

   /* If next highest order bit is set, return ASCII '1', decimal 49, hex 31 */
   /* else return ASCII '0', decimal 48, hex 30 */  
   if (ival >= 1) {
      *qbuf=49;
   } else { 
      *qbuf=48;    
   }
   qbuf++;
}
