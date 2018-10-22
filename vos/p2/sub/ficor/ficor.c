/*  This is the FICOR subroutine that will return conversion factors

    If mode = 0 the intensity conversion factor is returned
    If mode = 1 the flux conversion factor is returned.
    If mode = 2 both are returned

    The intensity con factor is returned assuming label is in units of
    nanowatts/(cm**2, st, nm, dn) returns watts/(cm**2,st,nm,dn)        */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void zficor();

/*  This is the Fortran Callable portion of the FICOR subroutine      */

void FTN_NAME2(ficor, FICOR) ( int *inunit, char buf[7200],
			float con[2], int *mode, ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     char c_string[7200];
     zficor(*inunit, c_string, con, *mode);
     zsc2for(c_string,7200,buf,&inunit,4,2,1, mode);
}


/*  This is the C Callable portion of the FICOR subroutine           */

void zficor( inunit, buf, con, mode)
int inunit, mode;
char buf[7200];
float con[2];
{
     int bufsize, istat,indx;
     int inc;
     char *i, *results, *ptr;
     float buf1;
     char msg[100], *rptr; 
     char *compare = " MULTIPLY DN VALUE BY";

     indx = 0;
     bufsize=7200;
     istat = zlgetlabel( inunit, buf, &bufsize);
     i = buf;

     if (istat != 1)
     {
          zvmessage(" UNABLE TO RETURN VICAR LABEL"," "); 
          exit(-1);
     }
  
     rptr = strstr(buf,compare);
     results = rptr;
  
     if (results == 0)
     {
          zvmessage(" CONV VALUE NOT FOUND"," ");
          exit(-1);
     }

     inc = (results-i)+22;

     if (mode != 1)
     {
          ptr = i+inc;
          sscanf((buf+inc), " %f " ,&buf1);
          con[0] = (buf1)*0.000000001;
          sprintf(msg,"FIRST CONV FACTOR = %11.4E", con[0]);
          zvmessage(msg," ");
          indx=1;
     }

     if (mode >= 1)
     {
          rptr = strstr((buf+inc),compare);
          results = rptr;
  
          if (results == 0)
          {
               zvmessage(" CONV VALUE NOT FOUND"," ");
               exit(-1);
          }

          inc = (results-i)+22;
          ptr = i+inc;
          sscanf((buf+inc), " %f " ,&buf1);
          con[indx] = buf1; 
          sprintf(msg,"SECOND CONV FACTOR = %11.4E", con[indx]);
          zvmessage(msg," ");
     }
}
