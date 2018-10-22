/*  This is a program that will test the C-Callable PGRID  */
/*  Subroutine.                                            */


#include "vicmain_c"
#include "ftnbridge.h"

main44()
{

     int inunit, status, s1, ss, nl, nli, nsi;
     int i,nloc, nc, nr, ns, mode, max;
     short int trix;
     float loc[1000][2];

     zveaction("SA"," ");
     zvmessage("Test the C Interface"," ");
     zvmessage(" "," ");

     status = zvunit(&inunit,"INP",1,0);
     status = zvopen(inunit,"OP", "READ","OPEN_ACT","SA",0);

     zvsize(&s1,&ss,&nl,&ns,&nli,&nsi,0);
    
     if (nli >= nl) 
          max = nli;
     else
          max = nl;
     nloc=max/2;

     for (i=0; i<nli; i++)
     {
          status = zvread(inunit,&loc[i][0],"nsamps",nsi,0);          
     }
     nr = 2;
     nc = 10;

     trix = 0;

     zpgrid(loc,nr,nc,trix,0);

     status = zvclose(inunit,0);
     zvmessage(" "," ");
     zvmessage("Test the FORTRAN Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tpgrid)();  
}
