#include "xvmaininc.h"
#include "ftnbridge.h"

void   FTN_NAME(tzgeomav) ()
{
  float   loc[2][202] ;
  float   ires[404] ;
  float   conv[2216] ;
  int     i , j ;

  zvmessage(" ", " ") ;
  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge version  ****** ", " ");
  zvmessage(" ", " ") ;

  for (j=0; j<404; j++)  ires[j] = 0.0 ;

  for (i=0; i<8; i++)
  { 
   j = i + 1 ;
   zgetres(&loc[0][0], j) ;
   zprnt(4, 1, &j, " CAMERA SN.") ;
   zprnt(7, 404, &loc[0][0], " LOC(L,S).") ;
 
   zgeomav(&conv[0], j, &ires[0]) ;

   zprnt(0, 4, &conv[0], "nah=.") ;
   zprnt(0, 4, &conv[1], "   =.") ;
   zprnt(4, 1, &conv[2], " nah=.") ;
   zprnt(0, 4, &conv[3], " nav=.") ;
   zprnt(0, 4, &conv[4], "    =.") ;
   zprnt(4, 1, &conv[5], " nav=.") ;
   zprnt(0, 4, &conv[6], " tiep=.") ;
   zprnt(0, 4, &conv[7], "    =. ") ;
   zprnt(4, 1, &j, " VIDICON SN.") ;
   zprnt(7, 2208, &conv[8], "geoma prameters=.") ; 
  }

}
