/*  A C-bridge routine, called by TABLE86.F, that tests the C-bridge version
    of ABLE86, Zable86.c    
*/

#include "xvmaininc.h"
#include "ftnbridge.h"
void FTN_NAME(tzable86) (npar)
int *npar;
{
  int  istat, id, iu ;  
  int  zuf[100];

  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge Version  ****** ", " ");

  istat = zvunit(&iu, "INP", 1, 0) ;
  istat = zvopen(iu,"OP","READ", "OPEN_ACT","SA", 0) ;
  if (istat != 1) 
  {
    zvmessage("  Can NOT open Input ...  Abending .......", " ") ;
    zabend();
  }
    zuf[0] = *npar;
    zable86(&id , iu, &zuf[0]) ; 
    zprnt(4, 1, &zuf[0], " LABEL TYPE=.") ;
    if (zuf[0]==1)
       zvmessage(" Ground Calib Data.......", " ") ;
    else if (zuf[0]==2)
       zvmessage(" Phase I Data.......", " ") ;
    else if (zuf[0]==3)
       zvmessage(" Phase II Data.......", " ") ;
    zprnt(4, 1, &zuf[1], " FRAME NO=.") ;
    zprnt(7, 1, &zuf[2], " EXPOSURE=.")  ;   
    zprnt(4, 1, &zuf[3], " FILTER POSITION=.") ;
    zprnt(4, 1, &zuf[4], " FRAME RATE=.") ;
    zprnt(4, 1, &zuf[5], " FIBE/MOFIBE=.");
    zprnt(4, 1, &zuf[6], " BOOM FLAG =.");
    zprnt(4, 1, &zuf[7], " GAIN =.");
    zprnt(4, 1, &zuf[8], " MOD10 =.");
    zprnt(4, 1, &zuf[9], " EVENT YEAR =.");
    zprnt(4, 1, &zuf[10], " EVENT DAY =.");
    zprnt(4, 1, &zuf[11], " EVENT HOUR =.");
    zprnt(4, 1, &zuf[12], " EVENT MINUTE =.");
    zprnt(4, 1, &zuf[13], " EVENT SECOND =.");
    zprnt(4, 1, &zuf[14], " EVENT MSEC =.");
    zprnt(4, 1, &zuf[15], " PARTITION =.");
    zprnt(99, 12, &zuf[16], " TARGET =.");
    zprnt(7, 1, &zuf[19], " IOF =.");
    zprnt(7, 1, &zuf[20], " CONV =.");
    zprnt(7, 1, &zuf[21], " SORANGE =.");

    zprnt(99, 20, &zuf[22], " DARK CURRENT FILE =.");
    zprnt(99, 20, &zuf[27], " RADIOMETRIC FILE =.");
    zprnt(99, 20, &zuf[32], " BLEMISH FILE =.");
    zprnt(99, 20, &zuf[37], " SHUTTER-OFFSET FILE =.");
    zprnt(99,  8, &zuf[42], " EDR TAPE =.");
    zprnt(4, 1, &zuf[44], " EDR FILE =.");
    zprnt(4, 1, &zuf[45], " UBWC =.");
    zprnt(99,  7, &zuf[46], " PICNO=.");
    zprnt(4, 1, &zuf[48], " SEQNO =.");
    zprnt(7, 1, &zuf[49], " ENTROPY =.");
    if (*npar > 50) {
       zprnt(99, 32, &zuf[50], " Dark-Current directory=.");
       zprnt(99, 32, &zuf[58], " Radiometric directory=.");
       zprnt(99, 32, &zuf[66], " Blemish directory=.");
       zprnt(99, 32, &zuf[74], " Shutter-offset directory=.");
       zprnt(4, 1, &zuf[82], " READOUTMODE =.");
       }
    zprnt(4, 1, &id, " IND =.");
 }
