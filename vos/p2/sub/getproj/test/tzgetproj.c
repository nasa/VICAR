#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  bridge to C callable version of TGETPROJ.F */
/************************************************************************/

void FTN_NAME(tzgetproj)(unit)
  int *unit;
{
  int   fds,ind,camera,status;
  char project[6];
  char msg[132];

      zgetproj(*unit,project,&camera,&fds,&ind);
      if(ind != 0) zprnt(4,1,&ind,"fatal indicator=.");
      sprintf(msg,"project=     %s",project);
      zvmessage(msg,"");
      zprnt(4,1,&camera,"camera serial number=.");
      zprnt(4,1,&fds,   "frame number=        .");
}
