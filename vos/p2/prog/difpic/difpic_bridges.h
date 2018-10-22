#ifndef __DIFPIC_BRIDGES_H__
#define __DIFPIC_BRIDGES_H__

extern "C"
{
  #include "xvmaininc.h"
  #include "applic.h"
  #include "ftnbridge.h"
  void FTN_NAME2(main44_ftn,MAIN44_FTN)(int *STATUS);
  bool labeldifC();
  bool histdifC();
  bool binaryheaderdifC();
  bool lineprefixdifC();
}

#endif
