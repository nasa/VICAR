/*************************************************************

 bridge to C callable version of GETCAMCON

*************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzgetcamcon)(FileUnit)
  int *FileUnit;
{
      int Camera,count,def,ind, FDS;
      float focal,oal,oas,scale;
      char Project[5];

      zgetproj (*FileUnit, Project, &Camera, &FDS, &ind);
      zvmessage(Project,"");
      zgetcamcon(Project,Camera,&focal,&oal,&oas,&scale,&ind);
      if(ind != 0)  zprnt(4,1,&ind,"fatal indicator=.");
      zprnt(7,1,&focal,"focal length=       .");
      zprnt(7,1,&oal,  "optical axis line=  .");
      zprnt(7,1,&oas,  "optical axis sample=.");
      zprnt(7,1,&scale,"image scale=        .");
}
