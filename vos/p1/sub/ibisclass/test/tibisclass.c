#include "vicmain_c"
#include "ibisfile.h"
#include "ftnbridge.h"
#include <ctype.h>
 
main44()
{
    int count,def;
    char command[20];
    
    zvparm("_SUBCMD",command,&count,&def,0,0);
    switch (tolower(command[0]))
    {
        case 'n' : new_class(); break;
        case 'g' : get_class(); break;
        case 'f' : switch(tolower(command[1]))
	{
		/* call FORTRAN from C */
		case 'n' : FTN_NAME(new_class_fortran)(); break;
		case 'g' : FTN_NAME(get_class_fortran)(); break;
	}
    }
}

 
new_class()
{
      int unit, i, ibis,status;
      int lutcols[3];
      int pixpt[3];
      int valpt[1];
      int frame[1];
      char *user_instance="myinst";

      for (i=0;i<3;i++) lutcols[i]=i+1;
      for (i=0;i<3;i++) pixpt[i]=i+4;
      valpt[0]=7; frame[0]=8;


      /* create IBIS File */
      status = zvunit(&unit,"out",1,0);
      status = IBISFileOpen(unit,&ibis,IMODE_WRITE,
          10,256,0,IORG_COLUMN);
      if (status!=1) IBISSignalU(unit,status,1); /* abort */

      /* 
       * Set up groups. If this were a pre-existing file
       *  we would have to check to see if the groups
       *  already existed, in which case we would use
       *  IBISGroupModify( "APPEND") to add the columns.
       */

      /*
       *  The stretch is by default only in the class C_VALUE.
       *   We need to override this so that it will be RGB.
       *   No instance needed, so we pass in 0.
       */
       
      status = ICLNewRGB(ibis,lutcols[0],lutcols[1],lutcols[2],0);
      if (status < 0) IBISSignal(ibis,status,0);
      status = ICLNewLOOKUP_TABLE(ibis,lutcols,3,0,0,"PSEUDOCOLOR","MyLut");
      if (status < 0) IBISSignal(ibis,status,0);


      /*
       *  Declare value and pixel subtypes for POINT
       */
      status = ICLNewVALUE(ibis,valpt,1,"DN");
      if (status < 0) IBISSignal(ibis,status,0);
      status = ICLNewPOS_IMAGE(ibis,pixpt[0],pixpt[1],pixpt[2],0);
      if (status < 0) IBISSignal(ibis,status,0);

      /* create the point class */
      status = ICLNewPOINT(ibis,pixpt,3,valpt,1,"PIXEL","MyPOINT");
      if (status < 0) IBISSignal(ibis,status,0);
      status = ICLNewPOINT(ibis,frame,1,0,0,"FRAME","MyPOINT");
      if (status < 0) IBISSignal(ibis,status,0);

      status = IBISFileClose(ibis,0);

}


get_class()
{
      int unit, ibis,status,count;
      int lutcols[3];
      int pixpt[3];
      int valpt[1];
      int frame[1];
      char message[80];

      /* create IBIS File */
      status = zvunit(&unit,"inp",1,0);
      status = IBISFileOpen(unit,&ibis,IMODE_READ,0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1); /* abort */

      /* 
       * Get groups. We go exactly backwards from the
       *  order in the "new" routine.
       */

      /*
       *  The stretch was by default only in the class C_VALUE.
       *   We needed to override this so that it would be RGB.
       *   No index needed, so we pass in 0.
       */
       
      ICLGetLOOKUP_TABLE(ibis,"$MyLut",0,"PSEUDOCOLOR","MyLut");
      ICLGetRGB(ibis,"$MyRED","$MyGRN","$MyBLU","$MyLut");

      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyRED",lutcols,1,3);
      sprintf(message,"%d RED LUT Cols: %d...",count,lutcols[0]);
      zvmessage(message," ");
      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyGRN",lutcols,1,3);
      sprintf(message,"%d GRN LUT Cols: %d...",count,lutcols[0]);
      zvmessage(message," ");
      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyBLU",lutcols,1,3);
      sprintf(message,"%d BLUE LUT Cols: %d...",count,lutcols[0]);
      zvmessage(message," ");

      /* Get the point class members */
      ICLGetPOINT(ibis,"$MyFrame",0,"FRAME","MyPOINT");
      ICLGetPOINT(ibis,"$MyPOS","$MyVal","PIXEL","MyPOINT");

      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyFrame",frame,1,1);
      sprintf(message,"%d FRAME Cols: %d...",count,frame[0]);
      zvmessage(message," ");
      
      /*
       *  Get value and pixel subtypes from POINT
       */
      ICLGetVALUE(ibis,"$MyDN","$MyVal & DN");
      count = IBISColumnFind(ibis,ITYPE_LOCAL,"$MyDN",valpt,1,1);
      sprintf(message,"%d DN Cols: %d...",count,valpt[0]);
      zvmessage(message," ");

      ICLGetPOS_IMAGE(ibis,"$MyLINE","$MySAMP","$MyBAND","$MyPOS");
      count = IBISColumnFind(ibis,ITYPE_ANY,
      		"$MyLINE | $MySAMP | $MyBAND",pixpt,1,3);
      sprintf(message,"%d POSITION Cols:(L,S,B)= %d,%d,%d",count,
      		pixpt[0],pixpt[1],pixpt[2]);
      zvmessage(message," ");

      status = IBISFileClose(ibis,0);

}
