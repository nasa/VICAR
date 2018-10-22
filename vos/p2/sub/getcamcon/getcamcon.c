#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include "zvproto.h"
/**************************************************************************

 GETCAMCON returns camera constants given project and camera from getproj.

 arguments:
   project  = spacecraft id. input.                        character *
              valid=MAR-9 MAR10 VIKOR VGR-1 VGR-2 GLL
                    WFPC1(old optics)  WFPC2(first optics upgrade)
   camera   = camera serial number. input                  int
   focal    = focal length in mm. returned                 float
   oal      = optical axis line object space. returned.    float
   oas      = optical axis sample object space. returned.  float
   scale    = object space scale. pixels/mm. returned.     float
   ind      = 0=normal   1=error                           int

**************************************************************************/
void zgetcamcon();
/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/
void FTN_NAME2(getcamcon, GETCAMCON) (char *project, int *camera, float *focal,
	float *oal, float *oas, float *scale, int *ind, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char proj[6];
   int length;
/*  ==================================================================  */

   length = 5;

/* 7 args for GETCAMCON, project is 1st arg and 1st string   */


   zsfor2c(proj, length, project, &project, 7, 1 , 1, ind);

   zgetcamcon(proj,*camera,focal,oal,oas,scale,ind);

}
/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/
      void zgetcamcon(project,camera,focal,oal,oas,scale,ind)
 
      char *project;
      int camera,*ind;
      float *focal, *oal, *oas, *scale;
{
      int icam;
      static int viksn[] = {0, 0, 0, 2, 0, 4, 1, 3};

/*-----------------------------------
 cassi
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc8[2][4] =
      {  {2000.00, 512.0, 512.0, 83.333333},
         {200.736, 512.0, 512.0, 83.333333}};

/*-------------------------------------
 GLL
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc5[1][4] = { 1501.039, 400.0, 400.0, 65.6167979 };

/*-----------------------------------
 vgr-1 and vgr-2
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc4[4][4] =
      {  {1500.19, 500.0, 500.0, 84.821428},    
         {200.465, 500.0, 500.0, 84.821428},    
         {1503.49, 500.0, 500.0, 84.821428},    
         {200.770, 500.0, 500.0, 84.821428} }; 

/*-------------------------------------
 vikor
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc3[4][4] =
     { {474.398, 575.0, 625.0, 85.0},     
       {474.448, 575.0, 625.0, 85.0},     
       {474.610, 575.0, 625.0, 85.0},    
       {474.101, 575.0, 625.0, 85. }};  

/*-------------------------------------
 mar10
=====================================
  1=focal length [mm.]
  2=optical axisline [o.s. pixels]
  3=optical axis sample [o.s. pixels]
  4=scale [pixels/mm.]
-------------------------------------*/

      static float foc2[4][4] = 
     { {1495.66, 400.0, 475.0, 74.78},
       {1503.69, 400.0, 475.0, 74.78},
       {62.02, 400.0, 475.0, 74.78},  
       {62.68, 400.0, 475.0, 74.78} };

/*-------------------------------------
 mar-9
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc1[2][4] = 
     { {52.267, 400.0, 475.0, 75.0},     
       {500.636, 400.0, 475.0, 75.0} };  

/*-------------------------------------
 wfpc1
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc6[2][4] = 
     { {67991., 400.0, 400.0, 66.66667},     
       {31168., 400.0, 400.0, 66.66667} };  

/*-------------------------------------
 wfpc2
=====================================
  1=focal length (mm.)
  2=optical axisline (o.s. pixels)
  3=optical axis sample (o.s. pixels)
  4=scale (pixels/mm.)
-------------------------------------*/

      static float foc7[2][4] = 
     { {67991., 400.0, 400.0, 66.66667},     
       {31168., 400.0, 400.0, 66.66667} };  

      *ind = 0;
      if (strncmp(project,"CASSI",5) == 0)
      {
          if (camera == 1 || camera == 2)         /* FULL summation mode */
          {
            *focal = foc8[camera-1][0];
            *oal = foc8[camera-1][1];
            *oas = foc8[camera-1][2];
            *scale = foc8[camera-1][3];
          }
          else if (camera == 21 || camera == 22)  /* SUM2 summation mode */
          {
            *focal = foc8[camera-21][0];
            *oal = foc8[camera-21][1]/2;
            *oas = foc8[camera-21][2]/2;
            *scale = foc8[camera-21][3]/2;
          }
          else if (camera == 41 || camera == 42)  /* SUM4 summation mode */
          {
            *focal = foc8[camera-41][0];
            *oal = foc8[camera-41][1]/4;
            *oas = foc8[camera-41][2]/4;
            *scale = foc8[camera-41][3]/4;
          }
          else
          {
             zvmessage("GETCAMCON: CASSI ILLEGAL CAMERA #","");
             *ind = 1;
          }

      }
      else if (strncmp(project,"GLL", 3) == 0)
      {
         *focal = foc5[0][0];

         if (camera==2)   /* SSI Summation Mode */
         {
           *oal = foc5[0][1]/2;
           *oas = foc5[0][2]/2;
           *scale = foc5[0][3]/2;
         }
         else             /* SSI Full Frame */
         {
           *oal = foc5[0][1];
           *oas = foc5[0][2];
           *scale = foc5[0][3];
         }
      }
      else if (strncmp(project,"VGR-1",5) == 0 || 
	       strncmp(project,"VGR-2",5) == 0)
      {

              if (camera >=  4 && camera <=  7) 
              {
		 icam = 7 - camera;
                 *focal = foc4[icam][0]; 
                 *oal = foc4[icam][1];
                 *oas = foc4[icam][2];
                 *scale = foc4[icam][3];
              }
              else 
              {
                 zvmessage("GETCAMCON: VGR ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }

      else if (strncmp(project,"VIKOR",5) == 0)
      {
              if(camera >=  4 && camera <=  8) 
              {
		 icam = camera - 1;
                 *focal = foc3[viksn[icam]-1][0];
                 *oal = foc3[viksn[icam]-1][1];
                 *oas = foc3[viksn[icam]-1][2];
                 *scale = foc3[viksn[icam]-1][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: VIKOR ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }

      else if (strncmp(project,"WFPC1",5) == 0)
      {
              if(camera >=  1 && camera <=  2) 
              {
		 icam = camera-1;
                 *focal = foc6[icam][0];
                 *oal = foc6[icam][1];
                 *oas = foc6[icam][2];
                 *scale = foc6[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: WFPC1 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }
      else if (strncmp(project,"WFPC2",5) == 0)
      {
              if(camera >=  1 && camera <=  2) 
              {
		 icam = camera-1;
                 *focal = foc7[icam][0];
                 *oal = foc7[icam][1];
                 *oas = foc7[icam][2];
                 *scale = foc7[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: WFPC2 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }

      else if (strncmp(project,"MAR10",5) == 0)
	{
              if(camera >=  1 && camera <=  4) 
              {
		 icam = camera - 1;
                 *focal = foc2[icam][0];
                 *oal = foc2[icam][1];
                 *oas = foc2[icam][2];
                 *scale = foc2[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: MAR10 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
      }
      else if (strncmp(project,"MAR-9",5) == 0)
      {
              if (camera >=  1 && camera <=  2) 
              {
		 icam = camera - 1;
                 *focal = foc1[icam][0];
                 *oal = foc1[icam][1];
                 *oas = foc1[icam][2];
                 *scale = foc1[icam][3]; 
              }
              else 
              {
                 zvmessage("GETCAMCON: MAR-9 ILLEGAL CAMERA #","");
		 *ind = 1;
              }
       } 
       else
       {
         zvmessage("GETCAMCON: UNRECOGNIZABLE PROJECT ID",""); 
	 *ind = 1;
       }
}
