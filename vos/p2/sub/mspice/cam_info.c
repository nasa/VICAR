/*--------------------------------------------------------------------------
 * Apr.     1998  ...S.Le......   Initial release.
 *
 * Apr. 29, 1998  ...T.Huang...   Ported onto Alpha/VMS.
 *                                Corrected SSI constants.
 *
 * Jun. 25, 1999  ...T.Huang...   mapped VGR-1 and VGR-2 camera serial numbers 
 *                                to ISSNA and ISSWA, which are the same for 
 *                                GLL in meeting the NAIF requirement.
 *                                Introduced trueCamSn for backward 
 *                                compatiable with legacy MIPS software. 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include "ms_defines.h"

/*----------------------------------------------------------------------*
 *      int cam_info(   sc_id, cam_name, cam_id, cam_sn,                *
 *                      rad_per_pix, focal_mm, opaxis_line,           	*
 *                      opaxis_samp, scale      )                       *
 *                                                                      *
 *      int     sc_id           (i): project code consistence with NAIF *
 *      char    *cam_name       (i): name of user camera                *
 *      int     *cam_id         (o): integer id of requested camera     *
 *      int     *cam_sn         (o): camera serial number               *
 *      double  *rad_per_pix    (o): radians per pixel of camera         *
 *      float   *focal_mm       (o):                                    *
 *      float   *opaxis_line    (o):                                    *
 *      float   *opaxis_samp    (o):                                    *
 *      float   *scale          (o):                                    *
 *----------------------------------------------------------------------*/
int zcam_info (int sc_id, char *cam_name, int *cam_id,
        int *cam_sn, double *rad_per_pix, float *focal_mm,
        float *opaxis_line, float *opaxis_samp, float *scale)
{
 int ind;
 int trueCamSn;  /* added to store the true camera serial number
                    (serial number used by legacy MIPS software) for 
                    retrieving camera info.  This was introduced to map
                    the Voyager camera number */
 char project[32];

 if (sc_id == GLL_SC_ID) {
    int icam;

    if (!strncmp(cam_name, "SSI", 3)) {
       icam = PLATFORM;
       if (!strncmp(cam_name, "SSI1", 4)) {
          *cam_sn     	 = ISSNA;
          trueCamSn      = ISSNA;
          *rad_per_pix   = 0.00000926;
          }
       else {
          *cam_sn     	 = ISSWA;
          trueCamSn      = ISSWA;
          *rad_per_pix   = 0.00001924;
          }

       *cam_id = -(abs(GLL_SC_ID) * 1000 + icam);
       strcpy (project, "GLL");
       }
    else {
       printf ("cam_info:ERROR::Unknown GLL camera name\n");
       printf ("User camera name: %s\n", cam_name);
       return -1;
       }
    }

 else if (sc_id == VGR_1_SC_ID) {
    if (!strncmp(cam_name, "ISSW", 4)) {
       *cam_sn      = ISSWA;
       trueCamSn    = 6;
       *rad_per_pix = 0.0000701;
       }
    else if (!strncmp(cam_name, "ISSN", 4)) {
       *cam_sn      = ISSNA;
       trueCamSn    = 7;
       *rad_per_pix = 0.00000926;
       }
    else {
       printf ("cam_info:Unknown VGR_1 camera name\n");
       printf ("User camera name: %s\n", cam_name);
       return -1;
       }

    strcpy (project, "VGR-1");
    *cam_id = -(abs(VGR_1_SC_ID) * 1000 + *cam_sn);
    }

 else if (sc_id == VGR_2_SC_ID) {
    if (!strncmp(cam_name, "ISSW", 4)) {
       *cam_sn      = ISSWA;
       trueCamSn    = 4;
       *rad_per_pix = 0.0000701;
       }
    else if (!strncmp(cam_name, "ISSN", 4)) {
       *cam_sn      = ISSNA;
       trueCamSn    = 5;
       *rad_per_pix = 0.00000926;
       }
    else {
       printf ("cam_info:Unknown VGR_2 camera name\n");
       printf ("User camera name: %s\n", cam_name);
       return -1;
       }

    strcpy (project, "VGR-2");
    *cam_id = -(abs(VGR_2_SC_ID) * 1000 + *cam_sn);
    }

 else if (sc_id == CAS_SC_ID) {
    strcpy (project, "CASSINI");   

   if (!strncmp(cam_name, CASISSWA_STR, 4)) {
     *cam_id       = -(abs(CAS_SC_ID) * 1000);
     *cam_sn      = CASISSWA;
     trueCamSn    = CASISSWA;
     *rad_per_pix = 0.00006;
   }
   else if (!strncmp(cam_name, CASISSWA_SUM22_STR, 4)) {
     *cam_id       = -(abs(CAS_SC_ID) * 1000);
     *cam_sn      = CASISSWA;
     trueCamSn    = CASISSWA_SUM22;
     *rad_per_pix = 0.00012;
   }
   else if (!strncmp(cam_name, CASISSWA_SUM44_STR, 4)) {
     *cam_id       = -(abs(CAS_SC_ID) * 1000);
     *cam_sn      = CASISSWA;
     trueCamSn    = CASISSWA_SUM44;
     *rad_per_pix = 0.00024;
   }
   else if (!strncmp(cam_name, CASISSNA_STR, 4)) {
     *cam_id       = -(abs(CAS_SC_ID) * 1000);
     *cam_sn      = CASISSNA;
     trueCamSn    = CASISSNA;
     *rad_per_pix = 0.000006;
   }
   else if (!strncmp(cam_name, CASISSNA_SUM22_STR, 4)) {
     *cam_id       = -(abs(CAS_SC_ID) * 1000);
     *cam_sn      = CASISSNA;
     trueCamSn    = CASISSNA_SUM22;
     *rad_per_pix = 0.000012;
   }
   else if (!strncmp(cam_name, CASISSNA_SUM44_STR, 4)) {
     *cam_id       = -(abs(CAS_SC_ID) * 1000);
     *cam_sn      = CASISSNA;
     trueCamSn    = CASISSNA_SUM44;
     *rad_per_pix = 0.000024;
   }
   else {
     printf ("cam_info:Unknown CAS camera name\n");
     printf ("User camera name: %s\n", cam_name);
     return -1;
   }
 }

 else if (sc_id == VIKOR_1_SC_ID) {
    printf ("cam_info:ERROR:VIKOR_1 is not supported yet\n");
    return -1;
    }
 else if (sc_id == VIKOR_2_SC_ID) {
    printf ("cam_info:ERROR:VIKOR_2 is not supported yet\n");
    return -1;
    }
 else if (sc_id == SIM_SC_ID) {
    printf ("cam_info:ERROR:SIM is not supported yet\n");
    return -1;
    }
 else {
    printf ("cam_info::ERROR:unknown sc_id: %d\n", sc_id);
    return -1;
    }

 ind = 0;

 zgetcamcon (project, trueCamSn, focal_mm,
	opaxis_line, opaxis_samp, scale, &ind);
 if (ind) {
    printf ("cam_info:zgetcamcon() failed\n");
    printf ("ERROR getting camera info\n");
    return -1;
    }

 return 0;
}
