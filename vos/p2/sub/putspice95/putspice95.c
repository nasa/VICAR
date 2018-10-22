#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdio.h>
#include <string.h>
#include "spc.h"
#include "spiceinc.h"
#include "ms_defines.h"

/*=================================================*
 * Fortran-Callable Version			   *
 *=================================================*/
void FTN_NAME2(putspice95, PUTSPICE95) (project, buf, mode, ind)
 int		*project;
 void	 	*buf;
 int		*mode;
 int		*ind;
{
 buf_union_typ	cbuf;
 memcpy((char *) &cbuf, (char *) buf,
		sizeof(buf_union_typ));
 *ind = zputspice95(*project, &cbuf, *mode);
}

/*=================================================*
 * putspice95()					   *
 *=================================================*/
int zputspice95(project, buf, mode)
 int		project;
 buf_union_typ	*buf;
 int		mode;
{
 int status;
 msCkStruct ckdata;

 int		i, tint, inst_val;
 float		tfloat;
 double		tdouble;
 char		inst_name[32];
 usr_kdb_typ 	usrinfo;

 memset ((void*) inst_name, '\0', 32);
 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &usrinfo, '\0', sizeof(usrinfo));

/** txh::removed because of invalid reference.
 memcpy ((char*) inst_name, (char*) &buf->intbuf[4], 4);
**/
/** txh::corrected reference **/
 memcpy ((char*) inst_name, (char*) &buf->intbuf[1], 4);

 ckdata.sc_id = buf->intbuf[0];

 if (project == CAS_SC_ID)
   {
     if ((!strncmp(inst_name, CASISSNA_STR, 4)) ||
	 (!strncmp(inst_name, CASISSNA_SUM22_STR, 4)) ||
	 (!strncmp(inst_name, CASISSNA_SUM44_STR, 4)))      
       {
	 ckdata.instrument = CASISSNA_NAIF_ID;
       }
     else if ((!strncmp(inst_name, CASISSWA_STR, 4)) ||
	 (!strncmp(inst_name, CASISSWA_SUM22_STR, 4)) ||
	 (!strncmp(inst_name, CASISSWA_SUM44_STR, 4)))      
       {
	 ckdata.instrument = CASISSWA_NAIF_ID;
       }
     else
       {
	 char msg[64];
	 memset(msg, '\0', sizeof(msg));
	 
	 sprintf(msg,"Unsupported instrument name '%s'", inst_name); 
	 zvmessage(msg, " ");
	 zvmessage ("\nPutspice95 returns with no data", " ");
	 return FAILURE;   
       }
   }
 else if ((project == GLL_SC_ID) || 
	  (project == VGR_1_SC_ID) || 
	  (project == VGR_2_SC_ID))
   {
     if (!strncmp (inst_name, "ISSN", 4)) 
       {
	 inst_val = ISSNA;
       }
     else if (!strncmp (inst_name, "ISSW", 4))
       {
	 inst_val = ISSWA;
       }
     else if (!strncmp (inst_name, "SSI", 3))
       {
	 inst_val = PLATFORM;
       }
     else
       {
	 char msg[64];
	 memset(msg, '\0', sizeof(msg));
	 
	 sprintf(msg,"Unsupported instrument name '%s'", inst_name); 
	 zvmessage(msg, " ");
	 zvmessage ("\nPutspice95 returns with no data", " ");
	 return FAILURE;   
       }

     /** txh::removed because of invalid instrument code.
	 ckdata.instrument = buf->intbuf[2];
     **/
     /** txh::corrected instrument code calculation based from ck.req **/
     ckdata.instrument = -(abs (buf->intbuf[0]) * 1000 + inst_val);
   }
 else
   {	
     char msg[64];
     memset(msg, '\0', sizeof(msg));
     
     sprintf(msg,"Unsupported project numer '%d'.", project); 
     zvmessage(msg, " ");
     zvmessage ("\nPutspice95 returns with no data", " ");
     return FAILURE;   
   }

 ckdata.system = buf->intbuf[9];
 for (i = 0; i < 6; i++) ckdata.scet[i] = buf->intbuf[2+i];
 for (i = 0; i < 3; i++) ckdata.av[i] = buf->doublebuf[37+i];
 for (i = 0; i < 9; i++) ckdata.c_matrix[i] = buf->doublebuf[40+i];
 ckdata.avFlag = 1;
 GetUsrInfo (&usrinfo, buf);
 memcpy ((void*) ckdata.seg_id, (void*)&buf->intbuf[188], 4);
 strncat (ckdata.seg_id, usrinfo.purpose, LEN_PURPOSE);
 strncat(ckdata.seg_id, usrinfo.prog_name, LEN_PROG_NAME);
 strncat(ckdata.seg_id, usrinfo.sp_ref, LEN_SP_REF);
 strncat(ckdata.seg_id, usrinfo.req_no, LEN_REQ_NO);
 if (strcmp(usrinfo.date_time, "000000000000") == 0)
    GetDateTime(usrinfo.date_time);
 strncat(ckdata.seg_id, usrinfo.date_time, LEN_DATE_TIME);
 strncat(ckdata.seg_id, usrinfo.usr_grp_id, LEN_USR_ID);
 memcpy((char*)ckdata.ck_id, (char*) &buf->intbuf[171], 4);
 memcpy((char*)ckdata.ck_source, (char*) &buf->intbuf[10], 4);

 if (mode == MODE_REMOTE) {
    switch (ckdata.sc_id) {
        case GLL_SC_ID:
            status = !msclt_gllputspice(&ckdata);
            break;
        case VGR_1_SC_ID:
            status = !msclt_vgr1putspice(&ckdata);
            break;
        case VGR_2_SC_ID:
            status = !msclt_vgr2putspice(&ckdata);
            break;
        case VIKOR_1_SC_ID:
            status = !msclt_vo1putspice(&ckdata);
            break;
        case VIKOR_2_SC_ID:
            status = !msclt_vo2putspice(&ckdata);
            break;
        case CAS_SC_ID:
            status = !msclt_casputspice(&ckdata);
            break;
        case SIM_SC_ID:
            status = !msclt_simputspice(&ckdata);
            break;
        default:
            zvmessage ("\nERROR:unknown remote mode SC_ID", " ");
            zvmessage ("\nPutspice95 returns with no data", " ");
            status = FAILURE;
            break;
        }
    }
 else if (mode == MODE_LOCAL) {
    switch (ckdata.sc_id) {
        case GLL_SC_ID:
            status = !mslcl_gllputspice(&ckdata);
            break;
        case VGR_1_SC_ID:
            status = !mslcl_vgr1putspice(&ckdata);
            break;
        case VGR_2_SC_ID:
            status = !mslcl_vgr2putspice(&ckdata);
            break;
        case VIKOR_1_SC_ID:
            status = !mslcl_vo1putspice(&ckdata);
            break;
        case VIKOR_2_SC_ID:
            status = !mslcl_vo2putspice(&ckdata);
            break;
        case CAS_SC_ID:
            status = !mslcl_casputspice(&ckdata);
            break;
        case SIM_SC_ID:
            status = !mslcl_simputspice(&ckdata);
            break;
        default:
            zvmessage ("\nERROR:unknown local mode SC_ID", " ");
            zvmessage ("\nPutspice95 returns with no data", " ");
            status = FAILURE;
            break;
        }
    }
 else {
    zvmessage("Unsupported MODE", " ");
    zvmessage ("\nPutspice95 returns with no data", " ");
    status = FAILURE;
    }

 /** txh::removed to return the correct status (i.e. 1=SUCCESS, 0=FAILUE)
 return (!status);
 **/

 return status;
}

