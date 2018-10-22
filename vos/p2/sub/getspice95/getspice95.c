/*========================================================*
 * GETSPICE95():					  *
 *      1. Check user info. Make sure that data in user   *
 *              info structure is correct. If bad data,   *
 *              fill in default values.                   *
 *      2. Fill the buffer, which is to be sent to the    *
 *              server (remote) or spice95 (local) to     *
 *              retrieve data. This function get the image*
 *              name, and retrieve the image label        *
 *              information and place them in the buffer. *
 *              - Place project in buf.intbuf[0]          *
 *              - Place system=J2000 in buf.inbuf[9]      *
 *              - Get image file's unit                   *
 *              - Get label content                       *
 *              - Put SCET data to buffer (2-7)           *
 *      3. Copy source string to buf.intbuf[10]           *
 *      4. Call local or remote spice read according to   *
 *              to mode parameter.                        *
 *	*** User must have read & write access to SPICEKER*
 *		when doing Local spice processing. 	  *
 *		SPICEKER is the directory where private	  *
 *		SPICE files are stored			  *
 *========================================================*
 * Note added (July 2011) by lwk:			  *
 * Users should be aware that this routine clears the	  *
 * SPICE kernel pool before returning, so subsequent calls*
 * to SPICE routines will fail unless INIT_SPICE() or an  *
 * equivalent routine is called.			  *
 *========================================================*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdio.h>
#include <string.h>
#include "spc.h"
#include "spiceinc.h"
#include "ms_defines.h"
#include "zvproto.h"
#include <ctype.h>

/** txh::added subroutine to add blank space to entries that are shorter 
         than the required length */
void add_space (char *, int);

/*========================================================*
 * GETSPICE95(): Fortran Callable Version		  *
 *========================================================*/
void FTN_NAME2(getspice95, GETSPICE95) (int *mode, int *sc_id, char *camera,
		int *scet, char *target_name, int *system, char *source,
		char *usr_info, void *buf, int *ind, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 int            maxlen,
                nelement;
 char           ccamera[15],
		ctarget_name[30],
		csource[15],
                *temp;
 prov_info_typ  cusr_info;
 static buf_union_typ buffer;
 
 maxlen = 5;
 zsfor2c(ccamera, maxlen, camera, &mode, 10, 3, 1, ind);

 maxlen = 15;
 zsfor2c(ctarget_name, maxlen, target_name, &mode, 10, 5, 2, ind);

 maxlen = 5;
 zsfor2c(csource, maxlen, source, &mode, 10, 7, 3, ind);

 maxlen = 10;
 nelement = 10; 
 zsfor2c_array(&temp, &maxlen, nelement, usr_info, &mode, 10, 8, 4, ind);
 memset((char *) &cusr_info, '\0', sizeof(prov_info_typ));

 memcpy(cusr_info.inst, temp+(0*maxlen), 4);
 memcpy(cusr_info.purpose, temp+(1*maxlen), 4);
 memcpy(cusr_info.prog_name, temp+(2*maxlen), 6);
 memcpy(cusr_info.sp_ref, temp+(3*maxlen), 4);
 memcpy(cusr_info.req_no, temp+(4*maxlen), 4);
 memcpy(cusr_info.year, temp+(5*maxlen), 4);
 memcpy(cusr_info.month_day, temp+(6*maxlen), 4);
 memcpy(cusr_info.hour_min, temp+(7*maxlen), 4);
 memcpy(cusr_info.file_id, temp+(8*maxlen), 4);
 memcpy(cusr_info.usr_grp_id, temp+(9*maxlen), 6);
 free (temp);

 memset((char*) &buffer, '\0', sizeof(buf_union_typ));

 *ind = zgetspice95(*mode, *sc_id, ccamera, scet,
		ctarget_name, *system, csource,
		&cusr_info, (buf_union_typ*) &buffer);

 memcpy((char*) buf, (char*) &buffer, sizeof(buf_union_typ));
}
/*========================================================*
 * ZGETSPICE95(): C-Callable Version			  *
 *========================================================*/

int zgetspice95(mode, sc_id, camera, scet,
	target_name, system, src, usr_info, buf)
 int		mode;			/* where to get data  	    */
 int		sc_id;			/* VGR_1 VGR_2 GLL	    */
 char		*camera;		/* ISSN ISSW SSI	    */
 int		*scet;			/* year day hr min sec msec */
 char		*target_name;		/* name of target to search */
 int		system;			/* coord syst: J2000 B1950  */
 char		*src;			/* source of file to read   */
 prov_info_typ	*usr_info;		/* usr's prov. info.	    */
 buf_union_typ	*buf;			/* out: returned data (see  */
{					/*	spice89.hlp)	    */
 int	i, status;
 char	*cptr;

 msUserRequestStruct    req;
 msCkStruct             ckdata;
 msSpkStruct            spkdata;

 /* check user info, if data is not good, fill in */
 /* default values				  */ 
 ChckUsrInfo(usr_info);
 buf->intbuf[0] = sc_id;

 memcpy((char*)&buf->intbuf[1],		/* cpy camera name(4char) */
	(char*)camera, 4);		/* ISSN ISSW SSI	  */
 buf->intbuf[2] = scet[0];
 buf->intbuf[3] = scet[1];		/* year day hr min 	  */
 buf->intbuf[4] = scet[2];		/* sec mil-sec		  */
 buf->intbuf[5] = scet[3];
 buf->intbuf[6] = scet[4];
 buf->intbuf[7] = scet[5];

 /* txh::determine target_id based on the target_name */
 if (sc_id == GLL_SC_ID || 
     sc_id == VGR_1_SC_ID || 
     sc_id == VGR_2_SC_ID ||
     sc_id == CAS_SC_ID)
 {
    status = zpbid (target_name, &buf->intbuf[8]);
    if (status != 1)
       zvmessage ("ZPBID::Unknown Target", " ");
 }

 buf->intbuf[9] = system;		/* int: J2000 or B1950	  */

 i = 0;					/* automatically convert  */
 while(src[i] || (i < 4)) {		/* the source string to	  */
    if (islower(src[i]))		/* upper case		  */
	src[i] = toupper(src[i]);
    i++;
    }
    
 memcpy((char*) &buf->intbuf[10],	/* copy source string	  */
		(char*) src, 4);	/* to buffer		  */

 cptr = (char*) target_name;

 for (status = 0; status < 12; status++) {		/* make   */
    if (islower(target_name[status]))			/* tname  */
	target_name[status] += (char) ('a' - 'A');	/* to upp */
    }							/* er case*/

 memcpy((char*) &buf->intbuf[178],		/* cpy target     */
	(char*)target_name, 12);		/* name to buf	  */

 memcpy((char*)&buf->intbuf[188],
	(char*)usr_info->inst, LEN_SPICE_ID);
 memcpy((char*)&buf->intbuf[172],
	(char*)usr_info->purpose, LEN_PURPOSE);
 memcpy((char*)&buf->intbuf[173],
	(char*)usr_info->prog_name, LEN_PROG_NAME);
 memcpy((char*)&buf->intbuf[13],
	(char*)usr_info->sp_ref, LEN_SP_REF);
 memcpy((char*)&buf->intbuf[175],
	(char*)usr_info->req_no, LEN_REQ_NO);
 memcpy((char*)&buf->intbuf[168],
	(char*)usr_info->year, LEN_SPICE_ID);
 memcpy((char*)&buf->intbuf[169],
	(char*)usr_info->month_day, LEN_SPICE_ID);
 memcpy((char*)&buf->intbuf[170],
	(char*)usr_info->hour_min, LEN_SPICE_ID);
 memcpy((char*)&buf->intbuf[171],
	(char*)usr_info->file_id, LEN_SPICE_ID);
 memcpy((char*)&buf->intbuf[176],
	(char*)usr_info->usr_grp_id, LEN_PROG_NAME);

 memset ((void*) &req, '\0', sizeof(req));
 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 req.sc_id	= sc_id;
 req.system	= system;
 req.scet[0]    = buf->intbuf[2];
 req.scet[1]    = buf->intbuf[3];
 req.scet[2]    = buf->intbuf[4];
 req.scet[3]    = buf->intbuf[5];
 req.scet[4]    = buf->intbuf[6];
 req.scet[5]    = buf->intbuf[7];
 strncpy (req.instrument_name, camera, LEN_SPICE_ID);
 strncpy (req.target_name, target_name, 12);
 strncpy (req.ck_source, src, strlen(src));
 add_space(usr_info->inst, LEN_SPICE_ID);
 i = 0; strncpy (&req.provInfo.seg_id[i], usr_info->inst, LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 add_space(usr_info->purpose, LEN_PURPOSE);
 strncpy (&req.provInfo.seg_id[i], usr_info->purpose, LEN_PURPOSE);
 i += LEN_PURPOSE;
 add_space(usr_info->prog_name, LEN_PROG_NAME);
 strncpy (&req.provInfo.seg_id[i], usr_info->prog_name, LEN_PROG_NAME);
 i += LEN_PROG_NAME;
 add_space(usr_info->sp_ref, LEN_SP_REF);
 strncpy (&req.provInfo.seg_id[i], usr_info->sp_ref, LEN_SP_REF);
 i += LEN_SP_REF;
 add_space(usr_info->req_no, LEN_REQ_NO);
 strncpy (&req.provInfo.seg_id[i], usr_info->req_no, LEN_REQ_NO);
 i += LEN_REQ_NO;
 strncpy (&req.provInfo.seg_id[i], usr_info->year, LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 strncpy (&req.provInfo.seg_id[i], usr_info->month_day, LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 strncpy (&req.provInfo.seg_id[i], usr_info->hour_min, LEN_SPICE_ID);
 i += LEN_SPICE_ID;

 /* txh::removed because this field is not part of SEG_ID  
 strncpy (&req.provInfo.seg_id[i], usr_info->file_id, LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 */

 strncpy (&req.provInfo.seg_id[i], usr_info->usr_grp_id, LEN_PROG_NAME);

 if (mode == MODE_REMOTE) {
    switch (sc_id) {
	case GLL_SC_ID:
            status = !msclt_gllgetspice(&req,&ckdata,&spkdata);
            break;
        case VGR_1_SC_ID:
            status = !msclt_vgr1getspice(&req,&ckdata,&spkdata);
            break;
        case VGR_2_SC_ID:
            status = !msclt_vgr2getspice(&req,&ckdata,&spkdata);
            break;
        case VIKOR_1_SC_ID:
            status = !msclt_vo1getspice(&req,&ckdata,&spkdata);
            break;
        case VIKOR_2_SC_ID:
            status = !msclt_vo2getspice(&req,&ckdata,&spkdata);
            break;
        case CAS_SC_ID:
            status = !msclt_casgetspice(&req,&ckdata,&spkdata);
            break;
        case SIM_SC_ID:
            status = !msclt_simgetspice(&req,&ckdata,&spkdata);
            break;
        default:
            zvmessage ("\nERROR:unknown remote mode SC_ID", " ");
            zvmessage ("\nGetspice95 returns with no data", " ");
            status = FAILURE;
            break;
	}
    }
 else if (mode == MODE_LOCAL) {
    switch (sc_id) {
	case GLL_SC_ID:
	    status = !mslcl_gllgetspice(&req,&ckdata,&spkdata);	
	    break;
	case VGR_1_SC_ID:
	    status = !mslcl_vgr1getspice(&req,&ckdata,&spkdata);
	    break;
	case VGR_2_SC_ID:
	    status = !mslcl_vgr2getspice(&req,&ckdata,&spkdata);
	    break;
	case VIKOR_1_SC_ID:
	    status = !mslcl_vo1getspice(&req,&ckdata,&spkdata);
	    break;
	case VIKOR_2_SC_ID:
	    status = !mslcl_vo2getspice(&req,&ckdata,&spkdata);
	    break;
	case CAS_SC_ID:
	    status = !mslcl_casgetspice(&req,&ckdata,&spkdata);
	    break;
	case SIM_SC_ID:
	    status = !mslcl_simgetspice(&req,&ckdata,&spkdata);
	    break;
	default:
	    zvmessage ("\nERROR:unknown local mode SC_ID", " ");
	    zvmessage ("\nGetspice95 returns with no data", " ");
	    status = FAILURE;
	    break;
	}
    }
 else {
    zvmessage("Unsupported MODE", " ");
    zvmessage ("\nGetspice95 returns with no data", " ");
    status = FAILURE;
    }

 memcpy ((void*) &buf->doublebuf[37],
        (void *) ckdata.av, sizeof(double) * 3);
 memcpy((void *) &buf->doublebuf[40],
        (void *) ckdata.c_matrix, sizeof(double) * 9);
 
 i = 0;
 memcpy((void*) &buf->intbuf[188], (void*) &ckdata.seg_id[i], LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 memcpy((void*) &buf->intbuf[172], (void*) &ckdata.seg_id[i], LEN_PURPOSE);
 i += LEN_PURPOSE;
 memcpy((void*) &buf->intbuf[173], (void*) &ckdata.seg_id[i], LEN_PROG_NAME);
 i += LEN_PROG_NAME;
 memcpy((void *) &buf->intbuf[13], (void*) &ckdata.seg_id[i], LEN_SP_REF);
 i += LEN_SP_REF;
 memcpy((void*) &buf->intbuf[175], (void*) &ckdata.seg_id[i], LEN_REQ_NO);
 i += LEN_REQ_NO;
 memcpy((void*) &buf->intbuf[168], (void*) &ckdata.seg_id[i], LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 memcpy((void*) &buf->intbuf[169], (void*) &ckdata.seg_id[i], LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 memcpy((void*) &buf->intbuf[170], (void*) &ckdata.seg_id[i], LEN_SPICE_ID);
 i += LEN_SPICE_ID;
 memcpy((void*) &buf->intbuf[176], (void*) &ckdata.seg_id[i], LEN_PROG_NAME);
 memcpy((void*) &buf->intbuf[171], (void*) ckdata.ck_id, 4);

 buf->doublebuf[12] = spkdata.tgt_radius_l_axis;
 buf->doublebuf[13] = spkdata.tgt_radius_s_axis;
 buf->doublebuf[14] = spkdata.tgt_polar_radius;

 for (i = 0; i < 3; i++) {
    buf->doublebuf[i + 15] = spkdata.sc_pos_bd_centered[i];
    buf->doublebuf[i + 18] = spkdata.pic_pos_sc_centered[i];
    buf->doublebuf[i + 21] = spkdata.rs_vector[i];
    }

 buf->doublebuf[24] = spkdata.range_pic_bd_2_sun;
 buf->doublebuf[25] = spkdata.range_sc_2_central_bd_cntr;
 buf->doublebuf[26] = spkdata.range_sc_2_picture_bd_cntr;

 buf->doublebuf[27] = spkdata.tgt_bd_cntr_2_sun_lat;
 buf->doublebuf[28] = spkdata.tgt_bd_cntr_2_sun_lon;
 buf->doublebuf[29] = spkdata.tgt_bd_cntr_2_sc_lat;
 buf->doublebuf[30] = spkdata.tgt_bd_cntr_2_sc_lon;

 memcpy((void *) &buf->doublebuf[49],
        (void *) spkdata.me_matrix, sizeof(double) * 9);
 memcpy((void *) &buf->doublebuf[58],
        (void *) spkdata.om_matrix, sizeof(double) * 9);
 buf->doublebuf[67] = spkdata.north_angle;
 buf->doublebuf[68] = spkdata.sub_sc_line;
 buf->doublebuf[69] = spkdata.sub_sc_samp;

 buf->doublebuf[76] = spkdata.p5_lat;
 buf->doublebuf[77] = spkdata.p5_lon;
 buf->doublebuf[78] = spkdata.p5_incidence_angle;
 buf->doublebuf[79] = spkdata.p5_emission_angle;
 buf->doublebuf[80] = spkdata.p5_phase_angle;
 buf->doublebuf[81] = spkdata.p5_vert_pix_size;
 buf->doublebuf[82] = spkdata.p5_horiz_pix_size;
 buf->doublebuf[83] = spkdata.range_sc2p5_intercept_pt;

 /** txh::removed to return the correct status (i.e. 1=SUCCESS, 0=FAILUE)
 return (!status);
 **/

 return status;
}

/** txh::added subroutine to add blank space to entries that are shorter 
         than the required length */
void add_space (char *usr_str, int str_len)
{
   int usr_len,i;

   usr_len = (int)strlen(usr_str);

   if (usr_len < str_len)
   {
      for (i=usr_len; i < str_len; i++)
         *(usr_str+i) = ' ';
   }
   return;
}


