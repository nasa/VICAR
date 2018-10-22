/*--------------------------------------------------------------------------
 * Apr.     1998  ...S.Le......   Initial release.
 *
 * Apr. 29, 1998  ...T.Huang...   Ported onto Alpha/VMS.
 *                                Cleaned the list of includes.
 *                                Modified to generate same results as the
 *                                 original SPICE routines.
 *                                Correct socket and file I/O calls.
 *                                Corrected illegal memory access problem.
 *                                Corrected C Kernel loading sequence.
 *                                Corrected to use source search when
 *                                 an invalid ck_id is passed.
 *
 * Oct. 08, 1998  ...T.Huang...   SPICE_CONFIG_FILE was never closed.  Client 
 *                                will run out of file descriptors when 
 *                                performing lots of transactions.  'fclose'
 *                                calls have been added to correct this 
 *                                problem.
 *
 * Jan. 22, 1999  ...T.Huang...   Add codes to mssvr_readspkdata to initialize
 *                                its local variables to zeros, before passing
 *                                them to another function.  This prevents 
 *                                the possiblity of Uninitialize Memory Read.
 *
 * Jun. 24, 1999  ...T.Huang...   Correct prototypes to <proj>putspice routines.
 *
 * Aug. 18, 1999  ...M.Brady...   mssvr_readckdata did not have return
 *                                statments for the second "if"-branch or
 *                                at the end of the function.
 *                                Added 'return -1' to the second "if"-branch
 *                                and 'return 0' at the end of the function.
 *
 *--------------------------------------------------------------------------*/


#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <stdlib.h>

#include "ms_defines.h"
#include "ms_bridge.h"
#include "cltsub.h"
#include "lclsub.h"

/*--------------------------------------------------------------------------*/
int mslcl_getgllenv (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256];
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getgllenv:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getgllenv:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }
 
 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if	    (!(strcmp(key, "GLL_KDB")))		strcpy (env->kdb, value);
    else if (!(strcmp(key, "GLL_SPICEKER")))	strcpy (env->spiceker, value);
    else if (!(strcmp(key, "GLL_MIPSKER")))	strcpy (env->mipsker, value);
    else if (!(strcmp(key, "GLL_SCLK")))	strcpy (env->sclk, value);
    else if (!(strcmp(key, "GLL_CONSTANTS")))	strcpy (env->consts, value);
    else if (!(strcmp(key, "GLL_BODY_IDS")))	strcpy (env->bodyids, value);
    else if (!(strcmp(key, "GLL_LEAPSECONDS")))	strcpy (env->leapsec, value); 
    }
 fclose(cfile);
 return 0; 
}
/*--------------------------------------------------------------------------*/
int mslcl_getcasenv (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256]; 
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getcasenv:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getcasenv:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if      (!(strcmp(key, "CAS_KDB")))         strcpy (env->kdb, value);
    else if (!(strcmp(key, "CAS_SPICEKER")))    strcpy (env->spiceker, value);
    else if (!(strcmp(key, "CAS_MIPSKER")))     strcpy (env->mipsker, value);
    else if (!(strcmp(key, "CAS_SCLK")))        strcpy (env->sclk, value);
    else if (!(strcmp(key, "CAS_CONSTANTS")))   strcpy (env->consts, value);
    else if (!(strcmp(key, "CAS_BODY_IDS")))    strcpy (env->bodyids, value); 
    else if (!(strcmp(key, "CAS_LEAPSECONDS"))) strcpy (env->leapsec, value);
    else if (!(strcmp(key, "CAS_FRAMES")))      strcpy (env->frames, value);
    }
 fclose(cfile);
 return 0; 
}
/*--------------------------------------------------------------------------*/
int mslcl_getsimenv (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256]; 
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getsimenv:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getsimenv:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if      (!(strcmp(key, "SIM_KDB")))         strcpy (env->kdb, value);
    else if (!(strcmp(key, "SIM_SPICEKER")))    strcpy (env->spiceker, value);
    else if (!(strcmp(key, "SIM_MIPSKER")))     strcpy (env->mipsker, value);
    else if (!(strcmp(key, "SIM_SCLK")))        strcpy (env->sclk, value);
    else if (!(strcmp(key, "SIM_CONSTANTS")))   strcpy (env->consts, value);
    else if (!(strcmp(key, "SIM_BODY_IDS")))    strcpy (env->bodyids, value);
    else if (!(strcmp(key, "SIM_LEAPSECONDS"))) strcpy (env->leapsec, value);
    }
 fclose(cfile);
 return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_getvgr1env (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256]; 
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getvgr1env:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getvgr1env:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if      (!(strcmp(key, "VGR1_KDB")))         strcpy (env->kdb, value);
    else if (!(strcmp(key, "VGR1_SPICEKER")))    strcpy (env->spiceker, value);
    else if (!(strcmp(key, "VGR1_MIPSKER")))     strcpy (env->mipsker, value);
    else if (!(strcmp(key, "VGR1_SCLK")))        strcpy (env->sclk, value);
    else if (!(strcmp(key, "VGR1_CONSTANTS")))   strcpy (env->consts, value);
    else if (!(strcmp(key, "VGR1_BODY_IDS")))    strcpy (env->bodyids, value);
    else if (!(strcmp(key, "VGR1_LEAPSECONDS"))) strcpy (env->leapsec, value);
    }
 fclose(cfile);
 return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_getvgr2env (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256];
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getvgr2env:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -2;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getvgr2env:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -2;
    }

 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if      (!(strcmp(key, "VGR2_KDB")))         strcpy (env->kdb, value);
    else if (!(strcmp(key, "VGR2_SPICEKER")))    strcpy (env->spiceker, value);
    else if (!(strcmp(key, "VGR2_MIPSKER")))     strcpy (env->mipsker, value);
    else if (!(strcmp(key, "VGR2_SCLK")))        strcpy (env->sclk, value);
    else if (!(strcmp(key, "VGR2_CONSTANTS")))   strcpy (env->consts, value);
    else if (!(strcmp(key, "VGR2_BODY_IDS")))    strcpy (env->bodyids, value);
    else if (!(strcmp(key, "VGR2_LEAPSECONDS"))) strcpy (env->leapsec, value);
    }
 fclose(cfile);
 return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_getvo1env (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256];
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getvo1env:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getvo1env:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if      (!(strcmp(key, "VO1_KDB")))         strcpy (env->kdb, value);
    else if (!(strcmp(key, "VO1_SPICEKER")))    strcpy (env->spiceker, value);
    else if (!(strcmp(key, "VO1_MIPSKER")))     strcpy (env->mipsker, value);
    else if (!(strcmp(key, "VO1_SCLK")))        strcpy (env->sclk, value);
    else if (!(strcmp(key, "VO1_CONSTANTS")))   strcpy (env->consts, value);
    else if (!(strcmp(key, "VO1_BODY_IDS")))    strcpy (env->bodyids, value);
    else if (!(strcmp(key, "VO1_LEAPSECONDS"))) strcpy (env->leapsec, value);
    }
 fclose(cfile);
 return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_getvo2env (msEnvStruct *env)
{
 FILE *cfile;
 char *cfname;
 char dataline[256];
 char key[256];
 char value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
        "mslcl_getvo2env:ERROR!!!!",
        "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
        "mslcl_getvo2env:Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
    sscanf (dataline, "%s%s", key, value);
    if      (!(strcmp(key, "VO2_KDB")))         strcpy (env->kdb, value);
    else if (!(strcmp(key, "VO2_SPICEKER")))    strcpy (env->spiceker, value);
    else if (!(strcmp(key, "VO2_MIPSKER")))     strcpy (env->mipsker, value);
    else if (!(strcmp(key, "VO2_SCLK")))        strcpy (env->sclk, value);
    else if (!(strcmp(key, "VO2_CONSTANTS")))   strcpy (env->consts, value);
    else if (!(strcmp(key, "VO2_BODY_IDS")))    strcpy (env->bodyids, value);
    else if (!(strcmp(key, "VO2_LEAPSECONDS"))) strcpy (env->leapsec, value);
    }
 fclose(cfile);
 return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_load_support_kernels (msEnvStruct *env)
{
 zms_reset ();
 zms_clpool ();
 zms_ldpool (env->sclk);
 zms_ldpool (env->consts);
 zms_ldpool (env->bodyids);
 zms_ldpool (env->leapsec);
 if (strcmp(env->frames, "") != 0)
 {
   zms_ldpool (env->frames);
 }

 if (zms_failed()) return -1;
 else return 0;
}
/*--------------------------------------------------------------------------*
 * input : sc_id
 * input : scet
 * output: sclk
 *--------------------------------------------------------------------------*/
int mslcl_scet2sclk (int sc_id, int *scet, double *sclk)
{
 double	ephem_time;
 char	utc_str[64];

 const char *cm = "mslcl_scet2sclk";

 memset ((void*) utc_str, '\0', 64);
 sprintf (utc_str, "%04d-%03d//%02d:%02d:%02d.%03d",
        scet[0], scet[1], scet[2],
        scet[3], scet[4], scet[5]);

 zms_utc2et (utc_str, &ephem_time);
 if (zms_failed()) {
    fprintf (stderr, "%s:%s\n",
	cm, "ERROR getting ephem_time from utc_str");
    return -1;
    }

 zms_sce2t (sc_id, ephem_time, sclk);
 if (zms_failed()) {
    fprintf (stderr, "%s:%s\n",
	cm, "ERROR getting sclk from ephem_time");
    return -1;
    }

 return 0;
}
/*--------------------------------------------------------------------------*/
/** txh::subroutine to perform comparison between the user input segment 
         id entry with the kernel segment id entry.  This subroutine will 
         return a match when:
            - the user input is empty;
            - the user input is defaulted; or
            - the user input matches the kernel entry.
**/
int mslcl_strcmp (char *usr_str, char *ker_str, char *def_str)
{
   /* if input field is empty, then return a match */
   if (strlen(usr_str) == 0) return 1;

   /* if input field is default, then return a match */
   if (!strncmp(usr_str, def_str, strlen(def_str))) return 1;

   /* if input field matches the kernel field, then return a match. */
   if (!strcmp(usr_str, ker_str)) return 1;

   /* otherwise, return unmatch.*/
   return 0;
} 
/*--------------------------------------------------------------------------*/
/** txh::modified to call mslcl_strcmp for spceific segment id entries 
         comparisons.
**/ 
int mslcl_identical_segid(char *usr_segid, char *ker_segid)
{
 char   u_inst[5], k_inst[5],
        u_pur[5], k_pur[5],
        u_pro[7], k_pro[7],
        u_sp_ref[5], k_sp_ref[5],
        u_job_no[5], k_job_no[5],
        u_date_time[13], k_date_time[13],
        u_usr_id[4], k_usr_id[4],
        u_grp_id[4], k_grp_id[4];

 int inst_flag, pur_flag, pro_flag, sp_ref_flag, 
     job_no_flag, date_time_flag,
     usr_id_flag, grp_id_flag;

 if (strlen(usr_segid) == 0) return 1;

 strncpy(u_inst, &usr_segid[0], 4);
 strncpy(k_inst, &ker_segid[0], 4);
 u_inst[4] = k_inst[4] = '\0';
 inst_flag = mslcl_strcmp (u_inst, k_inst, "NONE");

 strncpy(u_pur, &usr_segid[4], 4);
 strncpy(k_pur, &ker_segid[4], 4);
 u_pur[4] = k_pur[4] = '\0';
 pur_flag = mslcl_strcmp (u_pur, k_pur, "NONE");

 strncpy(u_pro, &usr_segid[8], 6);
 strncpy(k_pro, &ker_segid[8], 6);
 u_pro[6] = k_pro[6] = '\0';
 pro_flag = mslcl_strcmp (u_pro, k_pro, "*NONE*");

 strncpy(u_sp_ref, &usr_segid[14], 4);
 strncpy(k_sp_ref, &ker_segid[14], 4);
 u_sp_ref[4] = k_sp_ref[4] = '\0';
 sp_ref_flag = mslcl_strcmp (u_sp_ref, k_sp_ref, "NONE");

 strncpy(u_job_no, &usr_segid[18], 4);
 strncpy(k_job_no, &ker_segid[18], 4);
 u_job_no[4] = k_job_no[4] = '\0';
 job_no_flag = mslcl_strcmp (u_job_no, k_job_no, "NONE");

 strncpy(u_date_time, &usr_segid[22], 12);
 strncpy(k_date_time, &ker_segid[22], 12);
 u_date_time[12] = k_date_time[12] = '\0';
 date_time_flag = mslcl_strcmp (u_date_time, k_date_time, "000000000000");

 strncpy(u_usr_id, &usr_segid[34], 3);
 strncpy(k_usr_id, &ker_segid[34], 3);
 u_usr_id[3] = k_usr_id[3] = '\0';
 usr_id_flag = mslcl_strcmp (u_usr_id, k_usr_id, "*NO");

 strncpy(u_grp_id, &usr_segid[37], 3);
 strncpy(k_grp_id, &ker_segid[37], 3);
 u_grp_id[3] = k_grp_id[3] = '\0';
 grp_id_flag = mslcl_strcmp (u_grp_id, k_grp_id, "NE*");

 return (inst_flag && pur_flag && pro_flag && sp_ref_flag && 
         job_no_flag && date_time_flag && usr_id_flag && grp_id_flag);
}
/*--------------------------------------------------------------------------*/
int mslcl_kid_2_kinfo (msEnvStruct *env,
	char *kid, kernelInfoStruct *kinfo)
{
 int i, k_count;
 kernelInfoStruct tk_info[MAX_KERNEL_COUNT];

 memset ((void*) tk_info, '\0',
	sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT);
 k_count = mslcl_spk_info_from_kdb(env, tk_info);
 if (k_count && (k_count != (-1)) ) {
    for (i = 0; i < k_count; i++) {
	if (!strcmp(tk_info[i].id, kid)) {
	   memcpy ((void*) kinfo, (void*)&tk_info[i],
		sizeof(kernelInfoStruct));
	   return 0;
	   }
	}
    }

 memset ((void*) tk_info, '\0',
	sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT);
 k_count = mslcl_ck_info_from_kdb(env, tk_info);
 if (k_count && (k_count != (-1)) ) {
    for (i = 0; i < k_count; i++) {
	if (!strcmp(tk_info[i].id, kid)) {
	   memcpy ((void*) kinfo, (void*) &tk_info[i],
		sizeof(kernelInfoStruct));
	   return 0;
	   }
	}
    }

 return -1;
}
/*--------------------------------------------------------------------------*/
int mslcl_kname_2_kinfo (msEnvStruct *env,
	char *kname, kernelInfoStruct *kinfo)
{
 char	*sn_ptr,
	*un_ptr;
 int  i, k_count;
 kernelInfoStruct tk_info[MAX_KERNEL_COUNT];
 
 memset ((void*) tk_info, '\0',
        sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT);

 /* strip off any dir path in user kname */
 if (un_ptr = strrchr(kname, '/')) un_ptr++;
 else un_ptr = kname;

 k_count = mslcl_spk_info_from_kdb(env, tk_info);
 if (k_count && (k_count != (-1)) ) {
    for (i = 0; i < k_count; i++) {
	/* take off any dir path in kname from KDB */
	if (sn_ptr = strrchr(tk_info[i].fname, '/')) sn_ptr++;
	else sn_ptr = tk_info[i].fname;

        if (!strcmp(sn_ptr, un_ptr)) {
           memcpy ((void*) kinfo, (void*) &tk_info[i],
                sizeof(kernelInfoStruct));
           return 0;
           }
        }
    }
 
 memset ((void*) tk_info, '\0',
        sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT);
 k_count = mslcl_ck_info_from_kdb(env, tk_info);
 if (k_count && (k_count != (-1)) ) {
    for (i = 0; i < k_count; i++) {
	/* take off any dir path in kname from KDB */
	if (sn_ptr = strrchr(tk_info[i].fname, '/')) sn_ptr++;
	else sn_ptr = tk_info[i].fname;

        if (!strcmp(sn_ptr, un_ptr)) {
           memcpy ((void*) kinfo, (void*) &tk_info[i],
                sizeof(kernelInfoStruct));
           return 0;
           }
        }
    }
 
 return -1;
}
/*--------------------------------------------------------------------------*
 * read ck info from kdb. Return number of ckernels in kdb.
 * return -1: ERROR
 *--------------------------------------------------------------------------*/
int mslcl_ck_info_from_kdb (msEnvStruct *env, kernelInfoStruct *info)
{
 int count = mslcl_read_kernel_info (env, info, C_KERNEL_TYPE);
 if (count == (-1)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_ck_info_from_kdb",
	"ERROR reading ck_kdb from KERNELDB");
    return -1;
    }
 else return count;
}
/*--------------------------------------------------------------------------*/
int mslcl_spk_info_from_kdb (msEnvStruct *env, kernelInfoStruct *info)
{
 int count = mslcl_read_kernel_info (env, info, SP_KERNEL_TYPE);
 if (count == (-1)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_spk_info_from_kdb",
	"ERROR reading spk_kdb info from KERNELDB");
    return -1;
    }
 else return count;
}
/*--------------------------------------------------------------------------* 
 * - make sure you have at least MAX_KERNEL_COUNT in <info>
 *--------------------------------------------------------------------------*/
int mslcl_read_kernel_info (msEnvStruct *env,
		kernelInfoStruct *info, int type)
{
 FILE	*k_file;

 int	k_count,
	year_b,
	day_b,
	year_e,
	day_e;

 char	k_line[256],
	k_path[256],
	k_fname[256],
	time_b[256],
	time_e[256],
	month_b[5],
	month_e[5];

 kernelInfoStruct kinfo;

 const char *cm = "mslcl_read_kernel_info";

 k_count = 0;
 memset ((void*) k_line, '\0', 256);
 memset ((void*) k_path, '\0', 256);
 memset ((void*) k_fname, '\0', 256);
 memset ((void*) &kinfo, '\0', sizeof(kinfo));

 if ((k_file = fopen(env->kdb, "r")) == NULL) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR opening kdb file");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    fprintf (stderr, "%s:%s\n", "File name", env->kdb);
    return -1;
    }

 while ( (k_count < MAX_KERNEL_COUNT) &&
	 fgets(k_line, 255, k_file)) {
    sscanf (k_line, "%s%d%s%s%s%d%s%d%s%d%s%d%s",
		kinfo.id, &kinfo.type, kinfo.source,
		k_path, k_fname,
                &year_b, month_b, &day_b, time_b,
                &year_e, month_e, &day_e, time_e);

    if (kinfo.type != type) continue;

    sprintf (kinfo.utc_begin, "%d %s %d %s",
		year_b, month_b, day_b, time_b);
    zms_utc2et (kinfo.utc_begin, &kinfo.et_begin);
    if (zms_failed()) {
       fprintf (stderr, "%s:%s\n",
	cm, "ERROR getting begin et_range");
       fclose (k_file);
       return -1;
       }

    sprintf (kinfo.utc_end, "%d %s %d %s",
		year_e, month_e, day_e, time_e);
    zms_utc2et (kinfo.utc_end, &kinfo.et_end);
    if (zms_failed()) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR getting end et_range");
       fclose (k_file);
       return -1;
       }

    /* txh::modified to eliminate OS-file-directory-specific call.
    if (!strcmp(k_path, "SPICEKER"))
       sprintf (kinfo.fname, "%s/%s", env->spiceker, k_fname);
    else if (!strcmp (k_path, "MIPSKER"))
       sprintf (kinfo.fname, "%s/%s", env->mipsker, k_fname);
    */

    if (!strcmp(k_path, "SPICEKER"))
       sprintf (kinfo.fname, "%s%s", env->spiceker, k_fname);
    else if (!strcmp (k_path, "MIPSKER"))
       sprintf (kinfo.fname, "%s%s", env->mipsker, k_fname);
    else {
       fprintf (stderr, "%s:%s\n", cm, "ERROR:unknown kernel path");
       fprintf (stderr, "%s:%s\n", "Path name", k_path);
       fclose (k_file);
       return -1;
       }

    memcpy ((char*) &info[k_count++],
	(char*)&kinfo, sizeof(kinfo));
    }

 fclose (k_file);
 if (k_count >= MAX_KERNEL_COUNT) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:Too many kernel to load");
    return -1;
    }
 else return k_count;
}
/*--------------------------------------------------------------------------*
 * if all segid entries are at default, then just return 0. Which mean
 *	that we don't need to check for segid anymore. Any segid would
 *	be fine.
 * Else just return 1, which means that you have to check to see
 *	if the segid is ok.
 *--------------------------------------------------------------------------*/
int mslcl_use_prov_info (char *segid)
{
 /* if len is 0, don't care about prov */
 if (strlen(segid) == 0) return 0;

 /* if any of the entry if non-default, then	*/
 /* we are using provenance information.	*/

 if (strncmp(segid, "NONE", 4) != 0) return 1;		/* institution	*/
 if (strncmp(&segid[4], "NONE", 4) != 0) return 1;	/* purpose	*/
 if (strncmp(&segid[8], "*NONE*", 6) != 0) return 1;	/* prog_name	*/
 if (strncmp(&segid[14], "NONE", 4) != 0) return 1;	/* spk_ref	*/
 if (strncmp(&segid[18], "NONE", 4)  != 0) return 1;	/* req/job no	*/
 if (strncmp(&segid[22], "000000000000", 12) != 0) return 1;/* date/time*/
 if (strncmp(&segid[34], "*NO", 3) != 0) return 1;	/* usr id	*/
 if (strncmp(&segid[37], "NE*", 3) != 0) return 1;	/* group id	*/

 return 0;
} 
/*--------------------------------------------------------------------------*
 * If any segid key is null, it will be filled with default
 * values !!!!
 *
 * The order in which entries in segid is lowered:
 *	1/ date & time
 *	2/ req/job numb
 *	3/ purpose
 *	4/ program name
 *	5/ sp_ref
 *	6/ userid
 *	7/ groupid
 *	8/ institution
 *
 * return 0: if segid is already at the lowest point
 * return 1: if this was able to lower something.
 *--------------------------------------------------------------------------*/
int mslcl_lowerSegidPriority (char *segid)
{
 if (!segid[22])	memcpy (&segid[22], "000000000000", 12);
 if (!segid[18])	memcpy (&segid[18], "NONE", 4);
 if (!segid[4]) 	memcpy (&segid[4], "NONE", 4);
 if (!segid[8]) 	memcpy (&segid[8], "*NONE*", 6);
 if (!segid[14]) 	memcpy (&segid[14], "NONE", 4);
 if (!segid[34])        memcpy (&segid[34], "*NO", 3);
 if (!segid[37]) 	memcpy (&segid[37], "NE*", 3);
 if (!segid[0]) 	memcpy (&segid[0], "NONE", 4);

 if (memcmp(&segid[22], "000000000000", 12))		/* date-time	*/
    memcpy (&segid[22], "000000000000", 12);
 else if (memcmp (&segid[18], "NONE", 4))		/* req-no	*/
    memcpy (&segid[18], "NONE", 4);
 else if (memcmp (&segid[4], "NONE", 4))		/* purpose	*/
    memcpy (&segid[4], "NONE", 4);
 else if (memcmp (&segid[8], "*NONE*", 6))		/* program name	*/
    memcpy (&segid[8], "*NONE*", 6);
 else if (memcmp (&segid[14], "NONE", 4))		/* spk_ref	*/
    memcpy (&segid[14], "NONE", 4);
 else if (memcmp (&segid[34], "*NO", 3))                /* usr id     */
    memcpy (&segid[34], "*NO", 3);
 else if (memcmp (&segid[37], "NE*", 3))		/* group_id	*/
    memcpy (&segid[37], "NE*", 3);
 else if (memcmp (&segid[0], "NONE", 4))		/* institution	*/
    memcpy (&segid[0], "NONE", 4);
 else return 0;

 return 1;
}
/*--------------------------------------------------------------------------*
 * Spk load policy:
 *	- Load all SPK that fall in the specified range.
 *	- if <segid> specified a valid spk, load it last & return.
 *		If needed, you can unload then re-load it.
 *	- if <spk_ref> specifies a valid spk, load it last & return.
 *		If needed, you can unload then re-load it.
 *--------------------------------------------------------------------------*/
int mslcl_loadSpk (int sd, double et, char *spk_ref,
	char *segid, kernelInfoStruct *kinfo, int kernel_count)
{
 int	k_count,
	load_once;
 char	lspk_ref[8];
 double	tb_et,
	te_et;

 const char *cm = "mslcl_loadSpk";

 /* load every SPK we have in KDB that fall within the	  */
 /* user specified range.				  */
 load_once = 0;
 k_count   = 0;
 while (k_count < kernel_count) {
     tb_et = kinfo[k_count].et_begin - 60;
     te_et = kinfo[k_count].et_end * 1.0;
     if ((et > tb_et) && (et < te_et)) {
	zms_spklef (kinfo[k_count].fname, &kinfo[k_count].handle);
	if (zms_failed()) {
	   mssvr_write_info_log (sd, cm, "ERROR loading SPK", 0);
	   mssvr_write_info_log (sd, "spk name", kinfo[k_count].fname, 0);
	   return -1;
	   }
	else {
	   mssvr_write_info_log (sd, "loaded spk", kinfo[k_count].fname, 0);
	   if (!load_once) load_once = 1;
	   kinfo[k_count].isloaded   = 1;
	   }
        }
     k_count++;
     }

 /* load the SPK specified in <spk_ref>	*/
 if (strlen(spk_ref)) {
    k_count = 0;
    while (k_count < kernel_count) {
	if (!strcmp (spk_ref, kinfo[k_count].id)) {
	   if (kinfo[k_count].isloaded) {
	      zms_spkuef(kinfo[k_count].handle);
	      zms_spkcls(kinfo[k_count].handle);
	      }
	   zms_spklef (kinfo[k_count].fname,
			&kinfo[k_count].handle);
	   if (zms_failed()) {
	      mssvr_write_info_log (sd, cm,
		"ERROR loading SPK via <spk_ref>", 0);
	      mssvr_write_info_log (sd, "spk name",
		kinfo[k_count].fname, 0);
	      return -1;
	      }
	   else {
	      mssvr_write_info_log (sd, "spk", kinfo[k_count].fname, 0);
	      return 0;
	      }
	   }
	k_count++;
	}
    } 

 /* load the SPK specified in <segid>	*/
 k_count = 0;
 memset ((void*) lspk_ref, '\0', sizeof(char) * 8);
 strncpy (lspk_ref, &segid[14], 4);
 while (k_count < kernel_count) {
     if (!strcmp (lspk_ref, kinfo[k_count].id)) {
	if (kinfo[k_count].isloaded) {
	   zms_spkuef(kinfo[k_count].handle);
	   zms_spkcls(kinfo[k_count].handle);
	   }
	zms_spklef (kinfo[k_count].fname,
		&kinfo[k_count].handle);
	if (zms_failed()) {
	   mssvr_write_info_log (sd, cm,
		"ERROR loading SPK via <segid>", 0);
	   mssvr_write_info_log (sd, "spk name",
		kinfo[k_count].fname, 0);
	   return -1;
	   }
        else {
	   mssvr_write_info_log (sd, "spk", kinfo[k_count].fname, 0);
	   return 0;
	   }
	}
     k_count++;
     }

 if (!load_once) {
    mssvr_write_info_log (sd, cm,
	"ERROR:no SPK was loaded!!!!", 0);
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_unloadSpk (kernelInfoStruct *kinfo, int count)
{
 int i;
 for (i = 0; i < count; i++)
     if (kinfo[i].isloaded) {
	zms_spkuef (kinfo[i].handle);
	zms_spkcls (kinfo[i].handle);
	kinfo[i].handle = kinfo[i].isloaded = 0;
	}
 return 0;
} 
/*--------------------------------------------------------------------------*/
int mslcl_unloadCk (kernelInfoStruct *kinfo, int count)
{
 int i;
 for (i = 0; i < count; i++)
     if (kinfo[i].isloaded) {
	zms_ckupf (kinfo[i].handle);
	zms_ckcls (kinfo[i].handle);
	kinfo[i].handle = kinfo[i].isloaded = 0;
	}
 return 0;
}
/*--------------------------------------------------------------------------*
 * 1. Get requested et.
 * 2. Load all CKernels you have in the order specified in KDB.
 * 3. Check request to see if users specified any CK to load.
 *--------------------------------------------------------------------------*/
int mslcl_loadCk (int sd, msUserRequestStruct *req,
	kernelInfoStruct *kinfo, int kernel_count)
{
 char 	utc_str[64];
 int	k_count = 0,
	load_once = 0;
 double et, tb_et, te_et;
 
 const char *cm = "mslcl_loadCk";

 memset ((void*) utc_str, '\0', 64);
 sprintf(utc_str, "%04d-%03d // %02d:%02d:%02d.%03d",
        req->scet[0], req->scet[1], req->scet[2],
        req->scet[3], req->scet[4], req->scet[5]);
 zms_utc2et (utc_str, &et);
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
        "ERROR getting ephem_time", 1);
    return -1;
    }

 while (k_count < kernel_count) {
     tb_et = kinfo[k_count].et_begin - 60;
     te_et = kinfo[k_count].et_end * 1.0;

     if ((et > tb_et) && (et < te_et)) {
	zms_cklpf (kinfo[k_count].fname, &kinfo[k_count].handle);
        if (zms_failed()) {
           mssvr_write_info_log (sd, cm,
                "ERROR loading CKernel", 1);
           mssvr_write_info_log (sd, "ck name",
                kinfo[k_count].fname, 0);
	   while (kernel_count >= 0) {
		zms_ckupf(kinfo[k_count].handle);
		zms_ckcls(kinfo[k_count].handle);
		kinfo[k_count--].isloaded = 0;
		}
           return -1;
           }
        else {
           if (!load_once) load_once = 1;
           kinfo[k_count].isloaded   = 1;
           mssvr_write_info_log (sd, "loaded CK",
		kinfo[k_count].fname, 1);
           }
       }
    k_count++;
    }

 if (strlen(req->ck_name)) {
    k_count = 0;
    while (k_count < kernel_count) {
	if (strcmp(req->ck_name, kinfo[k_count].fname) == 0) {
	   if (kinfo[k_count].isloaded) {
	      zms_ckupf(kinfo[k_count].handle);
	      zms_ckcls(kinfo[k_count].handle);
	      kinfo[k_count].isloaded = 0;
	      }
	   zms_cklpf(kinfo[k_count].fname, &kinfo[k_count].handle);
	   if (zms_failed()) {
	      while (kernel_count >= 0) {
		  if (kinfo[kernel_count].isloaded) {
	             zms_ckupf(kinfo[kernel_count].handle);
	             zms_ckcls(kinfo[kernel_count].handle);
	             kinfo[kernel_count--].isloaded = 0;
		     }
		  }
	      mssvr_write_info_log (sd, cm,
		"ERROR loading requested CK name", 1);
	      mssvr_write_info_log (sd, "ck name",
		kinfo[k_count].fname, 1);
	      return -1;
	      }
 	   else {
              mssvr_write_info_log (sd, "loaded CK",
		kinfo[k_count].fname, 1);
	      kinfo[k_count].isloaded = 1;
	      return 0;
	      }
	   }
	k_count++;
        }
    }

 if (strlen(req->ck_id)) {
    k_count = 0;
    while (k_count < kernel_count) {

        /* txh::added checks for the correct 'et' range to avoid loading
                of unrelated kernel files. */
        tb_et = kinfo[k_count].et_begin - 60;
        te_et = kinfo[k_count].et_end * 1.0;
        if ((et > tb_et) && (et < te_et) &&
            !strcmp(req->ck_id, kinfo[k_count].id)) {
           if (kinfo[k_count].isloaded) {
              zms_ckupf(kinfo[k_count].handle);
              zms_ckcls(kinfo[k_count].handle);
              kinfo[k_count].isloaded = 0;
              }

           zms_cklpf(kinfo[k_count].fname, &kinfo[k_count].handle);
           if (zms_failed()) {
              while (kernel_count >= 0) {
                  if (kinfo[kernel_count].isloaded) {
                     zms_ckupf(kinfo[kernel_count].handle);
                     zms_ckcls(kinfo[kernel_count].handle);
                     kinfo[kernel_count--].isloaded = 0;
                     }
                  }
	      mssvr_write_info_log (sd, cm, 
		"ERROR loading requested CK name", 1);
              mssvr_write_info_log (sd, "ck name", kinfo[k_count].fname, 1);
              return -1;
              }
           else {
              mssvr_write_info_log (sd, "loaded CK",
		kinfo[k_count].fname, 1);
              kinfo[k_count].isloaded = 1;
              return 0;
              }
           }
	k_count++;
        }
    }

 if (strlen(req->ck_source)) {
    k_count = 0;
    while (k_count < kernel_count) {

        /* txh::added checks for the correct 'et' range to avoid loading
                of unrelated kernel files. */
        tb_et = kinfo[k_count].et_begin - 60;
        te_et = kinfo[k_count].et_end * 1.0;
        if ((et > tb_et) && (et < te_et) &&
            !strcmp(req->ck_source, kinfo[k_count].source)) {

           if (kinfo[k_count].isloaded) {
              zms_ckupf(kinfo[k_count].handle);
              zms_ckcls(kinfo[k_count].handle);
              kinfo[k_count].isloaded = 0;
              }
 
           zms_cklpf(kinfo[k_count].fname, &kinfo[k_count].handle);
           if (zms_failed()) {
              while (kernel_count >= 0) {
                  if (kinfo[kernel_count].isloaded) {
                     zms_ckupf(kinfo[kernel_count].handle);
                     zms_ckcls(kinfo[kernel_count].handle);
                     kinfo[kernel_count--].isloaded = 0;
                     }
                  }
	      mssvr_write_info_log (sd, cm,
		"ERROR loading requested CK name", 1);
              mssvr_write_info_log (sd, "ck name", kinfo[k_count].fname, 1);
              return -1;
              }
           else {
	      kinfo[k_count].isloaded = 1;
              mssvr_write_info_log (sd, "loaded CK",
		kinfo[k_count].fname, 1);
	      }
           }
        k_count++;
        }
    }

 return 0;
}
/*--------------------------------------------------------------------------*/
void mslcl_getDateTime (char *date_time)
{
 struct tm      *currtime;
 time_t         bintime;
 time(&bintime);
 currtime = (struct tm *) localtime(&bintime);
 sprintf(date_time, "%04d%02d%02d%02d%02d",
        currtime->tm_year + 1900,   /* years since 1900 */
        currtime->tm_mon + 1,
        currtime->tm_mday,
        currtime->tm_hour,
        currtime->tm_min);
}
/*--------------------------------------------------------------------------*/
int mslcl_gllgetspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_gllgetspice",
	"ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_casgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_casgetspice",
	"ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_simgetspice",
	"ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_vgr1getspice",
	"ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_vgr2getspice",
	"ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vo1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "mslcl_vo1getspice",
        "ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vo2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 if (mslcl_getspice(&env, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_vo2getspice",
	"ERROR reading data from kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*
 * This is the local function that reads data from local kernels....Users
 * should not call this directly. They should call appropriate
 * msclcl_xxx_getspice() functions, where "xxx_" is one of represents
 * different missions (e.g. gll, cas, sim, vgr1, vgr2, vo1, vo2).
 *
 * The first thing done in here is to diable the default SPICELIB's error
 * reporting routine by calling zms_erract(). By default, SPICELIB prints
 * internal message and exits. SPICELIB's error messages are sometime
 * incomprehensible by mips users....This function will return appropriate
 * error code print out its own message....
 *--------------------------------------------------------------------------*/
int mslcl_getspice (msEnvStruct *env, msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 int status = 0;
 FILE *tfile;
 char tfname[256];
 const char *cm = "mslcl_getspice";

 zms_erract ("set", "return");	/* the first thing done in here is to
				 * dis-able the default SPICELIB's error
				 * reporting routine by calling zms_erract().
				 * The default behavior print out internal
				 * message and exits. We don't want it to
				 * exit, we just want it to return appropriate
				 * error handling code.
				 */


 memset (tfname, '\0', sizeof(char) * 256);
 tmpnam (tfname);
 if (!(tfile = fopen(tfname, "w"))) {
    fprintf (stderr, "%s:%s\n%s:%s\n%s:%s\n",
	"mslcl_getspice", "ERROR open temp log file",
	"filename", tfname,
	"Sys mesg", strerror(errno));
    return -1;
    }

 ckdata->system  = req->system;
 ckdata->sc_id = spkdata->sc_id = req->sc_id;
 memcpy ((void*) ckdata->scet,
        (void*)req->scet, sizeof(int) * 6);

 fprintf (stdout, "%s:%s\n", cm, "Kernel read....initializing");
 fprintf (stdout, "%s:%s\n", cm, "Loading support kernels .....");
 if (mslcl_load_support_kernels(env)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_getspice", "ERROR loading support kernels");
    fclose (tfile);
    return -1;
    }
 if (t_mssvr_readckdata(fileno(tfile), env, req, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_getspice", "ERROR reading local CKdata");
    status = -1;
    }
 else if (mssvr_readspkdata(fileno(tfile), env,
	req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_getspice", "ERROR reading local SPKdata");
    status = -1;
    }

 fclose (tfile);
 remove (tfname);
 zms_clpool();
 return status;
}
/*--------------------------------------------------------------------------*/
int mslcl_gllputspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_gllputspice",
	"ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_casputspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_casputspice",
	"ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_simputspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_simputspice",
	"ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vgr1putspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_vgr1putspice",
	"ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vgr2putspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_vgr2putspice",
	"ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vo1putspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "mslcl_vo1putspice",
        "ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mslcl_vo2putspice (msCkStruct *ckdata)
{
 msEnvStruct env;
 memset ((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 if (mslcl_putspice(&env, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"mslcl_vo2putspice",
	"ERROR writing data to kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*
 * This is the local function that writes data to local kernels....Users
 * should not call this directly. They should call appropriate
 * msclcl_xxx_putspice() functions, where "xxx_" is one of represents
 * different missions (e.g. gll, cas, sim, vgr1, vgr2, vo1, vo2).
 *
 * The first thing done in here is to diable the default SPICELIB's error
 * reporting routine by calling zms_erract(). By default, SPICELIB prints
 * internal message and exits. SPICELIB's error messages are sometime
 * incomprehensible by mips users....This function will return appropriate
 * error code print out its own message....
 *--------------------------------------------------------------------------*/

int mslcl_putspice (msEnvStruct *env, msCkStruct *ckdata)
{
 FILE *tfile;
 char tfname[256];
 const char *cm = "mslcl_putspice";

 zms_erract ("set", "return");  /* the first thing done in here is to
                                 * dis-able the default SPICELIB's error
                                 * reporting routine by calling zms_erract().
                                 * The default behavior print out internal
                                 * message and exits. We don't want it to
                                 * exit, we just want it to return appropriate
                                 * error handling code.
                                 */

 memset ((void*)tfname, '\0', sizeof(char) * 256);
 tmpnam (tfname);
 if (!(tfile = fopen(tfname, "w"))) {
    fprintf (stderr, "%s:%s\n%s:%s\n", cm,
	"ERROR opening temp log file",
	"Sys mesg", tfname);
    return -1;
    }

 if (mslcl_load_support_kernels(env)) {
    fprintf (stderr, "%s:%s\n",
        "mslcl_putspice", "ERROR loading support kernels");
    fclose (tfile);
    remove (tfname);
    zms_clpool();
    return -1;
    }

 /* check to see if user specified specific ckid */
 /* to write to. If yes, make sure you check     */
 /* that the data is written correctly.		 */
 if (strlen(ckdata->ck_id)) {
    if ( mssvr_write_to_ckid (fileno(tfile),
		ckdata->ck_id, ckdata, env) ) {
        fprintf (stderr, "%s:%s\n%s:%s\n", cm,
		"ERROR writing to data to kernel via ck_id",
		"ck_id", ckdata->ck_id);
	return -1;
        }
    else {
       fclose (tfile);
       remove (tfname);
       zms_clpool ();
       return 0;
       }
    }


 /* check to see if user specified specific 	 	*/
 /* fname to write to. If yes, make sure you check     	*/
 /* that the data is written correctly.		 	*/
 if (strlen(ckdata->ck_name)) {
    if ( mssvr_write_to_ckname(fileno(tfile),
		ckdata->ck_name, ckdata, env) ) {
        fprintf (stderr, "%s:%s\n%s:%s\n", cm,
	    "ERROR writing to data to kernel via ck_name",
	    "ck_name", ckdata->ck_name);
	return -1;
        }
    else {
       fclose (tfile);
       remove (tfname);
       zms_clpool ();
       return 0;
       }
    }

 /* check to see if user specified specific 	 	*/
 /* source to write to. If yes, make sure you check     */
 /* that the data is written correctly.		 	*/
 if (strlen(ckdata->ck_source)) {
    if ( mssvr_write_to_cksource(fileno(tfile),
		ckdata->ck_source, ckdata, env) ) {
       fprintf (stderr, "%s:%s\n%s:%s\n", cm,
	   "ERROR writing to data to kernel via ck_source",
	   "ck_source", ckdata->ck_source);
       }
    else {
       fclose (tfile);
       remove (tfname);
       zms_clpool ();
       return 0;
       }
    }

 /* by the time that you have gotten here, you have	*/
 /* not found any valid source, ckid, ckname. Just	*/
 /* print out any error message and return !!!!		*/
 fprintf (stderr, "ERROR writing data to CKernel\n");
 fprintf (stderr, "mslcl_putspice:No valid id, source, name found\n");
 fclose (tfile);
 remove (tfname);
 zms_clpool ();
 return -1;
}
/*----------------------------------------------------------------*
 * given an opened socket, you want to call accept() on it to
 * return a new socket that client want to send request to. This
 * is where peer-to-peer communication occurs.
 *----------------------------------------------------------------*/
int mssvr_getNewClient (int ld)
{
 int		 sd;
 socklen_t	 len;
 int             On=1;
 struct sockaddr addr;

 const char *cm = "mssvr_getNewClient";

 len = 0;
 if ( (sd = accept(ld, (struct sockaddr*) NULL, &len)) == (-1) ) {
    msclt_log (cm, "ERROR accepting new client");
    msclt_log ("Sys mesg", strerror(errno));
    return -1;
    }
 if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, (char*)&On, sizeof(On)) < 0) {
    msclt_log (cm, "Warning: Failed on setting keepalive option on new client");
    msclt_log ("Sys mesg", strerror(errno));
 }
 
 return sd;
}
/*----------------------------------------------------------------*
 * Initialize the given socket as an acceptor. You need to read
 * the configuration file to get the PORT number that you are
 * suppose to be listening to.
 *----------------------------------------------------------------*/
int mssvr_initAcceptor (int *sd)
{
 char	*host;
 short	portno;
 int	status = 1;
 struct sockaddr_in addr;

 const char *cm = "mssvr_initAcceptor";

 if ( (portno = msclt_getPortno()) == (-1)) {
    msclt_log (cm, "Cannot get server port number");
    return -1;
    }

 memset ((void*) &addr, '\0', sizeof(addr));
 addr.sin_family	= AF_INET;
 addr.sin_port		= htons (portno);
 addr.sin_addr.s_addr	= htonl (INADDR_ANY);

 if ((*sd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
    msclt_log (cm, "Cannot initialize socket");
    msclt_log ("Sys mesg", strerror(errno));
    return -1;
    }

 if (setsockopt (*sd, SOL_SOCKET, SO_REUSEADDR,
	(char*) &status, sizeof(status)) < 0) {
    msclt_log (cm, "Cannot set socket option");
    msclt_log ("Sys mesg", strerror(errno));
    close (*sd);
    return -1;
    }

 if (bind (*sd, (struct sockaddr*) &addr, sizeof(addr)) < 0) {
    msclt_log (cm, "Cannot bind server socket");
    msclt_log ("Sys mesg", strerror(errno));
    close (*sd);
    return -1;
    }

 if (listen(*sd, 5) < 0) {
    msclt_log (cm, "ERROR listening for new request");
    msclt_log ("Sys mesg: ", strerror(errno));
    close (*sd);
    return -1;
    }
 
 return 0;
}
/*----------------------------------------------------------------*
 * read client name off the socket for archiving purpose. The
 * server required client to pass his/her name to the server as
 * part of the initialization process.
 *----------------------------------------------------------------*/
int mssvr_readClientName (int sd, char *clt_name)
{
 const char *cm = "mssvr_readClientName";

 if (msclt_read_string (sd, clt_name, CLIENT_NAME_LEN) == (-1)) {
    msclt_log (cm, "ERROR reading client's name from socket");
    return -1;
    }
 else return 0;
}  
/*----------------------------------------------------------------*
 * given an opened socket, read & return the request code
 * from the socket.
 *
 * return 0 if ERROR.
 * else return actual request.
 *----------------------------------------------------------------*/
XDR_u_long mssvr_readRequestCode (int sd)
{
 XDR_u_long req_code;
 const char *cm = "mssvr_readRequestCode";

 if (msclt_read_u_long(sd, &req_code)) {
    msclt_log (cm, "ERROR reading request code from socket");
    return ((XDR_u_long) 0);
    }
 return req_code; 
}
/*----------------------------------------------------------------*
 * This is called once a new client is accepted and it's ready
 * to accept a new request. Here is the protocol,
 *
 *	1. client send his/her name (20 chars).
 *	2. send request code (4bytes XDR_u_long value).
 *	3. send needed parameters.
 * 
 * Get client name off the socket.
 * Get the client host.
 * Write client info into SPICE_LOG.
 * Read request code off socket.
 * Log client's request to SPICE_LOG.
 *----------------------------------------------------------------*/
int mssvr_handleRequest (int sd)
{
 XDR_u_long	req_code;
 char	clt_info[256];
 char	clt_name[CLIENT_NAME_LEN];
 socklen_t		len;
 struct sockaddr	addr;
 struct sockaddr_in	*addrptr;
 struct hostent		*hp;

 const char *cm = "mssvr_handleRequest";

 len = sizeof (addr);
 if (getpeername(sd, &addr, &len) == (-1)) {
    msclt_log (cm, "ERROR getting peer socket info");
    msclt_log (cm, "Terminating client connection");
    return -1;
    }

 addrptr = (struct sockaddr_in*) &addr;
 if (!(hp = (struct hostent*) gethostbyaddr(
		(char*) &addrptr->sin_addr,
		(int) 4, (int) AF_INET))) {
    mssvr_write_err_log (sd, cm,
	"ERROR getting client host info");
    return -1;
    }

 memset ((void*) clt_name, '\0', CLIENT_NAME_LEN);
 if (msclt_is_readable(sd) &&
     mssvr_readClientName (sd, clt_name)) {
    mssvr_write_err_log (sd, cm,
	"ERROR reading client's name from socket");
    return -1;
    }

 sprintf (clt_info, "%s@%s", clt_name, hp->h_name);
 msclt_log (cm, clt_info);

 if (	msclt_is_readable(sd) &&
	(req_code = mssvr_readRequestCode(sd)) == 0) {
    mssvr_write_err_log (sd, cm,
	"ERROR reading client's request code");
    return -1;
    }

 switch (req_code) {
    case GLL_GETSPICE:
        msclt_log (cm, "Receiving GLL_GETSPICE request");
	mssvr_gllgetspice (sd);
        break;
    case GLL_PUTSPICE:
        msclt_log (cm, "Receiving GLL_PUTSPICE request");
	mssvr_gllputspice(sd);
        break;
    case GLL_GET_CK:
        msclt_log (cm, "Receiving GLL_GET_CK request");
	mssvr_gllsendck (sd);
        break;
    case GLL_PUT_CK:
        msclt_log (cm, "Receiving GLL_PUT_CK request");
	mssvr_gllreceiveck (sd);
        break;
    case GLL_GET_SPK:
        msclt_log (cm, "Receiving GLL_GET_SPK request");
	mssvr_gllsendspk (sd);
        break;
    case GLL_PUT_SPK:
        msclt_log (cm, "Receiving GLL_PUT_SPK request");
	mssvr_gllreceivespk(sd);
        break;

    case CAS_GETSPICE:
        msclt_log (cm, "Receiving CAS_GETSPICE request");
	mssvr_casgetspice (sd);
        break;
    case CAS_PUTSPICE:
	msclt_log (cm, "Receiving CAS_PUTSPICE request");
        mssvr_casputspice(sd);
        break;
    case CAS_GET_CK:
        msclt_log (cm, "Receiving CAS_GET_CK request");
	mssvr_cassendck (sd);
        break;
    case CAS_PUT_CK:
        msclt_log (cm, "Receiving CAS_PUT_CK request");
	mssvr_casreceiveck (sd);
        break;
    case CAS_GET_SPK:
        msclt_log (cm, "Receiving CAS_GET_SPK request");
	mssvr_cassendspk (sd);
        break;
    case CAS_PUT_SPK:
        msclt_log (cm, "Receiving CAS_PUT_SPK request");
	mssvr_casreceivespk(sd);
        break;

    case SIM_GETSPICE:
        msclt_log (cm, "Receiving SIM_GETSPICE request");
	mssvr_simgetspice (sd);
        break;
    case SIM_PUTSPICE:
        msclt_log (cm, "Receiving SIM_PUTSPICE request");
	mssvr_simputspice(sd);
        break;
    case SIM_GET_CK:
        msclt_log (cm, "Receiving SIM_GET_CK request");
	mssvr_simsendck (sd);
        break;
    case SIM_PUT_CK:
        msclt_log (cm, "Receiving SIM_PUT_CK request");
	mssvr_simreceiveck (sd);
        break;
    case SIM_GET_SPK:
        msclt_log (cm, "Receiving SIM_GET_SPK request");
	mssvr_simsendspk (sd);
        break;
    case SIM_PUT_SPK:
        msclt_log (cm, "Receiving SIM_PUT_SPK request");
	mssvr_simreceivespk(sd);
        break;

    case VGR1_GETSPICE:
        msclt_log (cm, "Receiving VGR1_GETSPICE request");
	mssvr_vgr1getspice (sd);
        break;
    case VGR1_PUTSPICE:
        msclt_log (cm, "Receiving VGR1_PUTSPICE request");
	mssvr_vgr1putspice(sd);
        break;
    case VGR1_GET_CK:
        msclt_log (cm, "Receiving VGR1_GET_CK request");
	mssvr_vgr1sendck (sd);
        break;
    case VGR1_PUT_CK:
        msclt_log (cm, "Receiving VGR1_PUT_CK request");
	mssvr_vgr1receiveck (sd);
        break;
    case VGR1_GET_SPK:
        msclt_log (cm, "Receiving VGR1_GET_SPK request");
	mssvr_vgr1sendspk (sd);
        break;
    case VGR1_PUT_SPK:
        msclt_log (cm, "Receiving VGR1_PUT_SPK request");
	mssvr_vgr1receivespk(sd);
        break;

    case VGR2_GETSPICE:
        msclt_log (cm, "Receiving VGR2_GETSPICE request");
	mssvr_vgr2getspice (sd);
        break;
    case VGR2_PUTSPICE:
        msclt_log (cm, "Receiving VGR2_PUTSPICE request");
	mssvr_vgr2putspice(sd);
        break;
    case VGR2_GET_CK:
        msclt_log (cm, "Receiving VGR2_GET_CK request");
	mssvr_vgr2sendck (sd);
        break;
    case VGR2_PUT_CK:
        msclt_log (cm, "Receiving VGR2_PUT_CK request");
	mssvr_vgr2receiveck (sd);
        break;
    case VGR2_GET_SPK:
        msclt_log (cm, "Receiving VGR2_GET_SPK request");
	mssvr_vgr2sendspk (sd);
        break;
    case VGR2_PUT_SPK:
        msclt_log (cm, "Receiving VGR2_PUT_SPK request");
	mssvr_vgr2receivespk(sd);
        break;

    case VO1_GETSPICE:
        msclt_log (cm, "Receiving VO1_GETSPICE request");
	mssvr_vo1getspice (sd);
        break;
    case VO1_PUTSPICE:
        msclt_log (cm, "Receiving VO1_PUTSPICE request");
	mssvr_vo1putspice(sd);
        break;
    case VO1_GET_CK:
        msclt_log (cm, "Receiving VO1_GET_CK request");
	mssvr_vo1sendck (sd);
        break;
    case VO1_PUT_CK:
        msclt_log (cm, "Receiving VO1_PUT_CK request");
	mssvr_vo1receiveck (sd);
        break;
    case VO1_GET_SPK:
        msclt_log (cm, "Receiving VO1_GET_SPK request");
	mssvr_vo1sendspk (sd);
        break;
    case VO1_PUT_SPK:
        msclt_log (cm, "Receiving VO1_PUT_SPK request");
	mssvr_vo1receivespk(sd);
        break;

    case VO2_GETSPICE:
        msclt_log (cm, "Receiving VO2_GETSPICE request");
        mssvr_vo2getspice (sd);
        break;
    case VO2_PUTSPICE:
        msclt_log (cm, "Receiving VO2_PUTSPICE request");
        mssvr_vo2putspice(sd);
        break;
    case VO2_GET_CK:
        msclt_log (cm, "Receiving VO2_GET_CK request");
        mssvr_vo1sendck (sd);
        break;
    case VO2_PUT_CK:
        msclt_log (cm, "Receiving VO2_PUT_CK request");
        mssvr_vo2receiveck (sd);
        break;
    case VO2_GET_SPK:
        msclt_log (cm, "Receiving VO2_GET_SPK request");
        mssvr_vo2sendspk (sd);
        break;
    case VO2_PUT_SPK:
        msclt_log (cm, "Receiving VO2_PUT_SPK request");
        mssvr_vo2receivespk(sd);
        break;

    default:
	sprintf (clt_info, "%s\n%s: %d\n%s",
		"Unknown request code received",
		"Request value", req_code,
		"Terminating client connection");
        mssvr_write_err_log (sd, cm, clt_info);
	return -1;
        break;
    }

 return 0;
}
/*--------------------------------------------------------------------------*/
void mssvr_gllgetspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 mssvr_getspice (sd, &env);
} 
/*--------------------------------------------------------------------------*/
void mssvr_gllputspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_gllsendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_gllsendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_gllreceiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_gllreceivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getgllenv (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_casgetspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 mssvr_getspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_casputspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_cassendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_cassendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_casreceiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_casreceivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getcasenv (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_simgetspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 mssvr_getspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_simputspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_simsendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_simsendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_simreceiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_simreceivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getsimenv (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr1getspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 mssvr_getspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr1putspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr1sendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr1sendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr1receiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr1receivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr1env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr2getspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 mssvr_getspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr2putspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr2sendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr2sendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr2receiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vgr2receivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvgr2env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo1getspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 mssvr_getspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo1putspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo1sendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo1sendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo1receiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo1receivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo1env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo2getspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 mssvr_getspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo2putspice (int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 mssvr_putspice (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo2sendck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo2sendspk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 mssvr_send_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo2receiveck(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
void mssvr_vo2receivespk(int sd)
{
 msEnvStruct env;
 memset((void*) &env, '\0', sizeof(env));
 mslcl_getvo2env (&env);
 mssvr_receive_kernel (sd, &env);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*
 * The request code should be read off the socket by now. The <env>
 * contain mission specific according to the request code. the
 * calling function will create appropriate <env> and passed it in here.
 *
 * This piece of code should be used/debugged along the its msclt_getspice()
 * counterpart.
 *
 * About socket termination, if the server encounters some type of error,
 * it will write error message to the client & close it. It does not write
 * OPS_COMPLETED message.
 *
 * 1. read request structure off the socket. This is useful only for
 *	doing getSpice().
 * 2. read CK data from kernel
 * 3. read SPK data from kernel
 *
 *--------------------------------------------------------------------------*/
void mssvr_getspice (int sd, msEnvStruct *env)
{
 msCkStruct	ckdata;
 msSpkStruct	spkdata;
 int		status  = 0;
 msUserRequestStruct req;
 const char *cm = "mssvr_getspice";

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 memset ((void*) &req, '\0', sizeof(req));

 /* writing messages back to clients so that they will know */
 /* what is going on.					    */
 mssvr_write_info_log (sd, cm, "Kernel read....initializing", 1);
 mssvr_write_info_log (sd, cm, "Loading support kernels .....", 1);

 if (mslcl_load_support_kernels(env)) {
    mssvr_write_err_log (sd, cm,
	"ERROR loading support kernels");
    return;
    }

 if (msclt_is_readable(sd) &&
     msclt_read_req_struct (sd, &req)) {
    mssvr_write_err_log (sd, cm,
	"ERROR reading client's request from socket");
    zms_clpool();
    return;
    }

 ckdata.system	= req.system;
 ckdata.sc_id = spkdata.sc_id = req.sc_id;
 memcpy ((void*) ckdata.scet,
	(void*)req.scet, sizeof(int) * 6);
 
 if (t_mssvr_readckdata(sd, env, &req, &ckdata)) {
    mssvr_write_info_log (sd, cm,
	"ERROR reading CK data from kernel", 1);
    status = -1;
    }
 else if (mssvr_readspkdata(sd, env,
	&req, &ckdata, &spkdata)) {
    mssvr_write_info_log (sd, cm,
	"ERROR reading SPK data from kernel", 1);
    status = -1;
   }

 zms_clpool ();

 if (status) mssvr_write_err_log (sd, cm,
	"Terminating client connection");
 else if (msclt_write_u_long(sd, SVR_CK_DATA)	||	/* write ck rsp code */
	  msclt_write_ck_struct(sd, &ckdata)	||	/* write ckstruct    */
	  msclt_write_u_long(sd, SVR_SPK_DATA)	||	/* write spk rsp code*/
	  msclt_write_spk_struct(sd, &spkdata)	||	/* write spkstruct   */
	  msclt_write_u_long (sd, OPS_COMPLETED))	/* write compl code  */
    msclt_log (cm, "ERROR writing completion code to client");
}
/*--------------------------------------------------------------------------*
 * - copy scet from request to <ckdata>
 * - create an utc_str from the <scet> in <req>
 * - convert the <utc_str> to <ephem_time> value.
 *--------------------------------------------------------------------------*/
int t_mssvr_readckdata (int sd, msEnvStruct *env,
		msUserRequestStruct *req, msCkStruct *ckdata)
{
 int	status,
	tol_count = 0;
 double	tol_value = NUMERIC_INIT_TOL;
 msUserRequestStruct treq;
 const char *cm = "t_mssvr_readckdata";

 while (tol_count < MAX_TOL_TRIES) {
    memcpy ((void*)&treq, (void*)req, sizeof(treq));
    status = mssvr_read_ckrecord (sd,
		env, &treq, ckdata, tol_value);
    if (status == 1) {				/* segid is at lowest	*/
       tol_count++;				/* priority and still	*/
       tol_value *= 2;				/* cannot find record	*/
       mssvr_write_info_log (sd, cm,
        "Increasing tolerance value....", 1);
       }
    else if (status == (-1)) {			/* have problem reading	*/
       mssvr_write_info_log (sd, cm,		/* the kernel.		*/
	"ERROR reading CKernels....", 1);	/* Have to return with	*/
       mssvr_write_info_log (sd, cm,		/* ERR....		*/
	"Returning with no data....", 1);
       return -1;
       }
    else if (status == 0) return 0;
    }

 /* if you've gotten here, you have exceeded the allowable tolerance	*/
 /* cound....Can't really do anything else here. Just return ERROR	*/
 /* code and ERROR message.						*/
 mssvr_write_info_log (sd, cm, "ERROR:max'ed out tolerance count...", 1);
 mssvr_write_info_log (sd, cm, "Data is still not found....", 1);
 mssvr_write_info_log (sd, cm, "Returning with no data.....", 1);
 return -1;
}


static int naif_id_of(const char* name)
{
  if ((strncmp(name, CASISSNA_STR, 4) == 0) || 
      (strncmp(name, CASISSNA_SUM22_STR, 4) == 0) ||      
      (strncmp(name, CASISSNA_SUM44_STR, 4) == 0))
    {
      return CASISSNA_NAIF_ID;
    }
  else if ((strncmp(name, CASISSWA_STR, 4) == 0) ||
	   (strncmp(name, CASISSWA_SUM22_STR, 4) == 0) ||
	   (strncmp(name, CASISSWA_SUM44_STR, 4) == 0))
    {
      return CASISSWA_NAIF_ID;
    }
  else
    {
      return -1;
    }
}


/*--------------------------------------------------------------------------*/
int mssvr_read_ckrecord (int sd, msEnvStruct *env,
	msUserRequestStruct *req, msCkStruct *ckdata, double tol)
{
  int   i,
	camera_sn,
        handle,
        use_prov,
        sns_fnd,
        pfs_fnd,
        found_once,
	ck_count = 0,
        icd[NUM_CK_INTEGERS];
 double sclkin,
        sclk_out,
        rad_per_pix,
        descr[NUM_CK_DESCR],
        dcd[2],
        rot[3][3];
 float  focal_mm,
        opaxis_line,
        opaxis_samp,
        scale;
 char   segid[MAX_SEG_ID_LEN];

 kernelInfoStruct kinfo[MAX_KERNEL_COUNT];
 const char *cm = "mssvr_read_ckrecord";

 memset ((void*) segid, '\0', MAX_SEG_ID_LEN);
 memset ((void*) descr, '\0', sizeof(double) * NUM_CK_DESCR);

 use_prov = mslcl_use_prov_info (req->provInfo.seg_id);
 
 if (zcam_info (ckdata->sc_id, req->instrument_name,
        &ckdata->instrument, &camera_sn,
        &rad_per_pix, &focal_mm, &opaxis_line,
        &opaxis_samp, &scale)) {
    mssvr_write_info_log (sd, cm,
        "ERROR getting request's camera info", 1);
    return -1;
    }

 if (mslcl_scet2sclk (ckdata->sc_id, ckdata->scet, &sclkin)) {
    mssvr_write_info_log (sd, cm,
        "ERROR getting request's sclk", 1);
    return -1;
    }
 
 memset ((void*) kinfo, '\0',				/* getting Ckernel  */
	sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT);	/* information.	    */
 ck_count = mslcl_ck_info_from_kdb(env, kinfo);
 if (ck_count == 0) {					/* check if you have*/
    mssvr_write_info_log (sd, cm,			/* any CK or if you */
        "ERROR:kdb has no CKernel", 1);			/* read the CKs info*/
    return -1;						/* SUCCESSFULLY.    */
    }
 else if (ck_count == (-1)) {
    mssvr_write_info_log (sd, cm,
        "ERROR reading ck_info from kerneldb", 1);
    return -1;
    }

 mssvr_write_info_log (sd, cm,			/* if you've gotten here, */
	"Loading targeted ckernels....", 1);	/* you've read the kernel */
 if (mslcl_loadCk (sd, req, kinfo, ck_count)) { /* info. Now try loading  */
    mssvr_write_info_log (sd, cm,		/* them. <mslcl_loadCk>   */
	"ERROR loading CKernels....", 1);	/* will decides how to    */
    mssvr_write_info_log (sd, cm,		/* load CKs according to  */
	"Returning with no CK data", 1);	/* <req>.		  */
    return -1;
    }

 do {
    zms_ckbss (ckdata->instrument, sclkin, tol, 0);
    if (zms_failed()) {
       mssvr_write_info_log (sd, cm,
        "ERROR starting segment search", 1);
       mssvr_write_info_log (sd, cm, "zms_ckbss() failed", 1);
       mslcl_unloadCk (kinfo, ck_count);
       return -1;
       }
 
    do {
       sns_fnd = 0;
       zms_cksns (&handle, descr, segid, &sns_fnd);
       if (zms_failed()) {
          mssvr_write_info_log (sd, cm,
                "ERROR searching for segment", 1);
          mssvr_write_info_log (sd, cm, "zms_cksns() failed", 1);
	  mslcl_unloadCk (kinfo, ck_count);
          return -1;
          }
 
       /* if you got here, then you have found a data           */
       /* record. Need to make sure the segid is what you       */
       /* want and that you can evaluate the returned           */
       /* segment.                                              */
       /*                                                       */
       /* If don't need to use prov or that the segid(s)        */
       /* are identical, then you can start to evaluate         */
       /* the returned segment now.                             */
       /*                                                       */
       /* before returning, make sure you print some kind of    */
       /* message to tell remote clients that you have found    */
       /* the target record.                                    */

       /* txh::moved out of the 'if' condition and modified the       */
       /*      'if' condition to avoid invalid metrix data retrieval. */
       if (sns_fnd)
          zms_ckpfs (handle, descr, sclkin, tol, 0,
                ckdata->c_matrix, ckdata->av, &sclk_out,
                &pfs_fnd);

       if (sns_fnd && !zms_failed() && pfs_fnd && ( !use_prov ||
            mslcl_identical_segid( req->provInfo.seg_id, segid)) ) {
 
             zms_dafus (descr, NUM_CK_DOUBLES,
                NUM_CK_INTEGERS, dcd, icd);
             if (zms_failed()) {
                mssvr_write_info_log (sd, cm,
                        "Cannot unpack segment descriptor", 1);
	        mslcl_unloadCk (kinfo, ck_count);
                return -1;
                }
 
             /* The retrieved C_MATRIX is in NAIF convention.   */
	     /* Before doing anything, we need to convert it    */
             /* into MIPS convention.                           */
             zms_xpose (ckdata->c_matrix, ckdata->c_matrix);
             /* If the returned data is not in the same ref     */
             /* coordinate system, then we will need to rotate  */
             /* both the return Angular Velocity & C_MATRIX     */
             /*                                                 */
             /* In this case, <ckdata->system> is your target   */
             /* system while <icd[1]> is your returned system   */
             /*                                                 */
             /* 1. need to get a rotational matrix.             */
             /* 2. rotate C_MATRIX                              */
             /* 3. rotate Angular Velocity.                     */
             if (ckdata->system != icd[1]) {
                zms_irfrot (ckdata->system, icd[1], rot);
                if (zms_failed()) {
                   mssvr_write_info_log (sd, cm,
                        "ERROR getting rotation matrix", 1);
	           mslcl_unloadCk (kinfo, ck_count);
                   return -1;
                   }
                zms_mxm (ckdata->c_matrix, rot, ckdata->c_matrix);
                if (zms_failed()) {
                   mssvr_write_info_log (sd, cm,
                        "Cannot rotate C_MATRIX", 1);
	           mslcl_unloadCk (kinfo, ck_count);
                   return -1;
                   }
                zms_mxv (rot, ckdata->av, ckdata->av);
                if (zms_failed()) {
                   mssvr_write_info_log (sd, cm,
                        "Cannot rotate Angular Velocity", 1);
	           mslcl_unloadCk (kinfo, ck_count);
                   return -1;
                   }
                }


	     /* If this is a Cassini query, then we have the c-matrix of
	      * the instrument platform.  We need to transform it 
	      * to the c-matrix of the instrument.
	      */
	     if (req->sc_id == CAS_SC_ID)
	     {
	       int inst_id = naif_id_of(req->instrument_name);
	       if (inst_id == -1)
	       {
		 mssvr_write_info_log (sd, cm,
		    "ERROR Invalid Cassini instrument name.", 1);
		 mslcl_unloadCk (kinfo, ck_count);
		 return -1;
	       }
	       else
	       {
		 double rot[3][3] = {0};
		 int frame = 0;
		 int wasFound = 0;
		 zms_tkfram(inst_id, rot, &frame, &wasFound);
		 if (!wasFound)
		   {
		     mssvr_write_info_log (sd, cm,
			"ERROR Instrument frame not found.", 1);
		     mslcl_unloadCk (kinfo, ck_count);
		     return -1;
		   }
		 else
		   {
		     double mat180[3][3] = {0};

		     zms_xpose(rot, rot);
		     zms_rotate(zms_pi(), 3, mat180);
		     zms_mxm(mat180, rot, rot);
		     zms_mxm(rot, ckdata->c_matrix, ckdata->c_matrix);
		   }
	       }
	     }
 
             /* this is a good segid...Get all needed data    */
	     /* over to ckdata....before cleanning up & return*/
             strcpy (ckdata->seg_id, segid);
             mssvr_write_info_log (sd, cm,
                        "Requested data found!!!!", 1);
             mssvr_write_info_log (sd, "Requested segid",
                        req->provInfo.seg_id, 1);
             mssvr_write_info_log (sd, "Returned segid ", segid, 1);
	     for (i = 0; i < ck_count; i++) {
		 if (kinfo[i].handle == handle) {
		    strcpy (ckdata->ck_id, kinfo[i].id);
		    strcpy (ckdata->ck_source, kinfo[i].source);
		    strcpy (ckdata->ck_name, kinfo[i].fname);
		    }
		 }

	     mslcl_unloadCk (kinfo, ck_count);
             return 0;
             }
          
       } while (sns_fnd);
       /* keep searching as long as */
       /* you've found something    */
 
    mssvr_write_info_log (sd, cm, "Lowering segid !!!", 1);
    } while (mslcl_lowerSegidPriority (req->provInfo.seg_id));
    /* keep lowering the segid until you can not lower it */
    /* anymore. this is where you get out of the loop     */
 
 /* if you get here, then no more data to */
 /* search. Just return 1 so that the     */
 /* calling function can inscrease the    */
 /* tolerance value or something.         */
 mslcl_unloadCk (kinfo, ck_count);
 return 1;
}
/*--------------------------------------------------------------------------*
 * - copy scet from request to <ckdata>
 * - create an utc_str from the <scet> in <req>
 * - convert the <utc_str> to <ephem_time> value.
 *--------------------------------------------------------------------------*/
int mssvr_readckdata(int sd, msEnvStruct *env,
		msUserRequestStruct *req, msCkStruct *ckdata)
{
 const char *cm = "mssvr_readckdata";

 /* writing informational message back to clients so that */
 /* they will know what the server is doing....		  */
 mssvr_write_info_log (sd, cm,
	"Starting remote ckernel read ....", 1);

 if (	strlen(req->ck_name) &&
	mssvr_readck_by_name(sd, env, req, ckdata) ) {
    mssvr_write_info_log (sd, cm, "ERROR reading CK by name", 1);
    mssvr_write_info_log (sd, "ck_name", req->ck_name, 1);
    return -1; 
    }
 else if (strlen(req->ck_id) &&
	mssvr_readck_by_id (sd, env, req, ckdata) ) {
    mssvr_write_info_log (sd, cm, "ERROR reading CK by id", 1);
    mssvr_write_info_log (sd, "ck_id", req->ck_id, 1);
    return -1;
    }
 else if (strlen(req->ck_source) &&
	mssvr_readck_by_source (sd, env, req, ckdata) ) {
    mssvr_write_info_log (sd, cm, "ERROR reading CK by source", 1);
    mssvr_write_info_log (sd, "ck_source", req->ck_source, 1);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*
 * 1. search all kernels with the same source
 * 2. search all kernels with diff source but not NAIF
 * 3. search all kernel with NAIF source.
 *--------------------------------------------------------------------------*/
int mssvr_readck_by_source (int sd, msEnvStruct *env,
	msUserRequestStruct *req, msCkStruct *ckdata)
{
 double	tol,
	ttol,
	et,
	tb_et,
	te_et;
 int	status,
	tol_tries,
	ck_count  = 0,
	tck_count = 0;
 char	utc_str[256];

 msUserRequestStruct treq;
 kernelInfoStruct ckinfo[256];

 const char *cm = "mssvr_readck_by_source";
 memset ((void*) &treq, '\0', sizeof(treq));

 /* writing informational message back to clients so that they */
 /* will know what is going on on the server side.	       */
 mssvr_write_info_log (sd, cm, "Reading ckernel by source....", 1);
 mssvr_write_info_log (sd, "Requested source", req->ck_source, 1);

 memset ((void*) utc_str, '\0', 256);
 sprintf (utc_str, "%04d-%03d//%02d:%02d:%02d.%03d",
        req->scet[0], req->scet[1], req->scet[2],
        req->scet[3], req->scet[4], req->scet[5]);
 zms_utc2et (utc_str, &et);
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting user ephem_time", 1);
    return -1;
    }

 ck_count = mslcl_ck_info_from_kdb(env, ckinfo);
 if (ck_count == 0) {
    mssvr_write_info_log (sd, cm,
	"ERROR:kdb has no CKernel", 1);
    return -1; 
    }
 else if (ck_count == (-1)) { 
    mssvr_write_info_log (sd, cm,
	"ERROR reading ck_info from kerneldb", 1);
    return -1;
    }

/*******************
 zms_sctiks (req->sc_id, INITIAL_TOL_VAL, &tol);
*******************/
 tol = NUMERIC_INIT_TOL;
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting initial tolerance value", 1);
    return -1;
    }

 /* search all CK with the same source & with */
 /* appropriate ET range.		      */
 ttol = tol;
 tol_tries = 0;
 tck_count = ck_count;
 while (tol_tries < MAX_TOL_TRIES) {
    while (--tck_count >= 0) {
	tb_et = ckinfo[tck_count].et_begin - 60.0;
	te_et = ckinfo[tck_count].et_end * 1.0;
	if ( ((et > tb_et) && (et < te_et)) &&
	     (!strcmp(req->ck_source, ckinfo[tck_count].source))) {

	   memcpy ((void*) &treq, (void*)req, sizeof(treq));
	   status = mssvr_readckfromfile (sd, &treq, ckdata,
			ttol, ckinfo[tck_count].fname);
	   if (status == (-1)) {
	      mssvr_write_info_log (sd, cm,
		"ERROR reading CKernel", 1);
	      return status;
	      }
	   else if (status == 0) {
	      strcpy (ckdata->ck_id, ckinfo[tck_count].id);
	      strcpy (ckdata->ck_name, ckinfo[tck_count].fname);
	      strcpy (ckdata->ck_source, ckinfo[tck_count].source);
	      return 0;
	      }
	   }
	}

    ttol *= 2.0;
    tol_tries++;
    tck_count = ck_count;
    }

 /* search all CK with different source but not with */
 /* source equal to NAIF or SEDR		     */
 ttol = tol;
 tol_tries = 0;
 tck_count = ck_count;
 while (tol_tries < MAX_TOL_TRIES) {
    while (--tck_count >= 0) {
	tb_et = ckinfo[tck_count].et_begin - 60;
	te_et = ckinfo[tck_count].et_end * 1.0;
	if ( ((et > tb_et) && (et < te_et)) &&
	     (strcmp(req->ck_source, ckinfo[tck_count].source)) &&
	     (strcmp(ckinfo[tck_count].source, "NAIF")) &&
             (strcmp(ckinfo[tck_count].source, "SEDR")) ) {

	   memcpy ((void*) &treq, (void*)req, sizeof(treq));
	   status = mssvr_readckfromfile (sd, &treq, ckdata,
                        ttol, ckinfo[tck_count].fname);
           if (status == (-1)) {
              mssvr_write_info_log (sd, cm,
                "ERROR reading CKernel", 1);
              return status;
              }
           else if (status == 0) {
	      strcpy (ckdata->ck_id, ckinfo[tck_count].id);
	      strcpy (ckdata->ck_name, ckinfo[tck_count].fname);
	      strcpy (ckdata->ck_source, ckinfo[tck_count].source);
              return 0;
	      }
	   }
	}

    ttol *= 2.0;
    tol_tries++;
    tck_count = ck_count;
    }

 /* search all CK with source equals to "NAIF" or "SEDR" */
 ttol = tol;
 tol_tries = 0;
 tck_count = ck_count;
 while (tol_tries < MAX_TOL_TRIES) {
    while (--tck_count >= 0) {
        tb_et = ckinfo[tck_count].et_begin - 60;        
        te_et = ckinfo[tck_count].et_end * 1.0;
        if ((et > tb_et) && (et < te_et) &&
	    (!strcmp(ckinfo[tck_count].source, "NAIF") ||
             !strcmp(ckinfo[tck_count].source, "SEDR")) ) {

	   memcpy ((void*) &treq, (void*)req, sizeof(treq));
           status = mssvr_readckfromfile (sd, &treq, ckdata,
                     ttol, ckinfo[tck_count].fname);
           if (status == (-1)) {
              mssvr_write_info_log (sd, cm,
                   "ERROR reading CKernel", 1);
              return status;
              }
           else if (status == 0) {
	      strcpy (ckdata->ck_id, ckinfo[tck_count].id);
	      strcpy (ckdata->ck_name, ckinfo[tck_count].fname);
	      strcpy (ckdata->ck_source, ckinfo[tck_count].source);
              return 0;
              }
           }
        }

    ttol *= 2.0;
    tol_tries++;
    tck_count = ck_count;
    }

 mssvr_write_info_log (sd, cm, "Cannot find requested data record", 1);
 mssvr_write_info_log (sd, cm, "Returning with no data from server", 1);
 return -1;
}
/*--------------------------------------------------------------------------*/
int mssvr_readck_by_id (int sd, msEnvStruct *env,
	msUserRequestStruct *req, msCkStruct *ckdata)
{
 int	i, status;
 char	ckfname[256];
 double	tol;

 kernelInfoStruct kinfo;
 msUserRequestStruct treq;

 const char *cm = "mssvr_readck_by_id";
 memset ((void*) ckfname, '\0', 256);
 memset ((void*) &kinfo, '\0', sizeof(kinfo));

 /* writing informational message back to clients so that they */
 /* will know what is going on on the server side.	       */
 mssvr_write_info_log (sd, cm, "Reading CKernel by id....", 1);
 mssvr_write_info_log (sd, "Requested ck_id", req->ck_id, 1);

 if (mslcl_kid_2_kinfo (env, req->ck_id, &kinfo)) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting CKINFO from id", 1);
    mssvr_write_info_log (sd, "ck_id ", req->ck_id, 1);
    return -1;
    }

/**************************
 zms_sctiks (req->sc_id, INITIAL_TOL_VAL, &tol);
****************************/
 tol = NUMERIC_INIT_TOL;

 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting initial tolerance value", 1);
    return -1;
    }

  for (i = 0; i < MAX_TOL_TRIES; i++) {

    memcpy ((void*) &treq, (void*)req, sizeof(treq));
    status = mssvr_readckfromfile (sd,
	&treq, ckdata, tol, kinfo.fname);
 
    if (status == (-1)) {                               /* kernel read ERR */
       mssvr_write_info_log (sd, cm,
        "ERROR reading data from kernel", 1);
       return -1;
       }
    else if (status == 0) {	                        /* data found      */
       strcpy (ckdata->ck_id, kinfo.id);
       strcpy (ckdata->ck_name, kinfo.fname);
       strcpy (ckdata->ck_source, kinfo.source);
       return 0;
       }
    else if (status == 1) tol *= (double) 2;            /* data not found  */
    }

 /* got here if you have max'ed out the tolerance value. */
 /* This is going too far from user requested sclk.      */
 /* Need to return ERR.					 */
 mssvr_write_info_log (sd, cm,
	"ERROR:max'ed out tolerance value", 1);
 return -1;
}
/*--------------------------------------------------------------------------*/
int mssvr_readck_by_name (int sd, msEnvStruct *env,
		msUserRequestStruct *req, msCkStruct *ckdata)
{
 int	i,
	status;
 double	tol;
 char	ckfname[256];

 kernelInfoStruct kinfo;
 msUserRequestStruct treq;

 const char *cm = "mssvr_readck_by_name";

 memset ((void*) ckfname, '\0', 256);
 memset ((void*) &kinfo, '\0', sizeof(kinfo));

 /* writing informational message back to clients so that they */
 /* will know what is going on on the server side.	       */
 mssvr_write_info_log (sd, cm, "Reading CKernel by name....", 1);

 if (mslcl_kname_2_kinfo(env, req->ck_name, &kinfo)) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting CKINFO from CK_NAME", 1);
    mssvr_write_info_log (sd, "ck_name", req->ck_name, 1);
    return -1;
    }

/************************
 zms_sctiks (req->sc_id, INITIAL_TOL_VAL, &tol);
************************/
 tol = NUMERIC_INIT_TOL;
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting initial tolerance value", 1);
    return -1;
    }

 for (i = 0; i < MAX_TOL_TRIES; i++) {

    memcpy ((void*) &treq, (void*)req, sizeof(treq));
    status = mssvr_readckfromfile (sd,
	&treq, ckdata, tol, kinfo.fname);

    if (status == (-1)) {				/* kernel read ERR */
       mssvr_write_info_log (sd, cm,
	"ERROR reading data from kernel", 1);
       return -1;
       }
    else if (status == 0) {	                        /* data found      */
       strcpy (ckdata->ck_id, kinfo.id);
       strcpy (ckdata->ck_name, kinfo.fname);
       strcpy (ckdata->ck_source, kinfo.source);
       return 0;
       }
    else if (status == 1) tol *= (double) 2;		/* data not found  */
    }

 /* got here if you have max'ed out the tolerance value. */
 /* This is going too far from user requested sclk.      */
 /* Need to return ERR.					 */
 mssvr_write_info_log (sd, cm,
	"ERROR:max'ed out tolerance value", 1);
 return -1;
}
/*--------------------------------------------------------------------------*
 * This assumes that all supporting kernels are loaded. All you need to do
 * is to load the ck, try to extract the data from it, and unload the ck.
 *
 * make sure you copy whatever you need from <req> to <ckdata> before
 *	calling this. (sc_id, system, scet)
 *
 * - check if we need to use prov_info.
 * - get camera_id from <ckdata>
 * - get sclk from <ckdata>
 * - load the target ckernel
 * - call ckbss to start segment search
 * - call cksns to do the search
 * - if found data make sure you have good segid also
 * - if has data but not good segid, then just return 0. This is the
 *	same thing as not found any data.
 * - if error occur when search & or setting up search you will need to
 *	return -1, to signal calling function to stop calling it.
 *
 * return -1: if ERROR occurred
 * return  0: if data found
 * return  1: if data not found
 *--------------------------------------------------------------------------*/
int mssvr_readckfromfile (int sd, msUserRequestStruct *req,
		msCkStruct *ckdata, double tol, char *ckfname)
{
 int	camera_sn,
 	handle,
 	nhandle,
	use_prov,
	sns_fnd,
	pfs_fnd,
	found_once,
	icd[NUM_CK_INTEGERS];
 double	sclkin,
	sclk_out,
	rad_per_pix,
	descr[5],
	dcd[2],
	rot[3][3];
 float	focal_mm,
	opaxis_line,
	opaxis_samp,
	scale;
 char	segid[MAX_SEG_ID_LEN];

 const char *cm = "mssvr_readckfromfile";

 memset ((void*) segid, '\0', MAX_SEG_ID_LEN);
 memset ((void*) descr, '\0', sizeof(double) * 5);
 mssvr_write_info_log (sd, "ckname", ckfname, 1);

 use_prov = mslcl_use_prov_info (req->provInfo.seg_id);

 if (zcam_info (ckdata->sc_id, req->instrument_name,
	&ckdata->instrument, &camera_sn,
	&rad_per_pix, &focal_mm, &opaxis_line,
	&opaxis_samp, &scale)) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting request's camera info", 1);
    return -1;
    }
 
 if (mslcl_scet2sclk (ckdata->sc_id, ckdata->scet, &sclkin)) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting request's sclk", 1);
    return -1;
    }

 zms_cklpf (ckfname, &handle);
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
    "ERROR loading target CK", 1);
    return -1;
    }

 do {
    zms_ckbss (ckdata->instrument, sclkin, tol, 0);
    if (zms_failed()) {
       mssvr_write_info_log (sd, cm,
	"ERROR starting segment search", 1);
       mssvr_write_info_log (sd, cm, "zms_ckbss() failed", 1);
       zms_ckupf (handle);
       zms_ckcls (handle);
       return -1;
       }

    do {
       sns_fnd = 0;
       zms_cksns (&handle, descr, segid, &sns_fnd);
       if (zms_failed()) {
          mssvr_write_info_log (sd, cm,
		"ERROR searching for segment", 1);
          mssvr_write_info_log (sd, cm, "zms_cksns() failed", 1);
          zms_ckupf (handle);
          zms_ckcls (handle);
          return -1;
          }

       /* if you got here, then you have found a data		*/
       /* record. Need to make sure the segid is what you	*/
       /* want and that you can evaluate the returned		*/
       /* segment.						*/
       /*							*/
       /* If don't need to use prov or that the segid(s)	*/
       /* are identical, then you can start to evaluate		*/
       /* the returned segment now.				*/
       /*							*/
       /* before returning, make sure you print some kind of	*/
       /* message to tell remote clients that you have found	*/
       /* the target record.					*/

       /* txh::moved out of the 'if' condition and modified the       */
       /*      'if' condition to avoid invalid metrix data retrieval. */
       if (sns_fnd)
          zms_ckpfs (handle, descr, sclkin, tol, 0,
                ckdata->c_matrix, ckdata->av, &sclk_out,
                &pfs_fnd);
 
       if (sns_fnd && !zms_failed() && pfs_fnd && ( !use_prov ||
            mslcl_identical_segid( req->provInfo.seg_id, segid)) ) {
	     zms_dafus (descr, NUM_CK_DOUBLES,
		NUM_CK_INTEGERS, dcd, icd);
	     if (zms_failed()) {
		mssvr_write_info_log (sd, cm,
			"Cannot unpack segment descriptor", 1);
		zms_ckupf (handle);
		zms_ckcls (handle);
		return -1;
		}

	     /* The retrieved C_MATRIX is in NAIF convention.   */
	     /* Before doing anything, we need to convert it    */
	     /* into MIPS convention.				*/
	     zms_xpose (ckdata->c_matrix, ckdata->c_matrix); 
	     /* If the returned data is not in the same ref	*/
	     /* coordinate system, then we will need to rotate	*/
	     /* both the return Angular Velocity & C_MATRIX	*/
	     /*							*/
	     /* In this case, <ckdata->system> is your target	*/
	     /* system while <icd[1]> is your returned system	*/
	     /*							*/
	     /* 1. need to get a rotational matrix.		*/
	     /* 2. rotate C_MATRIX				*/
	     /* 3. rotate Angular Velocity.			*/
	     if (ckdata->system != icd[1]) {
		zms_irfrot (ckdata->system, icd[1], rot);
		if (zms_failed()) {
		   mssvr_write_info_log (sd, cm,
			"ERROR getting rotation matrix", 1);
		   zms_ckupf (handle);
		   zms_ckcls (handle);
		   return -1;
		   }
		zms_mxm (ckdata->c_matrix, rot, ckdata->c_matrix);
		if (zms_failed()) {
		   mssvr_write_info_log (sd, cm,
			"Cannot rotate C_MATRIX", 1);
		   zms_ckupf (handle);
		   zms_ckcls (handle);
		   return -1;
		   }
		zms_mxv (rot, ckdata->av, ckdata->av);
		if (zms_failed()) {
		   mssvr_write_info_log (sd, cm,
			"Cannot rotate Angular Velocity", 1);
		   zms_ckupf (handle);
		   zms_ckcls (handle);
		   return -1;
		   }
		}

	     /* this is a good segid...close up shop & return */
             strcpy (ckdata->seg_id, segid);
	     mssvr_write_info_log (sd, cm,
			"Requested data found!!!!", 1);
	     mssvr_write_info_log (sd, "Requested segid",
			req->provInfo.seg_id, 1);
	     mssvr_write_info_log (sd, "Returned segid ", segid, 1);
             zms_ckupf (handle);
	     zms_ckcls (handle);
             return 0;
	     }
       } while (sns_fnd);
       /* keep searching as long as */
       /* you've found something    */

    mssvr_write_info_log (sd, cm, "Lowering segid !!!", 1);
    } while (mslcl_lowerSegidPriority (req->provInfo.seg_id));
    /* keep lowering the segid until you can not lower it */
    /* anymore. this is where you get out of the loop	  */

 /* if you get here, then no more data to */
 /* search. Just return 1 so that the	  */
 /* calling function can inscrease the	  */
 /* tolerance value or something.	  */
 zms_ckupf (handle);
 zms_ckcls (handle);
 return 1;
}
/*--------------------------------------------------------------------------*/
int mssvr_readspkdata (int sd, msEnvStruct *env,
	msUserRequestStruct *request, msCkStruct *ckdata,
	msSpkStruct *spkdata)
{
 const char *cm = "mssvr_readspkdata";

 int            i = 0,
		t_handle = 0,
		status = 0,             /* status flag			  */
                found = 0,              /* flag used to lookup target_id  */
                camera_id = 0,          /* camera_id from name		  */
                camera_sn = 0,          /* different camera parameters	  */
                intersect = 0;

 float          focal_mm = 0.0,         /* this is mission (or camera)	  */
                opaxis_line = 0.0,      /* specific.			  */
                opaxis_samp = 0.0,
                scale = 0.0,
                tlat = 0.0,
                tlon = 0.0,
                req = 0.0,
                focal_pix = 0.0,
                flat = 0.0,
                flag = 0.0,
		sub_sc_line = 0.0,
		sub_sc_samp = 0.0;

 double         et = 0.0,		/* need this to determine which	   */
					/* kernel to load.		   */
		camera_rad_per_pixel = 0.0,

                sc_state[6],            /* sc states (pos & vel) relative  */
                                        /* to the solar system barry-center*/

                tgt_state[6],           /* state of the target relative to */
                                        /* solar system bary-center	   */

                tgt2sc_state[6],        /* state of the target relative to */
                                        /* the space-craft.		   */

                ntgt2sc_state[3],       /* negated tgt2sc_state		   */

                tgt2sun_bd_fix[3],
                tgt2sc_bd_fix[3],       /* sc vector relative to target	   */

                tgt2sc_lt = 0.0,       	/* "light-time" one-way distance   */
					/* from the target to the	   */
					/* space-craft at at given	   */
					/* ephemeris time.		   */

                cntr_state[6],          /* center (of sc & target) states  */
                                        /* relative to the solar system	   */
                                        /* barry-center			   */

                cntr2sc_state[6],       /* space-craft state relative to   */
					/* the center (of sc & target)	   */

                cntr2sc_lt = 0.0,      	/* 'light-time' one-way distance   */
					/* from the space-craft to center. */

                tgt2sun[3],

                sun2sc_state[6],        /* state of the sun relative to the */
                                        /* space-craft.			    */

                sun2sc_lt = 0.0,       	/* 'light-time' one-way distancei   */
					/* from the sun to the space-craft  */

                tgt_radius[3],          /* target radius		    */

                tgt2sc_origin_radius = 0.0,
                tgt2sun_origin_radius = 0.0,
                tgt2p5_origin_radius = 0.0,

                tgt2npole[3],
                tgt2npole_bd_fix[3],
                tgt2npole_cm_fix[3],
                sc2p5[3],
                sc2p5_cm_fix[3],
                sc2p5_bd_fix[3],
                nsc2p5_bd_fix[3],
                sun2p5_bd_fix[3],
                nsun2p5_bd_fix[3],
                tgt2p5_bd_fix[3],
                tgt2p5_norm_bd_fix[3],

                ephem_time_dis_lt = 0.0, /* the difference in light-time      */
                                        /* distance between the current      */
                                        /* ephemeris-time and the distance   */
                                        /* from the target to space-craft    */
                me_ra = 0.0,
                me_dec = 0.0,          /* Euler angles used to calculate    */
                me_twist = 0.0,        /* me_matrix.			     */
                me_lambda = 0.0,
                rot_matrix[3][3];       /* rotational matrix used to convert */
                                        /* vector between two reference	     */
                                        /* frame. For MS, we will always     */
                                        /* assumpt that me_matrix from SPK   */
                                        /* is in J2000 reference frame,      */
                                        /* and that user can only pass in    */
                                        /* either J2000 or B1950 as requested*/
                                        /* reference frame !!!!		     */

 int            target_id = 0,          /* target_id derived from target_name*/
                center_id = 0,          /* center_id derived from target_id  */
                nreturn = 0,            /* temp variable		     */
		spk_count = 0;

 char           	ref_system[16],
			utc_str[128];
 kernelInfoStruct	kinfo[MAX_KERNEL_COUNT];

 /* writing informational message back to clients so that they */
 /* will know what is going on on the server side.	       */
 mssvr_write_info_log (sd, cm,
	"Starting remote SPKernel read....", 1);

 memset ((void*) kinfo, '\0',
	sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT); 
 spk_count = mslcl_spk_info_from_kdb(env, kinfo);
 if (spk_count == 0) {
    mssvr_write_info_log (sd, cm,
	"ERROR:kdb has no SPkernel", 1);
    return -1;
    }
 else if (spk_count == (-1)) {
    mssvr_write_info_log (sd, cm,
	"ERROR reading spk_info from kerneldb", 1);
    return -1;
    }

 sprintf(utc_str,
	"%04d-%03d // %02d:%02d:%02d.%03d",
	request->scet[0], request->scet[1], request->scet[2],
	request->scet[3], request->scet[4], request->scet[5]);

 zms_utc2et (utc_str, &et);

 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting ephem_time", 1);
    return -1;
    }
 if (mslcl_loadSpk (sd, et, request->spk_id,
		ckdata->seg_id, kinfo, spk_count)) {
    mssvr_write_info_log (sd, cm,
	"ERROR loading spkernel", 1);
    return -1;
    }

 /* convert target name to target_id	*/
 if (strlen(request->target_name) == 0) {
    mssvr_write_info_log (sd, cm,
	"ERROR:NULL target name!!!", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 zms_bodn2c(request->target_name, &target_id, &found);
 if (!found) {
    mssvr_write_info_log (sd, cm,
	"ERROR:Unknown target name", 1);
    mssvr_write_info_log (sd, "Target name: ",
	request->target_name, 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 /* calc. center id from target.	*/
 center_id = ((target_id < 10) || (target_id > 999)) ?
                target_id : (100 * (target_id/100) + 99);


                        /* zms_spkssb: get the state (pos & vel) of	     */
                        /* a given target body (sc_id, target_id,	     */
                        /* center_id) relative to the solar system	     */
                        /* barycenter.	     				     */
                        /*                                                   */
                        /* In the next three spkssb(), we are getting	     */
                        /* state information about the sc_id, target_id,     */
                        /* and center_id. These states will be used to	     */
                        /* calculate most (if not all) of the returned data. */

 if (request->system == REF_J2000)
    strcpy (ref_system, "J2000");
 else if (request->system == REF_B1950)
    strcpy (ref_system, "B1950");
 else {
    mssvr_write_info_log (sd, cm,
	"UNKNOWN user reference system", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 zms_spkssb(request->sc_id, et,                 	/* get SC state      */
        ref_system, sc_state);                          /* relavtive to the  */
 if (zms_failed()) {                                    /* solar system      */
    mssvr_write_info_log (sd, cm,
	"ERROR getting S/C Pos_Vel", 1);		/* bary-center	     */
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 zms_dafgh (&t_handle);				/* need to figure out where  */
 for (i = 0; i < spk_count; i++) {		/* the spk data come from    */
     if (t_handle == kinfo[i].handle) {		/* and log the spk_id into   */
	strncpy (&ckdata->seg_id[14],		/* the returned seg_id.      */
		kinfo[i].id, 4);
	break;
	}
     }
                        /* zspkapp: return the state (pos & vel) of a target */
                        /* body (targ_id) relative to an observer. Where the */
                        /* observer is his states relative to the solar	     */
                        /* system barycenter (not the observer's id).	     */

 zms_spkapp (center_id, et,        		/* get the sc's states	     */
        ref_system, sc_state,                 	/* relative to the	     */
        "LT", cntr2sc_state, &cntr2sc_lt);  	/* space-craft, and	     */
 if (zms_failed()) {                            /* get the light-time one-way*/
    mssvr_write_info_log (sd, cm,
	"can't get sc2cntr info", 1);   	/* distance from the sc to   */
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;                                  /* the center		     */
    }

 zms_spkapp (target_id, et,             	/* getting the target states */
        ref_system, sc_state,                   /* relative to the sc and the*/
        "LT", tgt2sc_state, &tgt2sc_lt);    	/* light-time one-way dist   */
 if (zms_failed()) {                            /* from the target to	     */
    mssvr_write_info_log (sd, cm,
	"can't get tgt2sc info", 1);    	/* the sc at eph_time	     */
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 zms_spkapp (10, et,                    	/* getting the sun states    */
        ref_system, sc_state,                   /* relative to the sc and the*/
        "LT", sun2sc_state, &sun2sc_lt);    	/* light-time one-way dist   */
 if (zms_failed()) {                            /* from the target to	     */
    mssvr_write_info_log (sd, cm,
	"can't get sun2sc info", 1);    	/* the sc at eph_time	     */
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 ephem_time_dis_lt = et - tgt2sc_lt;        		/* get the diff      */
 zms_bodeul (target_id, ephem_time_dis_lt,              /* in light-time dis */
        &me_ra, &me_dec, &me_twist, &me_lambda);        /* then calculate    */
 if (zms_failed()) {                                    /* the EULER angles. */
    mssvr_write_info_log (sd, cm,
	"ERROR getting Euler Angles", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 zms_eul2m (me_twist, zms_halfpi() - me_dec,            /* calculate the     */
        zms_halfpi() + me_ra, 3, 1, 3,                  /* me_matrix from    */
        (double*) spkdata->me_matrix);                  /* Euler angles	     */
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR calculating me_matrix", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 if (request->system == REF_B1950) {            /* If user passed in B1950   */
    zms_irfrot (REF_J2000, REF_B1950, rot_matrix);   /* reference frame,     */
    zms_mxm (spkdata->me_matrix,                /* get a rotational matrix   */
        rot_matrix, spkdata->me_matrix);        /* and rotate returned	     */
    }                                           /* me_matrix into requested  */
                                                /* reference frame !!!	     */
                                /* By doing this, we are assumming that the  */
                                /* me_matrix from SPK is always in J2000     */
                                /* reference frame, and that users can only  */
                                /* pass in either J2000 or B1950 as req.     */
                                /* reference frame			     */

 zms_mxmt (spkdata->me_matrix,			/* calculate om_mat      */
        ckdata->c_matrix,
        spkdata->om_matrix);
 zms_vminus (tgt2sc_state, ntgt2sc_state);      /* negate tgt2sc_state   */
 zms_mxv (spkdata->me_matrix,                   /* get sc vector	 */
                ntgt2sc_state, tgt2sc_bd_fix);  /* relative to target	 */

 zms_reclat (tgt2sc_bd_fix,             /* given the space-craft	     */
        &tgt2sc_origin_radius,          /* rectangular coordinates,	     */
        &spkdata->tgt_bd_cntr_2_sc_lon, /* tgt2sc_bd_fix, this returns	     */
        &spkdata->tgt_bd_cntr_2_sc_lat);/* the corresponding	     	     */
                                        /* latitudinal coordinates, and	     */
                                        /* returns tgt2sc_origin_radius, it  */
                                        /* is the distance from the sc to    */
                                        /* origin radius between the tgt &   */
                                        /* sc.				     */

 spkdata->tgt_bd_cntr_2_sc_lon = (double)
        (360.0 - (zms_dpr() * spkdata->tgt_bd_cntr_2_sc_lon));
 spkdata->tgt_bd_cntr_2_sc_lon =
        (spkdata->tgt_bd_cntr_2_sc_lon > 360) ? 
        (double) (spkdata->tgt_bd_cntr_2_sc_lon - 360) :
        spkdata->tgt_bd_cntr_2_sc_lon;
 spkdata->tgt_bd_cntr_2_sc_lat = (double)
        (zms_dpr() * spkdata->tgt_bd_cntr_2_sc_lat);

 nreturn = 0;                                           /* initialize and    */
 tgt_radius[0] = tgt_radius[1] = tgt_radius[2] = 0.0;   /* getting target    */
 zms_bodvar (target_id, "RADII", &nreturn, tgt_radius); /* information	     */
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting target information", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }
 else {
    spkdata->tgt_radius_l_axis = tgt_radius[0];         /* transfer the	    */
    spkdata->tgt_radius_s_axis = tgt_radius[1];         /* target radii	    */
    spkdata->tgt_polar_radius  = tgt_radius[2];         /* over and	    */

    if (((int) (spkdata->tgt_radius_l_axis * 10) < 1) ||  /* make sure that */
        ((int) (spkdata->tgt_radius_s_axis * 10) < 1) ||  /* they are OK    */
        ((int) (spkdata->tgt_polar_radius  * 10) < 1)) {
       mssvr_write_info_log (sd, cm,
		"invalid target radius", 1);
       mslcl_unloadSpk (kinfo, spk_count);
       return -1;
       }
    if ((target_id == 2) ||
        (target_id == 299)) {
       spkdata->tgt_radius_l_axis =
       spkdata->tgt_radius_s_axis =
       spkdata->tgt_polar_radius  = 6137.00;
       }
    }

 if (zcam_info(request->sc_id, request->instrument_name,     /* load camera  */
        &camera_id, &camera_sn, &camera_rad_per_pixel,       /* information. */
        &focal_mm, &opaxis_line, &opaxis_samp, &scale)) {    /* This is	     */
    mssvr_write_info_log (sd, cm,
		"ERROR getting camera info", 1);  	     /* mission	     */
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }

 focal_pix = focal_mm * scale;
 flat      = spkdata->tgt_radius_l_axis -
                spkdata->tgt_radius_s_axis;

 zmve(-9, 1, &spkdata->tgt_bd_cntr_2_sc_lat, &tlat, 1, 1);
 zmve(-9, 1, &spkdata->tgt_bd_cntr_2_sc_lon, &tlon, 1, 1);
 zmve(-9, 1, &spkdata->tgt_radius_l_axis, &req, 1, 1);
 tlon = 360.0 - tlon;

 /* ****************** DEBUGGING *********************
 printf ("\n\n\n");
 printf ("tlat		: %g\n", tlat);
 printf ("tlon		: %g\n", tlon);
 printf ("om_matrix	: %g\t%g\t%g\n\t\t  %g\t%g\t%g\n\t\t  %g\t%g\t%g\n\n",
	spkdata->om_matrix[0], spkdata->om_matrix[1], spkdata->om_matrix[2],
	spkdata->om_matrix[3], spkdata->om_matrix[4], spkdata->om_matrix[5],
	spkdata->om_matrix[6], spkdata->om_matrix[7], spkdata->om_matrix[8]);
 printf ("tgt2sc_bd_fix	: %g\t%g\t%g\n",
	tgt2sc_bd_fix[0], tgt2sc_bd_fix[1], tgt2sc_bd_fix[2]);
 printf ("focal_pix	: %g\n", focal_pix);
 printf ("req		: %g\n", req);
 printf ("flat		: %g\n", flat);
 printf ("sub_sc_line	: %g\n", spkdata->sub_sc_line);
 printf ("sub_sc_samp	: %g\n", spkdata->sub_sc_samp);
 printf ("opaxis-line	: %g\n", opaxis_line);
 printf ("opaxis-samp	: %g\n", opaxis_samp);
 printf ("flag		: %f\n\n\n", flag);
 *************************************************** */

 zcorcav(&status, &tlat, &tlon,
        (double*) spkdata->om_matrix,
        tgt2sc_bd_fix,
        &focal_pix, &req, &flat,
        &sub_sc_line, &sub_sc_samp,
        &opaxis_line, &opaxis_samp, &flag);
 if (status == 99) {
    mssvr_write_info_log (sd, cm,
	"ZCORCAV::Requested point behind planet", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }
 else if (status == (-1)) {
    mssvr_write_info_log (sd, cm,
	"ZCORCAV::Back-of-planet test failed", 1);
    mslcl_unloadSpk (kinfo, spk_count);
    return -1;
    }
 else {
    zmve(9, 1, &sub_sc_line, &spkdata->sub_sc_line, 1, 1);
    zmve(9, 1, &sub_sc_samp, &spkdata->sub_sc_samp, 1, 1);
    }

 zms_vadd_mat(ntgt2sc_state, sun2sc_state, tgt2sun);
 zms_mxv(spkdata->me_matrix, tgt2sun, tgt2sun_bd_fix);
 zms_reclat(tgt2sun_bd_fix, &tgt2sun_origin_radius,
        &spkdata->tgt_bd_cntr_2_sun_lon,
        &spkdata->tgt_bd_cntr_2_sun_lat);

 spkdata->tgt_bd_cntr_2_sun_lon = (double) (360.0 - (zms_dpr() *
                spkdata->tgt_bd_cntr_2_sun_lon));

 if (spkdata->tgt_bd_cntr_2_sun_lon > 360)
    spkdata->tgt_bd_cntr_2_sun_lon = (double)
                (spkdata->tgt_bd_cntr_2_sun_lon - 360.0);

 spkdata->tgt_bd_cntr_2_sun_lat = (double) (zms_dpr() *
                spkdata->tgt_bd_cntr_2_sun_lat);

 tgt2npole_bd_fix[0] = 0;
 tgt2npole_bd_fix[1] = 0;
 tgt2npole_bd_fix[2] = 1;

 zms_mtxv(spkdata->me_matrix, tgt2npole_bd_fix, tgt2npole);
 zms_mxv(ckdata->c_matrix, tgt2npole, tgt2npole_cm_fix);

 spkdata->north_angle = atan2(tgt2npole_cm_fix[1], tgt2npole_cm_fix[0]);
 spkdata->north_angle = zms_dpr() * spkdata->north_angle;
 if (spkdata->north_angle < 0)
    spkdata->north_angle = (double) (spkdata->north_angle + 360.0);

 sc2p5_cm_fix[0] = 0;
 sc2p5_cm_fix[1] = 0;
 sc2p5_cm_fix[2] = 1;
 zms_mtxv(ckdata->c_matrix, sc2p5_cm_fix, sc2p5);
 zms_mxv(spkdata->me_matrix, sc2p5, sc2p5_bd_fix);
 zms_surfpt(tgt2sc_bd_fix, sc2p5_bd_fix,
        spkdata->tgt_radius_l_axis,
        spkdata->tgt_radius_s_axis,
        spkdata->tgt_polar_radius,
        tgt2p5_bd_fix, &intersect);
 if (intersect) {
    zms_reclat(tgt2p5_bd_fix,
        &tgt2p5_origin_radius,
        &spkdata->p5_lon,
        &spkdata->p5_lat);
    spkdata->p5_lon = (double)
        (360.0 - (zms_dpr() * spkdata->p5_lon));
    if (spkdata->p5_lon > 360)
       spkdata->p5_lon = (double) (spkdata->p5_lon - 360.0);
    spkdata->p5_lat = zms_dpr() * spkdata->p5_lat;

    zms_vsub(tgt2p5_bd_fix, tgt2sc_bd_fix, sc2p5_bd_fix);
    zms_vminus(sc2p5_bd_fix, nsc2p5_bd_fix);

    zms_vsub(tgt2p5_bd_fix, tgt2sun_bd_fix, sun2p5_bd_fix);
    zms_vminus(sun2p5_bd_fix, nsun2p5_bd_fix);

    zms_surfnm(spkdata->tgt_radius_l_axis,
        spkdata->tgt_radius_s_axis,
        spkdata->tgt_polar_radius,
        tgt2p5_bd_fix, tgt2p5_norm_bd_fix);
    spkdata->p5_incidence_angle = (double)
                zms_vsep(tgt2p5_norm_bd_fix, nsun2p5_bd_fix);
    spkdata->p5_incidence_angle = (double) (zms_dpr() *
                spkdata->p5_incidence_angle);

    spkdata->p5_emission_angle = (double)
                zms_vsep(tgt2p5_norm_bd_fix, nsc2p5_bd_fix);
    spkdata->p5_emission_angle = (double) (zms_dpr() *
                spkdata->p5_emission_angle);
    spkdata->p5_phase_angle = (double)
                zms_vsep(nsun2p5_bd_fix, nsc2p5_bd_fix);
    spkdata->p5_phase_angle = (double) (zms_dpr() *
                spkdata->p5_phase_angle);
    spkdata->p5_horiz_pix_size    =
        spkdata->p5_vert_pix_size =
                camera_rad_per_pixel *
                zms_vnorm(sc2p5_bd_fix);
    spkdata->range_sc2p5_intercept_pt =
                zms_vnorm(sc2p5_bd_fix);
    }

 for (i = 0; i < 3; i++) {
     spkdata->sc_pos_bd_centered[i]     = -cntr2sc_state[i];
     spkdata->pic_pos_sc_centered[i]    = tgt2sc_state[i];
     spkdata->rs_vector[i]              = tgt2sc_bd_fix[i];
     }

 spkdata->range_pic_bd_2_sun            = zms_vnorm(tgt2sun);
 spkdata->range_sc_2_central_bd_cntr    = zms_vnorm(cntr2sc_state);
 spkdata->range_sc_2_picture_bd_cntr    = zms_vnorm(tgt2sc_state);

 mslcl_unloadSpk (kinfo, spk_count);
 return 0;
}
/*--------------------------------------------------------------------------*
 * 1. read ckstruct (which acts as a request structure) off the socket.
 * 2. try to wirte it to appropriate kernel.
 * 3. when done, write ops complete code to socket.
 *--------------------------------------------------------------------------*/
void mssvr_putspice (int sd, msEnvStruct *env)
{
 msCkStruct ckdata;
 const char *cm = "mssvr_putspice";

 /* writing messages back to clients so that they will know */
 /* what is going on.					    */
 mssvr_write_info_log (sd, cm, "Kernel write....initializing", 1);
 mssvr_write_info_log (sd, cm, "Loading support kernels .....", 1);

 if (mslcl_load_support_kernels(env)) {
    mssvr_write_err_log (sd, cm,
	"ERROR loading support kernels");
    return;
    }

 memset ((void*) &ckdata, '\0', sizeof(ckdata));

 if (msclt_is_readable(sd) &&
     msclt_read_ck_struct (sd, &ckdata)) {
    mssvr_write_err_log (sd, cm,
	"ERROR reading ckdata from socket");
    zms_clpool ();
    return;
    }

 if (strlen(ckdata.ck_id)) {
    if ( mssvr_write_to_ckid (sd, ckdata.ck_id, &ckdata, env) ) {
       mssvr_write_info_log (sd, cm, "ERROR writing data to ckernel", 1);
       mssvr_write_info_log (sd, "ck_id", ckdata.ck_id, 1);
       mssvr_write_err_log (sd, cm, "Terminating client connection");
       zms_clpool ();
       return;
       }
    else {
       if (msclt_write_u_long (sd, OPS_COMPLETED))
          msclt_log (cm, "ERROR writing completion code to client");
       zms_clpool ();
       return;
       }
    }

 if (strlen(ckdata.ck_name)) {
    if (mssvr_write_to_ckname (sd, ckdata.ck_name, &ckdata, env) ) {
       mssvr_write_info_log (sd, cm, "ERROR writing data to ckernel", 1);
       mssvr_write_info_log (sd, "ck_name", ckdata.ck_name, 1);
       mssvr_write_err_log (sd, cm, "Terminating client connection");
       zms_clpool ();
       return;
       }
    else {
       if (msclt_write_u_long (sd, OPS_COMPLETED))
          msclt_log (cm, "ERROR writing completion code to client");
       zms_clpool ();
       return;
       }
    }

 if (strlen(ckdata.ck_source)) {
    if (mssvr_write_to_cksource (sd, ckdata.ck_source, &ckdata, env)) {
       mssvr_write_info_log (sd, cm, "ERROR writing data to ckernel", 1);
       mssvr_write_info_log (sd, "ck_source", ckdata.ck_source, 1);
       mssvr_write_err_log (sd, cm, "Terminating client connection");
       zms_clpool ();
       return;
       }
    else {
       if (msclt_write_u_long (sd, OPS_COMPLETED))
          msclt_log (cm, "ERROR writing completion code to client");
       zms_clpool ();
       return;
       }
    }

 /* clear up pool before */
 /* returning!!!	 */
 zms_clpool ();

 /* By the time you have gotten here, you have not been able to use */
 /* any of the supplied id, ckname or source from request. Will	    */
 /* just return with nothing for now.				    */
 mssvr_write_info_log (sd, cm, "CK request has no id/ckname/source", 1);
 mssvr_write_err_log (sd, cm, "Terminating client connection !!!!\n");
}
/*--------------------------------------------------------------------------*/
int mssvr_write_to_ckid (int sd, char *ckid,
		msCkStruct *ckdata, msEnvStruct *env)
{
 kernelInfoStruct ckinfo;
 const char *cm = "mssvr_write_to_ckid";

 int i, ck_count;
 int len;
 kernelInfoStruct tkinfo[MAX_KERNEL_COUNT];

 memset ((void*) &ckinfo, '\0', sizeof(ckinfo));

 /* writing messages back to clients so that they will know */
 /* what is going on.					    */
 mssvr_write_info_log (sd, cm, "Writing to CK by id...", 1);
 mssvr_write_info_log (sd, "ck_id", ckid, 1);

 /* txh::added kernel file search by source when an invalide ck_id is entered */
 if (mslcl_kid_2_kinfo(env, ckid, &ckinfo)) 
 {
    mssvr_write_info_log (sd, cm, "ERROR getting kernel info", 1);
    mssvr_write_info_log (sd, "ck_id", ckid, 1);
    mssvr_write_info_log (sd, cm, "Determine kernel filename by source.", 1);

    memset ((void*) tkinfo, '\0',
        sizeof(kernelInfoStruct) * MAX_KERNEL_COUNT);
    ck_count = mslcl_ck_info_from_kdb(env, tkinfo);

    for (i=0; i<ck_count; i++)
    {
       len = (int)strlen(ckdata->ck_source);
       if (len != (int)strlen(tkinfo[i].source)) continue;
       if (!strncmp(ckdata->ck_source, tkinfo[i].source, len))
          strcpy (ckinfo.fname, tkinfo[i].fname);
   }
 }

 if (!strcmp (ckinfo.source, "NAIF")) {
    mssvr_write_info_log (sd, cm,
	"ERROR:Not allow update NAIF CKernel", 1);
    return -1;
    }

 if (!strcmp (ckinfo.source, "SEDR")) {
    mssvr_write_info_log (sd, cm,
        "ERROR:Not allow update SEDR CKernel", 1);
    return -1;
    }

 if ( mssvr_write_to_ckname (sd,
	ckinfo.fname, ckdata, env)) {	
    mssvr_write_info_log (sd, cm,
	"ERROR writing data to ckernel", 1);
    return -1;	 
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int mssvr_write_to_cksource (int sd, char *cksource,
		msCkStruct *ckdata, msEnvStruct *env)
{
 int ck_count;
 kernelInfoStruct ckinfo[MAX_KERNEL_COUNT];
 const char *cm = "mssvr_write_to_cksource";

 /* writing messages back to clients so that they will know */
 /* what is going on.					    */
 mssvr_write_info_log (sd, cm, "Writing to CK by source...", 1);
 mssvr_write_info_log (sd, "ck_id: ", cksource, 1);

 if (!strcmp(cksource, "NAIF")) {
    mssvr_write_info_log (sd, cm,
	"ERROR:Not allow to update NAIF CKernel", 1);
    return -1;
    }

 if (!strcmp(cksource, "SEDR")) {
    mssvr_write_info_log (sd, cm,
        "ERROR:Not allow to update SEDR CKernel", 1);
    return -1;
    }

 ck_count = mslcl_ck_info_from_kdb(env, ckinfo);
 if (ck_count == 0) {
    mssvr_write_info_log (sd, cm, "KDB has no CKernel", 1);
    return -1;
    }
 else if (ck_count == (-1)) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting ckernel info", 1);
    return -1;
    }

 while (--ck_count >= 0) { 
    if (!strcmp(cksource,  ckinfo[ck_count].source)) {
       if ( mssvr_write_to_ckname (sd,
		ckinfo[ck_count].fname, ckdata, env)) {
           mssvr_write_info_log (sd, cm,
		"ERROR writing data to ckernel", 1);
           return -1; 
           }
        else return 0;
        }
    }

 mssvr_write_info_log (sd, cm,
	"ERROR:request ck_source not found", 1);
 mssvr_write_info_log (sd, "ck_source", cksource, 1);

 return -1;
}
/*--------------------------------------------------------------------------*
 * 0. copy current date/time into seg_id
 * 1. from scet get the sclk
 * 2. rotate the c_matrix into J2000 if needed
 * 3. start writing to file
 *
 ***** Notice that we have to enter the current time/year/etc. to
 *	the appropriate area in the seg_id. This is done via
 *	<mslcl_getDateTime()>.
 *--------------------------------------------------------------------------*/
int mssvr_write_to_ckname (int sd, char *fname,
		msCkStruct *ckdata, msEnvStruct *env)
{
 int	handle,
	icd[NUM_CK_INTEGERS];
 double	done_flag = 1.0,
	dcd[NUM_CK_DOUBLES],
	descr[NUM_CK_DESCR],
	quat[4],
	rot_mat[3][3],
	ephem_time,
	sclk;
 char	utc_str[128],
	date_time[16];
 kernelInfoStruct kinfo;

 const char *cm = "mssvr_write_to_ckname";
 mssvr_write_info_log (sd, "ck_name", fname, 1);
 mssvr_write_info_log (sd, cm, "Starting ck write.....", 1);
 
 memset ((void*) &kinfo, '\0', sizeof(kinfo));
 if (mslcl_kname_2_kinfo (env, fname, &kinfo)) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting kernel info from kname", 1);
    mssvr_write_info_log (sd, "kernel_name", fname, 1);
    return -1;
    }
 
 memset ((void*)utc_str, '\0', 128);
 memset ((void*)date_time, '\0', 16);

 /* get the current date_time & copy into */
 /* the user's segid.			  */
 mslcl_getDateTime (date_time);
 strncpy (&ckdata->seg_id[22], date_time, 12);

 /* now prepare to get the sclk */
 sprintf (utc_str, "%04d-%03d // %02d:%02d:%02d.%03d",
        ckdata->scet[0], ckdata->scet[1], ckdata->scet[2],
        ckdata->scet[3], ckdata->scet[4], ckdata->scet[5]);

 zms_utc2et (utc_str, &ephem_time);
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR getting user ephm_time", 1);
    mssvr_write_info_log (sd, "utc", utc_str, 1);
    return -1;
    }

 zms_sce2t (ckdata->sc_id, ephem_time, &sclk);
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm, "ERROR getting user sclk", 1);
    return -1;
    }


 /* If this is a Cassini query, then we have the c-matrix of
  * the instrument.  We need to transform it 
  * to the c-matrix of the spacecraft.
  */
 if (ckdata->sc_id == CAS_SC_ID)
   {
     if ((ckdata->instrument != CASISSNA_NAIF_ID) &&
	 (ckdata->instrument != CASISSWA_NAIF_ID))
       {
	 mssvr_write_info_log (sd, cm,
			       "ERROR Invalid Cassini instrument code.", 1);
	 return -1;
       }
     else
       {
	 double rot[3][3] = {0};
	 int frame = 0;
	 int wasFound = 0;
	 zms_tkfram(ckdata->instrument, rot, &frame, &wasFound);
	 if (!wasFound)
	   {
	     mssvr_write_info_log (sd, cm,
				   "ERROR Instrument frame not found.", 1);
	     return -1;
	   }
	 else
	   {
	     double mat180[3][3] = {0};
	     zms_rotate(zms_pi(), 3, mat180);
	     
	     zms_xpose(rot, rot);
	     zms_invert(rot, rot);
	     zms_invert(mat180, mat180);
	     zms_mxm(rot, mat180, rot);
	     zms_mxm(rot, ckdata->c_matrix, ckdata->c_matrix);

	     ckdata->instrument = CAS_SC_ID * 1000;
	   }
       }
   }


 /* if system is not J2000, rotate c_matrix to J2000 */
 if (ckdata->system != REF_J2000) {
    zms_irfrot (ckdata->system, REF_J2000, rot_mat);
    zms_mxmt (ckdata->c_matrix, rot_mat, ckdata->c_matrix);
    }

 /* put c_matrix into NAIF convention */
 zms_xpose (ckdata->c_matrix, ckdata->c_matrix);

 
 /* open file for writing */
 zms_dafopw (kinfo.fname, &handle);
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"ERROR openning CK for writing", 1);
    mssvr_write_info_log (sd, "ck_name", fname, 1);
    return -1;
    }

 zms_m2q(ckdata->c_matrix, quat);			/* convert to	*/
 if (zms_failed()) {					/* quat. Make   */
    mssvr_write_info_log (sd, cm,
	"ERROR making quat from cm", 1);       		/* sure c_mat   */
    zms_ckcls (handle);
    return -1;
    }

 dcd[0] = sclk;                 /* setup start & end sclk. For instrument id,*/
 dcd[1] = sclk;                 /* we can compute it here from the inst.     */
 icd[0] = ckdata->instrument;   /* and sc_id. But let user to that outside.  */
                                /* Should be return by cam_info().	     */

 icd[1] = REF_J2000;            /* ref. will will always be J2000.	     */
 icd[2] = 1;                    /* write to CK as data type 1		     */
 icd[3] = (int) ckdata->avFlag; /* whether or not data has angular velocity  */
 icd[4] = 0;

 zms_dafps(NUM_CK_DOUBLES,                      /* pack the double & integer */
        NUM_CK_INTEGERS, dcd, icd, descr);      /* components into resulting */
                                                /* descriptor.		     */

 zms_dafbna (handle, descr, ckdata->seg_id);    /* copy content to array     */
 zms_dafada (quat, 4);                          /* quaternion (c_matrix)     */

 if (ckdata->avFlag)                            /* if needed, copy	     */
    zms_dafada (ckdata->av, 3);                 /*      angular velocity     */

 zms_dafada (&sclk, 1);                         /*      sclk		     */
 zms_dafada (&done_flag, 1);                    /*      mark end of segment  */
 if (zms_failed()) {
    mssvr_write_info_log (sd, cm,
	"Can't write segment to file", 1);
    mssvr_write_info_log (sd, "file name: ", fname, 1);
    zms_dafcls (handle);
    return -1;
    }

 zms_dafena ();                                 /* signal end of array	     */
 if (zms_failed()) {                            /* make sure it does not     */
    mssvr_write_info_log (sd, cm,
	"ERROR ending array", 1);      		/* fail.    		     */
    mssvr_write_info_log (sd, "File name: ", fname, 1);
    zms_dafcls (handle);
    return -1;
    }

 mssvr_write_info_log (sd, cm,
	"Done writing data to CKernel", 1);
 zms_dafcls (handle);
 return 0;
}
/*--------------------------------------------------------------------------*
 * *** don't have to close up client socket because main server will
 *	take care of that.
 *
 * *** You don't need to pass in the name of the kernel you supposed
 *	to send because you will read that from the socket. Need to pass
 *	in the <env> because that will tell you where the data should
 *	be.
 *
 * - read requested ck name from client.
 * - check to see if the server actually has this ck.
 * - if server DON'T have requested ck, tell the client so that
 *			client will terminate itself.
 * - if server DO    have requested ck, move on to send the bin_ck.
 *--------------------------------------------------------------------------*/
void mssvr_send_kernel (int sd, msEnvStruct *env)
{
 char	kname[SVR_MESG_BUF_LEN],
	sp_name[SVR_MESG_BUF_LEN],
	mp_name[SVR_MESG_BUF_LEN],
	real_kname[SVR_MESG_BUF_LEN];
 int	sp_stat, mp_stat;
 struct stat buf;

 const char *cm = "mssvr_send_kernel";

 memset ((void*) kname, '\0', SVR_MESG_BUF_LEN);
 memset ((void*) sp_name, '\0', SVR_MESG_BUF_LEN);
 memset ((void*) mp_name, '\0', SVR_MESG_BUF_LEN);
 memset ((void*) real_kname, '\0', SVR_MESG_BUF_LEN);

 if (msclt_read_string (sd, kname, SVR_MESG_BUF_LEN) == (-1)) {
    mssvr_write_err_log (sd, cm, "ERROR reading requested ckname");
    return;
    }

 sprintf (sp_name, "%s/%s", env->spiceker, kname);
 sprintf (mp_name, "%s/%s", env->mipsker, kname);
 sp_stat = stat(sp_name, &buf);
 mp_stat = stat(mp_name, &buf);
 if ((sp_stat == (-1)) && (mp_stat == (-1))) {
    mssvr_write_err_log (sd, cm,
	"User requested CKernel does not exist");
    return;
    }

 if (mp_stat == 0) memcpy (real_kname, mp_name, SVR_MESG_BUF_LEN);
 else memcpy (real_kname, sp_name, SVR_MESG_BUF_LEN);

 if (msclt_write_u_long (sd, OPS_COMPLETED)) {
    mssvr_write_err_log (sd, cm,
	"ERROR sending initialization code to client");
    return;
    }

 if (msclt_send_bin_kernel(sd, real_kname)) {
    mssvr_write_err_log (sd, cm,
	"ERROR sending bin kernel to client");
    return;
    }
}
/*--------------------------------------------------------------------------*/
void mssvr_receive_kernel (int sd, msEnvStruct *env)
{
 printf ("Doing nothing for now!!!!\n");
}
/*--------------------------------------------------------------------------*
 * send the message, along with the response code (SVR_ERR_MESG)
 * then log in local file with server problem.
 *--------------------------------------------------------------------------*/
int mssvr_write_err_log (int sd, const char *sub, const char *mesg)
{
 char	buf[SVR_MESG_BUF_LEN];
 const	char *cm = "mssvr_write_err_log";

 sprintf (buf, "%s:%s", sub, mesg);
 if (msclt_write_u_long(sd, SVR_ERR_MESG)) {
    msclt_log (cm, "ERROR writing response code to client");
    return -1;
    }

 if (msclt_write_string(sd, buf, SVR_MESG_BUF_LEN)) {
    msclt_log (cm, "ERROR writing response mesg to client");
    return -1;
    }

 msclt_log (sub, mesg);

 return 0;
}
/*--------------------------------------------------------------------------*/
int mssvr_write_info_log (int sd, const char *sub,
		const char *mesg, int logmesg)
{
 char	buf[SVR_MESG_BUF_LEN];
 const	char *cm = "mssvr_write_info_log";

 sprintf (buf, "%s:%s", sub, mesg);
 if (msclt_write_u_long (sd, SVR_INFO_MESG)) {
    msclt_log (cm, "ERROR writing response code to client");
    return -1;
    }
 else if (msclt_write_string (sd, buf, SVR_MESG_BUF_LEN)) {
    msclt_log (cm, "ERROR writing response mesg to client");
    return -1;
    }
 if (logmesg) msclt_log (sub, mesg);
 else fprintf (stderr, "%s\n", buf);

 return 0;
}
/*--------------------------------------------------------------------------*
 * - extract source & spk_id from segid
 * - here is the loading order:
 *	- try to load spk with id in segid first. If loaded ok,	return SUCCESS.
 *	- try to load usr request spk_ref. If load ok, return SUCCESS.
 *	- try to load using source in segid.
 *		- load all naif sp kernel first.
 *		- load all non-naif sp kernel with source different
 *			from the segid's institution.
 *		- finally load the sp kernel with source equals to
 *			segid's institution.
 *--------------------------------------------------------------------------*/
int mssvr_load_spk (int sd, double et, char *spk_ref,
	char *segid, msEnvStruct *env)
{
 int	i,
	kcount;
 char	source[8],
	spk_id[8],
	tet_begin,
	tet_end;

 kernelInfoStruct kinfo[MAX_KERNEL_COUNT];

 const char *cm = "mssvr_load_spk";

 memset ((void*) source, '\0', 8);		/* copy source & spk_id   */
 memcpy ((void*) source, (void*)segid, 4);	/* from segid into local  */
 memset ((void*) spk_id, '\0', 8);		/* memory space.	  */
 memcpy ((void*) spk_id, (void*) &segid[14], 4);
 
 if ((kcount = mslcl_spk_info_from_kdb(			/* read spk info  */
			env, kinfo)) == (-1)) {		/* from kerneldb  */
    mssvr_write_info_log (sd, cm,
	"ERROR reading spk info from kdb", 1);
    return -1;
    }
 
 if (strlen(spk_id)) {
     mssvr_write_info_log (sd, cm,
	"Loading with segid's spk_id", 1);
     mssvr_write_info_log (sd, "Target spk_id", spk_id, 1);

     for (i = 0; i < kcount; i++) {
         if (!strcmp(spk_id, kinfo[i].id)) {
	    zms_spklef (kinfo[i].fname, &kinfo[i].handle);
	    if (zms_failed()) {
	       mssvr_write_info_log (sd, cm,
			"ERROR loading sp kernel", 1);
	       mssvr_write_info_log (sd, "spk", kinfo[i].fname, 1);
	       return -1;
	       }
	    mssvr_write_info_log (sd, "load spk", kinfo[i].fname, 1);
	    return 0;
	    }
         }
     }

 if (strlen(spk_ref)) {
    mssvr_write_info_log (sd, cm,
	"Loading with user's request spk_id", 1);
    mssvr_write_info_log (sd, "Target spk_id", spk_ref, 1);

    for (i = 0; i < kcount; i++) {
         if (!strcmp(spk_ref, kinfo[i].id)) {
            zms_spklef (kinfo[i].fname, &kinfo[i].handle);
            if (zms_failed()) {
               mssvr_write_info_log (sd, cm,
                        "ERROR loading sp kernel", 1);
               mssvr_write_info_log (sd, "spk", kinfo[i].fname, 1);
               return -1;
               }
            mssvr_write_info_log (sd, "load spk", kinfo[i].fname, 1);
            return 0;
            }
         }
    }

 if (strlen(source)) {
    mssvr_write_info_log (sd, cm,
	"Loading with segid's source", 1);
    mssvr_write_info_log (sd, "Target source", source, 1);

    /* load all NAIF SPK and SPICE CK */
    for (i = 0; i < kcount; i++) {
	tet_begin = kinfo[i].et_begin - 60.0;
	tet_end   = kinfo[i].et_end * 1.0;
	if ( (kinfo[i].et_begin > tet_begin)	&&
	     (kinfo[i].et_end < tet_end)	&&
	     (!strcmp(kinfo[i].source, "NAIF") ||
              !strcmp(kinfo[i].source, "SEDR")) ) {
	   zms_spklef (kinfo[i].fname, &kinfo[i].handle);
	   if (zms_failed()) {
	      mssvr_write_info_log (sd, cm,
			"ERROR loading spkernel", 1);
	      mssvr_write_info_log (sd, "spkernel", kinfo[i].fname, 1);
	      return -1;
	      }
	   mssvr_write_info_log (sd, "load spk", kinfo[i].fname, 1);
	   }
        }

    /* load all non-NAIF & non-SEDR & non-segid source SPK */
    for (i = 0; i < kcount; i++) {
	tet_begin = kinfo[i].et_begin - 60.0;
        tet_end   = kinfo[i].et_end * 1.0;
        if ( (kinfo[i].et_begin > tet_begin)    &&
             (kinfo[i].et_end < tet_end)        &&
             strcmp(kinfo[i].source, "NAIF")	&&
	     strcmp(kinfo[i].source, "SEDR")    &&
             strcmp(kinfo[i].source, source) ) {
	   zms_spklef (kinfo[i].fname, &kinfo[i].handle);
           if (zms_failed()) {
              mssvr_write_info_log (sd, cm,
                        "ERROR loading spkernel", 1);
              mssvr_write_info_log (sd, "spkernel", kinfo[i].fname, 1);
              return -1;
              } 
           mssvr_write_info_log (sd, "load spk", kinfo[i].fname, 1);
	   }
        }

    /* load all spkernel with same source with segid's source */
    for (i = 0; i < kcount; i++) {
        tet_begin = kinfo[i].et_begin - 60.0;
        tet_end   = kinfo[i].et_end * 1.0;
        if ( (kinfo[i].et_begin > tet_begin)    &&
             (kinfo[i].et_end < tet_end)        &&
             !strcmp(kinfo[i].source, source) ) {
	   zms_spklef (kinfo[i].fname, &kinfo[i].handle);
           if (zms_failed()) {
              mssvr_write_info_log (sd, cm,
                        "ERROR loading spkernel", 1);
              mssvr_write_info_log (sd, "spkernel", kinfo[i].fname, 1);
              return -1;
              } 
           mssvr_write_info_log (sd, "load spk", kinfo[i].fname, 1);
	   }
	}
    }

 return 0;
}
