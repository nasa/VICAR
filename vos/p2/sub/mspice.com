$!****************************************************************************
$!
$! Build proc for MIPL module mspice
$! VPACK Version 1.9, Friday, October 08, 2010, 15:08:44
$!
$! Execute by entering:		$ @mspice
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module mspice ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mspice.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("mspice.imake") .nes. ""
$   then
$      vimake mspice
$      purge mspice.bld
$   else
$      if F$SEARCH("mspice.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mspice
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mspice.bld "STD"
$   else
$      @mspice.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mspice.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mspice.com -mixed -
	-s lclsub.c cltsub.c ms_xdr.c cam_info.c zms_bridge.c xms_bridge.f -
	-i mspice.imake -
	-t tstmspice.pdf tstMsLcl.c tstMsClient.c tstMsLcl.imake -
	   tstMsClient.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lclsub.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cltsub.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
 * Jul. 24, 1998  ...T.Huang...   Removed the 'free' subroutine call in 
 *                                subroutine msclt_write_client_name.  This 
 *                                caused some programs on SGI to crash.
 *
 * Oct. 08, 1998  ...T.Huang...   SPICE_CONFIG_FILE was never closed.  Client 
 *                                will run out of file descriptors when 
 *                                performing lots of transactions.  'fclose'
 *                                calls have been added to correct this 
 *                                problem.
 *
 * Feb. 08, 1999  ...T.Huang...   Added check inside msclt_readn for the case
 *                                when the Socket Descriptor is valid, but the
 *                                client no longer exist.  This check will 
 *                                exit the while loop and return an error
 *                                status of -1.
 *
 * Aug.  5, 1999  ...M.Brady...   Cleaned up things which were causing
 *                                compiler errors:
 *                                --Removed trailing '1' parameter from
 *                                two fprintf calls in msclt_read_req_struct.
 *                                --Removed two extra %s symbols from
 *                                format string of last fprintf in
 *                                msclt_put_kernel. 
 *                                  
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>

#include "ms_defines.h"
#include "cltsub.h"
#include <rpc/types.h>
#include <rpc/xdr.h>


/*--------------------------------------------------------------------------
 * retrieve the config file name from SPICE_CONFIG_FILE environment
 * variable. Search for an entry key SPICE_LOG and copy the config
 * file name into the given parameter.
 *
 * Caller must make sure that enough memory is passed into <fname>
 *--------------------------------------------------------------------------*/
int msclt_getLogFileName (char *fname)
{
 FILE	*cfile;
 char	*cfname;
 char	dataline[256];
 char	key[256];
 char	value[256];

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s\n%s\n",
	"msclt_getLogFileName:ERROR!!!!",
	"SPICE_CONFIG_FILE is not defined");
    return -1;
    }
 
 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
	"msclt_getLogFileName:Cannot open Config file\n",
	"Sys mesg", strerror(errno),
	"Config file name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
     sscanf (dataline, "%s%s", key, fname);     
     if (!(strcmp(key, CONFIG_FILE_LOG_KEY))) {
	fclose (cfile);
	return 0;
	}
     }
 fprintf (stderr, "%s\n%s:%s\n",
	"msclt_getLogFileName:ERROR getting log file name",
	"Config file name", cfname);
 fclose (cfile);
 return -1;
}
/*--------------------------------------------------------------------------
 * retrieve the config file name from SPICE_CONFIG_FILE environment
 * variable. Search for an entry name SPICE_TCP_PORT and return
 * the port number to caller.
 *--------------------------------------------------------------------------*/
short msclt_getPortno ()
{
 short portno;
 FILE   *cfile;
 char   *cfname;
 char   dataline[256];
 char   key[256];
 char	value[256];

 const char *cm = "msclt_getPortno";

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s:%s\n",
        cm, "SPICE_CONFIG_FILE is not defined");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s:%s\n%s:%s\n%s:%s\n",
        cm, "Cannot open Config file\n",
        "Sys mesg", strerror(errno),
        "Config file name", cfname);
    return -1;
    }

  while (fgets(dataline, 256, cfile)) {
     sscanf (dataline, "%s%s", key, value);
     if (!(strcmp(key, CONFIG_FILE_PORTNO_KEY))) {
	portno = atoi (value);
        fclose (cfile);
	return portno;
	}
     }

 fprintf (stderr, "%s\n%s:%s\n",
        "msclt_getPortno:ERROR getting portno",
	"Config file name", cfname);
 fclose (cfile);
 return -1;
}
/*--------------------------------------------------------------------------*
 * retrive the spice log file name from SPICE_CONFIG_FILE. Then write the
 * message to end of file along with the current time.
 *--------------------------------------------------------------------------*/
int msclt_log (const char *sub, const char *mesg)
{
 FILE	*lfile;
 char	lfname[256];
 char	lcl_mesg[512];
 char	tstr[30];
 int	length;

 time_t	bintime;
 struct tm *curtime;

 if (msclt_getLogFileName(lfname)) {
    fprintf (stderr, "%s\n",
	"msclt_log:ERROR getting log filename");
    return -1;
    }

 time (&bintime);
 curtime = (struct tm*) localtime (&bintime);
 strcpy (tstr, (char*) asctime(curtime));
 tstr[strlen(tstr) - 1] = '\0';

 sprintf (lcl_mesg, "%s-->%s:%s\n", tstr, sub, mesg);
 fprintf (stderr, "%s", lcl_mesg);
 length = strlen (lcl_mesg);

 if (!(lfile = fopen(lfname, "a"))) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s",
        "msclt_log:ERROR opening file",
        "Sys mesg", strerror(errno),
	"File name", lfname);
    return -1;
    }

 if ( (write(fileno(lfile), lcl_mesg, length)) != length ) {
    fprintf (stderr, "%s\n%s:%s\n%s:%s\n",
	"msclt_log:ERROR writing to log file",
	"Sys mesg", strerror(errno),
	"Log file", lfname);
    fclose (lfile);
    return -1;
    }
 else fclose (lfile);

 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_short (int sd, short *data)
{
 XDR	xdrs;
 char	buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:ERROR reading from socket\n", cm);
    fprintf (stderr, "%s:%s\n",
	"Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_short (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR deconding socket data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_short (int sd, short data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_short (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_u_short (int sd, u_short *data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_u_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR reading from socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_u_short (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR decoding server data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_u_short (int sd, u_short data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_u_short";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_u_short (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_long (int sd, XDR_long *data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR reading from socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_long (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR deconding socket data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_long (int sd, XDR_long data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_long (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_u_long (int sd, XDR_u_long *data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_read_u_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR reading from socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_u_long (&xdrs, data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR deconding socket data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_u_long (int sd, XDR_u_long data)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];

 const char *cm = "msclt_write_u_long";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_ENCODE);
 if (!xdr_u_long (&xdrs, &data)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user data");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * read a string from <sd> and put it in <str>. The max length of <str>
 * is specified in <max_str_len>. Notice that this function will fill up
 * the length of the string from the socket. It's up to the caller to
 * null out the string if you want.
 *
 * For all string the protocol is:
 *		- XDR_u_long for string len
 *		- followed by a buffer of SVR_MESG_BUF_LEN (256Bytes).
 *		- you can only decode up to <string len> bytes of
 *			real data from socket.
 *
 * RETURN: the length of the string read off the socket.
 *	   -1 if ERROR occurred.
 *--------------------------------------------------------------------------*/
int msclt_read_string (int sd, char *str, int max_str_len)
{
 XDR	xdrs;
 char	buf[SVR_MESG_BUF_LEN];
 XDR_u_long	length;

 const char *cm = "msclt_read_string";

 if (msclt_read_u_long(sd, &length)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading string length from socket");
    return -1;
    }

 if (length >= max_str_len) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:not enough mem passed in");
    return -1;
    }

 memset ((void*) buf, '\0', SVR_MESG_BUF_LEN);
 if (msclt_readn(sd, buf, SVR_MESG_BUF_LEN) != SVR_MESG_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading message off socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 xdrmem_create (&xdrs, buf, SVR_MESG_BUF_LEN, XDR_DECODE);
 if (!xdr_vector (&xdrs, str, length,
		sizeof(char), (xdrproc_t) xdr_char)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR decoding message from socket");
    fprintf (stderr, "%s:%s\n",
	"Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * protocol for writing strings:
 * - write the string len
 * - write the actual string upto SVR_MESG_BUF_LEN
 *
 * NOTICE that these are any form of string you want. It does not interpret
 *	the string. That is, if a server want to send an informational
 *	message to the client, it has to first write a SVR_INFO_MESG
 *	code to the socket, so that the client will know to interpret
 *	the incomming string as an informational message.
 *
 *	Similarly, when reading the string, you have to first read the
 *		preceeding response code to know that it's an
 *		informational string. This is why I don't read/write the
 *		response codes in here.
 *--------------------------------------------------------------------------*/
int msclt_write_string (int sd, char *str, int max_str_len)
{
 XDR	xdrs;
 char	buf[SVR_MESG_BUF_LEN];
 XDR_u_long	length;

 const char *cm = "msclt_write_string";

 length = strlen (str);
 if (length >= SVR_MESG_BUF_LEN) {
    fprintf (stderr, "%s:%s\n",
	cm, "ERROR:string is too long");
    return -1;
    }

 if (msclt_write_u_long (sd, length)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to string to socket");
    return -1;
    }

 memset ((void*) buf, '\0', SVR_MESG_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_MESG_BUF_LEN, XDR_ENCODE);
 if (!xdr_vector (&xdrs, str, length,
		sizeof(char), (xdrproc_t) xdr_char)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding message string");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, SVR_MESG_BUF_LEN) != SVR_MESG_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * WARNING: This call will block the socket indefinitely until
 *		the socket is available for reading.
 *
 * check to see if the given socket is readable.
 * return 1: ready for read
 *	 -1: error
 *	  0: not ready for read
 *--------------------------------------------------------------------------*/
int msclt_is_readable (int sd)
{
 fd_set	fds;
 int	status;
 struct timeval *tm = (struct timeval*) NULL;

 const char *cm = "msclt_is_readable";

 FD_ZERO ((fd_set*) &fds);
 FD_SET  ((int) sd, &fds);
 status = select ( ((int) sd + 1), (fd_set*) &fds,
		(fd_set*) 0, (fd_set*) 0, tm);
 if ((status > 0) && (FD_ISSET((int)sd, &fds))) return 1;
 else if (status < 0) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR select() failed");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*
 * WARNING: This call will block the socket indefinitely until
 *		the socket is available for writing.
 *
 * check to see if the given socket is readable.
 * return 1: ready for write
 *	 -1: error
 *	  0: not ready for write
 *--------------------------------------------------------------------------*/
int msclt_is_writeable (int sd)
{
 fd_set fds;
 int    status;
 struct timeval *tm = (struct timeval*) NULL;

 const char *cm = "msclt_is_writeable";

 FD_ZERO ((fd_set*) &fds);
 FD_SET  ((int) sd, &fds);
 status = select ( ((int) sd + 1), (fd_set*) 0,
		(fd_set*) &fds, (fd_set*) 0, tm);
 if ((status > 0) && (FD_ISSET((int)sd, &fds))) return 1;
 else if (status < 0) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR select() failed");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_ck_struct (int sd, msCkStruct *ckdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_write_ck_struct";

 memset ((void*)buf, '\0', SVR_DATA_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_ENCODE);
 if (!xdr_msCkStruct(&xdrs, ckdata)) {
    fprintf (stderr, "%s:%s\n%s:%s\n",
	cm, "ERROR encoding ck info",
	"Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing ckdata to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return 0; 
}
/*--------------------------------------------------------------------------*/
int msclt_write_spk_struct (int sd, msSpkStruct *spkdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_write_spk_struct";
 
 memset ((void*)buf, '\0', SVR_DATA_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_ENCODE);
 if (!xdr_msSpkStruct(&xdrs, spkdata)) {
    fprintf (stderr, "%s:%s\n%s:%s\n",
	cm, "ERROR encoding spk info",
	"Sys mesg", strerror(errno));
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR writing spkdata to socket");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_req_struct (int sd, msUserRequestStruct *req)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];
 
 const char *cm = "mssvr_write_req_struct";
 
 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 if (   msclt_is_readable(sd) &&
        msclt_readn(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR reading client request from socket");
    return -1;
    }
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_DECODE);
 if (!xdr_msUserRequestStruct(&xdrs, req)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR decoding client request from socket");

    /* txh::added */
    xdr_destroy (&xdrs);
    return -1;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_write_req_struct (int sd, msUserRequestStruct *req)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_write_req_struct";

 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_ENCODE);
 if (!xdr_msUserRequestStruct(&xdrs, req)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR encoding user request");
    xdr_destroy (&xdrs);
    return -1;
    }

 if (msclt_writen(sd, buf,  SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm, "Cannot send user request to socket");
    fprintf (stderr, "%s:%s\n", cm, "... Returning with no data ....");
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*
 * reading response code from server.
 * return 0 ==> ERROR.
 *--------------------------------------------------------------------------*/
XDR_u_long msclt_read_resp_code (int sd)
{
 XDR    xdrs;
 char   buf[REQ_CODE_LEN];
 XDR_u_long resp = 0;

 const char *cm = "msclt_read_resp_code";

 memset ((void*) buf, '\0', REQ_CODE_LEN);
 if (msclt_readn(sd, buf, REQ_CODE_LEN) != REQ_CODE_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading response code from server");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return 0;
    }

 xdrmem_create (&xdrs, buf, REQ_CODE_LEN, XDR_DECODE);
 if (!xdr_u_long (&xdrs, &resp)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR decoding server response");
    fprintf (stderr, "%s:%s\n", cm, "... Returning with no data ...");
    xdr_destroy (&xdrs);
    return 0;
    }

 /* txh::added */
 xdr_destroy (&xdrs);
 return resp;
}
/*--------------------------------------------------------------------------*/
int msclt_read_ck_struct (int sd, msCkStruct *ckdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_read_ck_struct";

 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 if (msclt_readn(sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n%s:%s\n", cm,
	"ERROR reading ckdata from server",
	"Sys mesg", strerror(errno));
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    return -1;
    }

 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_DECODE);
 if (!xdr_msCkStruct(&xdrs, ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR decoding ckdata from server");
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_read_spk_struct (int sd, msSpkStruct *spkdata)
{
 XDR    xdrs;
 char   buf[SVR_DATA_BUF_LEN];

 const char *cm = "msclt_read_spk_struct";

 memset ((void*) buf, '\0', SVR_DATA_BUF_LEN);
 if (msclt_readn (sd, buf, SVR_DATA_BUF_LEN) != SVR_DATA_BUF_LEN) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading spkdata from server");
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    return -1;
    }

 xdrmem_create (&xdrs, buf, SVR_DATA_BUF_LEN, XDR_DECODE);
 if (!xdr_msSpkStruct(&xdrs, spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR decoding spkdata from server");
    fprintf (stderr, "%s:%s\n", cm,
	"....Returning with no data....");
    xdr_destroy (&xdrs);
    return -1;
    }

 xdr_destroy (&xdrs);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_connectToSvr (int *sd)
{
 char			*head,
			*tail;
 char			hname[1024],
			err_mesg[1024];
 short			portno;
 struct hostent		*hp;
 struct sockaddr_in	host_addr;
 int                    On=1;

 const char *cm = "msclt_connectToSvr";

 if ((portno = msclt_getPortno()) == (-1) ) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting port no");
    fprintf (stderr, "%s:%s\n", cm, "Cannot connect to spice server");
    return -1;
    }

 memset ((void*)hname, '\0', 1024);
 if (msclt_getSvrHostNames(hname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting server host names");
    fprintf (stderr, "%s:%s\n", cm,
	"Cannot connect to spice server");
    return -1;
    } 

 head = strtok (hname, ":");
 while (head) {
    if (!(hp = (struct hostent*) gethostbyname (head))) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR getting host info");
       fprintf (stderr, "%s:%s\n", "Host name: ", head);
       continue;
       }

    memset ((void*) &host_addr, '\0', sizeof(host_addr));
    host_addr.sin_family	= AF_INET;
    host_addr.sin_port		= htons(portno);
    host_addr.sin_addr.s_addr	=
		((struct in_addr *)(hp->h_addr))->s_addr;

    if ((*sd = socket(AF_INET, SOCK_STREAM, 0)) == (-1)) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR initializing connection socket");
       fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
       continue;
       }

    if (setsockopt(*sd, SOL_SOCKET, SO_KEEPALIVE, (char*)&On, sizeof(On)) < 0)
    {
       fprintf (stderr, "%s:%s\n", "Warning: Could not set keepalive option.",
                strerror(errno));
       continue;
    }

    fprintf (stdout, "Connecting: msserver@%s.....", head);
    if (connect(*sd, (struct sockaddr*) &host_addr,
			sizeof(host_addr)) < 0) {
       fprintf (stdout, "FAILED\n");
       head = strtok(NULL, ":");
       close (*sd);
       continue;
       }

    fprintf (stdout, "SUCCESS\n");
    if (msclt_write_client_name(sd)) {
       fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing client's name to server");
       fprintf (stderr, "%s:%s\n",
	"Sys mesg", strerror(errno));
       close (*sd);
       return -1;
       }

    return 0;
    }

 fprintf (stderr, "%s:%s\n", cm,
	"Cannot find an active spice server");
 return -1;
}
/*--------------------------------------------------------------------------*/
/* txh::modified to use 'cuserid' function to eliminate pwd.h and unistd.h  */
/*      function calls.                                                     */

int msclt_write_client_name (int *sd)
{
 char *clt_ptr, *cuserid_p2();

 const char *cm = "msclt_write_client_name";

 clt_ptr = cuserid_p2 ();

 if (msclt_write_string(*sd, clt_ptr, strlen(clt_ptr))) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing client name to socket");
    return -1;
    }

 return 0;
}
/*--------------------------------------------------------------------------*
 * read the config file and return the svr hostnames separated by * ":" 
 *--------------------------------------------------------------------------*/
int msclt_getSvrHostNames (char *hname)
{
 FILE	*cfile;
 char	*cfname;
 char	dataline[256];
 char	key[256];
 char	value[256];

 const char *cm = "msclt_getSvrHostName";

 if (!(cfname = (char*) getenv ("SPICE_CONFIG_FILE"))) {
    fprintf (stderr, "%s:%s\n",
	cm, "SPICE_CONFIG_FILE not define");
    return -1;
    }

 if (!(cfile = fopen(cfname, "r"))) {
    fprintf (stderr, "%s:%s\n%s:%s\n%s: %s\n",
	cm, "ERROR openning config file",
	"Sys mesg", strerror(errno),
	"File name", cfname);
    return -1;
    }

 while (fgets(dataline, 256, cfile)) {
     sscanf (dataline, "%s%s", key, value);
     if (!strcmp (key, CONFIG_FILE_SVR_HOST_KEY)) {
	strcat (hname, value);
	strcat (hname, ":");
	}
     } 

 fclose (cfile);
 return 0;
}
/*--------------------------------------------------------------------------*
 * - initialize socket
 * - send request code to server
 * - send msUserRequest struct to server
 * - listen for return data.
 * 	- server informational message
 *	- server error message
 *	- ckdata struct
 *	- spkdata struct
 *	- server completion code
 *
 * RETURN: 0 on SUCCESS
 *	  -1 on FAILURE
 *
 * code		: input
 * req		: input
 * ckdata	: output
 * spkdata	: output
 *
 *--------------------------------------------------------------------------*/
int msclt_getspice (XDR_u_long req_code, msUserRequestStruct *req,
		msCkStruct *ckdata, msSpkStruct *spkdata)
{
 int	sd;
 XDR_u_long resp_code;
 char	svr_mesg[SVR_MESG_BUF_LEN];

 const char *cm = "msclt_getspice";

 if (msclt_connectToSvr(&sd)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR connecting to server");
    fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
    return -1;
    }

 if (msclt_write_u_long (sd, req_code)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request code to server");
    fprintf (stderr, "%s:%s\n", cm,
	"Returning with no data");
    socket_close (sd);
    return -1;
    }
 else if (msclt_write_req_struct(sd, req)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request info to server");
    fprintf (stderr, "%s:%s\n", cm,
	"Returning with no data");
    socket_close (sd);
    return -1;
    }

 while (msclt_is_readable(sd)) {
    if (!(resp_code = msclt_read_resp_code(sd))) { 
       fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading response code from server");
       fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
       socket_close (sd);
       return -1;
       }

    switch (resp_code) {
	case SVR_INFO_MESG:
	   memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
	   if (	msclt_is_readable(sd) &&
		msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading server's info mesg");
	      fprintf (stderr, "%s:%s\n", cm,
		"Returning with no data");
	      socket_close (sd);
	      return -1;
	      }     
	   else fprintf (stdout, "%s\n", svr_mesg);
	   break;

	case SVR_ERR_MESG:
	   memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
	   if (	msclt_is_readable(sd) &&
		msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading err mesg from server");
	      fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
	      }
	   else fprintf (stderr, "%s\n", svr_mesg);
	   socket_close (sd);
	   return -1;

	case SVR_CK_DATA:
	   if (	msclt_is_readable(sd) &&
		msclt_read_ck_struct(sd, ckdata)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading server ckdata");
	      fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
	      socket_close (sd);
	      return -1;
	      }
	   break;

	case SVR_SPK_DATA:
	   if (	msclt_is_readable(sd) &&
		msclt_read_spk_struct(sd, spkdata)) {
	      fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading SPK data from server");
	      fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
	      socket_close (sd);
	      return -1;
	      }
	   break;
	
	case OPS_COMPLETED:
	   fprintf (stderr, "%s:%s\n", cm,
		"Done reading data from server");
	   fprintf (stderr, "%s:%s\n", cm,
		"Request completed SUCCESSFULLY");
	   socket_close (sd);
	   return 0;

	default:
	   sprintf (svr_mesg, "%s\n%s: %d",
		"Unknown response code from server",
		"Response code", resp_code);
	   fprintf (stderr, "%s:%s\n", cm, svr_mesg);
	   socket_close (sd);
	   return -1;
        }
    }

 fprintf (stderr, "%s:%s\n", cm, "ERROR polling client socket");
 fprintf (stderr, "%s:%s\n", cm, "Returning with no data from server");

 return -1;
}
/*--------------------------------------------------------------------------*/
int msclt_gllgetspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(GLL_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_gllgetspice",
	"ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casgetspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(CAS_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_casgetspice",
	"ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simgetspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(SIM_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_simgetspice",
        "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(VGR1_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr1getspice",
        "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2getspice (msUserRequestStruct *req,
        msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice(VGR2_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr2getspice",
        "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1getspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice (VO1_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_vo1getspice", "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2getspice (msUserRequestStruct *req,
	msCkStruct *ckdata, msSpkStruct *spkdata)
{
 if (msclt_getspice (VO2_GETSPICE, req, ckdata, spkdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vo2getspice", "ERROR getting data from server");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*
 * - initialize socket
 * - send request code to server
 * - send msCkStruct to server
 * - listen for return data
 *	- server informational message
 *	- server error message
 *	- server completion code
 *
 * RETURN: 0 on SUCCESS
 *	  -1 on FAILURE
 *
 * req_code	: input
 * ckdata	: input
 *
 *--------------------------------------------------------------------------*/
int msclt_putspice (XDR_u_long req_code, msCkStruct *ckdata)
{
 int	sd;
 XDR_u_long	resp_code;
 char	svr_mesg[SVR_MESG_BUF_LEN];

 const char *cm = "msclt_putspice";

 if (msclt_connectToSvr(&sd)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR connecting to server");
    fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
    return -1;
    }
 
 if (msclt_write_u_long (sd, req_code)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request code to server");
    fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
    socket_close (sd);
    return -1;
    }
 else if (msclt_write_ck_struct(sd, ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing ckdata to server");
    socket_close (sd);
    return -1;
    }

  while (msclt_is_readable(sd)) {
    if (!(resp_code = msclt_read_resp_code(sd))) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading response code from server");
       fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
       socket_close (sd);
       return -1;
       }
 
    switch (resp_code) {
        case SVR_INFO_MESG:
           memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
           if (msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
              fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading server's info mesg");
              fprintf (stderr, "%s:%s\n", cm, "Returning with no data");
              socket_close (sd);
              return -1;
              }
           else fprintf (stdout, "%s\n", svr_mesg);
           break;
 
        case SVR_ERR_MESG:
           memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);
           if (msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {
              fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading err mesg from server");
              fprintf (stderr, "%s:%s\n", cm,
		"Terminating server connection");
              }
           else fprintf (stderr, "%s\n", svr_mesg);
           socket_close (sd);
           return -1;
 
        case OPS_COMPLETED:
           fprintf (stderr, "%s:%s\n", cm, "Done reading data from server");
           fprintf (stderr, "%s:%s\n", cm, "Request completed SUCCESSFULLY");
           socket_close (sd);
           return 0;
 
        default:
           sprintf (svr_mesg, "%s\n%s: %d",
                "Unknown response code from server",
                "Response code", resp_code);
           fprintf (stderr, "%s:%s\n", cm, svr_mesg);
           socket_close (sd);
           return -1;
        }
    }

 fprintf (stderr, "%s:%s\n", cm, "ERROR polling client socket");
 fprintf (stderr, "%s:%s\n", cm, "Returning with no data");

 return -1;
}
/*--------------------------------------------------------------------------*/
int msclt_gllputspice (msCkStruct *ckdata)
{
 if (msclt_putspice(GLL_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
	"msclt_gllputspice",
	"ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casputspice (msCkStruct *ckdata)
{
 if (msclt_putspice(CAS_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_casputspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simputspice (msCkStruct *ckdata)
{
 if (msclt_putspice(SIM_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_simputspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VGR1_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr1putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VGR2_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vgr2putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VO1_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vo1putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2putspice (msCkStruct *ckdata)
{
 if (msclt_putspice(VO2_PUTSPICE, ckdata)) {
    fprintf (stderr, "%s:%s\n",
        "msclt_vo2putspice",
        "ERROR writing data to remote kernel");
    return -1;
    }
 else return 0;
}
/*--------------------------------------------------------------------------*
 * both bname & tname are input. Caller must select a temp text file before
 * calling this.
 *--------------------------------------------------------------------------*/
int msclt_bin2text_file (char *bname, char *tname)
{
 struct stat buf;
 const char *cm = "msclt_bin2text_file";
 if (stat(bname, &buf) == (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting binary file info");
    fprintf (stderr, "%s:%s\n", "Bin file", bname);
    fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
    return -1;
    }

 if (stat(tname, &buf) != (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:temp text file already exist");
    fprintf (stderr, "%s:%s", "Temp file", tname);
    return -1;
    }

 zms_spcb2a (bname, tname);
 if (zms_failed()) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR converting binary to text file");
    return -1;
    }

 return 0; 
}
/*--------------------------------------------------------------------------*/
int msclt_text2bin_file (char *tname, char *bname)
{
 struct stat buf;
 const char *cm = "msclt_text2bin_file";
 if (stat(tname, &buf) == (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:input text file does not exist");
    return -1;
    }
 
 if (stat(bname, &buf) != (-1)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR:output bin file already exist");
    return -1;
    }

 zms_spca2b (tname, bname);
 if (zms_failed()) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR converting text to binary file");
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*
 * Caller should already have sent the request code and the resulting
 *	file name before calling this. Caller is responsible for removing
 *	the temp text file when done!!!!
 *
 * 1/ send file size to server.
 * 2/ start sending text file content (1K at a time)
 * 3/ ask for ACK every 3pkts
 * 4/ ask for OPS_COMPLETED ack when done
 *--------------------------------------------------------------------------*/
int msclt_send_text_file (int sd, char *tname)
{
 XDR_u_long ack,
	nleft,
        filesize;
 int	pkg_count = 0;
 char   databuf[1024];
 struct stat statbuf;

 const char *cm = "msclt_send_text_file";

 FILE *infile;

 if ((stat(tname, &statbuf)) == (-1)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting file info");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 else filesize = (XDR_u_long) statbuf.st_size;

 if (msclt_write_u_long (sd, filesize)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing file size to socket");
    return -1;
    }

 if ((infile = fopen(tname, "r")) == (FILE*) NULL) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR openning text file");
    fprintf (stderr, "%s:%s\n", "Sys mesg", strerror(errno));
    return -1;
    }
 while (filesize) {
    nleft = (filesize < 1024) ? filesize : 1024;
    if (fread((void*) databuf, 1, nleft, infile) != nleft) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading text data from file");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;
       }
    if (msclt_writen(sd, databuf, nleft) != nleft) {
       fprintf (stderr, "%s:%s\n", cm,
		"ERROR writing text data to socket");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;
       }

    if (pkg_count == 3) {
       if (	msclt_is_readable(sd) &&
		msclt_read_u_long (sd, &ack)) {
	  fprintf (stderr, "%s:%s\n", cm,
		"ERROR reading ACK from socket");
	  fclose (infile);
	  return -1;
	  }
       else pkg_count = 0;

       if (ack != SVR_PKG_ACK) {
	  fprintf (stderr, "%s:%s\n", cm,
		"Un-expected positive ACK from socket");
	  fclose (infile);
	  return -1;
	  }

       fprintf (stderr, "Bytes left to send....%d\n", filesize);

       }
    else pkg_count++;

    filesize -= nleft;
    }

 fclose (infile);
 return 0;
}
/*--------------------------------------------------------------------------*
 * Caller should already have sent the appropriate request code and
 *	the resulting file name before calling this. Caller is responsible
 *	for deleting any temp text file when done!!!!
 *
 * NOTICE that the file name that you are passing should be an empty
 *	temp file. Anything in it will be over-written.
 *--------------------------------------------------------------------------*/
int msclt_receive_text_file (int sd, char *tname)
{
 XDR_u_long nleft,
	filesize,
	ack = SVR_PKG_ACK;
 int	pkg_count = 0;
 char	databuf[1024];
 struct	stat statbuf;

 FILE	*infile;

 const char *cm = "msclt_receive_text_file";

 if (!(infile = fopen(tname, "w"))) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR openning text file");
    fprintf (stderr, "%s:%s\n", "sys mesg", tname);
    return -1; 
    }

 if (msclt_read_u_long(sd, &filesize)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading file size off socket");
    fclose (infile);
    return -1;
    }

 while (filesize) {
    nleft = (filesize < 1024) ? filesize : 1024;
    if (msclt_is_readable(sd) &&
	msclt_readn(sd, databuf, nleft) != nleft) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR reading data from socket");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;	
       }
    if (fwrite((void*) databuf, 1, nleft, infile) != nleft) {
       fprintf (stderr, "%s:%s\n", cm, "ERROR writing data to file");
       fprintf (stderr, "%s:%s\n", "sys mesg", strerror(errno));
       fclose (infile);
       return -1;
       }
    if (pkg_count == 3) {
       if (msclt_write_u_long(sd, ack)) {
	  fprintf (stderr, "%s:%s\n", cm, "ERROR writing ACK to sender");
	  fclose (infile);
	  return -1;
	  }
       else pkg_count = 0;
       fprintf (stderr, "Bytes left to receive....%d\n", filesize);
       }
    else pkg_count++;

    filesize -= nleft;
    }

 fclose (infile);
 return 0;
}
/*--------------------------------------------------------------------------*
 * the request code and the actual kernel name should have been sent
 * before calling this.
 *--------------------------------------------------------------------------*/
int msclt_send_bin_kernel (int sd, char *kname)
{
 char	tname[256];
 const char *cm = "msclt_send_bin_kernel";

 memset ((void*) tname, '\0', 256);
 tmpnam (tname);

 if (msclt_bin2text_file(kname, tname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR creating temp text file");
    remove (tname);
    return -1;
    }
 if (msclt_write_u_long (sd, OPS_COMPLETED)) {

    }

 if (msclt_send_text_file (sd, tname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR sending text file to socket");
    remove (tname);
    return -1;
    }
 remove (tname);
 return 0;
}
/*--------------------------------------------------------------------------*
 * - get a temp file name.
 * - call "msclt_receive_text_file" to put the data in temp file
 * - when done, convert the text file to bin kernel.
 * - remove temp file.
 *--------------------------------------------------------------------------*/
int msclt_receive_bin_kernel (int sd, char *kname)
{
 char 	tname[256];
 const char *cm = "msclt_receive_bin_kernel";

 memset ((void*) tname, '\0', 256);
 tmpnam (tname);

 if (msclt_receive_text_file (sd, tname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR receiving text file from socket");
    remove (tname);
    return -1;
    }

 if (msclt_text2bin_file (tname, kname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR converting text file to binary kernel");
    remove (tname);
    return -1;
    }

 remove (tname);
 return 0;
}
/*--------------------------------------------------------------------------*
 * - initialize connection to server
 * - send request code to server
 * - send requested kernel name to server.
 * - listen for server's initial response.
 * - call function to receive bin file from server.
 *--------------------------------------------------------------------------*/
int msclt_receive_kernel (XDR_u_long req_code, char *kname, char *local_dir)
{
 int sd;
 XDR_u_long resp_code;
 char	new_kname[SVR_MESG_BUF_LEN];

 const char *cm = "msclt_get_kernel";

 if (msclt_connectToSvr (&sd)) {			/* open connection */
    fprintf (stderr, "%s:%s\n", cm,			/* to server.	   */
	"ERROR connecting to spice server\n");
    return -1;
    }
 if (msclt_write_u_long (sd, req_code)) {		/* send request    */
    fprintf (stderr, "%s:%s\n", cm,			/* code to server. */
	"ERROR writing request code to server");
    close (sd);
    return -1;
    }
 if (msclt_write_string (sd, kname, strlen(kname))) {	/* send requested  */
    fprintf (stderr, "%s:%s\n", cm,			/* file name to    */
	"ERROR writing kernel name to socket");		/* server.	   */
    close (sd);
    return -1;
    }

 if (!(resp_code = msclt_read_resp_code(sd))) {			/* read	   */
    fprintf (stderr, "%s:%s\n", cm,				/* resp	   */
        	"ERROR reading response code from server");	/* code	   */
       fprintf (stderr, "%s:%s\n", cm,				/* from	   */
		"Returning with no data");			/* server. */
       close (sd);
       return -1;
       }

 if (resp_code == SVR_ERR_MESG) {				/* cannot  */
    char svr_mesg[SVR_MESG_BUF_LEN];				/* get     */
    memset ((void*) svr_mesg, '\0', SVR_MESG_BUF_LEN);		/* kernel  */

    if ( msclt_is_readable(sd) &&				/* file    */
         msclt_read_string(sd, svr_mesg, SVR_MESG_BUF_LEN)) {	/* from    */
       fprintf (stderr, "%s:%s\n", cm,				/* server. */
            "ERROR reading err mesg from server");
       fprintf (stderr, "%s:%s\n", cm,
            "Terminating server connection");
       }
    else fprintf (stderr, "%s\n", svr_mesg);

    close (sd);
    return -1;
    }

 sprintf (new_kname, "%s/%s", local_dir, kname);
 if (msclt_receive_bin_kernel(sd, new_kname)) {		/* cool, ready to  */
    fprintf (stderr, "%s:%s\n%s:%s", cm,		/* receive bin     */
	"ERROR sending binary kernel to socket",	/* kernel from	   */
	"Kernel name", kname);				/* socket.	   */
    close (sd);
    return -1;
    }

 close (sd);
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_put_kernel (XDR_u_long req_code, char *kname)
{
 int sd;
 const char *cm = "msclt_put_kernel";

 if (msclt_connectToSvr(&sd)) {
    fprintf (stderr, "%s:%s\n", cm, 
	"ERROR connecting to spice server");
    return -1;
    }
 if (msclt_write_u_long(sd, req_code)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing request code to server");
    close (sd);
    return -1;
    }
 if (msclt_write_string (sd, kname, strlen(kname))) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing kernel name to socket");
    close (sd);
    return -1;
    }
 if (msclt_receive_bin_kernel(sd, kname)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR receiving binary kernel from server");
    close (sd);
    return -1;
    }

 close (sd);
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllgetck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (GLL_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_gllgetck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casgetck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (CAS_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_casgetck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simgetck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (SIM_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_simgetck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VGR1_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vgr1getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VGR2_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vgr2getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VO1_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vo1getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2getck (char *ckname, char *local_dir)
{
 if (msclt_receive_kernel (VO2_GET_CK, ckname, local_dir)) {
    fprintf (stderr, "msclt_vo2getck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllgetspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (GLL_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_gllgetspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casgetspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (CAS_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_casgetspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simgetspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (SIM_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_simgetspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VGR1_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vgr1getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VGR2_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vgr2getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VO1_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vo1getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2getspk (char *spk, char *local_dir)
{
 if (msclt_receive_kernel (VO2_GET_SPK, spk, local_dir)) {
    fprintf (stderr, "msclt_vo2getspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllputck (char *ckname)
{
 if (msclt_put_kernel (GLL_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_gllputck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casputck (char *ckname)
{
 if (msclt_put_kernel (CAS_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_casputck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simputck (char *ckname)
{
 if (msclt_put_kernel (SIM_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_simputck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1putck (char *ckname)
{
 if (msclt_put_kernel (VGR1_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vgr1putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2putck (char *ckname)
{
 if (msclt_put_kernel (VGR2_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vgr2putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1putck (char *ckname)
{
 if (msclt_put_kernel (VO1_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vo1putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2putck (char *ckname)
{
 if (msclt_put_kernel (VO2_PUT_CK, ckname)) {
    fprintf (stderr, "msclt_vo2putck:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", ckname);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int msclt_gllputspk (char *spk)
{
 if (msclt_put_kernel (GLL_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_gllputspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_casputspk (char *spk)
{
 if (msclt_put_kernel (CAS_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_casputspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_simputspk (char *spk)
{
 if (msclt_put_kernel (SIM_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_simputspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr1putspk (char *spk)
{
 if (msclt_put_kernel (VGR1_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vgr1putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vgr2putspk (char *spk)
{
 if (msclt_put_kernel (VGR2_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vgr2putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo1putspk (char *spk)
{
 if (msclt_put_kernel (VO1_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vo1putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
int msclt_vo2putspk (char *spk)
{
 if (msclt_put_kernel (VO2_PUT_SPK, spk)) {
    fprintf (stderr, "msclt_vo2putspk:ERROR getting kernel from server\n");
    fprintf (stderr, "Kernel name: %s\n", spk);
    return -1;
    }
 return 0;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*
 * returns the number of bytes actually read from the socket.
 *--------------------------------------------------------------------------*/
int msclt_readn (int sd, char *ptr, int nbytes)
{
 int            nleft,
                nread,
                status,
                count = 0,
                try = 0;  /* txh::added for socket read try */
 fd_set         readfds;
 struct timeval timeout;
 struct stat    f_stat;
 
 nleft = nbytes;                                /* init the total number */
 FD_ZERO(&readfds);                             /* of bytes and zero and */
 FD_SET(sd, &readfds);                          /* set readfds.          */
 
 memset ((char *) &timeout, '\0',               /* init the timeout      */
        sizeof (struct timeval));               /* count                 */
 timeout.tv_sec = (long) 5;

 fstat(sd, &f_stat);
 
 while ((nleft > 0) && (count < 10) && (try < 10)) {       /* keep reading  */
    /* txh::modified to distinguish between file and socket read for VMS */
    if ((f_stat.st_mode & S_IFMT) != S_IFREG)
       status = select (sd+1, (fd_set *) &readfds,         /* make sure sd  */
           (fd_set *) 0, (fd_set *) 0,                     /* is ready to   */
           (struct timeval *) &timeout);                   /* be read       */
    else status = 1;  /* a file descriptor */

    if ((status > 0) && (FD_ISSET(sd, &readfds))) {
       if ((f_stat.st_mode & S_IFMT) == S_IFREG)
          nread = read(sd, ptr, nleft);         /* read from a file */
       else
       {
          nread = socket_read(sd, ptr, nleft);  /* read from socket */

          /* txh::added to detect for case where the client goes away but 
                  select on socket descriptor returns no error. */
          if (nread == 0)
          {
             /* txh::detecting for continuous no data socket. */
             sleep (1);
             ++try;
          }
          else try = 0;
       }
       if (nread < 0) return -1;
       nleft -= nread;
       ptr += nread;
       }
    else if (status == 0) count++;
    else if (status < 0) return -1;
    }
 if (nleft == 0) return (nbytes);
 else return -1;
}
/*--------------------------------------------------------------------------*
 * returns the number of bytes actually written to the socket.
 *--------------------------------------------------------------------------*/
int msclt_writen (int sd, char *ptr, int nbytes)
{
 int            nleft,                       
                nwritten,
                status,
                count = 0;
 fd_set         writefds;
 struct timeval timeout;
 struct stat    f_stat;
 
 nleft = nbytes;
 FD_ZERO(&writefds);
 FD_SET(sd, &writefds);
 memset ((char *) &timeout, '\0',
        sizeof (struct timeval));
 timeout.tv_sec = (long) 5;

 fstat(sd, &f_stat);
 
 while (nleft > 0) {
    /* txh::modified to distinguish socket and file write for VMS */
    if ((f_stat.st_mode & S_IFMT) != S_IFREG)
       status = select (sd+1, (fd_set *) 0, (fd_set *) &writefds,
           (fd_set *) 0, (struct timeval *) &timeout);
    else status = 1;  /* a file descriptor */

    if (status > 0) {
       if ((f_stat.st_mode & S_IFMT) == S_IFREG)
          nwritten = write (sd, ptr, nleft); /* write to a file descriptor */
       else
          nwritten = socket_write(sd, ptr, nleft); /* write to a socket */
       if (nwritten <= 0) return -1;
       nleft -= nwritten;
       ptr += nwritten;
       }
    else if (status == 0) count ++;
    else if (status < 0) return -1;
    }
 if (nleft == 0) return (nbytes);
 else return -1;
}
/*--------------------------------------------------------------------------*
 * print out content of user request struct.
 *--------------------------------------------------------------------------*/
void msclt_printuserrequeststruct (msUserRequestStruct req)
{
 printf ("sc_id:\t\t\t%d\n", req.sc_id);
 printf ("system:\t\t\t%d\n", req.system);
 printf ("scet[0]:\t\t%d %d %d %d %d %d\n",
        req.scet[0], req.scet[1], req.scet[2],
        req.scet[3], req.scet[4], req.scet[5]);
 printf ("instrument_name:\t%s\n", req.instrument_name);
 printf ("target_name:\t\t%s\n\n", req.target_name);
 printf ("ck_id:\t\t%s\n", req.ck_id);
 printf ("ck_name:\t\t\t%s\n", req.ck_name);
 printf ("ck_source:\t\t%s\n", req.ck_source);
 printf ("spk_id:\t\t\t%s\n", req.spk_id);
 printf ("spk_name:\t\t\t%s\n", req.spk_name);
 printf ("seg_id:\t\t\t%s\n", req.provInfo.seg_id);
}
/*--------------------------------------------------------------------------*
 * print out content of CK struct.
 *--------------------------------------------------------------------------*/
void msclt_printckstruct (msCkStruct ck)
{
 printf ("sc_id:\t\t\t%d\n", ck.sc_id);
 printf ("instrument:\t\t%d\n", ck.instrument);
 printf ("system:\t\t\t%d\n", ck.system);
 printf ("scet[0]:\t\t%d %d %d %d %d %d\n\n",
        ck.scet[0], ck.scet[1], ck.scet[2],
        ck.scet[3], ck.scet[2], ck.scet[3]);

 printf ("av:\t\t\t%g %g %g\n\n",
	ck.av[0], ck.av[1], ck.av[2]);
 printf ("c_matrix:\t\t%g\t%g\t%g\n",
	ck.c_matrix[0], ck.c_matrix[1],
	ck.c_matrix[2]);
 printf ("\t\t\t%g\t%g\t%g\n",
	ck.c_matrix[3], ck.c_matrix[4],
	ck.c_matrix[5]);
 printf ("\t\t\t%g\t%g\t%g\n\n", 
	ck.c_matrix[6], ck.c_matrix[7], 
	ck.c_matrix[8]);
 
 printf ("ck_id:\t\t\t%s\n", ck.ck_id);
 printf ("ck_source:\t\t%s\n", ck.ck_source);
 printf ("segid:\t\t\t%s\n", ck.seg_id);
 printf ("ck_name:\t\t%s\n", ck.ck_name);
}
/*--------------------------------------------------------------------------*
 * print out content of SPK struct.
 *--------------------------------------------------------------------------*/
void msclt_printspkstruct (msSpkStruct spk)
{
 printf ("sc_id:\t\t\t\t%d\n", spk.sc_id);
 printf ("tgt_radius_l_axis:\t\t%g\n", spk.tgt_radius_l_axis);
 printf ("tgt_radius_s_axis:\t\t%g\n", spk.tgt_radius_s_axis);
 printf ("tgt_polar_radius:\t\t%g\n", spk.tgt_polar_radius);
 printf ("sc_pos_bd_centered:\t\t%g\t%g\t%g\n",
                spk.sc_pos_bd_centered[0],
                spk.sc_pos_bd_centered[1],
                spk.sc_pos_bd_centered[2]);
 printf ("pic_pos_sc_centered:\t\t%g\t%g\t%g\n",
                spk.pic_pos_sc_centered[0],
                spk.pic_pos_sc_centered[1],
                spk.pic_pos_sc_centered[2]);
 printf ("rs_vector:\t\t\t%g\t%g\t%g\n", spk.rs_vector[0],
                spk.rs_vector[1], spk.rs_vector[2]);
 
 printf ("range_pic_bd_2_sun:\t\t%g\n", spk.range_pic_bd_2_sun);
 printf ("range_sc_2_central_bd_cntr:\t%g\n", spk.range_sc_2_central_bd_cntr);
 printf ("range_sc_2_picture_bd_cntr:\t%g\n", spk.range_sc_2_picture_bd_cntr);
 printf ("tgt_bd_cntr_2_sun_lat:\t\t%g\n", spk.tgt_bd_cntr_2_sun_lat);
 printf ("tgt_bd_cntr_2_sun_lon:\t\t%g\n", spk.tgt_bd_cntr_2_sun_lon);
 printf ("tgt_bd_cntr_2_sc_lat:\t\t%g\n", spk.tgt_bd_cntr_2_sc_lat);
 printf ("tgt_bd_cntr_2_sc_lon:\t\t%g\n", spk.tgt_bd_cntr_2_sc_lon);
 
 printf ("\nme_matrix\t\t\t%g\t%g\t%g\n", spk.me_matrix[0],
                spk.me_matrix[1], spk.me_matrix[2]);
 printf ("\t\t\t\t%g\t%g\t%g\n", spk.me_matrix[3],
                spk.me_matrix[4], spk.me_matrix[5]);
 printf ("\t\t\t\t%g\t%g\t%g\n", spk.me_matrix[6],
                spk.me_matrix[7], spk.me_matrix[8]);
 printf ("om_matrix\t\t\t%g\t%g\t%g\n", spk.om_matrix[0],
                spk.om_matrix[1], spk.om_matrix[2]);
 printf ("\t\t\t\t%g\t%g\t%g\n", spk.om_matrix[3],
                spk.om_matrix[4], spk.om_matrix[5]);
 printf ("\t\t\t\t%g\t%g\t%g\n\n", spk.om_matrix[6],
                spk.om_matrix[7], spk.om_matrix[8]);
 
 printf ("north_angle:\t\t\t%g\n", spk.north_angle);
 printf ("sub_sc_line:\t\t\t%g\n", spk.sub_sc_line);
 printf ("sub_sc_samp:\t\t\t%g\n", spk.sub_sc_samp);
 printf ("p5_lat:\t\t\t\t%g\n", spk.p5_lat);
 printf ("p5_lon:\t\t\t\t%g\n", spk.p5_lon);
 printf ("p5_incidence_angle:\t\t%g\n", spk.p5_incidence_angle);
 printf ("p5_emission_angle:\t\t%g\n", spk.p5_emission_angle);
 printf ("p5_phase_angle:\t\t\t%g\n", spk.p5_phase_angle);
 printf ("p5_phase_angle:\t\t\t%g\n", spk.p5_phase_angle);
 printf ("p5_vert_pix_size:\t\t%g\n", spk.p5_vert_pix_size);
 printf ("p5_horiz_pix_size:\t\t%g\n", spk.p5_horiz_pix_size);
 printf ("range_sc2p5_intercept_pt:\t%g\n\n\n", spk.p5_horiz_pix_size);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ms_xdr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * Please do not edit this file.
 * It was generated using rpcgen.
 */

/*--------------------------------------------------------------------------
 * Apr.     1998  ...S.Le......   Initial release.
 *
 * Apr. 29, 1998  ...T.Huang...   Ported onto Alpha/VMS.
 *                                Cleaned the list of includes.
 *--------------------------------------------------------------------------*/

#include "ms_defines.h"

bool_t
xdr_msProvInfoStruct(xdrs, objp)
	XDR *xdrs;
	msProvInfoStruct *objp;
{
	if (!xdr_vector(xdrs, (char *)objp->institution, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->purpose, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->program_name, 7, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->spk_id, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->request_number, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->year, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->month_day, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->hour_min, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->user_id, 4, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->group_id, 4, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->seg_id, MAX_SEG_ID_LEN, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_msUserRequestStruct(xdrs, objp)
	XDR *xdrs;
	msUserRequestStruct *objp;
{
	if (!xdr_int(xdrs, &objp->sc_id)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->system)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->scet, 6, sizeof(int), (xdrproc_t) xdr_int)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->instrument_name, 64, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->target_name, 64, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->ck_id, 4, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->ck_name, 64, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->ck_source, 4, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->spk_id, 4, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->spk_name, 64, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_msProvInfoStruct(xdrs, &objp->provInfo)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_msCkStruct(xdrs, objp)
	XDR *xdrs;
	msCkStruct *objp;
{
	if (!xdr_int(xdrs, &objp->sc_id)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->instrument)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->system)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->scet, 6, sizeof(int), (xdrproc_t) xdr_int)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->av, 3, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_u_char(xdrs, &objp->avFlag)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->c_matrix, 9, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->seg_id, MAX_SEG_ID_LEN, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->ck_id, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->ck_name, 65, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->ck_source, 5, sizeof(char), (xdrproc_t) xdr_char)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_msSpkStruct(xdrs, objp)
	XDR *xdrs;
	msSpkStruct *objp;
{
	if (!xdr_int(xdrs, &objp->sc_id)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_radius_l_axis)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_radius_s_axis)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_polar_radius)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->sc_pos_bd_centered, 3, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->pic_pos_sc_centered, 3, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->rs_vector, 3, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->range_pic_bd_2_sun)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->range_sc_2_central_bd_cntr)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->range_sc_2_picture_bd_cntr)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_bd_cntr_2_sun_lat)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_bd_cntr_2_sun_lon)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_bd_cntr_2_sc_lat)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->tgt_bd_cntr_2_sc_lon)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->me_matrix, 9, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->om_matrix, 9, sizeof(double), (xdrproc_t) xdr_double)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->north_angle)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->sub_sc_line)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->sub_sc_samp)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_lat)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_lon)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_incidence_angle)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_emission_angle)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_phase_angle)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_vert_pix_size)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->p5_horiz_pix_size)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->range_sc2p5_intercept_pt)) {
		return (FALSE);
	}
	return (TRUE);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cam_info.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zms_bridge.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*

C Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:

        BODEUL  DAFCLS  DPR     M2Q     SCENCD
        BODFND  DAFENA  ERRACT  MXM     SCE2T
        BODVAR  DAFFNA  ERRPRT  MXMT    SCS2E
	CKGP			MXV	SCT2E	
				MTXV	SPKAPP
        CKGPAV  DAFGS   ET2UTC  PI	SPKLEF
        CKLPF   DAFHSF  FAILED  RECLAT  SPKSSB
        CLPOOL  DAFOPR  HALFPI          TWOPI
        DAFADA  DAFOPW  IRFROT  ROTATE  UTC2ET
        DAFBFS  DAFPS   LDPOOL  ROTMAT  VMINUS
        DAFBNA  DAFUS   M2EUL   RTPOOL  XPOSE
	======================================
	CKBSS	CKCLS	CKSNS 	CKPFS	CKUPF
	DAFA2B	DAFB2A	EUL2M	SCTIKS
	SPCA2B	SPCB2A	SPKUEF	SPKCLS	SURFPT
	VADD	VNORM	VSEP	VSUB
	TXTOPR  SPCT2B	TXTCLS	SCE2S
	CKW01	RESET
	Written By: Sam Le, 4/27/95
	======================================
        INVERT  TKFRAM
	Written by: Michael Brady 10/29/2001
	======================================

  Apr.     1998  ...S.Le......   Initial release.
 
  Apr. 29, 1998  ...T.Huang...   Ported onto Alpha/VMS.
                                 Cleaned the list of includes.
                                 Modified to call zreset1() instead of
                                  Fortran RESET to avoid name collusion.
*/

void zms_bodeul(body, et, ra, dec, w, lambda )
int	body;
double	et;
double	*ra;
double	*dec;
double	*w;
double	*lambda;
{
   FTN_NAME2(bodeul, BODEUL) ( &body, &et, ra, dec, w, lambda );
}

/* 1st bridge for BODFND, called from 	 */
int zms_bodfnd(body, item)
int body;
char *item;
{
   int i, status;
   i=strlen(item);

   status = FTN_NAME2(xms_bodfnd, XMS_BODFND) (&body, item, &i );
   return status;
}

/* 1st bridge for BODVAR, called from C	*/
void zms_bodvar(body, item, dim, values)

int body;
char *item;
int *dim;
double *values;
{
   int i;
   i=strlen(item);

   FTN_NAME2(xms_bodvar, XMS_BODVAR) (&body, item, &i, dim, values);
}

/* 1st bridge for CKGP, called from C */
void zms_ckgp(inst, sclkdp, tol, ref, cmat, clkout, status)

int inst;          	/*input*/
double sclkdp;          /*input*/
double tol;          	/*input*/
char *ref;          	/*input*/
void *cmat;   		/*output*/
double *clkout;   	/*output*/
int *status;   		/*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME2(xms_ckgp, XMS_CKGP) (&inst,&sclkdp,&tol,ref,&i,cmat,clkout,status);
}

/*
 1st bridge for CKGPAV, called from C 
*/
void zms_ckgpav(inst, sclkdp, tol, ref, cmat, av, clkout, status)

int inst;          	/*input*/
double sclkdp;          /*input*/
double tol;          	/*input*/
char *ref;          	/*input*/
void *cmat;   		/*output*/
void *av;   		/*output*/
double *clkout;   	/*output*/
int *status;   		/*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME2(xms_ckgpav, XMS_CKGPAV) (&inst,&sclkdp,&tol,ref,&i,cmat,av,
								clkout,status);
 }

/*
 1st bridge for CKLPF, called from C 
*/
void zms_cklpf(x, handl)

char *x;
int *handl;
{
   int i;
   i=strlen(x);

   FTN_NAME2(xms_cklpf, XMS_CKLPF) (x, &i, handl);
}

/*
1st bridge for CLPOOL, called from C 
*/
void zms_clpool()
{
  FTN_NAME2(clpool, CLPOOL) ();
}

/*
Bridge for DAFADA, called from C 
*/
void zms_dafada( buf, n)
int n;       /*input*/
void *buf;      /*input*/

{
   FTN_NAME2(dafada, DAFADA) ( buf, &n);
}

/*
1st bridge for DAFBFS, called from C 
*/
void zms_dafbfs( handle )
int    handle;      /*input*/

{
   FTN_NAME2(dafbfs, DAFBFS) ( &handle );
}

/*
 1st-stage bridge for DAFBNA, called from C 
*/
void zms_dafbna( handle, sum, name)

char *name; /*input*/
int handle; /*input*/
void *sum; /*input*/
{
   int i;
   i=strlen(name);

   FTN_NAME2_(xms_dafbna, XMS_DAFBNA) (&handle, sum, name, &i);
}

/*
 1st bridge for DAFCLS, called from C 
*/
void zms_dafcls(handle)

int handle;      /*input*/
{
   FTN_NAME2(dafcls, DAFCLS) (&handle);
}

/*
Bridge for DAFENA, called from C 
*/
void zms_dafena()

{
  FTN_NAME2(dafena, DAFENA) ();
  return;
}


/*
 1st bridge for DAFFNA, called from C 
*/
void zms_daffna(status)
int *status;     /*outpu*/
{
   FTN_NAME2_(xms_daffna, XMS_DAFFNA) (status);
}

/*
1st bridge for DAFGS, called from C 
*/
void zms_dafgs( sum )
void *sum;      /*output*/

{
   FTN_NAME2(dafgs, DAFGS) ( sum );
}

/*
Bridge for DAFHSF, called from C 
*/
void zms_dafhsf( handle , nd, ni )
int    handle;      /*input*/
int    *nd;         /*output*/
int    *ni;         /*output*/

{
   FTN_NAME2(dafhsf, DAFHSF) ( &handle, nd, ni );
}

/*
 1st bridge for DAFOPR, called from C 
*/
void zms_dafopr(fname, handle)

char *fname;
int *handle;
{
   int i;
   i=strlen(fname);

   FTN_NAME2_(xms_dafopr, XMS_DAFOPR) (fname, &i, handle);
}


/*
 1st-stage bridge for DAFOPW, called from C 
*/
void zms_dafopw(fname, handle)

char *fname;
int *handle;
{
   int i;
   i=strlen(fname);

   FTN_NAME2_(xms_dafopw, XMS_DAFOPW) (fname, &i, handle);
}

/*
Bridge for DAFPS, called from C 
*/
void zms_dafps( nd, ni, dc, ic, sum )
int    nd;        /*input*/
int    ni;        /*input*/
void *dc;        /*input*/
void  *ic;        /*input*/
void *sum;      /*output*/

{
   FTN_NAME2(dafps, DAFPS) ( &nd, &ni, dc, ic, sum );
}

/*
1st bridge for DAFUS, called from C 
*/
void zms_dafus( sum, nd, ni, dc, ic )
void *sum;      /*input*/
int    nd;        /*input*/
int    ni;        /*input*/
void *dc;        /*output*/
void  *ic;        /*output*/

{
   FTN_NAME2(dafus, DAFUS) ( sum, &nd, &ni, dc, ic );
}

/*
1st bridge for DPR, called from C 
*/
double zms_dpr()
{
  double dpr();
  double FTN_NAME2(dpr, DPR) ();

  return FTN_NAME2(dpr, DPR) ();
}

/*
1st bridge for RPD, called from C 
*/
double zms_rpd()
{
  double rpd();
  double FTN_NAME2(rpd, RPD) ();

  return FTN_NAME2(rpd, RPD) ();
}

/*
 1st bridge for ERRACT, called from C 
*/
void zms_erract(x,y)
char *x,*y;
{
   int i,j;
   i=strlen(x);
   j=strlen(y);
   FTN_NAME(xms_erract) (x,&i,y,&j);
}

/*
 1st bridge for ERRPRT, called from C 
*/
void zms_errprt(x,y)
char *x;            /*input*/
char *y;          /*i/o*/
{
   int i,j;
   i=strlen(x);
   j=strlen(y);
   FTN_NAME(xms_errprt) (x,&i,y,&j);
}

/*

NAIF SPICE C-language Bridge for ET2UTC

Bridge to be called from C language applications. 

 Written by:		Justin McNeill
 Date:			February 14, 1994
 Cognizant Engineer:	Justin McNeill (JFM059)

 History:

 February 25, 1994   	DEVELOPER NOTE

 The UTC string variable UTC_temp is of fixed length 80 to provide an
 appropriate interface with FORTRAN SPICE routine ET2UTC. UTC_temp is pruned 
 to remove superfluous blanks and returned to the user in the string UTC.

 02mar95 -lwk- prune the UTC string *before* moving it to the user buffer,
		to reduce chance of overwriting memory (can't be totally
		avoided since there is no way to emulate the character*(*)
		argument to ET2UTC from C)
*/

void zms_et2utc(ephemeris_time,format,precision,UTC)
double ephemeris_time;
char format;
int precision;
char *UTC;
{
int  i;
char UTC_temp[80];

FTN_NAME(xms_et2utc) (&ephemeris_time, &format, &precision, UTC_temp);

/* Process FORTRAN character string */

/* Remove blanks from the end of this string */
i = 79;
while(UTC_temp[i] == ' ' && i>0)
	i--;

strncpy(UTC, UTC_temp, ++i);

/* Add NULL terminator */
UTC[i] = '\0';
}

/*
 1st bridge for FAILED, called from C 
*/
int zms_failed()

{
 int status;

   status = FTN_NAME(failed) ();
   return status;
}

/*
1st bridge for PI, called from C 
*/
double zms_pi()
{
  double pi();
  double FTN_NAME(pi) ();

  return FTN_NAME(pi) ();
}

/*
1st bridge for HALFPI, called from C 
*/
double zms_halfpi()
{
  double halfpi();
  double FTN_NAME(halfpi) ();

  return FTN_NAME(halfpi) ();
}

/*
1st bridge for TWOPI, called from C 
*/
double zms_twopi()
{
  double twopi();
  double FTN_NAME(twopi) ();

  return FTN_NAME(twopi) ();
}

/*
1st bridge for IRFROT, called from C 
*/
void zms_irfrot( refa, refb, rotab )
int    refa;  /*input*/
int    refb;  /*input*/
void *rotab;  /*output*/

{
 int    i, j;
 double *dptr, tmout[3][3];

 FTN_NAME(irfrot) ( &refa, &refb, tmout );

 dptr = rotab;                           	/* now rotate the output */
 for (i = 0; i < 3; i++)                 	/* matrix into C format  */
     for (j = 0; j < 3; j++) {
         *dptr = tmout[j][i];
         dptr++;
         }
}

/*
 1st bridge for LDPOOL, called from C 
*/
void zms_ldpool(x)
char *x;
{
   int i;
   i=strlen(x);
   FTN_NAME(xms_ldpool) (x,&i);
}

/*
1st bridge for M2EUL, called from C 
*/
void zms_m2eul( r, axis3, axis2, axis1, angle3, angle2, angle1 )
void *r;        /* input */
int    axis3;        /* input */
int    axis2;        /* input */
int    axis1;        /* input */
double *angle3;        /* output */
double *angle2;        /* output */
double *angle1;        /* output */

{
   FTN_NAME(m2eul) ( r, &axis3, &axis2, &axis1, angle3, angle2, angle1 );
}

/* Bridge for M2Q, called from C	*/
void zms_m2q( m1, mout )
void *m1;   /*input*/
void *mout;   /*output*/
{
 int	i, j;
 double *dptr,
	tin[3][3],
	tm1[3][3];

 memcpy((void*)tin, (void*)m1, sizeof(double) * 9);

/************ ********** **********
 don't need to rotate anything....
 just leave it alone for now.
 dptr = &tm1[0][0];
 for (i = 0; i < 3; i++)
     for (j = 0; j < 3; j++) {
	 *dptr = tin[j][i];
	 dptr++;
	 }
************ ********** **********/

 FTN_NAME(m2q) ( m1, mout );	/* don't care about output, because it's */
}				/* a one dimensional array		 */

/* 1st bridge for MXM, called from C	*/
void zms_mxm( m1, m2, mout )
void *m1;				/*input*/
void *m2;				/*input*/
void *mout;				/*output*/
{
 int	i, j;
 double	*dptr,
	tin[3][3],
	tm1[3][3],
	tm2[3][3],
	tmout[3][3];

 memcpy ((void*)tin, (void*) m1, sizeof(double) * 9);

 dptr = &tm1[0][0];			/* rotate the two input	*/
 for (i = 0; i < 3; i++)		/* matrices into	*/
     for (j = 0; j < 3; j++) {		/* fortran format	*/
	 *dptr = tin[j][i];
	 dptr++; 
	 }

 memcpy ((void*)tin, (void*) m2, sizeof(double) * 9);

 dptr = &tm2[0][0];
 for (i = 0; i < 3; i++)
     for (j = 0; j < 3; j++) {
	 *dptr = tin[j][i];
	 dptr++;
	 }

 FTN_NAME(mxm) ( tm1, tm2, tmout );

 dptr = mout;				/* now rotate the output	*/
 for (i = 0; i < 3; i++)		/* matrix into C format		*/
     for (j = 0; j < 3; j++) {
	 *dptr = tmout[j][i];
	 dptr++;
	 }	
}

/* 1st bridge for MXMT, called from C */
void zms_mxmt( m1, m2, mout )
void *m1;		/*input*/
void *m2;		/*input*/
void *mout;		/*output*/
{
 int    i, j;
 double *dptr,
	tin[3][3],
        tm1[3][3],
        tm2[3][3],
        tmout[3][3];
 
 memcpy ((void*)tin, (void*) m1, sizeof(double) * 9);

 dptr = &tm1[0][0];                     /* rotate the two input */
 for (i = 0; i < 3; i++)                /* matrices into        */
     for (j = 0; j < 3; j++) {          /* fortran format       */
         *dptr = tin[j][i];
         dptr++;
         }
 
 memcpy ((void*)tin, (void*) m2, sizeof(double) * 9);

 dptr = &tm2[0][0];
 for (i = 0; i < 3; i++)
     for (j = 0; j < 3; j++) {
         *dptr = tin[j][i];
         dptr++;
         }

 FTN_NAME(mxmt) ( tm1, tm2, tmout );

 dptr = mout;                           /* now rotate the output        */
 for (i = 0; i < 3; i++)                /* matrix into C format         */
     for (j = 0; j < 3; j++) {
         *dptr = tmout[j][i];   
         dptr++;        
         }      

}

/*
1st bridge for MXV, called from C 
*/
void zms_mxv( matrix, vin, vout )
void  *matrix;    	/*input*/
void  *vin;          	/*input*/
void  *vout;         	/*output*/

{
 int            i, j;
 double         *dbl_ptr,
                tmatrix[3][3];
 dbl_ptr = matrix;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       tmatrix[j][i] = *dbl_ptr;
       dbl_ptr++;
       }
 FTN_NAME(mxv) ( tmatrix, vin, vout);
}

/*
1st bridge for MTXV, called from C 
*/
void zms_mtxv( matrix, vin, vout )
void *matrix;    	/*input*/
void  *vin;          	/*input*/
void  *vout;         	/*output*/

{
 int    i, j;
 double *dbl_ptr,
        tmatrix[3][3];
 dbl_ptr = matrix;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       tmatrix[j][i] = *dbl_ptr;
       dbl_ptr++;
       }
 FTN_NAME(mtxv) ( tmatrix, vin, vout);
}

/*
1st bridge for RECLAT, called from C 
*/
void zms_reclat(rectan, radius, longi, lat)

void *rectan;  /*input*/
double *radius;
double *longi;
double *lat;
{
   FTN_NAME(reclat) (rectan, radius, longi, lat);
}


/*
1st bridge for ROTATE, called from C 
*/
void zms_rotate( angle, iaxis, mout )
double angle;        	/* input */
int    iaxis;         	/* input */
void *mout;   		/* output */

{
   FTN_NAME(rotate) ( &angle, &iaxis, mout );
}

/*
1st bridge for ROTMAT, called from C 
*/
void zms_rotmat( m1, angle, iaxis, mout )
void  *m1;      /*input*/
double angle;      /*input*/
int    iaxis;      /*input*/
void  *mout;      /*output*/

{
   FTN_NAME(rotmat) ( m1, &angle, &iaxis, mout );
}

/*
 1st bridge for RTPOOL, called from C 
*/
void zms_rtpool(x,dim,values,flag)
char 	*x;
int 	*dim;
double 	*values;
int 	*flag;
{
   int i;
   i=strlen(x);

   FTN_NAME(xms_rtpool) (x,&i,dim,values,flag);
}

/*
1st bridge for SCENCD, called from C 
*/
void zms_scencd( sc, text, sclkdp)
int    sc;           /*input*/
char *text;         /*input*/
double *sclkdp;       /*output*/

{
  int i;
  i=strlen(text);
  FTN_NAME(xms_scencd) (  &sc, text, &i, sclkdp);
}

/*
1st bridge for SCE2T, called from C 
*/
void zms_sce2t( sc, et, sclkdp)
int    sc;           /*input*/
double et;         /*input*/
double *sclkdp;       /*output*/

{
   FTN_NAME(sce2t) (  &sc, &et, sclkdp);
}

/*

NAIF SPICE C-language Bridge for SCS2E

Bridge to be called from C language applications. 

 Written by:		Justin McNeill
 Date:			February 14, 1994
 Cognizant Engineer:	Justin McNeill (JFM059)

*/
void zms_scs2e(spacecraft_code,SCLK,ephemeris_time)
int spacecraft_code;
char *SCLK;
double *ephemeris_time;
{
int i;

i=strlen(SCLK);		/* Get string length of SCLK */
FTN_NAME(xms_scs2e) (&spacecraft_code, SCLK, &i, ephemeris_time);
}

/*
1st bridge for SCT2E, called from C 
*/
void zms_sct2e( sc, sclkdp, et )
int    sc;           /*input*/
double sclkdp;      /*input*/
double *et;          /*output*/

{
   FTN_NAME(sct2e) ( &sc, &sclkdp, et );
}

/*
 1st bridge for SPKAPP, called from C 
*/
void zms_spkapp( targ, et, ref, sobs, abcorr, starg, lt )

int targ;          /*input*/
double et;        /*input*/
char *ref;        /*input*/
void *sobs;        /*input*/
char *abcorr;        /*input*/
void *starg;        /*output*/
double *lt;       /*output*/
{
   int i,j;
   i=strlen(ref);
   j=strlen(abcorr);

   FTN_NAME(xms_spkapp) (&targ,&et,ref,&i,sobs,abcorr,&j,starg,lt);
}


/*
 1st bridge for SPKLEF, called from C 
*/
void zms_spklef(x, handl)
char *x;
int *handl;
{
   int i;
   i=strlen(x);

   FTN_NAME(xms_spklef) (x, &i, handl);
}

/*
 1st bridge for SPKSSB, called from C 
*/
void zms_spkssb(targ, et, ref, starg)

int targ;           /*input*/
double et;           /*input*/
char *ref;           /*input*/
void *starg;      /*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xms_spkssb) (&targ, &et, ref, &i, starg);
}


/*
 1st bridge for UTC2ET, called from C 
*/
void zms_utc2et(utc, et)

char *utc;
double *et;

{
   int i;
   i=strlen(utc);

   FTN_NAME(xms_utc2et) (utc, &i, et);

}


/*
1st bridge for VMINUS, called from C 
*/
void zms_vminus( v1, vout )
void *v1;       /*input*/
void *vout;      /*output*/


{
   FTN_NAME(vminus) ( v1, vout );
}


/*
Bridge for XPOSE, called from C 
*/
void zms_xpose( m1, mout )
void *m1;   /*input*/
void *mout;   /*output*/


{
 int    i, j;
 double *dbl_ptr,
        tm1[3][3],
        tmout[3][3];

 dbl_ptr = m1;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       tm1[j][i] = *dbl_ptr;
       dbl_ptr++;
       }
 FTN_NAME(xpose) ( tm1, tmout );
 dbl_ptr = mout;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dbl_ptr = tmout[j][i];
       dbl_ptr++;
       }
}
/*=====================================================
1st bridge for CKBSS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Search through loaded file to find data segment with the
same s/c instrument and sclk value (or within sclk_tol)
=======================================================*/
void zms_ckbss(inst, sclk, tol, needav)
 int	inst;				/* input: instrument id	*/
 double	sclk;				/* input: sclk value	*/
 double	tol;				/* input: error_tolrnce */
 int	needav;				/* input: need av ?	*/
{
 FTN_NAME(ckbss) (&inst, &sclk, &tol, &needav);
}
/*=====================================================
1st bridge for CKCLS, called from C
Written By: Sam Le
Date      : 11/30/1997

Development Note: close up given CK
=======================================================*/
void zms_ckcls(handle)
 int    handle;
{
 FTN_NAME(ckcls) (&handle);
}
/*=====================================================
1st bridge for CKSNS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
CKBSS specifies the search criteria and CKSNS searches
through the loaded files for data segment(s) that match
the criteria; segment id and descriptor are also returned
=======================================================*/
void zms_cksns(int *handle, double *descr, char *segid, int *found,
								ZFORSTR_PARAM)
#if 0
 int		*handle;		/* output	*/
 double		*descr;			/* output	*/
 char 		*segid;			/* output	*/
 int		*found;			/* output	*/
#endif
{
 ZFORSTR_BLOCK
 char	temp[41];

 FTN_NAME(xms_cksns) (handle, descr, temp, found);
 zsfor2c(segid, 40, temp, &handle, 4, 3, 1, found);
}
/*==============================================
1st bridge for CKPFS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Evaluate pointing data from a given segment for
a given time.
================================================*/
void zms_ckpfs(handle, descr, sclkin, tol, needav, 
			cmat, av, clkout, found)
 int		handle;			/*   input	*/
 double		*descr;			/*   input	*/
 double		sclkin;			/*   input	*/
 double		tol;			/*   input	*/
 int		needav;			/*   input	*/
 double		*cmat;			/*   output	*/
 double		*av;			/*   output	*/
 double		*clkout;		/*   output	*/
 int		*found;			/*   output	*/
{
 int		i, j;
 double		*dptr, tcmat[3][3];

 FTN_NAME(xms_ckpfs) (&handle, descr, &sclkin, &tol,
		&needav, cmat, av, clkout, found);

/********** ********* **************
 don't need to rotate...just push
 the data through for now.

 dptr = cmat;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dptr = tcmat[j][i];
       dptr++;
       }
********** ********* **************/
}
/*=====================================================
1st bridge for CKUPF, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Unload a CK pointing file so that it will no longer be
searched by the readers.
=======================================================*/
void zms_ckupf(handle)
 int handle;
{
 FTN_NAME(ckupf) (&handle);
}
/*=====================================================
1st bridge for DAFA2B, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Given to file names "daf_file" & "ascii_file", DAFA2B
converts the ascii (text) file into its daf file (binary).
=======================================================*/
void zms_dafa2b(ascii_name, daf_name, resv)
 char	*ascii_name;
 char	*daf_name;
 int	resv;
{
 int	asc_len,
	daf_len;

 asc_len = strlen(ascii_name);
 daf_len = strlen(daf_name);

 if ((asc_len > 80) || (daf_len > 80))
    printf("ZDAFA2B::File Name Is Too Long\n");
 else
    FTN_NAME(xms_dafa2b) (ascii_name, &asc_len,
		daf_name, &daf_len, &resv);
}
/*=====================================================
1st bridge for DAFB2A, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Given to file names "daf_file" & "ascii_file", DAFB2A
converts the daf (binary) file into its ascii file (text).
=======================================================*/
void zms_dafb2a(dname, aname)
 char   *dname;
 char   *aname;
{
 int    daf_len,
        asc_len;

 asc_len = strlen(aname);
 daf_len = strlen(dname);
 if ((asc_len > 80) || (daf_len > 80))
    printf("ZDAFB2A::File Name Is Too Long\n");
 else
    FTN_NAME(xms_dafb2a) (dname, &daf_len, aname, &asc_len);
}
/*=====================================================
1st bridge for EUL2M, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
EUL2M constructs a rotation matrix from a set of Euler
angles and their rotation axes

last modified: Tue Oct  8 18:17:04 PDT 1996
	remove matrix inversion requested by LWK.
=======================================================*/
void zms_eul2m(angle3, angle2, angle1, axis3, axis2, axis1, matrix)
 double                 angle3;                 /* input        */
 double                 angle2;                 /* input        */
 double                 angle1;                 /* input        */
 int                    axis3;                  /* input        */
 int                    axis2;                  /* input        */
 int                    axis1;                  /* input        */
 double                 *matrix;                /* output       */
{
 int            i, j;
 double         *dptr, tmatrix[3][3];

 FTN_NAME(eul2m) (&angle3, &angle2, &angle1,
                &axis3, &axis2, &axis1, tmatrix);
 dptr = matrix;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dptr = tmatrix[j][i];
       dptr++;
       }
}
/*=====================================================
1st bridge for SPCA2B, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSPCA2B convert the given ascii CK/SPK file into its
binary format, including the comment area.
=======================================================*/
void zms_spca2b(ascfile, binfile)
 char *ascfile; char *binfile;
{
 int    asclen, binlen;

 asclen = strlen(ascfile);
 binlen = strlen(binfile);
 if ((asclen > 80) || (binlen > 80))
    printf("ZSPCA2B: File name is too long\n");
 else
    FTN_NAME(xms_spca2b) (ascfile, &asclen, binfile, &binlen);
}
/*=====================================================
1st bridge for SPCB2A, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSPCB2A convert the given binary CK/SPK file into its
ascii format, including the comment area.
=======================================================*/
void zms_spcb2a(binfile, ascfile)
 char *binfile;
 char *ascfile;
{
 int binlen, asclen;

 binlen = strlen(binfile);
 asclen = strlen(ascfile);
 if ((binlen > 80) || (asclen > 80))
    printf("ZSPCB2A: File name is too long\n");
 else
    FTN_NAME(xms_spcb2a) (binfile, &binlen, ascfile, &asclen);
}
/*=====================================================
1st bridge for SCTIKS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSCTIKS receives a s/c_id and a clock string then
convert the clock string into ticks for that specific
spacecraft
=======================================================*/
void zms_sctiks(sc, clkstr, ticks)
 int            sc;
 char           *clkstr;
 double         *ticks;
{
 int    i;
 char	for_str[15];

 i = strlen(clkstr);
 FTN_NAME(xms_sctiks) (&sc,clkstr, &i, ticks);
 }
/*=====================================================
1st bridge for SPKCLS, called from C
Written By: Sam Le
Date      : 11/30/1997

Development Note: close up given SPK
=======================================================*/
void zms_spkcls(handle)
 int    handle;
{
 FTN_NAME(spkcls) (&handle);
}
/*=====================================================
1st bridge for SPKUEF, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Unload an ephemeris file so that it will no longer be
searched by the readers
=======================================================*/
void zms_spkuef(handle)
 int handle;
{
 FTN_NAME(spkuef) (&handle);
}
/*=====================================================
1st bridge for VADD, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
add two vector v1[3] and v2[3] and return the results
back via vout[3]
=======================================================*/
void zms_vadd_mat(v1, v2, vout)
 double	*v1;
 double *v2;
 double *vout;
{
 FTN_NAME(vadd) (v1, v2, vout);
}
/*=====================================================
1st bridge for SURFPT, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Determine the intersection of a line-of-sight vector
with the surface of an ellipsoid.
=======================================================*/
void zms_surfpt(pos, u, a, b, c, pts, fnd)
 double *pos;			/* input	*/
 double *u;			/* input	*/
 double a;			/* input	*/
 double b;			/* input	*/
 double c;			/* input	*/
 double *pts;			/* output	*/
 int	*fnd;			/* output	*/
{
 FTN_NAME(xms_surfpt) (pos, u, &a, &b, &c, pts, fnd);
}
/*=====================================================
1st bridge for VNORM, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Compute the magnitude of a double precision,
3-dimensional vector.
=======================================================*/
double zms_vnorm(v)
 double *v;
{
 double value;
 double FTN_NAME(vnorm) (double *);
 return (FTN_NAME(vnorm) (v));
}
/*=====================================================
1st bridge for VSUB, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Compute the different between two 3-dimensional, double
precision vectors
=======================================================*/
void zms_vsub(v1, v2, vout)
 double	*v1;			/* input	*/
 double *v2;			/* input	*/
 double *vout;			/* output	*/
{
 FTN_NAME(vsub) (v1, v2, vout);
}
/*=====================================================
1st bridge for SURFNM, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Computes the outward-pointing, unit normal vector
from a point on the surface of an ellipsoid.
=======================================================*/
void zms_surfnm(a, b, c, pt, out)
 double	a;
 double b;
 double c;
 double *pt;
 double *out;
{
 FTN_NAME(surfnm) (&a, &b, &c, pt, out);
}
/*=====================================================
1st bridge for VSEP, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Find the separation angle in radians between two double
precision, 3-dimensional vectors. This angle is defined as
zero if either vector is zero.
=======================================================*/
double zms_vsep(v1, v2)
 double	*v1;
 double *v2;
{
 double FTN_NAME(vsep) (double *, double *);
 return (FTN_NAME(vsep) (v1, v2));
}
/*======================================================
1st bridge for TXTOPR, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Open a text (ASCII) file for reading, return the unit number
========================================================*/
void zms_txtopr(fname, unit)
 char *fname; int *unit;
{
 int flen;

 flen = strlen(fname);
 if ((flen > 80) || (flen <= 0)) {
    printf("ZTXTOPR: File name is too long/short\n");
    exit (0);
    }
 FTN_NAME(xms_txtopr) (fname, &flen, unit);
}
/*======================================================
1st bridge for TXTOPR, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Convert a given text file into its corresponding binary
format.
========================================================*/
void zms_spct2b(unit, fname)
 int	unit;
 char	*fname;
{
 int	len;
 len = strlen(fname);
 if (len >= 80) 
    printf("Error File Name Is Too Long\n");
 else
    FTN_NAME(xms_spct2b) (&unit, fname, len);
}
/*======================================================
1st bridge for TXTCLS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Close the text file associated with the given unit
========================================================*/
void zms_txtcls(unit)
 int	unit;
{
 FTN_NAME(xms_txtcls) (&unit);
}
/*=======================================================
1st bridge for SCE2S, called from C
Written By: Sam Le
Date	  : 06/6/1995

Development Note:
Convert a given ETC (double precision) to SCLK string
=========================================================*/
void zms_sce2s(sc, etc, sclk)
 int	sc;
 double	etc;
 char	*sclk;
{
 int	i;
 char	SCLK_temp[80];

 FTN_NAME(xms_sce2s) (&sc, &etc, SCLK_temp);

 i = 79;
 while (SCLK_temp[i] == ' ') i--;
 i++; SCLK_temp[i] = '\0';
 strcpy(sclk, SCLK_temp);
}
/*=======================================================
1st bridge for DAFGH, called from C
Written by: Sam Le
Date	  : 09/14/1995

Development Note:
DAFGH returns the handle of the file being searched
currenty. MIPS SPICE routines used this with GAFHFN to
retrieve the name and id of the file being searched.
=========================================================*/
void zms_dafgh(handle)
 int *handle;
{
 FTN_NAME(dafgh) (handle);
}
/*=========================================================
1st bridge for DAFHFN, called from C
Written by: Sam Le
Date	  : 09/14/1995

Development Note:
DAFHFN return the name of the file currently being searched
given its file handle. This subroutine does not check for
the string length of the fname. It assumes that the calling
program allocate enough memory space to store the file name.
===========================================================*/
void zms_dafhfn(int *handle, char *fname, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 char	temp_fname[256];

 FTN_NAME(xms_dafhfn) (handle, temp_fname);
 zsfor2c(fname, 256, temp_fname, &handle, 2, 2, 1, fname);
}
/*=========================================================
1st bridge for BODN2C, called from C
Written by: Sam Le
Date      : 2/20/1997

Development Note:
returns the target body NAIF code, given the target name
===========================================================*/
void zms_bodn2c (fname, id, found)
 char *fname;
 int  *id;
 int  *found;
{
 int  fname_len;
 fname_len = strlen(fname);
 if (fname_len > 80)
    printf ("BODN2C:target name is too long\n");
 else
    FTN_NAME(xms_bodn2c) (fname, &fname_len, id, found);
}
/*=========================================================
1st bridge for CKW01, called from C
Written by: Sam Le
Date      : 3/5/1997

Development Note:
add a type 1 segment to a C-kernel

===========================================================*/
void zms_ckw01 (int handle, double begtime, double endtime,
	int instrument, char *reference, int avflag,
	char *segid, int nrec, double sclkdp,
	double *quat, double *av)
{
 int len = strlen(reference);
 FTN_NAME(xms_ckw01) (&handle, &begtime, &endtime,
		&instrument, reference, &len, &avflag,
		segid, &nrec, &sclkdp, quat, av);
}
/*===========================================================*/
void zms_reset ()
{
 /* txh::modified to eliminate RESET call conflict 
 FTN_NAME(reset) ();
 */
 zreset1();
}
/*=========================================================
1st bridge for TKFRAM, called from C
Written by: Michael Brady
Date      : 10/29/2001

Development Note:
Returns a matrix <rot> for the specified frame <id>, as well as
a NAIF ID <frame>, if the frame is found in a currently loaded kernel.
===========================================================*/
void zms_tkfram(id, rot, frame, fnd)
     int id;		        /* input	*/
     double rot[3][3];		/* output	*/
     int* frame;		/* output	*/
     int* fnd;		        /* output	*/
{
 double trot[3][3] = {0};
 double* dptr = 0;
 int i = 0;
 int j = 0;
  
 FTN_NAME(xms_tkfram) (&id, trot, frame, fnd);

 dptr = &rot[0][0];                           	/* now rotate the output */
 for (i = 0; i < 3; ++i)                 	/* matrix into C format  */
     for (j = 0; j < 3; ++j) {
         *dptr = trot[j][i];
         ++dptr;
         }
}
/*=========================================================
1st bridge for INVERT, called from C
Written by: Michael Brady
Date      : 10/29/2001

Development Note:
Returns a matrix <rot> for the specified frame <id>, as well as
a NAIF ID <frame>, if the frame is found in a currently loaded kernel.
===========================================================*/
void zms_invert(m1, m2)
     double m1[3][3];	        /* input	*/
     double m2[3][3];		/* output	*/
{
 FTN_NAME(xms_invert) (m1, m2);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xms_bridge.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C
C	FORTRAN  Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:
C
C		BODFND
C		BODVAR
C		CKLPF
C		CKGP
C		CKGPAV
C		ERRACT
C		ERRPRT
C		ET2UTC
C		DAFBNA
C		DAFFNA
C		DAFOPR
C		DAFOPW
C		LDPOOL
C		RTPOOL
C		SCENCD
C		SCS2E
C		SPKAPP
C		SPKLEF
C		SPKSSB
C		UTC2ET
C		=============================================
C		EUL2M
C		SCTIKS		written by: Sam Le 04/27/1995
C		CKBSS
C		CKSNS
C		CKPFS
C		DAFA2B
C		DAFB2A
C		SPCA2B
C		SPCB2A
C		TXTOPR
C		SPCT2B
C		TXTCLS
C		=============================================
C               INVERT
C               TKFRAM   Written by: Michael Brady 10/29/2001
C		=============================================
C

c***************************************** 
c xms_bodfnd:
c 2nd-stage bridge to BODFND, in Fortran
c****************************************** 

      INTEGER FUNCTION xms_bodfnd(body, item, i)
     
      integer body
      byte item(1)
      integer i
      logical found
      logical bodfnd
      character*20 text

      text=' '

      if (i.gt.80) print*, 'xms_bodfnd: string is too long'

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      found = bodfnd(body,text)
      if ( found ) then
C txh::removed because of invalid variable name.
C       xbodfnd = 1
	xms_bodfnd = 1
      else
C txh::removed because of invalid variable name.
C       xbodfnd = 0
	xms_bodfnd = 0
      endif

      return
      end


c***************************************** 
c  xms_bodvar:
C  2nd-stage bridge to BODVAR, in Fortran
c****************************************** 

      subroutine xms_bodvar(body, item, i, dim, values)
      
      integer body
      byte item(1)
      integer i
      integer dim
      double precision values(*) 
      character*80 text

      text=' '

      if (i.gt.80) print*,'xms_bodvar, string too long'

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      call bodvar(body, text, dim, values)

      return
      end


c***************************************** 
c  xms_spklef:
C  2nd-stage bridge to SPKLEF, in Fortran
c****************************************** 

      subroutine xms_spklef(x, i, handl)
      
      integer i, handl
      byte x(1)
      character*80 text

      if (i.gt.80) print*,'xspklef, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call spklef(text, handl)

      return
      end


c***************************************** 
c  xms_utc2et:
C  2nd-stage bridge to UTC2ET, in Fortran
c****************************************** 

      subroutine xms_utc2et(utc, i, et)
      
      integer i
      byte utc(1)
      double precision et
      character*80 text

      if (i.gt.80) print*,'xutc2et, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(utc, text, i)

      call utc2et(text, et)

      return
      end


c***************************************** 
c xms_cklpf:
C 2nd-stage bridge to CKLPF, in Fortran
c****************************************** 

      subroutine xms_cklpf(x, i, handl)
      
      integer i, handl
      byte x(1)
      character*80 text

      if (i.gt.80) print*, 'xcklpf, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call cklpf(text, handl)

      return
      end


c***************************************** 
c xms_ckgp:
C  2nd-stage bridge to CKGP, in Fortran
c****************************************** 

      subroutine xms_ckgp(inst,sclkdp,tol,ref,i,cmat,clkout,status)

      
      double precision sclkdp
      double precision tol
      double precision cmat(3,3)
      double precision clkout
      integer inst
      integer status
      integer i
      byte ref(1)
      character*80 text
      logical found


      if (i.gt.80) print*, 'xckgp, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)

      call ckgp(inst, sclkdp, tol, text, cmat, clkout, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 

      return
      end


c***************************************** 
c xms_ckgpav:
C  2nd-stage bridge to CKGPAV, in Fortran
c****************************************** 

      subroutine xms_ckgpav(inst,sclkdp,tol,ref,i,cmat,av,clkout,status)

      
      double precision sclkdp
      double precision tol
      double precision cmat(3,3)
      double precision av(3)
      double precision clkout
      integer inst
      integer status
      integer i
      byte ref(1)
      character*80 text
      logical found


      if (i.gt.80) print*, 'xckgpav, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)

      call ckgpav(inst, sclkdp, tol, text, cmat, av, clkout, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 

      return
      end


c***************************************** 
c xms_errprt:
C 2nd-stage bridge to ERRPRT in Fortran
c****************************************** 

      subroutine xms_errprt(x,i,y,j)
      
      integer i, j
      byte x(1)
      byte y(1)
      character*80 cmd
      character*80 option

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	print*, 'xerrprt, first string is too long'
      endif

      if (j.gt.80) then
	print*, 'xerrprt, second string is too long'
      endif

C     Initialize arrays

      cmd=' '
      option=' '

C     Transformations to Fortran-string

      call mvlc(x,cmd,i)
      call mvlc(y,option,j)

      call errprt(cmd,option)

      return
      end


c***************************************** 
c xms_dafopr:
C 2nd-stage Bridge to DAFOPR, in Fortran
c****************************************** 

      subroutine xms_dafopr(fname,i,handle)
      
      integer handle
      integer i
      byte fname(1)
      character*80 text

      if (i.gt.80) print*, 'xdafopr, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(fname,text,i)

      call dafopr(text,handle)

      return
      end

c***************************************** 
c xms_spkapp:
C 2nd-stage bridge to SPKAPP in Fortran
c****************************************** 

      subroutine xms_spkapp(targ,et,ref,i,sobs,abcorr,j,starg,lt)
      
      double precision et
      double precision sobs(6)
      double precision starg(6)
      double precision lt
      integer targ
      integer i, j
      byte ref(1)
      byte abcorr(1)
      character*80 texta
      character*80 textb

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	print*, 'xspkapp, first string is too long'
      endif

      if (j.gt.80) then
	print*, 'xspkapp, second string is too long'
      endif

C     Initialize arrays

      texta = ' '
      textb = ' '

C     Transformations to Fortran-string

      call mvlc(ref,texta,i)
      call mvlc(abcorr,textb,j)

      call spkapp(targ,et,texta,sobs,textb,starg,lt)

      return
      end



c***************************************** 
c   xms_spkssb:
C   2nd-stage bridge to SPKSSB, in Fortran
c*****************************************

      subroutine xms_spkssb(targ,et,ref,i,starg)
      
      double precision et
      double precision starg(6)
      integer targ
      integer i
      byte ref(1)
      character*80 text

      if (i.gt.80) print*, 'xspkssb, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(ref,text,i)
      call spkssb(targ,et,text,starg)

      return
      end


c***************************************** 
c  xms_erract:
C  2nd-stage bridge to ERRACT in Fortran
c****************************************** 

      subroutine xms_erract(x,i,y,j)
      
      integer i, j
      byte x(1)
      byte y(1)
      character*80 cmd
      character*80 option

C     Check string lengths and print error message if needed

      if (i.gt.80) then
	print*, 'xerract, first string is too long'
      endif

      if (j.gt.80) then
	print*, 'xerract, second string is too long'
      endif

C     Initialize arrays

      cmd    = ' '
      option = ' '

C     Transformations to Fortran-string

      call mvlc(x,cmd,i)
      call mvlc(y,option,j)

      call erract(cmd,option)

      return
      end


c***************************************** 
c   xms_daffna:
C   2nd-stage bridge to DAFFNA, in Fortran
c*****************************************
      subroutine xms_daffna(status)

      integer status
      logical found

      call daffna(found)

      if (found) then
	status = 1         !TRUE
      else
	status = 0         !FALSE
      endif 

      return
      end


c***************************************** 
c   xms_dafopw:
C   2nd-stage Bridge to DAFOPW, in Fortran
c*****************************************
      subroutine xms_dafopw(fname,i,handle)
      
      integer handle
      integer i
      byte fname(1)
      character*80 text

      if (i.gt.80) print*, 'xdafopw, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(fname,text,i)

      call dafopw(text,handle)

      return
      end


c*****************************************
c   xms_dafbna:
C   2nd-stage Bridge to DAFBNA, in Fortran
c***************************************** 

      subroutine xms_dafbna(handle, sum, name, i)
      
      double precision sum(*)
      integer handle
      integer i
      byte name(1)
      character*80 text

      if (i.gt.80) print*, 'xdafbna, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(name,text,i)

      call dafbna(handle,sum,text)

      return
      end


c***************************************** 
c  xms_ldpool:
C  2nd-stage bridge to LDPOOL, in Fortran
c*****************************************

      subroutine xms_ldpool(x,i)
      
      integer i
      byte x(1)
      character*80 text

      if (i.gt.80) print*, 'xldpool, string too long'

      text=' '

C     Transformation to Fortran-string
      call mvlc(x,text,i)

      call ldpool(text)

      return
      end


C***************************************** 
C xms_scencd:
C 2nd-stage bridge to SCENCD, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
      subroutine xms_scencd(sc, btext, i, sclkdp)      
      integer sc
      byte btext(1)
      integer i
      double precision sclkdp

      character*80 text

C     Transformation to Fortran-string

      if (i.gt.80) then
	print*, 'xscencd, string too long'
      else
        text = ' '
        call mvlc( btext, text, i)
        call scencd(sc, text, sclkdp)      
      endif

      return
      end


C***************************************** 
C xms_scs2e:
C 2nd-stage bridge to SCS2E, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
C Written by:		Justin McNeill
C Date:			February 14, 1994
C Cognizant Engineer:	Justin McNeill (JFM059)
C
      subroutine xms_scs2e(sc, SCLK, i, et)
      
      integer sc
      byte SCLK(1)
      integer i
      double precision et

      character*80 text

C     Transformation to Fortran-string

      if (i.gt.32) then
	print*, 'xscs2e, string too long'
      else
        text = ' '
        call mvlc(SCLK, text, i)
        call scs2e(sc, text, et)
      endif

      return
      end


C***************************************** 
C xms_et2utc:
C 2nd-stage bridge to ET2UTC, in Fortran
C****************************************** 
C
C Part of bridge to be called from C language applications 
C
C Written by:		Justin McNeill
C Date:			February 14, 1994
C Cognizant Engineer:	Justin McNeill (JFM059)
C
      subroutine xms_et2utc(et,format,prec,UTCtmp) 

      double precision et
      integer prec
      byte format(1)
      byte UTCtmp(80)
      character*80 text
      character*1  ftext

      call mvlc(format,ftext,1)

      call et2utc(et,ftext,prec,text)

      call mvcl(text, UTCtmp, 80)

      return
      end

c***************************************** 
c  xms_rtpool:
C  2nd-stage bridge to RTPOOL, in Fortran
c  23nov94 -lwk- fixed 'flag' to be -1 on success
c****************************************** 

      subroutine xms_rtpool(item, i, dim, values, flag)
      
      byte item(1)
      integer i
      integer dim
      double precision values(*) 
      integer flag
      logical status

      character*80 text

      text=' '

      if (i.gt.80) print*, 'xrtpool, string too long'

C     Transformation to Fortran-string
      call mvlc(item, text, i)

      call rtpool(text, dim, values, status)

      if ( status ) then
	flag = -1
      else
	flag = 0
      endif

      return
      end
C*************************************************
C xms_sctiks:
C 2nd-stage bridge to SCTIKS, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by: 		Sam Le
C Date:			April 27, 1995
C*************************************************
	subroutine xms_sctiks(sc, clkstr, i,  ticks)
	integer			sc, i
	byte			clkstr(1)
	character*12 		text
	double precision	ticks

C	Transformation to Fortran-string

	if (i .gt. 12) then
	   print*, 'xsctiks, clkstr is too long'
        else
	   text = ' '
	   call mvlc(CLKSTR, text, i)
	   call sctiks(sc, text, ticks)
        end if

	return
	end
C*************************************************
C xms_ckbss:
C 2nd-stage bridge to xckbss, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_ckbss(inst, sclk, tol, needav)
	integer			inst
	double precision	sclk
	double precision	tol
	integer			needav
	logical			flag

	if (needav .eq. 1) then
	   flag = .true.
	else
	   flag = .false.
	endif

	call ckbss(inst, sclk, tol, flag)

	return
	end
C*************************************************
C xms_cksns:
C 2nd-stage bridge to xcksns, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_cksns(handle, descr, segid, found)
	integer			handle
	double precision	descr(5)
	byte			segid(40)
	integer			found
	logical			status
	character*40		tempsegid

	call cksns(handle, descr, tempsegid, status)
	call mve(1, 40, tempsegid, segid, 1, 1)
 
	if (status) then
	   found = 1
	else
	   found = 0
	endif

	return
	end
C*************************************************
C xms_ckpfs:
C 2nd-stage bridge to xckpfs, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_ckpfs(handle, descr, sclkin, tol, 
     1			needav, cmat, av, clkout, found)
	integer			handle
	double precision	descr (*)
	double precision	sclkin
	double precision	tol
	integer			needav
	double precision	cmat (3,3)
	double precision	av(*)
	double precision	clkout
	integer			found
	logical			in, out

	if (needav .eq. 1) then
	   in = .true.
	else 
	   in = .false.
	endif
	
	call ckpfs(handle, descr, sclkin, tol, in, 
     1			cmat, av, clkout, out)
	if (out) then
	   found = 1
	else 
	   found = 0
	endif

	return
	end
C*************************************************
C xms_surfpt:
C 2nd-stage bridge to xsurfpt, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_surfpt(pos, u, a, b, c, pts, fnd)
	double precision	pos(3), u(3)
	double precision	a, b, c, pts(3)
	integer			fnd
	logical			flag

	call surfpt(pos, u, a, b, c, pts, flag)
	if (flag) then
	   fnd = 1
	else
	   fnd = 0
	endif

	return
	end
C*************************************************
C xms_dafa2b:
C 2nd-stage bridge to dafa2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_dafa2b(as, alen, da, dlen,resv)
	byte as(1)
	integer alen
	byte da(1)
	integer dlen
	integer resv
	character*80 aname, dname

	aname = ' '
	dname = ' '
	call mvlc(as, aname, alen)
	call mvlc(da, dname, dlen)
	call dafa2b(aname, dname, resv)

	return
	end
C*************************************************
C xms_dafb2a:
C 2nd-stage bridge to dafb2a, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_dafb2a(da, dlen, as, alen)
	byte 		da(1)
	integer 	dlen
	byte 		as(1)
	integer 	alen
	character*80 	dname, aname

	dname = ' '
	aname = ' '
	call mvlc(da, dname, dlen)
	call mvlc(as, aname, alen)

	call dafb2a(dname, aname)

	return
	end
C*************************************************
C xms_spca2b:
C 2nd-stage bridge to spca2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_spca2b(ascfile, asclen, binfile, binlen)
        integer asclen, binlen
        byte	 ascfile(1)
	byte	 binfile(1)
        character*80 fascfile, fbinfile

        fascfile = ' '
        fbinfile = ' '

        call mvlc(ascfile, fascfile, asclen)
        call mvlc(binfile, fbinfile, binlen)

        call spca2b(fascfile, fbinfile)
        return
        end
C*************************************************
C xms_spcb2a:
C 2nd-stage bridge to spcb2a, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_spcb2a(binfile, binlen, ascfile, asclen)
        integer binlen, asclen
        byte	binfile(1)
	byte	ascfile(1)
        character*80 fbinfile, fascfile

        fbinfile = ' '
        fascfile = ' '

        call mvlc(binfile, fbinfile, binlen)
        call mvlc(ascfile, fascfile, asclen)

        call spcb2a(fbinfile, fascfile)

        return
        end
C*************************************************
C xms_txtopr:
C 2nd-stage bridge to txtopr, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_txtopr(fname, flen, unit)
        integer flen, unit
        character*80 fname, ffname

        ffname = ' '

        call mvlc(fname, ffname, flen)
        call txtopr(ffname, unit)
        return
        end
C*************************************************
C xms_spct2b:
C 2nd-stage bridge to spct2b, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
	subroutine xms_spct2b(unit, fname, len)
	integer 	unit
	character*80	fname
	integer		len
	character*80	ffname

	ffname = ' '

	call mvlc(fname, ffname, len)
	call spct2b(unit, ffname)

	return
	end
C***************************************************
C xms_txtcls:
C 2nd-stage bridge to close, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date:                 April 27, 1995
C*************************************************
        subroutine xms_txtcls(txtunit)
        integer txtunit
        close (txtunit)
        return
        end
C***************************************************
C xms_sce2s:
C 2nd-stage bridge to convert SCET to SCLK string
C
C Part of bridge to be called from C applications
C Written by:   	Sam Le
C Date      :		June 6, 1995
C***************************************************
	subroutine xms_sce2s(sc, etc, sclk)
	integer			sc
	double precision	etc
	character*(*)		sclk
	character*80		text

	call sce2s(sc, etc, text)
	call mvcl(text, sclk, 80)

	return
	end
C*******************************************************************
C xms_dafhfn:
C 2nd-stage bridge to xdafhfn, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:		Sam Le
C Date	    : 		September 14, 1995
C*******************************************************************
	subroutine xms_dafhfn (handle, fname)
	integer		handle
	byte		fname(256)
	character*256	temp

	call dafhfn(handle, temp)
	call mve(1, 256, temp, fname, 1, 1)

	return
	end 
C*******************************************************************
C xms_bodn2c:
C 2nd-stage bridge to bodn2c, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date      :           2/20/1997
C*******************************************************************
        subroutine xms_bodn2c(name, len, id, found)
        integer len, id, found, code
	logical lfound
	byte name(1)
        character*80 tname

        tname = ' '

        call mvlc(name, tname, len)
        call bodn2c(tname, code, lfound)

	if (lfound) then
	   found = 1
	   id    = code
	else
	   found = 0
	endif

        return
        end
C*******************************************************************
C xms_ckw01:
C 2nd-stage bridge to ckw01, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Sam Le
C Date      :           3/5/1997
C*******************************************************************
        subroutine xms_ckw01(handle, begtime, endtime, inst, ref,
     1		len, avflag, segid, nrec, sclkdp, quat, av)

        integer handle
        double precision begtime
	double precision endtime
	integer inst
	byte ref(1)
	integer len
	integer avflag
	byte segid
	integer nrec
	double precision sclkdp
	double precision quat(4)
	double precision av(3)
	logical flag

	character*8 tref
	character*40 tsegid

        tref = ' '
	tsegid = ' '

	call mvlc (ref, tref, len)
	call mvlc (segid, tsegid, 40)

	if (avflag .eq. 1) then
	   flag = .true.
	else
	   flag = .false.
	endif

	call ckw01 (handle, begtime, endtime, inst, tref,
     1		flag, tsegid, nrec, sclkdp, quat, av)

	return
	end
C*******************************************************************
C xms_tkfram:
C 2nd-stage bridge to tkfram, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Michael Brady
C Date      :           10/29/2001
C*******************************************************************
        subroutine xms_tkfram(id, rot, frame, fnd)
        integer                 id
        double precision        rot   ( 3, 3 )
        integer                 frame
	integer			fnd
	logical			flag

	call tkfram(id, rot, frame, flag)
	if (flag) then
	   fnd = 1
	else
	   fnd = 0
	endif

	return
	end
C*******************************************************************
C xms_invert:
C 2nd-stage bridge to invert, in Fortran
C
C Part of bridge to be called from C applications
C
C Written by:           Michael Brady
C Date      :           10/29/2001
C*******************************************************************
        subroutine xms_invert(m1, m2)
        double precision        m1   ( 3, 3 )
        double precision        m2   ( 3, 3 )

	call invert(m1, m2)

	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mspice.imake
/* Imake file for VICAR subroutine MSPICE */

#define SUBROUTINE   mspice

#define MODULE_LIST  cltsub.c lclsub.c ms_xdr.c cam_info.c \
		zms_bridge.c xms_bridge.f

#define USES_FORTRAN
#define USES_ANSI_C

#define FTN_STRING
#define P2_SUBLIB
#define LIB_NETWORK

/* #define DEBUG */ /* Remove before delivery. */


$ Return
$!#############################################################################
$Test_File:
$ create tstmspice.pdf
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage

body
local dir string
let $autousage="none"
let _onfail="continue"
let $echo="yes"

if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/mipl/gll/"
else
   let dir = "wms_test_work:[testdata.mipl.gll]"
end-if

ibisupdate inp=&"dir"e17_regmap.sedr target=europa project=gll +
   'object 'update

let $echo="no"
write "The above case only tested the remove SPICE client, because not all  "
write "platforms have local SPICE kernels.  If you are on Solaris, HP, or   "
write "VMS, please add the following case to the test pdf:                  "
write " "
write "    ibisupdate inp=&"dir"e17_regmap.sedr target=europa project=gll + "
write "        'object 'update 'local                                       "
write " "

end-proc

$!-----------------------------------------------------------------------------
$ create tstMsLcl.c
#include "xvmaininc.h"

#include <stdio.h>
#if VMS_OS
#else
#include <sys/types.h>
#include <sys/wait.h>
#endif

#include "ms_defines.h"
#include "cltsub.h"
#include "lclsub.h"

main (int argc, char **argv)
{
 int sd;
 msUserRequestStruct	req;
 msCkStruct		ckdata;
 msSpkStruct		spkdata;

 const char *cm = "tstMsLcl";

 printf ("***** ************************* ******\n");
 printf ("***** DOING LOCAL GLL_GETSPICE ******\n");
 printf ("***** ************************* ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= -77;
 req.system	= REF_B1950;
 req.scet[0]	= 1990;
 req.scet[1]	= 44;
 req.scet[2]	= 5;
 req.scet[3]	= 58;
 req.scet[4]	= 16;
 req.scet[5]	= 962;
 
 strcpy (req.instrument_name, "SSI");
 strcpy (req.target_name, "VENUS");
 strcpy (req.ck_source, "NEAR");
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (mslcl_gllgetspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting GLL SPICE");
	exit (0);
    }

 printf ("***** ************************* ******\n");
 printf ("***** DOING LOCAL GLL_PUTSPICE ******\n");
 printf ("***** ************************* ******\n");
 if (mslcl_gllputspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "GLL Kernel write SUCCESSFULL !!!\n");


/*
 printf ("***** ************************** ******\n");
 printf ("***** DOING LOCAL VGR1_GETSPICE ******\n");
 printf ("***** ************************** ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id      = VGR_1_SC_ID;
 req.system     = REF_B1950;
 req.scet[0]    = 1979;
 req.scet[1]    = 65;
 req.scet[2]    = 19;
 req.scet[3]    = 22;
 req.scet[4]    = 59;
 req.scet[5]    = 0;
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 strcpy (req.instrument_name, "ISSW");
 strcpy (req.target_name, "CALLISTO");
 strcpy (req.ck_source, "NEAR");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (mslcl_vgr1getspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting VGR1 SPICE");
    }

 printf ("***** ************************** ******\n");
 printf ("***** DOING LOCAL VGR1_PUTSPICE ******\n");
 printf ("***** ************************** ******\n");
 strcpy (ckdata.ck_id, "M002");
 strcpy (ckdata.ck_source, "NEAR");
 if (mslcl_vgr1putspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "VGR1 Kernel write SUCCESSFULL !!!\n");


 printf ("***** ************************** ******\n");
 printf ("***** DOING LOCAL VGR2_GETSPICE ******\n");
 printf ("***** ************************** ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= VGR_2_SC_ID;
 req.system     = REF_B1950;
 req.scet[0]    = 1979;
 req.scet[1]    = 160;
 req.scet[2]    = 0;
 req.scet[3]    = 0;
 req.scet[4]    = 0;
 req.scet[5]    = 0;
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "JUPITER");
 strcpy (req.ck_source, "NEAR");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (mslcl_vgr2getspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting VGR2 SPICE");
    }

 printf ("***** ************************** ******\n");
 printf ("***** DOING LOCAL VGR2_PUTSPICE ******\n");
 printf ("***** ************************** ******\n");
 if (mslcl_vgr2putspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "VGR2 Kernel write SUCCESSFULL !!!\n");
*/

 printf ("***** ************************* ******\n");
 printf ("***** DOING LOCAL CAS_GETSPICE ******\n");
 printf ("***** ************************* ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= -82;
 req.system	= REF_B1950;
 req.scet[0]	= 2000;
 req.scet[1]	= 350;
 req.scet[2]	= 0;
 req.scet[3]	= 0;
 req.scet[4]	= 0;
 req.scet[5]	= 0;
 
 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "JUPITER");
 strcpy (req.ck_source, "NEAR");
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (mslcl_casgetspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting CAS SPICE");
	exit (0);
    }

 printf ("***** ************************* ******\n");
 printf ("***** DOING LOCAL CAS_PUTSPICE ******\n");
 printf ("***** ************************* ******\n");

 strcpy (ckdata.ck_id, "M902");
 ckdata.instrument = CASISSNA_NAIF_ID;
 if (mslcl_casputspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "CAS Kernel write SUCCESSFULL !!!\n");


/****************************
 printf ("\n***** REQUEST ******\n");
 msclt_printuserrequeststruct(req);
 printf ("\n***** CKDATA ******\n");
 msclt_printckstruct(ckdata);
 printf ("\n***** SPKDATA ******\n");
 msclt_printspkstruct(spkdata);

 printf ("***** **************** ******\n");
 printf ("***** DOING GLL_GET_CK ******\n");
 printf ("***** **************** ******\n");
 if (msclt_gllgetck("mips_nav2.ck", "/home/sle")) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 if (msclt_gllgetspk ("gll_long_2.bsp", "/home/sle")) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading SPK from server");
    exit(0);
    }
***************************/
}
$!-----------------------------------------------------------------------------
$ create tstMsClient.c
#include "xvmaininc.h"

#include <stdio.h>
#if VMS_OS
#else
#include <sys/types.h>
#include <sys/wait.h>
#endif

#include "ms_defines.h"
#include "cltsub.h"

main (int argc, char **argv)
{
 int sd;
 msUserRequestStruct	req;
 msCkStruct		ckdata;
 msSpkStruct		spkdata;

 const char *cm = "tstMsClient";
 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE GLL_GETSPICE ******\n");
 printf ("***** ************************* ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= -77;
 req.system	= REF_B1950;
 req.scet[0]	= 1990;
 req.scet[1]	= 44;
 req.scet[2]	= 5;
 req.scet[3]	= 58;
 req.scet[4]	= 16;
 req.scet[5]	= 962;
 
 strcpy (req.instrument_name, "SSI");
 strcpy (req.target_name, "VENUS");
 strcpy (req.ck_source, "NEAR");
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_gllgetspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting GLL SPICE");
	exit (0);
    }

 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE GLL_PUTSPICE ******\n");
 printf ("***** ************************* ******\n");
 if (msclt_gllputspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "GLL Kernel write SUCCESSFULL !!!\n");

/*
 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR1_GETSPICE ******\n");
 printf ("***** ************************** ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id      = VGR_1_SC_ID;
 req.system     = REF_B1950;
 req.scet[0]    = 1979;
 req.scet[1]    = 63;
 req.scet[2]    = 19;
 req.scet[3]    = 23;
 req.scet[4]    = 0;
 req.scet[5]    = 0;
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "IO");
 strcpy (req.ck_source, "NEAR");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_vgr1getspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting VGR1 SPICE");
    }

 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR1_PUTSPICE ******\n");
 printf ("***** ************************** ******\n");
 if (msclt_vgr1putspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "VGR1 Kernel write SUCCESSFULL !!!\n");


 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR2_GETSPICE ******\n");
 printf ("***** ************************** ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= VGR_2_SC_ID;
 req.system     = REF_B1950;
 req.scet[0]    = 1979;
 req.scet[1]    = 160;
 req.scet[2]    = 0;
 req.scet[3]    = 0;
 req.scet[4]    = 0;
 req.scet[5]    = 0;
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "JUPITER");
 strcpy (req.ck_source, "NEAR");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_vgr2getspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm, "ERROR getting VGR2 SPICE");
    }

 printf ("***** ************************** ******\n");
 printf ("***** DOING REMOTE VGR2_PUTSPICE ******\n");
 printf ("***** ************************** ******\n");
 if (msclt_vgr2putspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
        "ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "VGR2 Kernel write SUCCESSFULL !!!\n");
*/

 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE CAS_GETSPICE ******\n");
 printf ("***** ************************* ******\n");
 memset ((void*) &req, '\0', sizeof(req));
 req.sc_id	= -82;
 req.system	= REF_B1950;
 req.scet[0]	= 2000;
 req.scet[1]	= 350;
 req.scet[2]	= 0;
 req.scet[3]	= 0;
 req.scet[4]	= 0;
 req.scet[5]	= 0;
 
 strcpy (req.instrument_name, "ISSN");
 strcpy (req.target_name, "JUPITER");
 strcpy (req.ck_source, "NEAR");
 strcpy (req.provInfo.seg_id, "NONENONE*NONE*NONENONE000000000000*NONE*");

 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &spkdata, '\0', sizeof(spkdata));
 if (msclt_casgetspice(&req, &ckdata, &spkdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR getting CAS SPICE");
	exit (0);
    }

 printf ("***** ************************* ******\n");
 printf ("***** DOING REMOTE CAS_PUTSPICE ******\n");
 printf ("***** ************************* ******\n");

 strcpy (ckdata.ck_id, "M902");
 ckdata.instrument = CASISSNA_NAIF_ID;
 if (msclt_casputspice(&ckdata)) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 else fprintf (stderr, "CAS Kernel write SUCCESSFULL !!!\n");

/****************************
 printf ("\n***** REQUEST ******\n");
 msclt_printuserrequeststruct(req);
 printf ("\n***** CKDATA ******\n");
 msclt_printckstruct(ckdata);
 printf ("\n***** SPKDATA ******\n");
 msclt_printspkstruct(spkdata);

 printf ("***** **************** ******\n");
 printf ("***** DOING GLL_GET_CK ******\n");
 printf ("***** **************** ******\n");
 if (msclt_gllgetck("mips_nav2.ck", "/home/sle")) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR writing to remote kernel");
    exit (0);
    }
 if (msclt_gllgetspk ("gll_long_2.bsp", "/home/sle")) {
    fprintf (stderr, "%s:%s\n", cm,
	"ERROR reading SPK from server");
    exit(0);
    }
***************************/
}
$!-----------------------------------------------------------------------------
$ create tstMsLcl.imake
#define  PROGRAM   tstMsLcl
#define  MAIN_LANG_C
#define  USES_ANSI_C

#define MODULE_LIST tstMsLcl.c

#define FTN_STRING
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_MATH77
#define LIB_NETWORK


/* #define DEBUG */ /* Remove before delivery. */
/* #define LIB_LOCAL */ /* Remove before delivery. */
$!-----------------------------------------------------------------------------
$ create tstMsClient.imake
#define  PROGRAM   tstMsClient
#define  MAIN_LANG_C
#define  USES_ANSI_C

#define MODULE_LIST tstMsClient.c

#define FTN_STRING
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_MATH77
#define LIB_NETWORK


/* #define DEBUG */ /* Remove before delivery. */
/* #define LIB_LOCAL */ /* Remove before delivery. */

$ Return
$!#############################################################################
