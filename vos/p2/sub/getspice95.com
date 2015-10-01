$!****************************************************************************
$!
$! Build proc for MIPL module getspice95
$! VPACK Version 1.9, Tuesday, July 12, 2011, 08:17:02
$!
$! Execute by entering:		$ @getspice95
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
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module getspice95 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to getspice95.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("getspice95.imake") .nes. ""
$   then
$      vimake getspice95
$      purge getspice95.bld
$   else
$      if F$SEARCH("getspice95.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getspice95
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getspice95.bld "STD"
$   else
$      @getspice95.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getspice95.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getspice95.com -mixed -
	-s spicesubs95.c getspice95.c -
	-i getspice95.imake -
	-t tgetspice95.f tzgetspice95.c tgetspice95.imake tgetspice95.pdf -
	   tstgetspice95.pdf -
	-o getspice95.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spicesubs95.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===============================================================*
 *		spicesubs95.c					 *
 *								 *
 * History:							 *
 *	svl (8/25)	- modified rad_per_pixel attribute in	 *
 *			  getIdx95() function.			 *
 *			  rad_per_pixel = 0.00000962 (sn = 1)	 *
 *			  rad_per_pixel = 0.00001924 (sn = 2)	 *
 *
 *     M.Brady 8/18/99  - In gll2Segid(), changed:               *
 *                            else if (curr_time[0] = '1')       *
 *                        to:                                    *
 *                            else if (curr_time[0] == '1')      *
 *
 *                      - In FillMipsBuf, added: return SUCCESS  *
 *===============================================================*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>

#if VMS_OS
#include <unixlib.h>
#include <stat.h>
#define DONT_DECLARE_MALLOC
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#endif

#include "spc.h"
#include "spiceinc.h"

#define BODY_SIZE          36
#define NAME_POS           16
#define LINE_LENGTH        80
#define SCLK_BEGIN_FIELD   89      /* field positions in ascii file KERNELDB */
#define SCLK_END_FIELD    111
#define ET_BEGIN_FIELD     89
#define ET_END_FIELD      111

int GetKdb95(kernel_db_typ*, int, int);
int GetKernelDbFname(char*, int);
int ReadKdb95(kernel_db_typ*, char*, int);
int GetUsrInfo(usr_kdb_typ*, buf_union_typ*);
int GetKernelFname(usr_kdb_typ*);
int GetIds95(buf_union_typ*, int*, int*, double*);
int LoadCk95(double, usr_kdb_typ*, kernel_db_typ*);
int LoadMipsCK95(double, usr_kdb_typ*, kernel_db_typ*);
int LoadSpk95(double, char *, char*, kernel_db_typ*);
int UnloadCk95(kernel_db_typ*);
int UnloadSpk95(kernel_db_typ*);
int UnloadKernels95(kernel_db_typ*);
int Cmatrix95(usr_kdb_typ, int, double, double, int,
		double*, double*, double*, char*);
int IsGoodSegid(char*, char*);
int buf2scet(buf_union_typ*, int*, char*);
int Segid2Prov(char*, prov_info_typ*);
int ChckUsrInfo(prov_info_typ*);
int FillMipsBuf(char*, int, buf_union_typ*);
int Handle2Id(kernel_db_typ [], int, int, char*, char*);
int GetDateTime(char*);
int CheckGllKer();
int CheckGllSpcRead();
int CheckGllSpcWrite();
int IsFileReadable(char []);
int IsFileWriteable(char []);
int IsFileReadWriteable(char []);
int CheckLeapSeconds();
int CheckConstants();
int CheckSclk();
int CheckBinPool();
int CheckKernelDB();
int CheckBody_ids();
int CheckSpiceKer();
int IsGoodSource(char [], int);
int RecvTextFile(int, int, char*);
int RecvBinFile(int, int, char*);
int IsDir(char*);
int AlnumStr(char*, int);
int spaceFilledStr(char*, int);
int NumbStr(char*, int);
int UseProvInfo(usr_kdb_typ*);
int ShowKernelDbTyp(kernel_db_typ);
int readn(int, char*, int);
int readline(int, char*, int);
int writen(int, char*, int);
int InitBufUnion(buf_union_typ*);
int InitKernelDb(kernel_db_typ*);
int InitBegin(begin_typ*);
int InitEnd(end_typ*);
int InitUsrKdb(usr_kdb_typ*);
void zirfrotSPICE(int, int, void*);
void zeul2mSPICE(double, double, double,
	int, int, int, double*);
void zmxmSPICE(void*, void*, void*);
void zmxmtSPICE(void*, void*, void*);
void zmxvSPICE(void*, void*, void*);
void zmtxvSPICE(void*, void*, void*);
void zxposeSPICE(void*, void*);

/*================================================================*
 * GetKdb95(): go out and read the content of file $KERNELDB	  *
 *	and store into array kernel_db. Each line in $KERNELDB	  *
 *	contains the summary of a file in the kernel. 		  *
 *	INPUT: 							  *
 *		usrcnt: max number of lines (or max number of	  *
 *			files) in the kernel table, currently set *
 *			at 256.					  *
 *		Each line in the kernel table corresponds to a 	  *
 *		file in MIPS SPICE kernel. Right now the maximum  *
 *		number of files per project in MIPS kernel is 256 *
 *								  *
 *		Notice that each project has its own kernel table *
 *								  *
 *	OUTPUT: 						  *
 *		kernel_db: array to store kernel files summary	  *
 *		function also returns number of kernel files 	  *
 *		in KERNELDB					  *
 *								  *
 *	1. Call GetKernelDbFname() to get the kernel table fname  *
 *	2. Call stat() to make sure the table fname is good	  *
 *	3. Call ReadKdb95() to read file info. stored in k-table  *
 *================================================================*/
int GetKdb95(kernel_db, prjt, usrcnt)
 kernel_db_typ	*kernel_db;
 int		prjt;
 int		usrcnt;
{
 int	status;
 char	fname[256];
 struct stat statbuf;

 status = GetKernelDbFname(fname, prjt);
 if (status < 0) {
    zvmessage("ERROR::KernelDbFname() Failed", " ");
    return FAILURE;
    }

 status = stat(fname, &statbuf);
 if (status != 0) {
    char	local_mesg[256];

    zvmessage("GetKdb95::KERNELDB Not Found", " ");
    sprintf(local_mesg, "KERNELDB: %s", fname);
    zvmessage(local_mesg, " ");
    return FAILURE;
    }

 status = ReadKdb95(kernel_db, fname, MAX_KERNELS);
 if (status < 0) {
    char	local_mesg[256];

    zvmessage("ERROR::ReadKdb95() Failed", " ");
    sprintf(local_mesg, "KERNELDB: %s", fname);
    zvmessage(local_mesg, " ");
    return FAILURE;
    }
 return SUCCESS;
}
/*----------------------------------------------------------*
 * int GetKernelDbFname(): given the project id code, this  *
 *	return the file name of the kernel table.	    *
 *							    *
 *	1. Call getenv ("SPICEKER") to get the directory    *
 *		path to SPICE files			    *
 *	2. Check project code and append appropriate file   *
 *		name to the directory path		    *
 *	Notice that if on UNIX, add a "/" when needed	    *
 *----------------------------------------------------------*/
int GetKernelDbFname(fname, prjt)
 char	*fname;
 int	prjt;
{
 char *env;
 if ((env = getenv ("KERNELDB")) == NULL) {
    zvmessage("GetKernelDbFname::SPICEKER Not Defined", " ");
    return (-4);
    }
 else {
    strcpy(fname, env);
    return SUCCESS;
    }
}
/*--------------------------------------------------------------*
 * int ReadKdb95(): Read the kernel table file information	*
 * 	then store it in kernel_db. Usrcnt is the maximum size  *
 * 	of kernel_db, which is an array of kernel_db_typ	*
 *	1. Open the kernel table and read its content by line	*
 *	2. Store different portion of the line into 		*
 *		kernel_db_typ structure				*
 *	3. Get the filename by getting the directory path, then *
 *		append the "/" and the file name to dir path    *
 *--------------------------------------------------------------*/
int ReadKdb95(kernel_db, fname, usrcnt)		/* return # of file in  */
 kernel_db_typ 	*kernel_db;			/* kernel data base     */
 char		*fname;
 int 	       	usrcnt;
{
 int	year_b,
	year_e,
	day_b,
	day_e, 
	d1, d2, i;

 char 	*env,
	utc[21],
	s1[4], s2[9],
	kdb_line[256],
	kernel_path[50], 
	real_path[256],
	kernel_file[132],
	month_b[5], month_e[5],
	time_b[5], time_e[5];

 FILE	*kdb_file;

 for(i = 0; i < usrcnt; i++) 
    memset((char *) &kernel_db[i], 
	'\0', sizeof(kernel_db_typ));

 kdb_file = fopen(fname, "r");
 if (kdb_file == NULL) {
    zvmessage("GET_KDB95::KERNELDB fopen() failed", " ");
    return (-1);
    }

 for(i = 0; ((i < MAX_KERNELS) && (i <= usrcnt)); i++) {
    if (fgets(kdb_line, 256, kdb_file) == NULL) break;		/* copy */
    sscanf(kdb_line, "%s%d%s%s%s%d%s%d%s%d%s%d%s",		/* file	*/
		kernel_db[i].id, 				/* id	*/
		&kernel_db[i].type,				/* type	*/
		kernel_db[i].source,				/* src	*/
		kernel_path, kernel_file,			/* name */
		&year_b, month_b, &day_b, time_b,		/* time */
		&year_e, month_e, &day_e, time_e);		/* etc..*/

   sscanf(kdb_line + 45, "%d %s %d %s", &d1, s1, &d2, s2);	/* copy	*/
   sprintf(utc, "%d %s %02d %s", d1, s1, d2, s2);		/* begn */
   zutc2et(utc, &kernel_db[i].range_begin.et_begin);		/* ET	*/
   if (zfailed()) {
      zvmessage("GET_KDB95::ZUTC2ET() Failed", " ");
      return (-2);
      }
   sscanf(kdb_line + 67, "%d %s %d %s", &d1, s1, &d2, s2);	/* copy	*/
   sprintf(utc, "%d %s %02d %s", d1, s1, d2, s2);		/* end	*/
   zutc2et(utc, &kernel_db[i].range_end.et_end);		/* ET	*/
   if (zfailed()) {
      zvmessage("GET_KDB95::ZUTC2ET() Failed", " ");
      return (-3);
      }

   if ((env = getenv(kernel_path)) == NULL) {
      zvmessage("GET_KDB95::Undefined KERNEL_PATH", " ");
      return (-4);
      }
   else strcpy(real_path,env);

#if UNIX_OS
   if ((char) real_path[strlen(real_path) -1] != (char) '/')
      strcat(real_path, "/");
#endif

   sprintf(kernel_db[i].filename, "%s%s",
		real_path, kernel_file);
   sprintf(kernel_db[i].scet_begin,
	   "%d %s %02d %s",
           year_b, month_b, day_b, time_b);
   sprintf(kernel_db[i].scet_end,
	   "%d %s %02d %s",
	   year_e, month_e, day_e, time_e);   
   }						/* end for loop*/

 fclose (kdb_file);
 if (i >= usrcnt) { 					/* usr did not      */
    zvmessage("ReadKDB95::KERNELDB Read Failed", " "); 	/* give enough room */
    return (-5);					/* to store kernel  */
    }							/* file info	    */
 else {
    kernel_db[i].type  = 0;
    kernel_db[i].id[0] = '\0';
    return (i);					/* i: count of file */
    }
}
/*==================================================================*
 * GetUsrInfo(): this function extract data from MIPS buffer and    *
 *	fills in usrinfo structure, which is defined in SPICEINC.H. *
 *	usrinfo contains the provenance information that will be    *
 *	added to the segment id.				    *
 *==================================================================*/
int GetUsrInfo(usrinfo, buf)
 usr_kdb_typ *usrinfo;
 buf_union_typ *buf;
{
 char		temp[100];

 memcpy((char *) usrinfo->date_time,
	(char *) &buf->intbuf[POS_DATE_TIME],
	LEN_DATE_TIME);					/* date & time	*/
 if ((int)strlen(usrinfo->date_time) < LEN_DATE_TIME)
    spaceFilledStr(usrinfo->date_time, LEN_DATE_TIME);
 if (!AlnumStr(usrinfo->date_time, LEN_DATE_TIME))	/* data was 	*/
    strcpy(usrinfo->date_time, "000000000000");		/* produced	*/

 memcpy((char *) usrinfo->sp_c_id, 			/* get id of fl */
	(char *) &buf->intbuf[POS_SP_C],		/* to search 	*/
	LEN_SPICE_ID);					/* for data or	*/
 usrinfo->sp_c_id[LEN_SPICE_ID] = '\0';			/* write data	*/
 if ((int)strlen(usrinfo->sp_c_id) < LEN_SPICE_ID)
    spaceFilledStr(usrinfo->sp_c_id, LEN_SPICE_ID);
 if (!AlnumStr(usrinfo->sp_c_id, LEN_SPICE_ID))		/* to		*/
    strcpy(usrinfo->sp_c_id, "NONE");	

 memcpy((char *) usrinfo->prog_name, 			/* get name of 	*/
	(char *) &buf->intbuf[POS_PROG_NAME],		/* prog that	*/
	LEN_PROG_NAME);					/* is calling	*/
 usrinfo->prog_name[LEN_PROG_NAME] = '\0';		/* this routine	*/
 if ((int)strlen(usrinfo->prog_name) < LEN_PROG_NAME)
    spaceFilledStr(usrinfo->prog_name, LEN_PROG_NAME);
 if (!AlnumStr(usrinfo->prog_name, LEN_PROG_NAME))
    strcpy(usrinfo->prog_name, "*NONE*");

 memcpy((char *) usrinfo->purpose,
 	(char *) &buf->intbuf[POS_PURPOSE],		/* get purpose	*/
	LEN_SPICE_ID);					/* of data, why	*/
 usrinfo->purpose[LEN_SPICE_ID] = '\0';			/* data was 	*/
 if ((int)strlen(usrinfo->purpose) < LEN_PURPOSE)
    spaceFilledStr(usrinfo->purpose, LEN_PURPOSE);
 if (!AlnumStr(usrinfo->purpose, LEN_PURPOSE))		/* produced ? 	*/
    strcpy(usrinfo->purpose, "NONE");

 memcpy((char *) usrinfo->req_no, 
	(char *) &buf->intbuf[POS_REQ_NO],		/* request or	*/
	LEN_REQ_NO);					/* batch number */
 usrinfo->req_no[LEN_REQ_NO] = '\0';			/* of data seg	*/
 if ((int)strlen(usrinfo->req_no) < LEN_REQ_NO)
    spaceFilledStr(usrinfo->req_no, LEN_REQ_NO);
 if (!AlnumStr(usrinfo->req_no, LEN_REQ_NO))
    strcpy(usrinfo->req_no, "NONE");

 memcpy((char *) usrinfo->source, 			/* source of fl */
	(char *) &buf->intbuf[POS_SOURCE], 4);		/* to search or */
 usrinfo->source[LEN_PURPOSE] = '\0';			/* update	*/
 if ((int)strlen(usrinfo->source) < LEN_PURPOSE)
    spaceFilledStr(usrinfo->source, LEN_PURPOSE);
 if (!AlnumStr(usrinfo->source, LEN_PURPOSE))
    strcpy(usrinfo->source, "NONE");

 memcpy((char *) usrinfo->sp_ref, 			/* id of  	*/
	(char *) &buf->intbuf[POS_SP_REF], 		/* sp file used */
	LEN_SP_REF);					/* to produce 	*/
 usrinfo->sp_ref[LEN_SP_REF] = '\0';			/* data		*/
 if ((int)strlen(usrinfo->sp_ref) < LEN_SP_REF)
    spaceFilledStr(usrinfo->sp_ref, LEN_SP_REF);
 if (!AlnumStr(usrinfo->sp_ref, LEN_SP_REF))
    strcpy(usrinfo->sp_ref, "NONE");

 memcpy((char *) usrinfo->usr_grp_id, 
	(char *) &buf->intbuf[POS_USR_ID],		/* grp or usr id*/
	 LEN_USR_ID);					/* who created  */
 usrinfo->usr_grp_id[LEN_USR_ID] = '\0';		/* data		*/
 if ((int)strlen(usrinfo->usr_grp_id) < LEN_USR_ID)
    spaceFilledStr(usrinfo->usr_grp_id, LEN_USR_ID);
 if (!AlnumStr(usrinfo->usr_grp_id, LEN_USR_ID))
    strcpy(usrinfo->usr_grp_id, "*NONE*");

 memcpy((char*)temp, (char*) &buf->intbuf[188], LEN_SPICE_ID);
 temp[LEN_SPICE_ID] = '\0';
 if ((int)strlen(temp) < LEN_SPICE_ID)
    spaceFilledStr(temp, LEN_SPICE_ID);
 if (!AlnumStr(temp, (int)strlen(temp)))
    strcpy(temp, "NONE");

 strcpy(usrinfo->segid, temp);
 strcat(usrinfo->segid, usrinfo->purpose);
 strcat(usrinfo->segid, usrinfo->prog_name);
 strcat(usrinfo->segid, usrinfo->sp_ref);
 strcat(usrinfo->segid, usrinfo->req_no);
 strcat(usrinfo->segid, usrinfo->date_time);
 strcat(usrinfo->segid, usrinfo->usr_grp_id);

 return SUCCESS;
}
/*----------------------------------------------------*
 * GetKernelFname(): locate the kernel file name      *
 *	and kernel type, consistence with the sp_c_id *
 * 	and source value in usrinfo structure. That   *
 *	is, read the file id from usrinfo structure   *
 *	and go out and get the full name of the file  *
 *	with the given source and sp_c_id	      *
 *----------------------------------------------------*/
int GetKernelFname(usrinfo)
 usr_kdb_typ *usrinfo;
{
 int		i,
		year_b, year_e,
		day_b, day_e;
 char 		*env,
		file_name[256],
		kernel_path[50],
		kernel_file[132],
		kdb_line[256], 
		month_b[5], month_e[5],
		time_b[10], time_e[10];
 kernel_db_typ	kernel_db;
 FILE		*kdb_file;

 if ((env = getenv("KERNELDB")) == NULL) {
    zvmessage("GetKernelFname::KERNELDB not defined", " ");
    return (-1);
    }
 else strcpy(file_name, env);

 if ((kdb_file = fopen(file_name, "r")) == 0) {
    zvmessage("ERROR: Cannot open $KERNELDB/kdb.gll", " ");
    return (-1);
    } 
 
 for(i = 0; i < MAX_KERNELS; i++) {
    if (fgets(kdb_line, 256, kdb_file) == NULL) {	/* read line by	*/
       zvmessage("ERROR: Requested file not found", " ");/* line to get	*/
       fclose (kdb_file);				/* the needed 	*/
       strcpy(usrinfo->filename, "*NONE*");		/* file		*/
       return (-1);
       }

    sscanf(kdb_line,"%s%d%s%s%s%d%s%d%s%d%s%d%s",
	   kernel_db.id,
           &kernel_db.type,
           kernel_db.source,
           kernel_path,kernel_file,
           &year_b,month_b,&day_b,time_b,
           &year_e,month_e,&day_e,time_e);

    if ((strncmp(usrinfo->sp_c_id, "NONE", LEN_SPICE_ID)) &&
        (strncmp(usrinfo->sp_c_id, kernel_db.id, LEN_SPICE_ID) == 0) &&
        (strncmp(kernel_db.source, usrinfo->source, 4) == 0)) {
        if ((env = getenv(kernel_path)) == NULL) {
 	   char	local_mesg[256];
	   sprintf(local_mesg, "ERROR: %s is not defined", kernel_path); 
	   zvmessage(local_mesg, " ");
	   fclose (kdb_file);
           return (-1);
           }
        sprintf(usrinfo->filename, "%s/%s", env, kernel_file);
        usrinfo->type = kernel_db.type;
	break;
        }
    }
 fclose (kdb_file);
 return (i);
}
/*=============================================================*
 * GetIds95():							*
 *==============================================================*/
int GetIds95(buf, camera_id, camera_sn, rad_per_pixel)
 buf_union_typ	*buf;			/* input	*/
 int    	*camera_id;             /* output       */
 int    	*camera_sn;             /* output       */
 double 	*rad_per_pixel;         /* output       */
{
 int	icam;

 if (!strncmp((char *) &buf->intbuf[1], "ISSN", 4)) {
    icam = ISSNA;
    *rad_per_pixel = 0.00000926;
    if (buf->intbuf[0] == VGR_1) 
       *camera_sn = 7;
    else if (buf->intbuf[0] == VGR_2) 
       *camera_sn = 5;
    }
 else if (!strncmp((char *) &buf->intbuf[1], "ISSW", 4)) {
    icam = ISSWA;
    *rad_per_pixel = 0.0000701;
    if (buf->intbuf[0] == VGR_1) 
       *camera_sn = 6;
    else if (buf->intbuf[0] == VGR_2) 
       *camera_sn = 4;
    }
 else if (!strncmp((char *) &buf->intbuf[1], "SSI", 3)) {
    icam = PLATFORM;
    if (!strncmp((char*) &buf->intbuf[1], "SSI1", 4)) {
       *rad_per_pixel = 0.00000926;
       *camera_sn = 1;
       }
    else {
       *rad_per_pixel = 0.00001924;
       *camera_sn = 2;
       }
    }
 else {
    char temp[5];
    memcpy((void*)temp, (void*)&buf->intbuf[1], 4);
    temp[4] = '\0';
    printf("Instrument: %s\n", temp);

    zvmessage("S/C & Instrument mismatch", " ");
    zvmessage("Unknwn Instrument", " ");
    return (-2);
    }
 *camera_id = -(abs(buf->intbuf[0]) * 1000 + icam);
 return (0);
}
/*==========================================================*
 * int LoadMipsCk95()					    *
 *	only load mips CK. If the file name has the string  *
 *	"mips_"	then it is a MIPS CK. Basically this	    *
 *	function checks for to see if the file name has     *
 *	a substring "mips_" and the sclk is appropriate     *
 *	then it load the file. The file being loaded in	    *
 *	the order writen in KERNELDB. If the user specify   *
 *	a CK file name to load in usrinfo, this function    *
 *	will only load that file.			    *
 *==========================================================*/
int LoadMipsCk95(et, usrinfo, kernel_db)
 double		et;
 usr_kdb_typ	*usrinfo;
 kernel_db_typ	*kernel_db;
{
 int	i,
	lkCount	     = 0,
	kernel_count = 0;

 double	tet_beg,
	tet_end;

 while(kernel_db[kernel_count].id[0] &&		/* get the count of	*/
       kernel_db[kernel_count].type) {		/* available kernels	*/
     kernel_count++;				/* and count of loaded	*/
     if (kernel_db[kernel_count].isloaded) lkCount++;	/* kernels	*/
     }

 if (lkCount >= MAX_KERNEL_LOADED) {				/* make sure */
    zvmessage("LOAD_CK95::Error Loading MIPS_CK Files", " ");	/* no. of    */
    zvmessage("Already Too many files loaded", " " );		/* loaded    */
    return (-4);						/* kernels is*/
    }								/* below lim.*/
 
 if ((usrinfo->type == CK) &&					/* load user */
     (strncmp(usrinfo->sp_c_id, "NONE", LEN_SPICE_ID))) {	/* requested */
    zcklpf(usrinfo->filename, &usrinfo->handle);		/* kernel.   */
    if (zfailed()) {
       zvmessage("LOAD_CK95::Error Loading MIPS_CK Files", " ");
       return (-4);
       }

    for (i = 0; i < kernel_count; i++)				/* mark rqst */
	if (!strcmp(kernel_db[i].filename, usrinfo->filename))	/* kernel as */
	   kernel_db[i].isloaded = 1;				/* loaded    */

    return SUCCESS;
    }

 for(i = 0; i < kernel_count; i++) {
    tet_beg = kernel_db[i].range_begin.et_begin - 60.0;
    tet_end = kernel_db[i].range_end.et_end * 1.0;
    if ((kernel_db[i].type == CK) &&
        (et > tet_beg) && (et < tet_end) &&
        (strstr(kernel_db[i].filename, "mips_"))) {

       printf ("Loading CK: %s\n", kernel_db[i].filename);
       if (lkCount >= MAX_KERNEL_LOADED) {
	  zvmessage("LOAD_CK95::ERROR Loading MIPS_CK", " ");
	  zvmessage("Already Too many files loaded", " ");
	  return (-4);
	  }

       zcklpf(kernel_db[i].filename, &kernel_db[i].handle);

       if (zfailed()) {
          zvmessage("LOAD_CK95::Error Opening CK Files","");
          return (-4);
          }
       else {
	  kernel_db[i].isloaded = 1;
	  lkCount++;
	  }
       }
    }
 return SUCCESS;
}
/*==========================================================*
 * int LoadCk95():					    *
 *==========================================================*/
int LoadCk95(et, usrinfo, kernel_db)
 double		et;
 usr_kdb_typ 	*usrinfo;
 kernel_db_typ 	*kernel_db;
{
 int 		i,
		lkCount      = 0,
		kernel_count = 0;

 double		tet_beg,
		tet_end;

 /* DEBUG */
 printf("userinfo->source: %s\n", usrinfo->source);

						/* come here if user does  */
 while(kernel_db[kernel_count].id[0] && 	/* not specify CK if to	   */
       kernel_db[kernel_count].type) {		/* open			   */
     kernel_count++;
     if (kernel_db[kernel_count].isloaded) lkCount++;
     }

 if (lkCount >= MAX_KERNEL_LOADED) {
    zvmessage ("LOAD_CK95::ERROR Loading CK files", " ");
    zvmessage ("MAX_KERNEL_LOADED exceeded !!!!", " ");
    return (-4);
    }
 
 if ((usrinfo->type == CK) && 					/* open	   */
     (strncmp(usrinfo->sp_c_id, "NONE", LEN_SPICE_ID))) {	/* only fls*/
    zcklpf(usrinfo->filename, &usrinfo->handle);		/* spcfied */
    if (zfailed()) {						/* by user */
       zvmessage("LOAD_CK95::Error Opening CK Files", " ");
       return (-4);
       }

    for (i = 0; i < kernel_count; i++)                          /* mark rqst */
        if (!strcmp(kernel_db[i].filename, usrinfo->filename))  /* kernel as */
           kernel_db[i].isloaded = 1;                           /* loaded    */

       return SUCCESS;
    }
						/* come here if user does  */
						/* not specify specific    */
						/* kernel to load	   */

 for(i = 0; i < kernel_count; i++) {
    tet_beg = kernel_db[i].range_begin.et_begin - 60.0;
    tet_end = kernel_db[i].range_end.et_end * 1.0;
    if ((kernel_db[i].type == CK) && 				/* open CK  */
        (et > tet_beg) && (et < tet_end) &&			/* fls with */
        (strncmp(usrinfo->source, kernel_db[i].source, 4))) {   /* diff.    */

       printf("loading CK: %s\n", kernel_db[i].filename);
       if (lkCount >= MAX_KERNEL_LOADED) {
	  zvmessage ("LOAD_CK95::ERROR Loading CK file", " ");
	  zvmessage("Already too many files loaded", " ");
	  return (-4);
	  }

       zcklpf(kernel_db[i].filename, &kernel_db[i].handle);	/* source & */
								/* covered  */
       if (zfailed()) {						/* given et */
          zvmessage("LOAD_CK95::Error Opening CK Files","");	/* value    */
          return (-4);
          } 
       else {
	  kernel_db[i].isloaded = 1;
	  lkCount++;
	  }

       }
    }

 for(i = 0; i < kernel_count; i++) {
    tet_beg = kernel_db[i].range_begin.et_begin - 60.0;
    tet_end = kernel_db[i].range_end.et_end * 1.0;
    if ((kernel_db[i].type == CK) &&				/* open CK  */
        (et > tet_beg) && (et < tet_end) &&			/* fls with */
        (!strncmp(usrinfo->source, kernel_db[i].source, 4))) {	/* same     */

       printf("loading CK: %s\n", kernel_db[i].filename);
       if (lkCount >= MAX_KERNEL_LOADED) {
          zvmessage("LOAD_CK95::Error Loading CK Files", " ");
	  zvmessage("Already too many files loaded", " ");
	  return (-4);
	  }
 
       zcklpf(kernel_db[i].filename, &kernel_db[i].handle);	/* source & */
        							/* covered  */
       if (zfailed()) {						/* given et */
          zvmessage("LOAD_CK95::Error Loading CK Files", " ");	/* value    */
          return (-4);
          }
       else {
	  kernel_db[i].isloaded = 1;
	  lkCount++;
	  }
       }

    }
 return SUCCESS;
}
/*================================================*
 * int LoadSpk95()				  *
 *================================================*/
int LoadSpk95(et, spk_ref, segid, kernel_db)
 double		et;				/* function to load SPK */
 char		*spk_ref;
 char		*segid;				/* files according to	*/
 kernel_db_typ	*kernel_db;			/* to the segid passed	*/
{						/* in by calling program*/
 int		i,
		lkCount      = 0,
		kernel_count = 0;
 double		tet_beg,
		tet_end;
 prov_info_typ	prov_info;

 while(kernel_db[kernel_count].id[0] &&         /* get the count of     */
       kernel_db[kernel_count].type) {          /* available kernels    */
     kernel_count++;                            /* and count of loaded  */
     if (kernel_db[kernel_count].isloaded) lkCount++;      /* kernels   */
     }

 if (lkCount >= MAX_KERNEL_LOADED) {                            /* make sure */
    zvmessage("LOAD_SPK95::Error Loading SPK", " ");  		/* no. of    */
    zvmessage("Already Too many files loaded", " " );           /* loaded    */
    return (-4);                                                /* kernels is*/
    }                                                           /* below lim.*/

 Segid2Prov(segid, &prov_info);				/* parse segid	*/

 if (strncmp(prov_info.inst, "MIPS", 4) != 0) {
    for(i = 0; i < kernel_count; i++) {				/* if	*/
       tet_beg = kernel_db[i].range_begin.et_begin - 60.0;	/* not	*/
       tet_end = kernel_db[i].range_end.et_end * 1.0;		/* MIPS */
       if ((kernel_db[i].type == SPK) &&			/* open */
           (et > tet_beg) && (et < tet_end)) {			/* all	*/
          tet_beg = kernel_db[i].range_begin.et_begin - 60.0;	/* SPK  */
          tet_end = kernel_db[i].range_end.et_end * 1.0;	/* with */
 
          if (strncmp(kernel_db[i].source, "NAIF", 4) == 0) {	/* src	*/
	     printf ("Loading SPK: %s\n", kernel_db[i].filename);


	     if (lkCount >= MAX_KERNEL_LOADED) {
                zvmessage("LOAD_SPK95::ERROR Loading SPK", " ");
                zvmessage("Already Too many files loaded", " ");
                return (-4);
                }

	     zspklef(kernel_db[i].filename,
			&kernel_db[i].handle); 			/* equal*/
             if (zfailed()) {					/* to 	*/
	        char	local_mesg[256];			/* NAIF	*/

                zvmessage("ZSPKLEF::Error Openning SPK Files", " ");
	        sprintf(local_mesg, "FileName: %s\n",
		   kernel_db[i].filename);	/* this is not needed   */
 	        zvmessage(local_mesg, " ");	/* since all SPK come	*/
                return (-4);			/* from NAIF anyway...  */
                }
	     else {
		kernel_db[i].isloaded = 1;
		lkCount++;
		}				/* end else		*/
	    }					/* end if		*/
         }					/* end if		*/
       }					/* end for		*/
    }						/* end if		*/
 else {								/* this is  */
    for(i = 0; i < kernel_count; i++) {				/* segid    */
       tet_beg = kernel_db[i].range_begin.et_begin - 60.0;	/* come from*/
       tet_end = kernel_db[i].range_end.et_end * 1.0;		/* MIPS CK  */
								/* load all */
       if ((kernel_db[i].type == SPK) &&			/* NAIF SPK */
	   (et > tet_beg) && (et < tet_end) &&			/* first,   */
	   (strncmp(kernel_db[i].source, "NAIF", 4) == 0)) {
	  printf ("Loading SPK: %s\n", kernel_db[i].filename);
	  if (lkCount >= MAX_KERNEL_LOADED) {
                zvmessage("LOAD_SPK95::ERROR Loading SPK", " ");
                zvmessage("Already Too many files loaded", " ");
                return (-4);
                }

	  zspklef(kernel_db[i].filename, &kernel_db[i].handle); 
	  if (zfailed()) {
	     char	local_mesg[256];

	     zvmessage("ZSPKLEF::Error Openning NAIF SPK", " ");
	     sprintf(local_mesg, "FileName: %s",
		kernel_db[i].filename);
	     zvmessage(local_mesg, " ");
	     return (-4);
	     }
	  else {
	     kernel_db[i].isloaded = 1;
	     lkCount++;
	     }
	  }
       }

    for(i = 0; i < kernel_count; i++) {				/* now   */
       tet_beg = kernel_db[i].range_begin.et_begin - 60.0;	/* load  */
       tet_end = kernel_db[i].range_end.et_end * 1.0;		/* non-  */
       if ((kernel_db[i].type == SPK) &&			/* NAIF  */
	   (et > tet_beg) && (et < tet_end) &&			/* SPK	 */
	   (strncmp(kernel_db[i].source, "NAIF", 4) != 0)) {
	  printf ("Loading SPK: %s\n", kernel_db[i].filename);
          if (lkCount >= MAX_KERNEL_LOADED) {
             zvmessage("LOAD_SPK95::ERROR Loading SPK", " ");
             zvmessage("Already Too many files loaded", " ");
             return (-4);
             }

          zspklef(kernel_db[i].filename, &kernel_db[i].handle);
          if (zfailed()) {
	     char	local_mesg[256];

             zvmessage("ZSPKLEF::Error Openning non-NAIF SPK", " ");
	     sprintf(local_mesg, "FileName: %s",
			kernel_db[i].filename);
	     zvmessage(local_mesg, " ");
             return (-4);
             }
	  else {
	     kernel_db[i].isloaded = 1;
	     lkCount++;
	     }
          }
       }
    }

 /* load user specified SPK
 for (i = 0; i < kernel_count; i++) {
     if (strncmp(spk_ref, kernel_db[i].id, LEN_SPICE_ID) == 0) {
        if (kernel_db[i].handle >= 0) zspkuef (kernel_db[i].handle);

        printf ("Loading usr SPK: %s\n", kernel_db[i].filename);
        if (lkCount >= MAX_KERNEL_LOADED) {
           zvmessage("LOAD_SPK95::ERROR Loading SPK", " ");
           zvmessage("Already Too many files loaded", " ");
           return (-4);
           }

        zspklef (kernel_db[i].filename, &kernel_db[i].handle);
        if (zfailed()) {
           char       local_mesg[256];
  
           zvmessage("ZSPKLEF::Error Openning SPK Files", " ");
           sprintf(local_mesg, "FileName: %s\n",
             kernel_db[i].filename);
           zvmessage(local_mesg, " ");
           return (-4);
           }
        else {
   	  kernel_db[i].isloaded = 1;
	  lkCount++;
	  }

       break;
       }
    }

 Since the retrieved C-matrix also has a SPK-ref,	*/
 /* we need to load that also				*/
 for(i = 0; i < kernel_count; i++) {
    if (strncmp(prov_info.sp_ref, kernel_db[i].id, LEN_SPICE_ID) == 0) {
       /* if previously loaded, un-load it	*/
       if (kernel_db[i].handle >= 0) zspkuef (kernel_db[i].handle);

       /* now load target file			*/
       printf ("Load CM SPK: %s\n", kernel_db[i].filename);
       if (lkCount >= MAX_KERNEL_LOADED) {
          zvmessage("LOAD_SPK95::ERROR Loading SPK", " ");
          zvmessage("Already Too many files loaded", " ");
          return (-4);
          }

       zspklef (kernel_db[i].filename, &kernel_db[i].handle);
       if (zfailed()) {                                      /* to   */
          char       local_mesg[256];                        /* NAIF */

          zvmessage("ZSPKLEF::Error Openning SPK Files", " ");
          sprintf(local_mesg, "FileName: %s\n",
                kernel_db[i].filename);         /* this is not needed   */
          zvmessage(local_mesg, " ");        /* since all SPK come   */
          return (-4);                       /* from NAIF anyway...  */
          }
       else {
	  kernel_db[i].isloaded = 1;
	  lkCount++;
	  }

       break;
       }
    }

 return SUCCESS;
}
/*================================================*
 * int UnloadMipsCk95()				  *
 *================================================*/
int UnloadMipsCk95(kernel_db)
 kernel_db_typ *kernel_db;
{
 int    i, count = 0;

 while (kernel_db[count].id[0] &&
        kernel_db[count].type) count++;
 for(i = 0; i < count; i++)
    if ((kernel_db[i].isloaded) &&
	(strstr(kernel_db[i].filename, "mips_"))) {
       zckupf(kernel_db[i].handle);
       zdafcls(kernel_db[i].handle);
       kernel_db[i].handle   = 0;
       kernel_db[i].isloaded = 0;
       }
 return SUCCESS;
} 
/*================================================*
 * int UnloadCk95()				  *
 *================================================*/
int UnloadCk95(kernel_db)
 kernel_db_typ *kernel_db;
{
 int	i, count = 0;

 while (kernel_db[count].id[0] &&
	kernel_db[count].type) count++;
 for(i = 0; i < count; i++)
    if (kernel_db[i].isloaded &&
	(kernel_db[i].type == CK)) {
       zckupf(kernel_db[i].handle);
       zdafcls(kernel_db[i].handle);
       kernel_db[i].handle   = 0;
       kernel_db[i].isloaded = 0;
       }
 return SUCCESS;
}
/*================================================*
 * int UnloadSpk95()				  *
 *================================================*/
int UnloadSpk95(kernel_db)
 kernel_db_typ	*kernel_db;
{
 int	i, count = 0;

 while (kernel_db[count].id[0] &&
	kernel_db[count].type) count++;
 for(i = 0; i < count; i++)
    if (kernel_db[i].isloaded) {
       zspkuef(kernel_db[i].handle);
       zdafcls(kernel_db[i].handle);
       kernel_db[i].handle   = 0;
       kernel_db[i].isloaded = 0;
       }
 return SUCCESS;
}	
/*================================================*
 * int UnloadKernels95():			  *
 *================================================*/
int UnloadKernels95(kernel_db)
 kernel_db_typ *kernel_db;
{
 int	i, count = 0;

 while(kernel_db[count].id[0] &&
       kernel_db[count].type) count++;
 for(i = 0; i < count; i++) {
    if ((kernel_db[i].handle > 0) &&
	(kernel_db[i].type == CK)) {
       zckupf(kernel_db[i].handle);
       zdafcls(kernel_db[i].handle);
       kernel_db[i].handle = 0;
       }
    if ((kernel_db[i].handle > 0) &&
	(kernel_db[i].type == SPK)) {
       zspkuef(kernel_db[i].handle);
       zdafcls(kernel_db[i].handle);
       kernel_db[i].handle = 0;
       }
    }
 return SUCCESS;
}
/*================================================*
 * int Cmatrix95(): given the search specification*
 *	cam_id, sclk, etc. Cmatrix95 searches 	  *
 *	through loaded MIPS CK and return the	  *
 *	pointing data (C-MATRIX) at the specified *
 *	SCLK range and SCLK tolerance value. If	  *
 *	search specification (usrinfo) only 	  *
 *	contain default values, it will be ignored*
 *	then the most recently C-MATRIX found is  *
 *	returned				  *
 *	Cmatrix95 return (<0) if error occur else *
 *	it return the handle of CK which contains *
 *	the C-matrix it found			  *
 *================================================*/
int Cmatrix95(usrinfo, cam_id, sclkin, tol, isystem,
		cmat_out, av_out, sclk_out, segid_out)
 usr_kdb_typ	usrinfo;			/* input: search specs	*/
 int 		cam_id;				/* input: camera id	*/
 double		sclkin;				/* input: sclk value	*/
 double		tol;				/* input: allowed tol	*/
 int		isystem;			/* input: search ref	*/
 double		*cmat_out;			/* output: return Cmat  */
 double		*av_out;			/* output: angular vel  */
 double		*sclk_out;			/* output: sclk found   */
 char		*segid_out;			/* output: segid found  */
{
 int		handle, 
		thandle=0,
		more_search,
                sns_fnd,                        /* flag for cksns */
                pfs_fnd,                        /* flag for ckpfs */
                fst_fnd,                        /* 1st seg. found */
                use_prv,                        /* use prov info? */
                ref_system,
		icd[6];
 double		tav[3],
		sclkout, 
		tsclkout=0.0,
		tcmat[3][3],
		descr[5],
		tdescr[5],
		dcd[2],
		rot[3][3];
 char		segid[41],
		tsegid[41];

 descr[0] = 0.0;				/* init descriptor	  */
 descr[1] = descr[2] = 0.0;
 descr[3] = descr[4] = 0.0;

 more_search = 1;
 use_prv = UseProvInfo(&usrinfo);		/* need to use prov info? */
 while (more_search) {
    printf("UserSegid: %s\n", usrinfo.segid);
    zckbss(cam_id, sclkin, tol, 0);			/* begin segment  */
    if (zfailed()) {					/* search	  */
       zvmessage("CMATRIX95::ZCKBSS() Failed", " ");
       zvmessage("ERROR Starting Search", " ");
       return (-1);
       }

    sns_fnd = 1;					/* initialize flag */
    fst_fnd = pfs_fnd = 0;

    while (sns_fnd) {
       zcksns(&handle, descr, segid, &sns_fnd);		/* get next seg	   */
       if (zfailed()) {
          zvmessage("CMATRIX95::ZCKSNS() Failed", " ");
          zvmessage("Segment Search CKSNS Failed", " ");
          return (-1);
          }

       if ((!sns_fnd) && (!fst_fnd)) {		/* no segment found	*/
          zvmessage("CMATRIX95::Data Segment Not Found", " ");
						/* in db and no first	*/
          return (-2);				/* seg found either.	*/
          }					/* Return error		*/

       if (sns_fnd) {				/* get pointing only if */
          zckpfs(handle, descr, sclkin, tol, 0,	/* it found a segment	*/
   	     cmat_out, av_out, &sclkout, &pfs_fnd); /* If reach eof and	*/
          if (zfailed()) {			    /* got first segment*/
             zvmessage("ZCKPFS::Error Evaluating CK Data", " ");
							/* don't do this*/
             return (-3);				/* fall through	*/
             }
          }

       if (pfs_fnd) {
	  if (!fst_fnd) {				/* if pfs found */
             fst_fnd = 1;				/* for the 1st  */
             tsclkout = sclkout;			/* time, copy   */
             strcpy(tsegid, segid);			/* to temp	*/
             memcpy((char *) tav, (char *) av_out, 
			sizeof(double) * 3);		/* location	*/
             memcpy((char *) tdescr, (char *) descr,
			sizeof(double) * 5);
             memcpy((char *) tcmat, (char *) cmat_out,
			sizeof(double) * 9);
             thandle = handle;
             if (!use_prv) {				/* if don't use */
                sns_fnd = more_search = 0;		/* prov info,	*/
                break;					/* just get 1st */
                }					/* match seg	*/
             }
          if (use_prv && IsGoodSegid(usrinfo.segid, segid)) {
             more_search = FAILURE;			/* good segid	*/
             break;					/* set flag	*/ 
             }						/* to leave	*/
	  }
       }

   if (more_search) {
      printf("****Lowered Segid Priority !!!\n");
      more_search = use_prv = lowerSegidPriority(&usrinfo);
      }
   }

 if (!sns_fnd) {
    strcpy(segid, tsegid);
    sclkout = tsclkout;
    memcpy((char *) av_out, (char *) tav,
			sizeof(double) * 3);
    memcpy((char *) descr, (char *) tdescr,
			sizeof(double) * 5);
    memcpy((char *) cmat_out, (char *) tcmat,
			sizeof(double) * 9);
    handle = thandle;
    }

 *sclk_out = sclkout;
 strcpy(segid_out, segid);

 printf("Returned Segid: %s\n", segid_out);

 zdafus(descr, NUM_CK_DOUBLES, 
	NUM_CK_INTEGERS, dcd, icd);
 if (zfailed()) {
    zvmessage("CMATRIX95::Error Unpacking Seg. Dscrtr", " ");
    return (-1);
    }

 ref_system = icd[1];
 if (isystem != ref_system) {
    zirfrotSPICE(isystem, ref_system, rot);
    if (zfailed()) {
       zvmessage("ZIRFROT::Error Getting Rot. Matrix", " ");
       return (-4);
       }
    zmxmSPICE(cmat_out, rot, cmat_out);
    if (zfailed()) {
       zvmessage("ZMXM::Error Rotating C_MATRIX", " ");
       return (-4);
       }
    zmxvSPICE(rot, av_out, av_out);
    if (zfailed()) {
       zvmessage("ZMXV::Error Rotating ANG. VEL", " ");
       return (-4);
       }
    }

 return (handle);
}
/*================================================*
 * IsGoodSegid(): Check to see if the user's	  *
 *	segid match with the segid retrieved from *
 *	the kernel. If good return 1 else ret 0	  *
 *================================================*/
int IsGoodSegid(usr_segid, ker_segid)
 char *usr_segid; char *ker_segid;
{
 char	u_inst[5], k_inst[5],
	u_pur[5], k_pur[5],
	u_pro[7], k_pro[7],
	u_sp_ref[5], k_sp_ref[5],
	u_job_no[5], k_job_no[5],
	u_date_time[13], k_date_time[13],
	u_grp_id[6], k_grp_id[6];

/* DEBUG
   printf("KernSegid: %s\n", ker_segid);
   END DEBUG
*/

 strncpy(u_inst, &usr_segid[0], 4);
 strncpy(k_inst, &ker_segid[0], 4);
 u_inst[4] = k_inst[4] = '\0';
 if (strncmp(u_inst, "NONE", 4))
    if (strcmp(u_inst, k_inst))
       return FAILURE;

 strncpy(u_pur, &usr_segid[4], 4);
 strncpy(k_pur, &ker_segid[4], 4);
 u_pur[4] = k_pur[4] = '\0';
 if (strncmp(u_pur, "NONE", 4))
    if (strcmp(u_pur, k_pur))
       return FAILURE;

 strncpy(u_pro, &usr_segid[8], 6);
 strncpy(k_pro, &ker_segid[8], 6);
 u_pro[6] = k_pro[6] = '\0';
 if (strncmp(u_pro, "*NONE*", 6))
    if (strcmp(u_pro, k_pro)) return (0);

 strncpy(u_sp_ref, &usr_segid[14], 4);
 strncpy(k_sp_ref, &ker_segid[14], 4);
 u_sp_ref[4] = k_sp_ref[4] = '\0';
 if (strncmp(u_sp_ref, "NONE", 4))
    if (strcmp(u_sp_ref, k_sp_ref)) return (0);


 strncpy(u_job_no, &usr_segid[18], 4);
 strncpy(k_job_no, &ker_segid[18], 4);
 u_job_no[4] = k_job_no[4] = '\0';
 if (strncmp(u_job_no, "NONE", 4))
    if (strcmp(u_job_no, k_job_no))
       return FAILURE;

 strncpy(u_date_time, &usr_segid[22], 12);
 strncpy(k_date_time, &ker_segid[22], 12);
 u_date_time[12] = k_date_time[12] = '\0';
 if (strncmp(u_date_time, "00000000000000", 12))
    if (strcmp(u_date_time, k_date_time))
       return FAILURE;
 
 strncpy(u_grp_id, &usr_segid[34], 3);
 strncpy(k_grp_id, &ker_segid[34], 3);
 u_grp_id[3] = k_grp_id[3] = '\0';
 if (strncmp(u_grp_id, "*NO", 3))
    if (strcmp(u_grp_id, k_grp_id))
       return FAILURE;
 
 strncpy(u_grp_id, &usr_segid[37], 3);
 strncpy(k_grp_id, &ker_segid[37], 3);
 u_grp_id[3] = k_grp_id[3] = '\0';
 if (strncmp(u_grp_id, "NE*", 3))
    if (strcmp(u_grp_id, k_grp_id))
       return FAILURE;

 return SUCCESS;
}
/*================================================*
 * Thu Apr  4 16:40:58 PST 1996			  *
 * lowerSegidPriority(): this was added to allow  *
 *	user to select prov. information with     *
 *	with priority. Basically, if the segid	  *
 *	is all defaults, then you just return.    *
 *	Else, you decrease the priority every	  *
 *	time you call this. The priority are: 	  *
 *		1/ date & time			  *
 *		2/ reqnum			  *
 *		3/ purpose			  *
 *		4/ userid			  *
 *		5/ groupid			  *
 *		6/ institution			  *
 * Basically, if the first time, we couldn't find *
 * any segment which match exactly what we want   *
 * we push the priority down by making the	  *
 * 'date & time' field defaults. By making it the *
 * default value, it will not be compare anymore  *
 * Keep on doing this until, all the field become *
 * default or a segment is found....Eventually we *
 * have to check to see if the prov. is needed    *
 * before the NAIF kernels are loaded.		  *
 * Since the MIPS kernels do not have the segment *
 * with the prov. info we wanted, the NAIF kernels*
 * certainly do not have the exact segment...So   *
 * by the time we load the NAIF kernels, we don't *
 * care about the prov. anymore.		  *
 *						  *
 * This return FAILURE if the usrinfo structure   *
 * is already at its defaults........		  *
 * Return SUCCESS otherwise			  * 
 *================================================*/
int lowerSegidPriority(usr_kdb_typ *usrinfo)
{
 char temp[5];

 if (strncmp(usrinfo->date_time, "00000000000000", 12))
    strcpy(usrinfo->date_time, "000000000000");
 else if (strncmp(usrinfo->req_no, "NONE", 4))
    strcpy(usrinfo->req_no, "NONE");
 else if (strncmp(usrinfo->purpose, "NONE", 4))
    strcpy(usrinfo->purpose, "NONE");
 else if (strncmp(usrinfo->prog_name, "*NONE*", 6))
    strcpy(usrinfo->prog_name, "*NONE*");
 else if (strncmp(usrinfo->sp_ref, "NONE", 4))
    strcpy(usrinfo->prog_name, "NONE");
 else if (strncmp(usrinfo->usr_grp_id, "*NO", 3))
    strncpy(usrinfo->usr_grp_id, "*NO", 3);
 else if (strncmp(&usrinfo->usr_grp_id[3], "NE*", 3))
    strcpy(&usrinfo->usr_grp_id[3], "NE*");
 else if (strncmp(usrinfo->segid, "NONE", 4))
    strcpy(usrinfo->segid, "NONE");
 else
    return FAILURE;

 memcpy((void*)temp, usrinfo->segid, 4);
 temp[4] = '\0';

 strcpy(usrinfo->segid, temp);
 strcat(usrinfo->segid, usrinfo->purpose);
 strcat(usrinfo->segid, usrinfo->prog_name);
 strcat(usrinfo->segid, usrinfo->sp_ref);
 strcat(usrinfo->segid, usrinfo->req_no);
 strcat(usrinfo->segid, usrinfo->date_time);
 strcat(usrinfo->segid, usrinfo->usr_grp_id);

 return SUCCESS;
} 
/*================================================*
 * buf2scet(): Extract scet data from user's	  *
 * 	buffer, also make utc string		  *
 *================================================*/
int buf2scet(buf, scet, utc)
 buf_union_typ	*buf;
 int		*scet;
 char		*utc;
{
 scet[0] = buf->intbuf[2];
 scet[1] = buf->intbuf[3];
 scet[2] = buf->intbuf[4];
 scet[3] = buf->intbuf[5];
 scet[4] = buf->intbuf[6];
 scet[5] = buf->intbuf[7];

 if ((scet[0] < 0) || 					/* Make sure	*/
     (scet[1] < 0) || (scet[1] > 365) ||		/* SCET data is */
     (scet[2] < 0) || (scet[2] > 23) ||			/* correct	*/
     (scet[3] < 0) || (scet[3] > 59) ||
     (scet[4] < 0) || (scet[4] > 59) ||
     (scet[5] < 0) || (scet[5] > 999)) {
    zvmessage("BUF2SCET::Invalid SCET Data", " ");
    return (-2);
    }
 sprintf(utc, "%04d-%03d//%02d:%02d:%02d.%03d",    	/* if good SCET */
	scet[0], scet[1], scet[2],      		/* make UTC     */
	scet[3], scet[4], scet[5]);     		/* string       */
 return (1);
}
/*================================================*
 * Segid2Prov();				  *
 *================================================*/
int Segid2Prov(segid, prov_info)
 char 		*segid;
 prov_info_typ 	*prov_info;
{
/*----------------------------
 if (GllMipsSegid95(segid)) {|
 -----------------------------
*/
    memcpy(&prov_info->inst[0], &segid[0], 4);
    prov_info->inst[4] = '\0';
    if ((int) strlen(prov_info->inst) < 4)
       spaceFilledStr(prov_info->inst, 4);
    if (!AlnumStr(prov_info->inst, 4))
       strcpy(prov_info->inst, "NONE");

    memcpy(&prov_info->purpose[0], &segid[4], 4);
    prov_info->purpose[4] = '\0';
    if ((int)strlen(prov_info->purpose) < 4)
       spaceFilledStr(prov_info->purpose, 4);
    if (!AlnumStr(prov_info->purpose, 4))
       strcpy(prov_info->purpose, "NONE");

    memcpy(&prov_info->prog_name[0], &segid[8], 6);
    prov_info->prog_name[6] = '\0';
    if ((int)strlen(prov_info->prog_name) < 6)
       spaceFilledStr(prov_info->prog_name, 6);
    if (!AlnumStr(prov_info->prog_name, 6))
       strcpy(prov_info->prog_name, "*NONE*");

    memcpy(&prov_info->sp_ref[0], &segid[14], 4);
    prov_info->sp_ref[4] = '\0';
    if ((int)strlen(prov_info->sp_ref) < 4)
       spaceFilledStr(prov_info->sp_ref, 4);
    if (!AlnumStr(prov_info->sp_ref, 4))
       strcpy(prov_info->sp_ref, "NONE");

    memcpy(&prov_info->req_no[0], &segid[18], 4);
    prov_info->req_no[4] = '\0';
    if ((int)strlen(prov_info->req_no) < 4)
       spaceFilledStr(prov_info->req_no, 4);
    if (!AlnumStr(prov_info->req_no, 4))
       strcpy(prov_info->req_no, "NONE");

    memcpy(&prov_info->year[0], &segid[22], 4);
    prov_info->year[4] = '\0';
    if (!NumbStr(prov_info->year, 4))
       strcpy(prov_info->year, "0000");

    memcpy(&prov_info->month_day[0], &segid[26], 4);
    prov_info->month_day[4] = '\0';
    if (!NumbStr(prov_info->month_day, 4))
       strcpy(prov_info->month_day, "0000");

    memcpy(&prov_info->hour_min[0], &segid[30], 4);
    prov_info->hour_min[4] = '\0';
    if (!NumbStr(prov_info->hour_min, 4))
       strcpy(prov_info->hour_min, "0000");

    memcpy(&prov_info->usr_grp_id[0], &segid[34], 3);
    prov_info->usr_grp_id[3] = '\0';
    if (!AlnumStr(prov_info->usr_grp_id, 3))
       strcpy(prov_info->usr_grp_id, "*NO"); 

    memcpy(&prov_info->usr_grp_id[3], &segid[37], 3);
    prov_info->usr_grp_id[6] = '\0';
    if (!AlnumStr(&prov_info->usr_grp_id[3], 3))
       strcpy(&prov_info->usr_grp_id[3], "NE*"); 

/*-------------------------------------------------------
    }							|
 else if (GllMipsSegid89(segid)) {			|
   memcpy(&prov_info->inst[0], &segid[0], 4);		|
   prov_info->inst[4] = '\0';				|
   if (strlen(prov_info->inst) < 4)			|
       spaceFilledStr(prov_info->inst, 4);		|
   if (!AlnumStr(prov_info->inst, 4))			|
      strcpy(prov_info->inst, "NONE");			|
							|
   memcpy(&prov_info->purpose[0], &segid[4], 4);	|
   if (strlen(prov_info->purpose) < 4)			|
       spaceFilledStr(prov_info->purpose, 4);		|
   if (!AlnumStr(prov_info->purpose, 4))		|
      strcpy(prov_info->purpose, "NONE");		|
							|
   strcpy(prov_info->prog_name, "*NONE*");		|
   GllSegid2Kernelid(segid, prov_info->sp_ref);		|
   strcpy(prov_info->req_no, "NONE");			|
							|
   GllSegid2Year(segid, prov_info->year);		|
   GllSegid2Month_day(segid, prov_info->month_day);	|
							|
   strcpy(prov_info->hour_min, "0000");			|
   strncpy(prov_info->usr_grp_id, &segid[28], 6);	|
   prov_info->usr_grp_id[6] = '\0';			|
   }							|
 else if (GllNaifSegid(segid)) {			|
   strcpy(prov_info->inst, "NAIF");			|
							|
   strncpy(prov_info->purpose, &segid[15], 4);		|
   prov_info->purpose[4] = '\0';			|
   if (!AlnumStr(prov_info->purpose, 4))		|
      strcpy(prov_info->purpose, "NONE");		|
							|
   strncpy(prov_info->prog_name, &segid[7], 6);		|
   prov_info->prog_name[6] = '\0';			|
   if (!AlnumStr(prov_info->prog_name, 6))		|
      strcpy(prov_info->prog_name, "*NONE*");		|
							|
   strcpy(prov_info->sp_ref, "NONE");			|
   strcpy(prov_info->req_no, "NONE");			|
							|
   strncpy(prov_info->year, &segid[33], 4);		|
   prov_info->year[4] = '\0';				|
   if (!NumbStr(prov_info->year, 4))			|
      strcpy(prov_info->year, "0000");			|
							|
   NaifSegid2Month_day(segid, prov_info->month_day); 	|
							|
   strcpy(prov_info->hour_min, "0000");			|
							|
   strncpy(prov_info->usr_grp_id, &segid[0], 6);	|
   prov_info->usr_grp_id[6] = '\0';			|
   if (!AlnumStr(prov_info->usr_grp_id, 6))		|
      strcpy(prov_info->usr_grp_id, "*NONE*");		|
   } 							|
 else {							|
   strcpy(prov_info->inst, "NONE");			|
   strcpy(prov_info->purpose, "NONE"); 			|
   strcpy(prov_info->prog_name, "*NONE*");		|
   strcpy(prov_info->sp_ref, "NONE");			|
   strcpy(prov_info->req_no, "NONE");			|
   strcpy(prov_info->year, "0000");			|
   strcpy(prov_info->month_day, "0000");		|
   strcpy(prov_info->hour_min, "0000");			|
   strcpy(prov_info->usr_grp_id, "*NONE*"); 		|
   }							|
---------------------------------------------------------
*/
 return (SUCCESS);
}
/*========================================================*
 * int NaifSegid2Month_day()				  *
 *========================================================*/
int NaifSegid2Month_day(naif_segid, month_day)
 char	*naif_segid;
 char	*month_day;
{
 char	day[3],
	month[4];
 int 	month_numb;

 strncpy(day, &naif_segid[26], 2);
 day[2] = '\0';

 strncpy(month, &naif_segid[29], 3);
 month[3] = '\0';

 if (strncmp(month, "JAN", 3) == 0)
    month_numb = 1;
 else if (strncmp(month, "FEB", 3) == 0)
    month_numb = 2;
 else if (strncmp(month, "MAR", 3) == 0)
    month_numb = 3;
 else if (strncmp(month, "APR", 3) == 0)
    month_numb = 4;
 else if (strncmp(month, "MAY", 3) == 0)
    month_numb = 5;
 else if (strncmp(month, "JUN", 3) == 0)
    month_numb = 6;
 else if (strncmp(month, "JUL", 3) == 0)
    month_numb = 7;
 else if (strncmp(month, "AUG", 3) == 0)
    month_numb = 8;
 else if (strncmp(month, "SEP", 3) == 0)
    month_numb = 9;
 else if (strncmp(month, "OCT", 3) == 0)
    month_numb = 10;
 else if (strncmp(month, "NOV", 3) == 0)
    month_numb = 11;
 else if (strncmp(month, "DEC", 3) == 0)
    month_numb = 12;
 else
    month_numb = 0;

 sprintf(month_day, "%2d%s", month_numb, day);
 
 return SUCCESS;
}
/*========================================================*
 * int GllSegid2Month_day()				  *
 *========================================================*/
int GllSegid2Month_day(segid, month_day)
 char	*segid;
 char	*month_day;
{
 char	month[3],
	day[3];

 memset(day, '\0', 3);
 memset(month, '\0', 3);
 memset(month_day, '\0', 4);

 strncpy(day, &segid[21], 2);
 strncpy(month, &segid[19], 2);

 sprintf(month_day, "%s%s", month, day);
 if (NumbStr(month_day, 4))
    strcpy(month_day, "0000");

 return SUCCESS;
}
/*========================================================*
 * int GllSegid2Year()					  *
 *========================================================*/
int GllSegid2Year(segid, year)
 char 	*segid;
 char	*year;
{
 int	year_val;
 char	year_str[3],
 	curr_time[100];

 GetDateTime(curr_time);

 memset (year_str, '\0', 3);
 strncpy(year_str, &segid[23], 2);

 year_val = (int) atoi(year_str);
 GetDateTime(curr_time);
 if (curr_time[0] == '2')
    year_val += 2000;
 else if (curr_time[0] == '1')
    year_val += 1900;

 sprintf(year, "%4d", year_val);

 return SUCCESS;
}
/*================================================*
 * int GllSegid2Kernelid()			  *
 *================================================*/
int GllSegid2Kernelid(segid, kernelid)
 char	*segid;
 char 	*kernelid;
{
 if (GllMipsSegid95(segid)) {
    memset(kernelid, '\0', 5);
    memcpy(kernelid, &segid[14], 4);
    return SUCCESS;
    }

 if (GllMipsSegid89(segid)) {
    int			i, status,
			kernel_count;
    char		f_fileid[3];
    kernel_db_typ	kernel_db[MAX_KERNELS];

    memset(kernelid, '\0', 5);
    memset(f_fileid, '\0', 3);

    initspice();
    status = GetKdb95(kernel_db, GLL, MAX_KERNELS);
    if (status != SUCCESS) {
       zvmessage("GllSegid2Kernelid::Cannot Read KERNELDB", " ");
       strcpy(kernelid, "NONE");
       return FAILURE;
       }
 
    kernel_count = 0;
    while (kernel_db[kernel_count].id[0] &&
	   kernel_db[kernel_count].type)
       kernel_count++;

    strncpy(f_fileid, &segid[10], 2);

    for (i = 0; i < kernel_count; i++) 
       if (strncmp(&kernel_db[i].id[2], f_fileid, 2) == 0) {
          strncpy(kernelid, kernel_db[i].id, 4);
          return SUCCESS;
          }

    strcpy(kernelid, "NONE");
    return FAILURE;
    }

 return FAILURE;
}
/*================================================*
 * int GllMipsSegid95()			  	  *
 *================================================*/
int GllMipsSegid95(segid)
 char *segid;
{
 int	i;

 for (i = 0; i < 40; i++)
    if ((segid[i] == '/') || 
	(segid[i] == '.') ||
	(segid[i] == ' ') ||
 	(segid[i] == '_') ||
	(segid[i] == '*') ||
	((segid[i] >= 'A') && (segid[i] <= 'z')) ||
	((segid[i] >= '0') && (segid[i] <= '9'))) { }
    else
       return FAILURE;

 return SUCCESS;
} 
/*================================================*
 * int GllMipsSegid89()				  *
 *================================================*/
int GllMipsSegid89(segid)
 char *segid;
{
 char tempsegid[41];

 memset((char *) tempsegid, '\0', 41);
 memcpy(tempsegid, segid, 40);

 if ((tempsegid[8] != (char) ' ') ||
     (tempsegid[9] != (char) ' ') ||
     (tempsegid[18] != (char) ' ') ||
     (tempsegid[27] != (char) ' '))
    return FAILURE;

 if (strncmp(&tempsegid[34], "      ", 6) != 0)
    return FAILURE;

 return SUCCESS;
}
/*================================================*
 * int GllNaifSegid()				  *
 *================================================*/
int GllNaifSegid(segid)
 char	*segid;
{
 char	tempsegid[41];

 memset(tempsegid, '\0', 41);
 memcpy(tempsegid, segid, 40);

 if ((tempsegid[6] != ' ') ||
     (tempsegid[14] != ' ') ||
     (tempsegid[18] != ' ') ||
     (tempsegid[25] != ' '))
    return FAILURE;

 return SUCCESS;
}
/*================================================*
 * int ChckUsrInfo():				  *
 *================================================*/
int ChckUsrInfo(usr_info)
 prov_info_typ *usr_info;
{
 spaceFilledStr(usr_info->inst, LEN_SPICE_ID);
 spaceFilledStr(usr_info->purpose, LEN_PURPOSE);
 
 if (!AlnumStr(usr_info->inst, strlen(usr_info->inst)))
    strcpy(usr_info->inst, "NONE");
 if (!AlnumStr(usr_info->purpose, strlen(usr_info->purpose)))
    strcpy(usr_info->purpose, "NONE");
 if (!AlnumStr(usr_info->prog_name, strlen(usr_info->prog_name)))
    strcpy(usr_info->prog_name, "*NONE*");
 if (!AlnumStr(usr_info->sp_ref, strlen(usr_info->sp_ref)))
    strcpy(usr_info->sp_ref, "NONE");
 if (!AlnumStr(usr_info->req_no, strlen(usr_info->req_no)))
    strcpy(usr_info->req_no, "NONE");
 if (!NumbStr(usr_info->year, strlen(usr_info->year)))
    strcpy(usr_info->year, "0000");
 if (!NumbStr(usr_info->hour_min, strlen(usr_info->hour_min)))
    strcpy(usr_info->hour_min, "0000");
 if (!AlnumStr(usr_info->file_id, strlen(usr_info->file_id)))
    strcpy(usr_info->file_id, "NONE");
 if (!AlnumStr(usr_info->usr_grp_id, strlen(usr_info->usr_grp_id)))
    strcpy(usr_info->usr_grp_id, "*NONE*");
 return (1);
}
/*==================================================*
 * int FillMipsBuf()                                *
 *==================================================*/
int FillMipsBuf(imgfile, prjt, buf)
 char           *imgfile;
 int            prjt;
 buf_union_typ  *buf;
{
 int    unit,
        data[80],
        status = 0;
 char   project[6],
        camera[20];

 buf->intbuf[0] = prjt;
 if (prjt == VGR_1) {
    zvmessage("VOYAGER-1 Is Not Yet Supported", " ");
    return FAILURE;
    }
 if (prjt == VGR_2) {
    zvmessage("VOYAGER-2 Is Not Yet Supported", " ");
    return FAILURE;
    }
 if (prjt == GLL) {
    strcpy(camera, "SSI ");
    memcpy((char *) &buf->intbuf[1],
                (char *) camera, 4);
    buf->intbuf[9] = J2000;
    strcpy(project, "GLL   ");
    }

 zvunit(&unit, "xxx", status + 1,
        "u_name", imgfile, NULL);
 zvopen(unit, "OPEN_ACT", "SA", NULL);
 zgetlabcon(unit, project, data, &status);

 buf->intbuf[2] = data[7];
 buf->intbuf[3] = data[8];
 buf->intbuf[4] = data[9];
 buf->intbuf[5] = data[10];
 buf->intbuf[6] = data[11];
 buf->intbuf[7] = data[12];
 memcpy((char *) &buf->intbuf[178],
        (char *) &data[24], 12);
 zvclose(unit, NULL);

 if (status == 2) {
    zvmessage("ZGETLABCON::Invalid Image Label", " ");
    return FAILURE;
    }
 else if (status == 3) {
    zvmessage("ZGETLABCON::Invalid Project Code", " ");
    return FAILURE;
    }
 else if (status == 1) {
    zvmessage("ZGETLABCON::Incomplete Image Label", " ");
    return FAILURE;
    }
 return SUCCESS;
}
/*================================================*
 * Handle2Id(): 				  *
 *================================================*/
int Handle2Id(kernel_db, handle, ftype, id, fname)
 kernel_db_typ	kernel_db[];			/* summary of kernel files */
 int		handle;				/* handle of requested file*/
 int		ftype;				/* type of requested file  */
 char		*id;				/* requested file id	   */
 char		*fname;				/* requested file name	   */
{
 int	i, kernel_count = 0;

 while (kernel_db[kernel_count].id[0] &&	/* get the number of items */
	kernel_db[kernel_count].type)		/* in kernel_db		   */
    kernel_count++;

 for(i = 0; i < kernel_count; i++)		/* look to see if file has */
    if ((kernel_db[i].type == ftype) &&		/* the same type and 	   */
	(kernel_db[i].handle == handle)) {	/* handle with the rqsted  */
       memcpy(id, kernel_db[i].id, 4);		/* file. If yes, copy id   */
       strcpy(fname, kernel_db[i].filename);	/* and name, then return   */
       return SUCCESS;
       }

 strcpy(id, "NONE");				/* if comes here, no file  */
 strcpy(fname, "*NONE*");			/* has the same handle and */
 return FAILURE;				/* type, return FAILURE	   */
}
/*================================================*
 * int GLLId2FileName(id, filename)		  *
 *================================================*/
int GllId2FileName(id, filename)
 char	*id;
 char	*filename;
{
 int		i,
		status,
		count;
 kernel_db_typ	kernel_db[MAX_KERNELS];
 
 status = GetKdb95(kernel_db, GLL, MAX_KERNELS);
 if (status != SUCCESS)
    return FAILURE;

 count = 0;
 while (kernel_db[count].id[0] &&
 	kernel_db[count].type) count++;

 for (i = 0; i < count; i++)
    if (strncmp(kernel_db[i].id, id, 4) == 0) {
       strcpy(filename, kernel_db[i].filename);
       return SUCCESS;
       }

 strcpy(filename, "NONE");
 return FAILURE;
} 
/*================================================*
 * GetDateTime(): fills in string data, 12        *
 *      chars, with the current yyyymmddhhmm      *
 *================================================*/
int GetDateTime(data)
 char *data;
{
 struct tm      *currtime;
 time_t         bintime;
 time(&bintime);
 currtime = (struct tm *) localtime(&bintime);
 sprintf(data, "%04d%02d%02d%02d%02d",
        currtime->tm_year + 1900,
        currtime->tm_mon + 1,
        currtime->tm_mday,
        currtime->tm_hour,
        currtime->tm_min);
 return SUCCESS;
}
/*======================================================*
 * int CheckGllKer(): Only check support files in 	*
 *	SPICEKER for GLL. The following files are	*
 *	checked: binpool, kerneldb, body_ids, 		*
 *	leapseconds, sclk, constants and directory	*
 *	spiceker. Except for spiceker, user only need  	*
 *	to have read permission on these file. To do	*
 *	local SPICE processing, users must have both	*
 *	read & write permision on SPICKER directory.	*
 *	It does not check CK & SPK files. SPICEKER is   *
 *	the directory where local SPICE files are 	*
 *	stored. 					*
 *======================================================*/
int CheckGllKer()
{
 if ((CheckSpiceKer()    != SUCCESS) ||
     (CheckKernelDB()    != SUCCESS) ||

/*****************************************
 Thu May 15 11:56:44 PDT 1997
 This is being commented out because later
 versions of NAIF Toolkit does not support
 BinPool.....

     (CheckBinPool()     != SUCCESS) ||
 *****************************************/

     (CheckBody_ids()    != SUCCESS) ||
     (CheckSclk()        != SUCCESS) ||
     (CheckConstants()   != SUCCESS) ||
     (CheckLeapSeconds() != SUCCESS))
    return FAILURE;
 else
    return SUCCESS;
}
/*=======================================================*
 * int CheckGllSpcRead():				 *
 *=======================================================*/
int CheckGllSpcRead()
{
 int		i,
		count,
		status;
 kernel_db_typ	kernel_db[MAX_KERNELS];

 for (i = 0; i < MAX_KERNELS; i++)		
    memset((char *) &kernel_db[i],
	'\0', sizeof(kernel_db_typ));

 status = GetKdb95(kernel_db, GLL, MAX_KERNELS);
 if (status != SUCCESS) {
    zvmessage("CheckGllSpcRd::Error Reading Kernel Table", " ");
    return FAILURE;
    }

 count = 0;
 while (kernel_db[count].id[0] &&
	kernel_db[count].type)
    count++;

 for (i = 0; i < count; i++) {
    status = IsFileReadable(kernel_db[i].filename);
    if (status != SUCCESS)
       return FAILURE;
    }
 return SUCCESS;
}
/*=======================================================*
 * int CheckGllSpcWrite()				 *
 *=======================================================*/
int CheckGllSpcWrite()
{
  int            i,
                count,
                status;
 kernel_db_typ  kernel_db[MAX_KERNELS];

 for (i = 0; i < MAX_KERNELS; i++)
    memset((char *) &kernel_db[i],
        '\0', sizeof(kernel_db_typ));

 status = GetKdb95(kernel_db, GLL, MAX_KERNELS);
 if (status != SUCCESS) {
    zvmessage("CheckGllSpcWr::Error Reading Kernel Table", " ");
    return FAILURE;
    }

 count = 0;
 while (kernel_db[count].id[0] &&
        kernel_db[count].type)
    count++;


 for (i = 0; i < count; i++)
    if (strcmp(kernel_db[i].source, "NAIF")) {
       status = IsFileReadWriteable(kernel_db[i].filename);
       if (status != SUCCESS) return FAILURE;
       }
    else {
       status = IsFileReadable(kernel_db[i].filename);
       if (status != SUCCESS) return FAILURE;
       }
 return SUCCESS;
}
/*=======================================================*
 * int IsFileReadable(): given a file name, this function*
 *	checks to see if the user (calling the function) *
 *	has read permission on the file. If yes, return  *
 *	SUCCESS(1), else return FAILURE(0)		 *
 *=======================================================*/
int IsFileReadable(fname)
 char fname[];
{
 int	bit;

#if UNIX_OS
 uid_t	uid;
 gid_t	gid;
#endif

#if VMS_OS
 unsigned int	uid;
 unsigned int	gid;
#endif
 
 int	status;
 struct stat statbuf;

#if UNIX_OS
 status = stat(fname, &statbuf);
 if (status != 0) {
    char	local_mesg[256];

    zvmessage("IsFileReadable::stat() Failed", " ");
    perror("stat()");
    sprintf(local_mesg, "Filename: %s", fname);
    zvmessage(local_mesg, " ");
    return FAILURE;
    }
#endif

 uid = getuid();
 gid = getgid();

#if UNIX_OS
 if (uid == statbuf.st_uid)
    bit = (int) ((int) (statbuf.st_mode & 0700) / (int) 64);
 else if (gid == statbuf.st_gid)
    bit = (int) ((int ) (statbuf.st_mode & 070) / (int) 8);
 else
    bit = (int) (statbuf.st_mode & 07);
#endif

#if VMS_OS
 if (uid == (unsigned short) statbuf.st_uid)
    bit = (int) ((int) (statbuf.st_mode & 0700) / (int) 64);
 else if (gid == (unsigned short) statbuf.st_gid) 
    bit = (int) ((int) (statbuf.st_mode & 070) / (int) 8);
 else
    bit = (int) (statbuf.st_mode & 07);

 return SUCCESS;
#endif


 if ((bit > 3) && (bit < 8))
    return SUCCESS;
 else
    return FAILURE;
}
/*=======================================================*
 * int IsFileWriteable(): given a file name, this 	 *
 *	function checks to see if the user calling this	 *
 *	has write permission on the file. If yes, 	 *
 *	return SUCCESS(1) else return FAILURE(0)	 *
 *=======================================================*/
int IsFileWriteable(fname)
 char	fname[];
{
 int	bit;

#if UNIX_OS
 uid_t	uid;
 gid_t	gid;
#endif

#if VMS_OS
 int	uid;
 int	gid;
#endif

 struct stat statbuf;

 if (stat(fname, &statbuf) == (-1)) {
    char	local_mesg[256];

    zvmessage("IsFileWriteable::WARNING::stat() Failed", " ");
    sprintf(local_mesg, "Stat FileName: %s", fname);
    zvmessage(local_mesg, " ");
    perror("SYS_MESG:");
    }

 uid = getuid();
 gid = getgid();

#if UNIX_OS
 if (uid == statbuf.st_uid) 
    bit = (int) ((int) (statbuf.st_mode & 0700) / (int) 64);
 else if (gid == statbuf.st_gid) 
    bit = (int) ((int) (statbuf.st_mode & 070) / (int) 8);
 else 
    bit = (int) (statbuf.st_mode & 07);
#endif
 
#if VMS_OS
 if (uid == (unsigned short) statbuf.st_uid)
    bit = (int) ((int) (statbuf.st_mode & 0700) / (int) 64);
 else if (gid == (unsigned short) statbuf.st_gid) 
    bit = (int) ((int) (statbuf.st_mode & 070) / (int) 8);
 else
    bit = (int) (statbuf.st_mode & 07);
#endif

 if ((bit == 2) || (bit == 3) ||
     (bit == 6) || (bit == 7))
    return SUCCESS;
 else
    return FAILURE;
}
/*=======================================================*
 * int IsFileReadWriteable(): given a file name, this    *
 *	file to see if the user has both read & write 	 *
 *	permission on the file. If yes return SUCCESS(1) *
 *	If no return FAILURE(0)				 *
 *=======================================================*/
int IsFileReadWriteable(fname)
 char fname[];
{
 int	bit;
 struct stat statbuf;

#if UNIX_OS
 uid_t	uid;
 gid_t  gid;
#endif

#if VMS_OS
 int	uid;
 int	gid;
#endif

#if UNIX_OS
 if (stat(fname, &statbuf) == (-1)) {
    char	local_mesg[256];

    zvmessage("IsFileRW-able::ERROR::stat() Failed", " ");
    sprintf(local_mesg, "Stat File Name: %s", fname);
    zvmessage(local_mesg, " ");
    return FAILURE;
    }
#endif

 uid = getuid();
 gid = getgid();

#if UNIX_OS
 if (uid == statbuf.st_uid) 
    bit = (int) ((int) (statbuf.st_mode & 0700) / (int) 64);
 else if (gid == statbuf.st_gid) 
    bit = (int) ((int) (statbuf.st_mode & 070) / (int) 8);
 else 
    bit = (int) (statbuf.st_mode & 07);
#endif

#if VMS_OS
 if (uid == (unsigned short) statbuf.st_uid)
    bit = (int) ((int) (statbuf.st_mode & 0700) / (int) 64);
 else if (gid == (unsigned short) statbuf.st_gid) 
    bit = (int) ((int) (statbuf.st_mode & 070) / (int) 8);
 else
    bit = (int) (statbuf.st_mode & 07);

 return SUCCESS;
#endif

 if ((bit == 6) || (bit == 7))
    return SUCCESS;
 else
    return FAILURE;
}
/*=======================================================*
 * int CheckLeapSeconds(): This function is called by	 *
 *	CheckLclGllKer(). It checks to see if the calling*
 *	user has read permission on leapseconds file.	 *
 *	Leapseconds file being check here is the	 *
 *	one defined by the environment variable		 *
 *	$LEAPSECONDS					 *
 *	1. get SPICEKER environment variable		 *
 *	2. check for read permission, IsFileReadable().	 *
 *=======================================================*/
int CheckLeapSeconds()
{
 char	*ptr,
	fname[256];

 printf("	Leapsecond...");
 if ((ptr = getenv("LEAPSECONDS")) == NULL) {		/* get leapsec	*/
    zvmessage("ERROR::LEAPSECONDS Not Defined", " ");	/* file		*/
    return FAILURE;
    }
 else strcpy(fname, ptr);
 if (IsFileReadable(fname) == SUCCESS) {		/* is file	*/
    printf("SUCCESS\n");				/* readable	*/
    return SUCCESS;
    }
 else {
    printf("FAILED::Not readable\n");
    return FAILURE;
    }
}
/*=======================================================*
 * int CheckConstants(): This function is called by    	 *
 *      CheckLclGllKer(). It checks to see if the calling*
 *      user has read permission on CONSTANTS file.      *
 *      Constants file being check here is the       	 *
 *      one defined by the environment variable          *
 *      $CONSTANTS					 *
 *      1. get SPICEKER environment variable             *
 *      2. check for read permission, IsFileReadable().  *
 *=======================================================*/
int CheckConstants()
{
 char   *ptr,
        fname[256];

 printf("	Constants...");
 if ((ptr = getenv ("CONSTANTS")) == NULL) {          	/* get constants*/
    zvmessage("ERROR::CONSTANTS Not Defined", " ");	/* file name	*/
    return FAILURE;
    }
 else strcpy(fname, ptr);
 if (IsFileReadable(fname) == SUCCESS) {
    printf("SUCCESS\n");
    return SUCCESS;
    }
 else {
    printf("FAILED\n");
    return FAILURE;
    }
}
/*=======================================================*
 * int CheckSclk(): This function is called by     	 *
 *      CheckLclGllKer(). It checks to see if the calling*
 *      user has read permission on SCLK file.      	 *
 *      Constants file being check here is the       	 *
 *      one defined by the environment variable          *
 *      $SCLK						 *
 *      1. get SPICEKER environment variable             *
 *      2. check for read permission, IsFileReadable().  *
 *=======================================================*/
int CheckSclk()
{
 char   *ptr,
        fname[256];

 printf("	SCLK...");
 if ((ptr = getenv ("SCLK")) == NULL) {       		/* get SCLK	*/
    zvmessage("ERROR::SCLK Not Defined", " ");		/* file name	*/
    return FAILURE;
    }
 else strcpy(fname, ptr);
 if (IsFileReadable(fname) == SUCCESS) {                /* is file      */
    printf("SUCCESS\n");				/* readable	*/
    return SUCCESS;
    }
 else {
    printf("FAILED\n");
    return FAILURE;
    }
}
/*=======================================================*
 * int CheckBinPool(): This function is called by        *
 *      CheckLclGllKer(). It checks to see if the calling*
 *      user has read permission on BINPOOL file.        *
 *      Constants file being check here is the      	 *
 *      one defined by the environment variable          *
 *      $BINPOOL.					 *
 *      1. get SPICEKER environment variable             *
 *      2. check for read permission, IsFileReadable().  *
 *=======================================================*/
int CheckBinPool()
{
 char   *ptr,
        fname[256];

 printf("	BinPool...");
 if ((ptr = getenv("BINPOOL")) == NULL) { 	        /* get binpool  */
    zvmessage("ERROR::BINPOOL Not Defined", " ");	/* file name	*/
    return FAILURE;
    }
 else strcpy(fname, ptr);
 if (IsFileReadable(fname) == SUCCESS)  {
    printf("SUCCESS\n");
    return SUCCESS;
    }
 else {
    printf("FAILED\n");
    return FAILURE;
    }
}
/*=======================================================*
 * int CheckKernelDB(): This function is called by       *
 *      CheckLclGllKer(). It checks to see if the calling*
 *      user has read permission on KERNELDB file.    	 *
 *      KERNELDB file being check here is the     	 *
 *      one defined by the environment variable          *
 *      $KERNELDB.					 *
 *      1. get SPICEKER environment variable             *
 *      2. check for read permission, IsFileReadable().  *
 *=======================================================*/
int CheckKernelDB()
{
 char   *ptr,
        fname[256];

 printf("	kernelDB...");
 if ((ptr = getenv("KERNELDB")) == NULL) {
    zvmessage("CheckKernelDB::KERNELDB Not Defined", " ");
    return FAILURE;
    }
 else strcpy(fname, ptr);
 if (IsFileReadable(fname) == SUCCESS) {
    printf("SUCCESS\n");
    return SUCCESS;
    }
 else {
    printf("FAILED\n");
    return FAILURE;
    }
}
/*=======================================================*
 * int CheckBody_ids(): This function is called by    	 *
 *      CheckLclGllKer(). It checks to see if the calling*
 *      user has read permission on BODY_IDS file.    	 *
 *      BODY_IDS file being check here is the    	 *
 *      one defined by the environment variable          *
 *      $BODY_IDS					 *
 *      1. get SPICEKER environment variable             *
 *      2. check for read permission, IsFileReadable().  *
 *=======================================================*/
int CheckBody_ids()
{
 char   *ptr,
        fname[256];

 printf("	Body_ids...");
 if ((ptr = getenv ("BODY_IDS")) == NULL) {		/* get BODY_IDS */
    zvmessage("ERROR::BODY_IDS Not Defined", " ");	/* file name	*/
    return FAILURE;
    }
 else strcpy(fname, ptr);
 if (IsFileReadable(fname) == SUCCESS) {                /* is file      */
    printf("SUCCESS\n");				/* readable	*/
    return SUCCESS;
    }
 else {
    printf("FAILED\n");
    return FAILURE;
    }
}
/*=======================================================*
 * int CheckSpiceKer(): This function is called by       *
 *      CheckLclGllKer(). It checks to see if the calling*
 *      user has read permission on SPICEKER file.       *
 *      SPICEKER file being check here is the 	  	 *
 *      one defined by the environment variable          *
 *      $SPICEKER.					 *
 *      1. get SPICEKER environment variable             *
 *      2. check for read permission, IsFileReadable().  *
 *=======================================================*/
int CheckSpiceKer()
{
 char   *ptr,
        fname[256];

 printf("	SPICEKER...");
 if ((ptr = getenv("SPICEKER")) == NULL) {              /* get SPICEKER */
    zvmessage("ERROR::SPICEKER Not Define", " "); 	/* path. This   */
    perror("getenv");                                   /* is where your*/
    return FAILURE;                                     /* local SPICE  */
    }
 else							/* files are 	*/
    strcpy(fname, ptr);					/* stored	*/
 if (IsFileReadable(fname) == SUCCESS) {
    printf("SUCCESS\n");
    return SUCCESS;
    }
 else {
    printf("FAILED\n");
    return FAILURE;
    } 
}
/*=======================================================*
 * int IsGoodSource(): given a source string and project *
 *	code, you want to check if the project code 	 *
 *	against kernel table to see if the source match	 *
 *	with any of those in the kernel table. There 	 *
 *	probably are other source values, but SPICE only *
 *	knows source string kept in KERNELDB		 *
 *=======================================================*/
int IsGoodSource(source, project_code)
 char	source[];
 int	project_code;
{
 int		i,
		len,
		count,
		status;
 kernel_db_typ	kernel_db[MAX_KERNELS];

 if ((project_code != GLL) &&
     (project_code != VGR_1) &&
     (project_code != VGR_2)) {
    char	local_mesg[256];
    sprintf(local_mesg, "ERROR::Unknown project code: %d\n",
			project_code);
    zvmessage(local_mesg, " ");
    return FAILURE;
    }

 for (i = 0; i < MAX_KERNELS; i++)
    memset((char *) &kernel_db[i], '\0',
	   sizeof(kernel_db_typ));

 status = GetKdb95(kernel_db, GLL, MAX_KERNELS);
 if (status != SUCCESS) {
    zvmessage("IsGoodSource::GetKdb95() Failed", " ");
    zvmessage("ERROR Reading Kernel Table", " ");
    return FAILURE;
    }

 count = 0;
 while (kernel_db[count].id[0] &&
	kernel_db[count].type) count++;

 for (i = 0; i < count; i++) {
    len = strlen(kernel_db[i].source);
    if (strncmp(source, kernel_db[i].source, len) == 0)
       return SUCCESS;
    }

 return FAILURE;
}
/*==================================================*
 * int IsGoodMipsGllCkId(ck_id):		    *
 *==================================================*/
int IsValidMipsGllCkId(ck_id)
 char	*ck_id;
{
 int		i,
	 	status,
		count;
 kernel_db_typ	kernel_db[MAX_KERNELS];

 status = GetKdb95(kernel_db, GLL, MAX_KERNELS);
 if (status != SUCCESS)
    return FAILURE;
 count = 0;
 while (kernel_db[count].id[0] &&
	kernel_db[count].type) count ++;

 for (i = 0; i < count; i++)
    if ((strncmp(ck_id, kernel_db[i].id, 4) == 0) &&
        (strncmp("NAIF", kernel_db[i].source,
	 strlen(kernel_db[i].source)) != 0))
       return SUCCESS;

 return FAILURE;
}
/*==================================================*
 * RecvTextFile(): this function does not close the *
 *	cli_sd. The calling function must close the *
 *	socket when done. 			    *
 *==================================================*/
int RecvTextFile(cli_sd, filesize, usrfile)
 int    cli_sd;
 int    filesize;
 char   *usrfile;
{
 int            nleft,
                status;
 char           data[1024];
 FILE           *outfile;
 
 outfile = fopen(usrfile, "w");                         /* open new */
 if (outfile == NULL) {                                 /* file to  */
    char	local_mesg[256];

    zvmessage("RecvTextFile::fopen() Failed", " ");
    sprintf(local_mesg, "File Name: %s", usrfile);
    zvmessage(local_mesg, " ");       			/* write    */
    close (cli_sd);					/* into	    */
    return (FAILURE); 
    }
 
 zvmessage("Reading File Off Socket...", " ");
 nleft = filesize;
 while (nleft > 0) {
    int size;
    if (nleft <= 1024)
       size = nleft;
    else
       size = 1024;
 
    status = readn(cli_sd, data, size);                         /* read */
    if (status < size) {                                        /* data */
       zvmessage("RecvTextFile::Socket read() Failed", " ");    /* from */
       perror("readn()");
       close (cli_sd);
       return (FAILURE);
       }
 
    status = fwrite((void *) data, 1, size, outfile);           /* write */
    if (status < size) {                                        /* data  */
       zvmessage("Local Output File Write Failed", " ");        /* into  */
       perror("fwrite()");
       close (cli_sd);                                          /* file  */
       fclose (outfile);
       return (FAILURE);
       }
    nleft -= size;                      /* how much more to read/write?  */
    }
 
 fclose (outfile);
 return (SUCCESS);
}
/*=======================================================*
 * RecvBinFile(): This function does not close the cli_sd*
 *	the calling function must close the socket when  *
 *	it is done.					 *
 *=======================================================*/
int RecvBinFile(cli_sd, filesize, usrfile)
 int    cli_sd;
 int    filesize;
 char   *usrfile;
{
 int    nleft,
        status;
 char   data[1024],
        txtfile[256];
 FILE   *infile;

#if UNIX_OS
 pid_t  pid;
#endif

#if VMS_OS 
 int	pid;
#endif
 pid = getpid();
 sprintf(txtfile, "%s%d%s", "spc_c", pid, ".txt");
 infile = fopen(txtfile, "w");
 if (infile == NULL) {
    zvmessage("GETFILE95::ASCII Out File fopen() Failed", " ");
    close (cli_sd);
    return (FAILURE);
    }
 
 zvmessage("...Reading File Off Socket....", " ");
 nleft = filesize;
 while (nleft > 0) {
    int size;
    if (nleft <= 1024)
       size = nleft;
    else
       size =  1024;
 
    status = readn(cli_sd, data, size);
    if (status != size) {
       zvmessage("RecvBinFile::Socket read() Failed", " ");
       close (cli_sd);
       fclose (infile);
       return (FAILURE);
       }
    else {
       char	ack;
       ack = (char) SUCCESS;
       status = writen(cli_sd, &ack, sizeof(char));
       if (status != sizeof(char)) {
          zvmessage("ERROR Sending ACK To Server", " ");
	  zvmessage("Client Program Terminated", " ");
	  close (cli_sd);
	  fclose (infile);
	  return FAILURE;
	  }
       }

    status = fwrite((void *) data, 1, size, infile);
    if (status < size) {
       zvmessage("Temp ASCII File fwrite() Failed", " ");
       close (cli_sd);
       fclose (infile);
       return (FAILURE);
       }
    nleft -= size;
    }

 fclose (infile);
 
 zvmessage("...Converting ASCII To Binary....", " ");
 zspca2b(txtfile, usrfile);
 if (zfailed()) {
    char	local_mesg[256];

    zvmessage("****ERROR: File Conversion Failed", " ");
    sprintf(local_mesg, "Requested File: %s", usrfile);
    zvmessage(local_mesg, " ");
    }
 else {
    char        local_command[256];

#if UNIX_OS
    sprintf(local_command, "rm %s", txtfile);
#endif

#if VMS_OS
    sprintf(local_command, "delete %s;*", txtfile);
#endif

    system (local_command);
    }

 return (SUCCESS);
}
/*==================================================================*
 * int GetFileSize(): given a file name, this function return the   *
 *	size of the file in bytes. This is not necessary since      *
 *	part of the system call "stat" return the same information  *
 *	but on the VAX, "stat" returns more that the actual size    *
 *	of the file......use this for portability		    *
 *==================================================================*/
int GetFileSize(fname)
 char	*fname;
{
 int		filesize;
 struct stat	statbuf;
#if VMS_OS
 FILE		*infile;
#endif

 if (stat(fname, &statbuf) == (-1))
    return FAILURE;

#if UNIX_OS
 filesize = statbuf.st_size;
#endif

#if VMS_OS
 infile = fopen (fname, "r");
 if (infile == (FILE *) NULL)
    return FAILURE;

 filesize = 0;
 while (fseek(infile, 1, SEEK_CUR) == 0) 
    filesize++;
 fclose (infile);
#endif

 return (filesize);
}
/*==================================================================*
 * int IsDir(): check to see if the given string is a dir or file   *
 *	name							    *
 *==================================================================*/
int IsDir(str)
 char *str;
{
#if VMS_OS
 char		*c;
 int		i,
		len,
		left_flag,
		right_flag;
#endif
 struct stat	statbuf;

#if UNIX_OS
 if (stat(str, &statbuf) == (-1))
    return FAILURE;
 if ((statbuf.st_mode & S_IFMT) == S_IFDIR) 
    return SUCCESS;
 else
    return FAILURE;
#endif

#if VMS_OS
 len = strlen(str);
 left_flag = right_flag = 0;
 for (i = 0; i < len; i++) {
    c = str;
    if ((*c == (char) '[') && 
	(!right_flag) && 
	(!left_flag))
       left_flag = 1;
    else if (*c == (char) '[')
       return FAILURE;
    else if ((*c == (char) ']') &&
	     (left_flag) &&
             (!right_flag))
       right_flag = 1;
    else if (*c == ']')
       return FAILURE;
    }
 if (*c == (char) ']')
    return SUCCESS;
 else
    return FAILURE;
#endif
}
/*=======================================================*
 * LowerString():					 *
 *=======================================================*/
int lowerString(str)
 char	*str;
{
 char	*curr;
 int	i, len;

 curr = str;
 len  = strlen (str);
 for (i = 0; i < len; i++) {
    if (((char) *curr >= (char) 'A') &&
        ((char) *curr <= (char) 'Z'))
       *curr = (char) ((char) *curr + (char) 'a' - (char) 'A'); 
    curr++;
    }

 return SUCCESS;
}
/*=======================================================* 
 * AlnumStr(): given a character string and its     	 *
 * 	length, not counting the terminating NULL char,  *
 *	it determines if the whole string consists of    *
 *	only alphanumeric characters, 0-9 &  A-z         *
 *=======================================================*/
int AlnumStr(str, length)
 char *str; int length;
{
 int 	i;
 char 	*ptr;

 ptr = str;
 for(i = 0; i < length; i++) {
    if ((*ptr == '/') || 
	(*ptr == '.') ||
	(*ptr == '_') ||
	(*ptr == ' ') ||
	((*ptr >= 'A') && (*ptr <= 'z')) ||
	((*ptr >= '0') && (*ptr <= '9'))) ptr++;
    else
       return (FAILURE);
    }
 return (SUCCESS);
}
/*=======================================================*
 * spaceFilledStr(): given a character string and its    *
 *      length, not counting the terminating NULL char,  *
 *      it fills up the string non-alpha numeric chars   *
 *      with spaces                                      *
 *=======================================================*/
int spaceFilledStr(str, length)
 char *str; int length;
{
 int    i;
 char   *ptr;

 ptr = str;
 for(i = 0; i < length; i++) {
    if ((*ptr == '/') ||
        (*ptr == '.') ||
        (*ptr == '_') ||
        ((*ptr >= 'A') && (*ptr <= 'z')) ||
        ((*ptr >= '0') && (*ptr <= '9'))) ptr++;
    else {
       *ptr = ' ';
       ptr++;
       }
    }
 return (SUCCESS);
}
/*==============================================*
 * NumbStr(str, length):			*
 *==============================================*/
int NumbStr(str, length)
 char *str; int length;
{
 int 	i;
 char	*ptr;
 ptr = str;
 for(i = 0; i < length; i++) {
    if (((char) *ptr >= '0') &&
	((char) *ptr <= '9')) ptr++;
    else return (0);
    }
 return (1);
}
/*==============================================*
 * UseProvInfo(): if usrinfo struct only 	*
 * contains default values then you just search *
 * for the latest data segment in the kernel.	*
 *  if usrinfo only has default return 1, else  *
 *  return 0					*
 *==============================================*/
int UseProvInfo(usrinfo)
 usr_kdb_typ *usrinfo;
{
 if ((!strncmp(usrinfo->sp_c_id, "NONE", 4)) &&
     (!strncmp(usrinfo->prog_name, "*NONE*", 6)) &&
     (!strncmp(usrinfo->purpose, "NONE", 4)) &&
     (!strncmp(usrinfo->req_no, "NONE", 4)) &&
     (!strncmp(usrinfo->usr_grp_id, "*NONE*", 6)) &&
     (!strncmp(usrinfo->date_time, "000000000000", 12)) &&
     (!strncmp(usrinfo->segid, "NONE", 4))) {
     return FAILURE;
     }
 else
    return SUCCESS;
}
/*==============================================*
 * ShowKernelDbTyp()				*
 *==============================================*/
int ShowKernelDbTyp(kernel_db)
 kernel_db_typ kernel_db;
{
 char	local_mesg[256];

 sprintf(local_mesg, "filename: %s", kernel_db.filename);
 zvmessage(local_mesg, " ");

 sprintf(local_mesg, "SCET: %s   %s\n",
	kernel_db.scet_begin,
	kernel_db.scet_end);
 zvmessage(local_mesg, " ");

 sprintf(local_mesg, "id: %s  type: %d  handle: %d  source: %s\n",
		kernel_db.id, kernel_db.type,
		kernel_db.handle, kernel_db.source);
 zvmessage(local_mesg, " ");

 sprintf(local_mesg, "RANGE: %g  %g\n",
	kernel_db.range_begin.et_begin,
	kernel_db.range_end.et_end);
 zvmessage(local_mesg, " ");

 return SUCCESS;
}
/*==============================================*
 * readn()					*
 *==============================================*/
int readn (sd, ptr, nbytes)
 int sd; char *ptr; int nbytes;			/* returns the number of */
{         					/* bytes actually read	 */
 int    	nleft,				/* from the socket	 */
		nread,
		status,
		count = 0;
 fd_set		readfds;
 struct timeval	timeout;

 nleft = nbytes;				/* init the total number */
 FD_ZERO(&readfds);				/* of bytes and zero and */
 FD_SET(sd, &readfds);				/* set readfds.		 */

 memset ((char *) &timeout, '\0',		/* init the timeout	 */
	sizeof (struct timeval));		/* count		 */
 timeout.tv_sec = (long) 5;

 while ((nleft > 0) && (count < 10)) {			/* keep reading  */
    status = select (sd+1, (fd_set *) &readfds,		/* make sure sd	 */
	(fd_set *) 0, (fd_set *) 0,			/* is ready to	 */
	(struct timeval *) &timeout);			/* be read	 */
    if ((status > 0) && (FD_ISSET(sd, &readfds))) {
       nread = read(sd, ptr, nleft);
       if (nread < 0) return FAILURE;
       nleft -= nread;
       ptr += nread;
       }
    else if (status == 0)
       count++;
    else if (status < 0)
       return FAILURE;
    }
 if (nleft == 0)
    return (nbytes);
 else
    return FAILURE;
}
/*==============================================*
 * readline()					*
 *==============================================*/
int readline(int sd, char *data, int maxlen)
{
 int	n, rc;
 char	c;
 for (n = 1; n < maxlen; n++) {
    if ((rc = read(sd, &c, 1)) == 1) {
       *data++ = c;
       if (c == '\n') break;
       }
    else if (rc == 0) {
       if (n == 1) return (0);
       else break;
       }
    else return (-1);
    }
 data--;
 *data = '\0';
 return (n);
}
/*==============================================*
 * getServerName()				*
 *==============================================*/
int getServerName(char *svr0, char *svr1, char *svr2)
{
 char	*env,
	intfname[LEN_FILE_NAME];
 char	sv_name[1000],
	*server_name0,
	*server_name1,
	*server_name2;
 int	index,
	status;

 struct stat statbuf;
 FILE	*infile;

 env = (char *) getenv ("MIPSKER");			/* build the name */
 if (env == (char *) NULL) {				/* for spice-	  */
    zvmessage ("ERROR::SPICEKER Not Defined", " ");	/* interface file */
    return FAILURE;
    } 
 else {
#if UNIX_OS
    sprintf(intfname, "%s/%s", env, "spiceinterface");
#endif
#if VMS_OS
    sprintf(intfname, "%s%s", env, "SPICEINTERFACE");
#endif
    }

 if (stat(intfname, &statbuf)) {
    zvmessage("ERROR::getServerName Stat() Failed", " ");
    perror("stat()");
    return FAILURE;
    }

 infile = fopen (intfname, "r");				/* open the  */
 if (infile == (FILE *) NULL) {					/* interface */
    zvmessage("ERROR::Cannot Open SpiceInterface File", " ");	/* file      */
    return FAILURE;
    }
 status = fread((void*) sv_name, 1, statbuf.st_size, infile);
 if (status <= 0) {
    zvmessage("ERROR Reading SpiceInterface File", " ");
    printf("StatusIs: %d\n", status);
    perror("fread");
    fclose(infile);
    return FAILURE;
    } 
 else
    sv_name[status] = '\0';

 index = 0;
 server_name0 = (char*) &sv_name[0];
 server_name1 = server_name2 = server_name0;

 while ((*server_name1 != '\0') && (*server_name1 != '\n'))
    server_name1++;
 if (*server_name1 != '\0') {
    *server_name1 = '\0';
    server_name1++;
    }
 server_name2 = server_name1;
 while ((*server_name2 != '\0') && (*server_name2 != '\n'))
    server_name2++;
 if (*server_name2 != '\0') {
    *server_name2 = '\0';
    server_name2++;
    }

 strcpy(svr0, server_name0);
 strcpy(svr1, server_name1);
 strcpy(svr2, server_name2);

 return SUCCESS;
}
/*==============================================*
 * writen()					*
 *==============================================*/
int writen(sd, ptr, nbytes)
 int sd; char *ptr; int nbytes;			/* returns the number 	*/
{						/* of bytes actually 	*/
 int    	nleft,				/* actually written to  */
		nwritten,			/* to the given socket	*/
		status,
		count = 0;
 fd_set		writefds;
 struct timeval timeout;

 nleft = nbytes;
 FD_ZERO(&writefds);
 FD_SET(sd, &writefds);
 memset ((char *) &timeout, '\0',
	sizeof (struct timeval));
 timeout.tv_sec = (long) 5;

 while (nleft > 0) {
    status = select (sd+1, (fd_set *) 0, (fd_set *) &writefds,
	(fd_set *) 0, (struct timeval *) &timeout);
    if (status > 0) {
       nwritten = write(sd, ptr, nleft);
       if (nwritten <= 0) return FAILURE;
       nleft -= nwritten;
       ptr += nwritten;
       }
    else if (status == 0)
       count ++;
    else if (status < 0)
       return FAILURE;
    }
 if (nleft == 0)
    return (nbytes);
 else
    return FAILURE;
}
/*==============================================*
 * InitBufUnion(): initialize data structure	*
 *	buf_union_typ with '\0'.		*
 *==============================================*/
int InitBufUnion(data)
 buf_union_typ *data;
{
 memset((char *) data, '\0', sizeof(buf_union_typ));
 return SUCCESS;
}
/*==============================================*
 * InitKernelDb(): initialize data structure  *
 *      init_kernel_db with '\0'.               *
 *==============================================*/
int InitKernelDb(data)
 kernel_db_typ *data;
{
 memset((char *) data, '\0', sizeof(kernel_db_typ));
 return SUCCESS;
}
/*==============================================*
 * InitBegin(): initialize data structure  	*
 *      begin_typ with '\0'.                	*
 *==============================================*/
int InitBegin(data)				/* initialize data  */
 begin_typ *data;				/* struct begin_typ */
{
 memset((char *) data->sclk_begin, '\0', 18);
 return SUCCESS;
}
/*==============================================*
 * InitEnd(): initialize data structure  	*
 *      end_typ with '\0'. 	               	*
 *==============================================*/
int InitEnd(data)				/* initialize data  */
 end_typ *data;					/* struct end_typ   */
{
 memset((char *) data->sclk_end, '\0', 18);
 return SUCCESS;
}
/*==============================================*
 * InitUsrKdb(): initialize data structure  	*
 *      init_usr_kdb with '\0'.                	*
 *==============================================*/
int InitUsrKdb(data)				/* initialize data  */
 usr_kdb_typ *data;				/* structure	    */
{						/* usr_kdb_typ	    */
 memset((char *) data, '\0', sizeof(usr_kdb_typ));
 return SUCCESS;
}
/*-----------------------------------------------
 * Modified C Bridges: These are C-bridges to	|
 * fortran subroutines. They are in addition to |
 * those defined in "spbri.com", but the two	|
 * dimensional array are transposed. I changed	|
 * the bridges in spbri.com directly, but this	|
 * would kill Lucas' program....Thus, leave the |
 * subroutines in spbri.com and make new bridge |
 * here						|
 *----------------------------------------------*/
/*
1st bridge for IRFROT, called from C
*/
void zirfrotSPICE( refa, refb, rotab )
int    refa;  /*input*/
int    refb;  /*input*/
void *rotab;  /*output*/

{
 int            i, j;
 double         *dptr, trotab[3][3];
 FTN_NAME2(irfrot, IRFROT) ( &refa, &refb, trotab );
 dptr = rotab;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dptr = trotab[j][i];
       dptr++;
       }
}

/*
1st bridge for MXM, called from C
*/
void zmxmSPICE( m1, m2, mout )
void *m1;   /*input*/
void *m2;   /*input*/
void *mout;   /*output*/


{
 int    i, j;
 double *dbl_ptr1,
        *dbl_ptr2,
        tm1[3][3],
        tm2[3][3],
        tmout[3][3];

 dbl_ptr1 = m1;
 dbl_ptr2 = m2;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       tm1[j][i] = *dbl_ptr1;
       tm2[j][i] = *dbl_ptr2;
       dbl_ptr1++;
       dbl_ptr2++;
       }
 FTN_NAME2(mxm, MXM) ( tm1, tm2, tmout );
 dbl_ptr1 = mout;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dbl_ptr1 = tmout[j][i];
       dbl_ptr1++;
       }
}

/*
1st bridge for MXMT, called from C
*/
void zmxmtSPICE( m1, m2, mout )
void *m1;               /*input*/
void *m2;               /*input*/
void *mout;             /*output*/


{
 int    i, j;
 double *dbl_ptr1, *dbl_ptr2,
        tm1[3][3], tm2[3][3],
        tmout[3][3];

 dbl_ptr1 = m1;
 dbl_ptr2 = m2;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       tm1[j][i] = *dbl_ptr1;
       tm2[j][i] = *dbl_ptr2;
       dbl_ptr1++;
       dbl_ptr2++;
       }

 FTN_NAME2(mxmt, MXMT) ( tm1, tm2, tmout );

 dbl_ptr1 = mout;
 for(i = 0; i < 3; i++) {
    for(j = 0; j < 3; j++) {
       *dbl_ptr1 = tmout[j][i];
       dbl_ptr1++;
       }
    }
}

/*
1st bridge for MXV, called from C
*/
void zmxvSPICE( matrix, vin, vout )
void  *matrix;          /*input*/
void  *vin;             /*input*/
void  *vout;            /*output*/

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
 FTN_NAME2(mxv, MXV) ( tmatrix, vin, vout);
}

/*
1st bridge for MTXV, called from C
*/
void zmtxvSPICE( matrix, vin, vout )
void *matrix;           /*input*/
void  *vin;             /*input*/
void  *vout;            /*output*/

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
 FTN_NAME2(mtxv, MTXV) ( tmatrix, vin, vout);
}

/*
Bridge for XPOSE, called from C
*/
void zxposeSPICE( m1, mout )
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
 FTN_NAME2(xpose, XPOSE) ( tm1, tmout );
 dbl_ptr = mout;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dbl_ptr = tmout[j][i];
       dbl_ptr++;
       }
}

void zeul2mSPICE(angle3, angle2, angle1, axis3, axis2, axis1, matrix)
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

 FTN_NAME2(eul2m, EUL2M) (&angle3, &angle2, &angle1,
                &axis3, &axis2, &axis1, tmatrix);
 dptr = matrix;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dptr = tmatrix[j][i];
       dptr++;
       }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create getspice95.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getspice95.imake
/* Imake file for VICAR subroutine GETSPICE95 */

#define SUBROUTINE   getspice95

#define MODULE_LIST  spicesubs95.c getspice95.c

#define FTN_STRING
#define USES_FORTRAN
#define USES_ANSI_C

#define P2_SUBLIB
#define LIB_NETWORK

/*#define DEBUG		/* Remove before delivery. */
/*#define LIB_LOCAL	/* Remove before delivery. */
$ Return
$!#############################################################################
$Test_File:
$ create tgetspice95.f
C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                    GETSPICE95
C
C
C Change:  MPB  8/5/99
C       Created the string proj_name.  Previously, calls to 
C       getproj() and getlabcon() were using 
C       the integer 'project' as a parameter, 
C       which was causing them to fail in IRIX.
C
C********************************************************
C
	include 'VICMAIN_FOR'
	subroutine main44

        character*6 proj_name
	integer*4 mode
	character*80 imgfile
	character*10 source
	integer*4 project
	character*10 usr_info(10)
	double precision dbuf(100)
	integer*4 system
	integer*4 scet(6)
	integer*4 ibuf(200)
	integer*4 ind, count, def
	equivalence (dbuf, ibuf)
	character*5 camera
	character*12 target_name
	character*30 temp
	integer*4 data(80)

C	Selecting mode to tell getspice95()
C	where to get the data
	print *, 'Input mode (local = 0, remote = 1)'
	read *, mode

	call xvunit(unit, 'INP', 1, status, ' ')
	call xvopen(unit, status, 'OPEN_ACT', 'SA', ' ')
	call getproj(unit, proj_name, cam, fds, ind)
	call getlabcon(unit, proj_name, data, ind)

	call mve(4, 6, data(8), scet, 1, 1)
	call mvlc(data(25), target_name, 12)

        call xvparm('INP', imgfile, count, def, 0)
        call xvparm('SOURCE', source, count, def, 0)
	call xvparm('PROJECT', project, count, def, 0)
	call xvparm('CAMERA', camera, count, def, 0)
	call xvparm('TARGET', temp, count, def, 0)

	if (def .EQ. 0) then
	   target_name=temp
	endif

	call xvparm('INSTITUTE', usr_info(1), count, def, 0)
	call xvparm('PURPOSE', usr_info(2), count, def, 0)
	call xvparm('PROG_NAME', usr_info(3), count, def, 0)
	call xvparm('SP_REF', usr_info(4), count, def, 0)
	call xvparm('REQ_NO', usr_info(5), count, def, 0)
	call xvparm('YEAR', usr_info(6), count, def, 0)
	call xvparm('MONTH_DAY', usr_info(7), count, def, 0)
	call xvparm('HOUR_MIN', usr_info(8), count, def, 0)
	call xvparm('FILE_ID', usr_info(9), count, def, 0)
	call xvparm('USR_GRP_ID', usr_info(10), count, def, 0)

	if (project .eq. -77) then
	   system = 2
	else
	   system = 1
	endif

	call xvmessage(' ', ' ')
	call xvmessage('....................................', ' ')
        if (mode .eq. 0) then
           call xvmessage('GETSPICE95::MODE_LOCAL Fortran Test', ' ')
        else if (mode .eq. 1) then
	   call xvmessage('GETSPICE95::MODE_REMOTE Fortran Test', ' ')
	endif

	call getspice95(mode, project, camera, scet, target_name,
	1	system, source, usr_info, dbuf, ind)

	if (ind .eq. 0) then
	   call xvmessage('ZGETSPICE95::Test From FORTRAN:FAILURE', ' ')
	else
	   call xvmessage('ZGETSPICE95::Test From FORTRAN:SUCCESS', ' ')
	   temp = ' '
	   call mvlc(ibuf(2), temp, 4)
	   print*, 'Instrument                    : ', temp
	   print*, 'measurement time year         : ', ibuf(3)
	   print*, 'measurement time day          : ', ibuf(4)
	   print*, 'measurement time hour         : ', ibuf(5)
	   print*, 'measurement time minute       : ', ibuf(6)
	   print*, 'measurement time second       : ', ibuf(7)
	   print*, 'measurement time millisec     : ', ibuf(8)
	   print*, 'Target Body Code (target_id)  : ', ibuf(9)
	   
	   print*, ibuf(10)

	   if (ibuf(10) .eq. 2) then
	      print*, 'Coordinate System             : B1950'
	   else if (ibuf(10) .eq. 1) then
	      print*, 'Coordinate System             : J2000'
	   endif
	   print*, ' '
	   print*, 'XYZ of SC relative central body: '
	   print*, '       ', dbuf(16), dbuf(17), dbuf(18)
	   print*, '           '
	   print*, 'XYZ of picture body relative to SC: '
	   print*, '       ', dbuf(19), dbuf(20), dbuf(21)
	   print*, ' '
	   print*, 'SC range from sun           : ', dbuf(25)
	   print*, 'SC range from central body  : ', dbuf(26)
	   print*, 'SC range from picture body  : ', dbuf(27)
	   print*, ' '
	   print*, 'lat & lon of sun rel pic body: '
	   print*, '        ', dbuf(28), dbuf(29)
	   print*, ' '
	   print*, 'lat & lon of SC rel pic body : '
	   print*, '        ', dbuf(30), dbuf(31)
	   print*, ' '
	   print*, 'C_MATRIX: '
	   print*, dbuf(41), dbuf(42), dbuf(43)
	   print*, dbuf(44), dbuf(45), dbuf(46)
	   print*, dbuf(47), dbuf(48), dbuf(49)
	   print*, ' '
	   print*, 'lat & lon at P5 point: ', dbuf(77), dbuf(78)
	   print*, ' '
	   print*, 'incidence angle at P5 point: ', dbuf(79)
	   print*, 'emission angle at P5 point : ', dbuf(80)
	   print*, 'phase angle at P5 point    : ', dbuf(81)
	   print*, 'Hor & Vert pix size at P5  : ', dbuf(82), dbuf(83)
	   print*, '           '
	   print*, 'ME_MATRIX: '
	   print*, dbuf(50), dbuf(51), dbuf(52)
	   print*, dbuf(53), dbuf(54), dbuf(55)
	   print*, dbuf(56), dbuf(57), dbuf(58)
	   print*, '            '
	   print*, 'SC range to P5:	', dbuf(84)
	   print*, 'North Angle   :     ', dbuf(68)
	   print*, '            '
	   print*, 'Picture body equat radius, long : ', dbuf(13)
	   print*, 'Picture body equat radius, short: ', dbuf(14)
	   print*, 'Picture body equat radius       : ', dbuf(15)
	   print*, ' '
	   print*, 'OM_MATRIX: '
	   print*, dbuf(59), dbuf(60), dbuf(61)
	   print*, dbuf(62), dbuf(63), dbuf(64)
	   print*, dbuf(65), dbuf(66), dbuf(67)
	   print*, ' '
	   print*, 'RS-Vector: ', dbuf(22), dbuf(23), dbuf(24)
	   print*, 'line   sub-sc-point   : ', dbuf(69)
	   print*, 'sample sub-sc-point   : ', dbuf(70)

	   temp = '               '
	   call mvlc(ibuf(189), temp, 4)
	   print*, 'Institute          : ', temp
	   temp = '               '
	   call mvlc(ibuf(169), temp, 4)
	   print*, 'year               : ', temp
	   temp = '               '
	   call mvlc(ibuf(170), temp, 4)
	   print*, 'month-day          : ', temp
           temp = '               '
	   call mvlc(ibuf(171), temp, 4)
	   print*, 'hour-min           : ', temp
           temp = '               '
	   call mvlc(ibuf(14), temp, 4)
	   print*, 'SPK_ref            : ', temp
	   temp = '               '
	   call mvlc(ibuf(172), temp, 4)
	   print*, 'CK_ref             : ', temp
           temp = '               '
	   call mvlc(ibuf(173), temp, 4)
	   print*, 'purpose            : ', temp
           temp = '               '
	   call mvlc(ibuf(174), temp, 6)
	   print*, 'prog_name          : ', temp
           temp = '               '
	   call mvlc(ibuf(176) ,temp, 4)
	   print*, 'job_req_no,        : ', temp
           temp = '               '
	   call mvlc(ibuf(177), temp, 6)
	   print*, 'usr_grp_id         : ', temp
	endif

	call xvmessage('             ', ' ')
	call xvmessage( '..................................', ' ')
	call xvmessage('GETSPICE95::Test From C....', ' ')
        call tzgetspice95(mode, project, camera, scet, target_name,
	1	system, source, usr_info, dbuf, ind)

        if (ind .eq. 1) then
	   call xvmessage('GETSPICE95::Test SUCCESS', ' ')
	else
	   call xvmessage('Fatal Indicator::Test Failed', ' ')
	endif

	return
	end
$!-----------------------------------------------------------------------------
$ create tzgetspice95.c
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spc.h"
#include "spiceinc.h"

/************************************************************************/
/* Test Program For FORTRAN AND C Callable Subroutine GETSPICE95.F      */
/************************************************************************/

void FTN_NAME(tzgetspice95)(int *mode, int *sc_id, char *camera, int * scet,
		char *target_name, int *system, char *source,
		void *usr_info[10], void *buf, int *ind, ZFORSTR_PARAM)
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

 if (*ind == SUCCESS) {
    printf("Test From C::SUCCESS\n");
    printBuffer(&buffer);
    memcpy((char *) buf, (char *) &buffer,
		sizeof(buf_union_typ));
    }
 else
    printf("Test From C::FAILURE\n"); 
}
/*========================================================*
 * int printBuffer(): Print the content of the MIPS	  *
 *			buffer				  *
 *========================================================*/
int printBuffer(buf) 
 buf_union_typ *buf;
{
 char str[200];

 printf("SC_ID                          : %d\n", buf->intbuf[0]);

 memcpy((char *) str, (char *) &buf->intbuf[1], 4);
 str[4] = '\0';
 printf("Instrument                     : %s\n", str);

 printf("measurement time year          : %d\n", buf->intbuf[2]);
 printf("measurement time day           : %d\n", buf->intbuf[3]);
 printf("measurement time hour          : %d\n", buf->intbuf[4]);
 printf("measurement time minute        : %d\n", buf->intbuf[5]);
 printf("measurement time second        : %d\n", buf->intbuf[6]);
 printf("measurement time millisec      : %d\n", buf->intbuf[7]);
 printf("Target body code (target_id)   : %d\n\n", buf->intbuf[8]);

 if (buf->intbuf[9] == 1)
    strcpy(str, "J2000");
 else if (buf->intbuf[9] == 2)
    strcpy(str, "B1950");
 printf("Coordinate System              : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[10], 4);
 str[4] = '\0';
 printf("Source file to search          : %s\n\n", str);

 printf("XYZ of SC relative central body: %g %g %g\n\n",
                        buf->doublebuf[15],
                        buf->doublebuf[16],
                        buf->doublebuf[17]);
 printf("XYZ of picture body relative SC: %g %g %g\n\n",
                        buf->doublebuf[18],
                        buf->doublebuf[19],
                        buf->doublebuf[20]);
 printf("SC range from sun              : %g\n", buf->doublebuf[24]);
 printf("SC range from central body     : %g\n", buf->doublebuf[25]);
 printf("SC range from picture body     : %g\n\n", buf->doublebuf[26]);

 printf("lat & lon of sun rel pic body : %g %g\n",
                buf->doublebuf[27], buf->doublebuf[28]);
 printf("lat & lon of sc rel pic body  : %g %g\n\n",
                buf->doublebuf[29], buf->doublebuf[30]);

 printf("C_MATRIX:\n%g %g %g\n%g %g %g\n%g %g %g\n\n",
        buf->doublebuf[40], buf->doublebuf[41], buf->doublebuf[42],
        buf->doublebuf[43], buf->doublebuf[44], buf->doublebuf[45],
        buf->doublebuf[46], buf->doublebuf[47], buf->doublebuf[48]);

 printf("lat & lon of P5 point: %g %g\n\n",
        buf->doublebuf[76], buf->doublebuf[77]);
 printf("incidence angle at P5 point: %g\n",
                        buf->doublebuf[78]);
 printf("emission  angle at P5 point: %g\n",
                        buf->doublebuf[79]);
 printf("phase     angle at P5 point: %g\n",
                        buf->doublebuf[80]);
 printf("Hor & Vert pix size at P5: %g %g\n\n",
        buf->doublebuf[81], buf->doublebuf[82]);
 printf("ME_MATRIX:\n%g %g %g\n%g %g %g\n%g %g %g\n\n",
        buf->doublebuf[49], buf->doublebuf[50], buf->doublebuf[51],
        buf->doublebuf[52], buf->doublebuf[53], buf->doublebuf[54],
        buf->doublebuf[55], buf->doublebuf[56], buf->doublebuf[57]);
 printf("SC Range to P5 : %g\n", buf->doublebuf[83]);
 printf("North Angle    : %g\n\n", buf->doublebuf[67]);

 printf("Picture body equat radius, long : %g\n", buf->doublebuf[12]);
 printf("Picture body equat radius, short: %g\n", buf->doublebuf[13]);
 printf("Picture body polar radius       : %g\n\n", buf->doublebuf[14]);
 printf("OM_MATRIX:\n%g %g %g\n%g %g %g\n%g %g %g\n\n",
        buf->doublebuf[58], buf->doublebuf[59], buf->doublebuf[60],
        buf->doublebuf[61], buf->doublebuf[62], buf->doublebuf[63],
        buf->doublebuf[64], buf->doublebuf[65], buf->doublebuf[66]);

 printf("RS-Vector: %g %g %g\n", buf->doublebuf[21],
                buf->doublebuf[22], buf->doublebuf[23]);

 printf("line   sub-s/c-point: %g\n", buf->doublebuf[68]);
 printf("sampel sub-s/c-point: %g\n", buf->doublebuf[69]);

 printf("\n******** Provenance Information *******\n");
 memcpy((char*) str, (char*) &buf->intbuf[188], 4);
 str[4] = '\0';
 printf("Institute             : %s\n", str);
 memcpy((char *) str, (char *)&buf->intbuf[168], 4);
 str[4] = '\0';
 printf("Year                  : %s\n", str);
 memcpy((char *) str, (char *) &buf->intbuf[169], 4);
 str[4] = '\0';
 printf("Month_day             : %s\n", str);
 memcpy((char *) str, (char *) &buf->intbuf[170], 4);
 str[4] = '\0';
 printf("Hour_min              : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[13], 4);
 str[4] = '\0';
 printf("SPK_ref               : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[171], 4);
 str[4] = '\0';
 printf("CK_ref                : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[172], 4);
 str[4] = '\0';
 printf("Purpose               : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[173], 7);
 str[7] = '\0';
 printf("Prog_name             : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[175], 4);
 str[4] = '\0';
 printf("Job_req_no            : %s\n", str);

 memcpy((char *) str, (char *) &buf->intbuf[176], 6);
 str[6] = '\0';
 printf("Usr_grp_id            : %s\n", str);
 return SUCCESS;
}
$!-----------------------------------------------------------------------------
$ create tgetspice95.imake
/* Imake file for Test of VICAR subroutine getspice95 */

#define PROGRAM tgetspice95

#define MODULE_LIST tgetspice95.f tzgetspice95.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_ANSI_C

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
$ create tgetspice95.pdf
!PDF file for VICAR test program TGETSPICE95
!tgetspice95.pdf		6/15/1994

process help=* option=nointerrupt 
parm inp        type=string count=1
parm source     type=string count=1
parm project    type=integer count=1
parm camera     type=string count=1 default=SSI
parm target     type=string count=1 default=""
parm institute  type=string count=1 default=MIPS
parm purpose    type=string count=1 default=NONE
parm prog_name  type=string count=1 default=*NONE*
parm sp_ref     type=string count=1 default=NONE
parm req_no     type=string count=1 default=NONE
parm year       type=string count=1 default=0000
parm month_day  type=string count=1 default=0000
parm hour_min   type=string count=1 default=0000
parm file_id    type=string count=1 default=NONE
parm usr_grp_id type=string count=1 default=*NONE*
end-proc
$!-----------------------------------------------------------------------------
$ create tstgetspice95.pdf
!*****************************************************************************
! tstgetspice.pdf - unit test for getspice95
!
! Testers: please read the unit test for information!
! ftp from the vax into your local directory the test file venus.img
! located in mipldisk:[mipl.gll]
! (don't foget to say binary )
! make sure that the spiceserver is up and running.....
! If it's not ask Pam Woncik or Sam Le to start it
! get into VICAR and run tstgetspice.
! Getspice will ask you for the mode to use. Answer 0 for local access or
!  1 for remote server access. You can test remote on all platforms but
!  local only where the spice kernels exist, as on sun-solr.
!*****************************************************************************
procedure help=*
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let  $autousage="none"

local path1 string
local path2 string

if ($syschar(1) = "VAX_VMS")
   let path1="wms_test_work:[testdata.mipl.gll]"
   let path2="wms_test_work:[testdata.mipl.vgr]"
else
   let path1="/project/test_work/testdata/mipl/gll/"
   let path2="/project/test_work/testdata/mipl/vgr/"
end-if

!**************
!testonly
!**************


!  Tests for UNIX only (not VMS).

if ($syschar(1) = "VAX_VMS")
else

write "Testing CAS getspice95"
tgetspice95 inp=/project/test_work/testdata/mipl/cas/n1373703343.1 source=FARE project=-82 camera=ISSN

end-if


! Tests for UNIX and VMS 

write ""
write ""
write ""
write "Testing VGR getspice95"
tgetspice95 inp=&"path2"f1636832.fic source=NEAR project=-31 +
   target=IO camera=ISSN

write ""
write ""
write ""
write "Testing GLL getspice95"
tgetspice95 inp=&"path1"venus.img source=FARE project=-77


end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getspice95.hlp
1 GETSPICE
 
  Description:

	VICAR2 subroutine GETSPICE95 to return the spice navigation 
	data for an image from GLL. It can (will) be expanded to include
	other flight project. But for now, only GLL is supported because
	data from other project are not available.

	Users should be aware that these routines clear the SPICE kernel 
	pool before returning, so subsequent calls to SPICE routines will 
	fail unless INIT_SPICE() or an equivalent routine is called.


  FORTRAN calling sequence:
	integer*4	mode
	integer*4	sc_id
	character*5	camera
	integer*4	scet(6)
	character*12	target_name
	integer*4	system
	character*4	source
	character*10	usr_info(10)
	real*8		buf
	integer*4	ind
	call getspice(MODE,SC_ID,CAMERA,SCET, TARGET_NAME,SYSTEM,SOURCE,USR_INFO,buf,ind)

  C calling sequence:
        #include "xvmainc.h"
	#include "ftnbridge.h"
	#include "spc.h"
	#include "spiceinc.h"

	int		mode;
	int		sc_id;
	char		*camera;
	int		*scet;
	char		*target_name;
	int		system;
	char		*src;
	prov_info_typ	*usr_info;
	buf_union_typ	*buf;
	zgetspice(mode,sc_id,camera,scet,target_name,system,src,usr_info,buf);

	*** for C calling sequence, getspice95() returns an
		integer as its indicator code.
2 ARGUMENTS

  INPUT ARGUMENTS:
     mode     = 0 use local spice kernels
	      = 1 use MIPS spice kernel via "spiceserver"

     sc_id    = GLL VGR_1 VGR_2
     camera   = SSI ...
     scet     = year, date, hour, min, sec, mili-sec.
		of the data seg you want to retrieve.
		These info can be retrieved using
		getlabcon, they are in data(8-14),
		where data is the buffer returned by getlabcon
		
     target_name = name of the target object, getlabcon will
		return this for you. The target should be in
		the data[24] in C or data(25) in fortran, where
		data is the buffer returned by getlabcon
 
     system   = B1950 or J2000

     source   = SEDR source                                     character*4
                Valid are: DAVI NAV FARE NAV2 NEAR AMOS NTEL AACS NPRE
 		Notice that the source string are all in upper case. If
		you supply lower cased string, it will automatically
		converted to upper case, before sending the request
		to SpiceServer or before calling spice95.
 
     usr_info = provenance information used to select data
		from MIPS spice kernel. When calling getspice95,
		user can be more specific in the type of data
		they are looking for, by specifying appropriate
		values in usr_info structure

		fortran format: character*10 usr_info(10)
		C       format: struct prov_info_typ {
					char inst[5],
					     purpose[5],
					     prog_name[7],
					     sp_ref[5],
					     req_no[5],
					     year[5],
					     month_day[5],
					     hour_min[5],
					     file_id[5],
					     usr_grp_id[7];
					     }
		FORTRAN   	C
		usr_info(1)	inst[5]
			- specifies where the data segment comes from.
			  So far "NAIF" and "MIPS" are valid because
			  MIPS kernel only contain data from MIPS and
			  NAIF. As more user add data to MIPS kernel,
			  more inst values will become available to select

		usr_info(2)	purpose[5]
			- specifies the reason the data segment was produced,
			  what is it for, etc.
			  A user reading data from MIPS kernel might chose to
			  to read data from specific purpose (e.g. LIMB NAVIG.,
			  C-SMITHING, etc.).
			  default: "NONE"

		usr_info(3)	prog_name[7]
			- name of program that produce the data segment
			  default: "*NONE*"

		usr_info(4)	sp_ref[5]
			- id number of SPK file which was used to produce
			  the data segment. SPK id number are defined in
			  MIPS KERNELDB
			  default: "NONE"

		usr_info(5)	req_no[5]
			- request or job id of the process which produces
			  the data segment.
			  default: "NONE"

		usr_info(6)	year[5]
			- year that the data segment was produced
			  default: "0000"

		usr_info(7)	month_day[5]
			- month and day that the data segment was produced
			  default: "0000"

		usr_info(8)	hour_min[5]
			- hour and minute that the data segment was produced
			  default: "0000"

		usr_info(9)	file_id[5]
			- MIPS kernel file id the user want to read data from.
			  This is useful for users who want to read data only
			  from a specific kernel
			  default: "NONE"

		usr_info(10)	usr_grp_id[7]		
			- if user only want data from a specific group or
			  user, then specify the user or group name here.
			  default: "*NONE*"

		*** it might take longer to retrieve data when calling getspice95()
		    with provenance information. This is because getspice95() search
		    throught the whole MIPS kernel to retrieve the requested segment.
		    If the requested segment is not found, the latest data segment
		    which has the same sclk value with the specified image is returned.

		*** if the user does not specify any provenance information,
		    getspice95() ignores usr_info and retrieves the latest
		    data segment with the same sclk value with the specified image.

		*** items in usr_info are all in character format. This is because
		    each CK data segment id can only be 40 character long. There
		    won't be enough from in the segment id to pack non-character
		    provenance information.

		*** for C calling sequence, getspice95() returns an
                    integer as its indicator code.

		*** getspice95() assumes the input buffer is empty. It calls
		    getlabcon() to fill in the SCET data, then call spice95()


  OUTPUT ARGUMENTS:
  
     buf     = 100 word buffer containing SEDR data            real*8

     ind     = status of the call                              integer*4
	       status values are as follows:
		  1 = Success
		  0 = Failure


  BUF contents: The following table provides the index, description and 
		datatype.  (All angles are in degrees, all ranges in KM.)

   .------------- < is a 4 byte word address
  /
  | 
  | I  .--------- < is an 8 byte word address
  | n /
  | d |
  | e |
  | x |            Description                                               |Type|
 -V --V----------------------------------------------------------------------------
 |1         - |  Spacecraft ID (as defined by NAIF)                             |I|
 |            |                                                                 | |
 |            |               Galileo        -77                                | |
 |            |               Voyager 1      -31                                | |
 |            |               Voyager 2      -32                                | |
 |            |               Cassini        -82                                | |
 |            |                                                                 | |
 |	      |  -Getspice95:
 |            |  -Putspice95: This field is not valid upon entering	     	| |
 |            |		putspice95(). On return, it contains SC ID of 	     	| |
 |            |		requested SPICE data segment (-31, -32, -77, etc.)   	| |
 |            |							     		| |
 |2         - |  Instrument (ISSN = ISSNA = 1, ISSW = ISSWA = 1, SSI)       	|C|
 |            |                                                                 | |
 |            |                    Cassini instruments                          | |
 |            |                                                                 | |
 |            |       Value  Instrument  Summation  MIPL ID  NAIF ID            | |
 |            |                            Mode                                 | |
 |            |                                                                 | |
 |            |       ISSN    ISSNA        None        1     -82360             | |
 |            |       ISN2    ISSNA        2x2        21     -82360             | |
 |            |       ISN4    ISSNA        4x4        41     -82360             | |
 |            |       ISSW    ISSWA        None        2     -82361             | |
 |            |       ISW2    ISSWA        2x2        22     -82361             | |
 |            |       ISW4    ISSWA        4x4        42     -82361             | |
 |            |                                                                 | |
 |            |                                                                 | |
 |            |  -Getspice95:
 |            |  -Putspice95: This is used by c95() to calculate coded      	| |
 |            |		instrument number when writing data segment to CK    	| |
 |            |		file, which is stored in the first integer component 	| |
 |            |		of the data segment, icd[0].			     	| |
 |            |		icd[0] = -(abs(SC ID) * 1000 + instrument)	    	| |
 |            |		If user does not specify (ISSN or ISSW) putspice95() 	| |
 |            |		will assign instrument = PLATFORM = 1		     	| |
 |            |							     		| |
 |3         - |  SCET - Measurement time (Years AD)                         	|I|
 |4         - |  SCET - Measurement time (Day of year)                      	|I|
 |5         - |  SCET - Measurement time (Hour of day)                      	|I|
 |6         - |  SCET - Measurement time (Minute of hour)                   	|I|
 |7         - |  SCET - Measurement time (Second of minute)                 	|I|
 |8         - |  SCET - Measurement time (Millisecond of second)            	|I|
 |            |                                                                 | |
 |            |  SCET DATA are used by both getspice95() and putspice95()	| |
 |            |	 -Getspice95:					     		| |
 |            |	 -Putspice95: c95() concatenate the SCET values and  		| |
 |            |		produce the SCLK value for the data segment  		| |
 |            |		The SCLK value will be written along with    		| |
 |            |		the C-MATRIX and its angular velocity	     		| |
 |     	      |							     		| |
 |9         - |  Target body code (PSS - P=planet #, SS=Satellite #)        	|I|
 |            |  -Getspice95:							| |
 |            |  -Putspice95:							| |
 |            |                                                                 | |
 |10        - |  System                                                     	|I|
 |            |  -Getspice95:						     	| |
 |     	      |  -Putspice95: if system=B1950, putspice95() rotates the       	| |
 |	      |		C-matrix and angular vel. data to J2000.		| |
 |	      |		Data written to CK files are in J2000 ref. system	| | 
 |            |							    		| | 
 |11        - |  Source of C,ME,OM matrices and RS vector                   	|C|
 |            |  valid: DAVI,NAV,FARE,NAV2,NEAR,AMOS,NAIF                       | |
 |            |  -Getspice95: required						| |
 |            |  -Putspice95: required						| |
 |            |                                                                 | |
 |12        - |  VGR Fds or GLL Sclk availability TBD (Not used)	     	|I|
 |13        - |  SPICE update date availablility TBD (Not used)	     		|I|
 |            |							     		| |
 |14        - |  SPK kernel id used in creating buffer (SPK source)         	|I|
 |            |  -Getspice95: this field is not valid upon entering	     	| |
 |            |		getspice95(). After exiting getspice95(), it contains	| |
 |            |		the id number of the SPK file which has the data     	| |
 |            |		used to calculate the ME (50-58) and ME (59-67)	     	| |
 |            |		matrix and other data (e.g. radii, range, etc)	     	| |
 |            |  -Putspice95: valid upon enterring putspice95(). It's part  	| |
 |            |		of provenance information. If not specified by user  	| |
 |     	      |		its default value ("NONE") will be stored in segid.  	| |
 |     	      |							     		| |
 |15-24	    - |  SPK file name used in creating buffer (36 chars)           	|C|
 |     	      |  -Getspice95: not valid when entering getspice95(). On exit 	| |
 |     	      |		it contains the file name of SPK file which contain  	| |
 |     	      |		the data used to calculate OM & ME & other data	     	| |
 |     	      |  -Putspice95: not used			     			| |
 |     	      |							     		| |
 | -  	    13|  Target body equatorial radius, long axis                   	|D|
 | -  	    14|  Target body equatorial radius, short axis                  	|D|
 | -   	    15|  Target body polar radius                                   	|D|
 | -	 16-18|  Cartesian position of Spacecraft, central body centered    	|D|
 | -	 19-21|  Cartesian position of picture body, spacecraft centered    	|D|
*| -	 22-24|  Target body to Spacecraft vector in			     	|D|
 |     	      |  prime meridian of data [RS-vector]                         	| |
*| -  	    25|  Range from picture body center to sun                      	|D|
*| -  	    26|  Range from spacecraft to central body center               	|D|
*| -  	    27|  Range from spacecraft to picture body center               	|D|
*| -	 28-29|  Latitude and longitude of the vector from                  	|D|
 |     	      |  target-body center to the sun                              	| |
*| -	 30-31|  Latitude and longitude of the vector from		     	|D|
 |     	      |  target-bodycenter to the spacecraft                        	| |
 | -	 32-37|  Spare	                                                      	|D|
 | -     38-40|  Camera angular velocity vector in inertial coordinates		|D|
*| -	 41-49|  Transformation from camera coordinate                      	|D|
 |     	      |  system to B1950 or J2000 [C matrix]                        	| |
*| -	 50-58|  Transformation matrix between picture body equator of date 	|D|
 |            |  at measurement time and B1950 or J2000 [XME matrix]        	| |
*| -	 59-67|  OM matrix = (inv) [C matrix] * [ME matrix]                 	|D|
*| -  	    68|  Angle between P5-P6 vector and target body spin axis vector	|D|
 |	      |  measured in the clockwise direction in the image plane     	| |
*| -        69|  Line coordinate of sub spacecraft point                    	|D|
*| -  	    70|  Sample coordinate of sub spacecraft point                  	|D|
 | -	 71-76|  Spare                                                      	|D|
#| -	 77-78|  Planetocentric latitude and longitude of P5 intercept point	|D|
#| -  	    79|  Incidence angle at the P5 point                            	|D|
#| -  	    80|  Emission angle at the P5 point                             	|D|
#| -  	    81|  Phase angle at the P5 point                                	|D|
#| -	 82-83|  Horizontal and vertical pixel size at P5 point (km/pix)    	|D|
#| -  	    84|  Range from spacecraft to P5 intercept point                	|D|
 |            |  							        | |
 |	      |  (double 13-84) used by both getspice95 and putspice95		| |
 |	      |  -Getspice95: upon entering getspice95() these field's content  | |
 |	      |		are ignored. Getspice95() copies retrieved data into	| |
 |            |		them on the way out					| |
 |	      |  -Putspice95: ignored and doesn't touch most of these fields	| |
 |	      |		except (38-49). (38-40) must contain the angular vel.	| |
 |	      |		information on the way in, and (41-49) must have the	| |
 |	      |		C-matrix information.					| |
 |            |                                                                 | |
 |169       - |  year that update data (putspice) was produced              	|C|
 |170       - |  month and date that data was produced                      	|C|
 |171       - |  hour & min data was produced                               	|C|
 |	      | (169-171) provenance information				| |
 |	      |  -Getspice95: date & time the CK retrieved data segment was	| |
 |	      |		produced. If the data data segment has no provenance 	| |
 |	      |		information, the default value, 0000000000,		| |
 |	      |		is returned. Upon entering getspice95(), this   	| |
 |	      |		field is ignored. When a CK segment is found		| |
 |	      |		getspice95() copies the information over		| |
 |	      |  -Putspice95: date & time the data segment (to be written to	| |
 |	      |		CK file) was produced.					| |
 |	      |		If user leave it blank or has default value, putspice95 | |
 |	      |			copies the current date & time to it, and the	| |
 |	      |			current date & time are stored in segid		| |
 |            |                                                                 | |
 |172       - |  id of CK file to search or update (CK source)		     	|C|
 |            |  -Getspice95: contains the CK file id number to	search.		| |
 |            |  	If contains default value ("NONE") or blank, use source | |
 |            |			field (11) to determine which file to search. 	| |
 |            |         If contains an id string, only search the file with	| |
 |            |			that id						| |
 |     	      |  	If the id string does not match with any CK in 		| |
 |            |			"SPICEKER" getspice95() return with ind=0 	| |
 |            |  	This should only be id of CK file. If the given id 	| |
 |	      |			matches	a non-CK file, ind=0 will be returned. 	| |
 |            |  	If you are not sure about which file to search just 	| |
 |            |			leave it blank or copy "NONE" to the string 	| |
 |            |  	Upon return, getspice95() copies the id-number of CK	| |
 |            |			file which contains the C-Matrix (field 41-49)	| |
 |	      |			it's returning					| |
 |	      |  -Putspice95: contain the CK file id number to update		| |
 |	      |		If contain default value ("NONE") or blank, use source  | |
 | 	      |			field (11) to determine which file to update	| |
 |	      |		if contains an id string, only update the file with 	| |
 | 	      |			the given id string				| |
 |	      |		If given id string is not a CK string or an invalid CK  | |
 |	      |			string, putspice95 return ind=0			| |
 |            |                                                                 | |
 |173       - |  purpose of data, why was data produced & archived          	|C|
 |            |  -We have yet to decide which are valid and invalid purpose 	| |
 |            |  string. Users should be consistent with the values they    	| |
 |     	      |  chose. Purpose string length should be = 4		     	| |
 |174-75    - |  program name that produced the data, only 6 chars are used 	|C|
 |176       - |  request or job number                                      	|C|
 |177-78    - | user/group id of data owner, only 6 chars are used         	|C|
 |179-81    - | Target body name                                           	|C|
 |182-88    - | Name of file to search or update (56 characters)           	|C|
 |189	    - | Name of institution which produced data. Up to this point	|C|
 |	      | (4/8/96), NAIF and MIPS are the two know institutions which	|C|
 |	      | produce CK segments						|C|
 | -    96-100| Spare                                                      	|D|
 ----------------------------------------------------------------------------------
		* -- calculated internally
		# -- calculated internally, except that the field is set to
			-999.0 if the P5 point does not intersect the target
                I -- Field is in integer format
                D -- Field is in double precision format
                C -- Field is an ASCII string

3. HISTORY

Written By:   S Le     JUN-19-95      

   Apr. 29, 1998  ...T.Huang...   Modified to work with the new MSPICE 
                                  subroutines.
                                  Removed packing of file_id (ck_id) into
                                  seg_id to prevent illegal memory access.
                                  Removed SPC subroutine calls.

   May 21, 1998   ...T.Huang...   Added subroutine ADD_SPACE to insert 
                                  blank space to entries: INSTITUTE, 
                                  PURPOSE, PROGRAM, SPKID, and REQNUM to 
                                  assist SEG_ID construction and search.

   Oct 22, 1998   ...T.Huang...   Modifed to return the correct status.
                                  (i.e. 1=SUCCESS, 0=FAILURE).
                                  The 'msclt' and 'mslcl' subroutines return
                                  0 for success and -1 for failure.

   Jun 03, 1999   ...T.Huang...   Obsoleted spice95.c

*** This help file is originally written by Jean Lorre
$ Return
$!#############################################################################
