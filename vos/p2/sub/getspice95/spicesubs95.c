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
