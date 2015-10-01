$!****************************************************************************
$!
$! Build proc for MIPL module istatfile
$! VPACK Version 1.9, Monday, December 07, 2009, 16:24:37
$!
$! Execute by entering:		$ @istatfile
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
$ write sys$output "*** module istatfile ***"
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
$ write sys$output "Invalid argument given to istatfile.com file -- ", primary
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
$   if F$SEARCH("istatfile.imake") .nes. ""
$   then
$      vimake istatfile
$      purge istatfile.bld
$   else
$      if F$SEARCH("istatfile.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake istatfile
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @istatfile.bld "STD"
$   else
$      @istatfile.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create istatfile.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack istatfile.com -mixed -
	-s istatfile.c -
	-i istatfile.imake -
	-t tstistatfile.pdf tistatfile.pdf tistatfile.c tistatfile_main.f -
	   tistatfile.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create istatfile.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  ibisstatfile.c
 *
 *    High-level interface to creating and reading
 *    STATS style IBIS-2 files.
 *
 *   Revision History:
 *   -----------------
 *
 *    6/13/95          New       NDR
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#define CLASS_NAME "CLASS_NAME"
#define NUMBER_OF_PIXELS "NUM_PIX"
#define NUMBER_OF_BANDS "NUM_BAND"
#define COVARIANCE "COVARIANCE"
#define MEAN "MEAN"

#define BAND_LIMIT 12
#define MAX_FILES 40

typedef struct {
    int ibis;
    int nbands;
    int nclasses;
    int classnamecol;
    int npixcol;
    int nbandcol;
    int meanrec;
    int covarrec;
} statrecord_t;

static statrecord_t stats[MAX_FILES];
static int first_time=1;

static void _InitializeStats()
{
	memset(stats,0,sizeof(stats));
	first_time=0;
}


static int _s_strcmp_nocase(str1,str2)
char *str1;
char *str2;
{
	char c1,c2;
	
	for((c1= *str1,c2= *str2); c1&&c2; (c1= *++str1,c2= *++str2) )
		if (tolower(c1)!=tolower(c2)) return 1;
	
	return (c1 || c2);
}

/*
 * Open a STATS file for reading or writing. IBIS-level stuff hidden
 *  from user. Mode is 'READ' or 'WRITE'
 */
int ISTATFileOpen(unit, mode, nclasses, nbands, inst)
int unit; 
char *mode; 
int nclasses; 
int nbands; 
char *inst;
{
	int status;
	int ibis=0;
	int ncovar=0, ncol=0; 
	int create=0;
	int col,cur_col;
	int meancols[I2_MAX_COL];
	int covarcols[I2_MAX_COL];
	char *org = (char *)0;
	char *format = (char *)0, *cptr;
	
	if (first_time) _InitializeStats();
	
	if (nbands > BAND_LIMIT) return IBIS_COLUMN_LIMIT_EXCEEDED;
	
	switch (tolower(*mode))
	{
		case 'r': break;
		case 'w':
			org = IORG_ROW;
			create=1;
			ncovar = nbands*(nbands+1)/2;
			ncol = 3 + nbands + ncovar;
			if (ncol>I2_MAX_COL) return IBIS_COLUMN_LIMIT_EXCEEDED;
			
			/* set up column formatting */
			format = (char *)malloc((size_t)IFMT_SIZE*ncol);
			cptr = format+ IFMT_SIZE*3;
			cur_col = 4;
			for (col=0;col<nbands; col++,cptr+=IFMT_SIZE)
			{
				meancols[col] = cur_col++;
				strcpy(cptr,IFMT_DOUB);
			}
			for (col=0;col<ncovar; col++,cptr+=IFMT_SIZE)
			{
				covarcols[col] = cur_col++;
				strcpy(cptr,IFMT_REAL);
			}
			strcpy(format,"A8");   /* Class Name */
			strcpy(format+IFMT_SIZE,IFMT_FULL);  /* number of pix */
			strcpy(format+2*IFMT_SIZE,IFMT_FULL); /* number of bands */

			stats[unit].classnamecol = 1;
			stats[unit].npixcol = 2;
			stats[unit].nbandcol = 3;

			break;
		default:
			/* OWRITE mode not supported */
			return IBIS_INVALID_OPEN_MODE;
			break;
	}
	
	status = IBISFileOpen( unit, &ibis, mode, ncol,nclasses, format, org );
	if (status < 0) return status;
	
	if (create)
	{
		/* create the groups */
		
		status = ICLNewSTATISTICS(ibis,&stats[unit].classnamecol,
                     1,0,0,CLASS_NAME,inst);
		if (status < 0) goto failure;
		status = ICLNewSTATISTICS(ibis,&stats[unit].npixcol,
                     1,0,0,NUMBER_OF_PIXELS,inst);
		if (status < 0) goto failure;
		status = ICLNewSTATISTICS(ibis,&stats[unit].nbandcol,
                     1,0,0,NUMBER_OF_BANDS,inst);
		if (status < 0) goto failure;
		status = ICLNewSTATISTICS(ibis,meancols,nbands,0,0,MEAN,inst);
		if (status < 0) goto failure;
		status = ICLNewROOT(ibis,covarcols,ncovar,COVARIANCE);
		status = ICLNewSTATISTICS(ibis,covarcols,ncovar,
                   0,0,COVARIANCE,inst);
		if (status < 0) goto failure;

		/* Set the subfile type */
		status = IBISFileSet( ibis, IFILE_TYPE, "STATISTICS", 0 );
		if (status < 0) goto failure;
	}
	else
	{
		/* find the groups */
		ICLGetSTATISTICS(ibis,"$MyClass",0,CLASS_NAME,inst);
		status = IBISColumnFind(ibis,"any","$MyClass",
                            &stats[unit].classnamecol,1,1);
		if (status < 0) goto failure;
		ICLGetSTATISTICS(ibis,"$MyPix",0,NUMBER_OF_PIXELS,inst);
		status = IBISColumnFind(ibis,"any","$MyPix",
                            &stats[unit].npixcol,1,1);
		if (status < 0) goto failure;
		ICLGetSTATISTICS(ibis,"$MyBand",0,NUMBER_OF_BANDS,inst);
		status = IBISColumnFind(ibis,"any","$MyBand",
                            &stats[unit].nbandcol,1,1);
		if (status < 0) goto failure;

		ICLGetSTATISTICS(ibis,"$MyMean",0,MEAN,inst);
		nbands = IBISColumnFind(ibis,"any","$MyMean",
                       meancols,1,I2_MAX_COL);
		status = (nbands < 0) ? nbands : 1;
		if (status < 0) goto failure;
		stats[unit].nbands = nbands;

		ICLGetSTATISTICS(ibis,"$MyCovar",0,COVARIANCE,inst);
		ncovar = IBISColumnFind(ibis,"any","$MyCovar",
                      covarcols,1,I2_MAX_COL);
		status = (ncovar < 0) ? nbands : 1;
		if (status < 0) goto failure;
		IBISFileGet(ibis,IFILE_NR,&stats[unit].nclasses,1,0,0);
	}
	
	/* Set up records and formatting */
	status = IBISRecordOpen( ibis, &stats[unit].covarrec, 
		0, covarcols, ncovar,IFMT_REAL);
	if (status < 0) goto failure;
	status = IBISRecordOpen( ibis, &stats[unit].meanrec, 
		0, meancols, nbands,IFMT_REAL);
	if (status < 0) goto failure;
	
	status = IBISColumnSet( ibis, ICOLUMN_U_FORMAT, "A8", 
                stats[unit].classnamecol );
	if (status < 0) goto failure;
	status = IBISColumnSet( ibis, ICOLUMN_U_FORMAT, IFMT_FULL,
                stats[unit].npixcol );
	if (status < 0) goto failure;
	status = IBISColumnSet( ibis, ICOLUMN_U_FORMAT, IFMT_FULL, 
                stats[unit].nbandcol );
	if (status < 0) goto failure;

	/* store info in structure */
	stats[unit].ibis = ibis;
	
	return status;
failure:
	if (ibis) IBISFileClose(ibis,0);
	return status;
}

void ISTATSignal(unit, status, abendflag)
int unit;
int status;
int abendflag;
{
	int ibis = stats[unit].ibis;

	if (ibis) IBISSignal(ibis,status,abendflag);
	else IBISSignalU(unit,status,abendflag);
}

int ISTATFileClose(unit)
int unit;
{
	int ibis = stats[unit].ibis;
	stats[unit].ibis = 0;
	return IBISFileClose(ibis, 0);
}

void ISTATFileInfo(unit,nclasses,nbands,ibis)
int unit;
int *nclasses;
int *nbands;
int *ibis;
{
	if (nclasses) *nclasses = stats[unit].nclasses;
	if (nbands) *nbands = stats[unit].nbands;
	if (ibis) *ibis = stats[unit].ibis;
}

int ISTATRecordWrite( unit, recnum, name, npix, nbands, means, covar)
int unit;
int recnum;
char *name;
int npix;
int nbands;
float *means;
float *covar;
{
	int status=1;
	int npix1 = npix;
	int nband = nbands;
	int ibis = stats[unit].ibis;
	
	if (name)
	{
		status =  IBISColumnWrite(ibis,name,stats[unit].classnamecol,recnum,1);
		if (status < 0) return status;
	}
	status =  IBISColumnWrite(ibis,(char *)&npix1,stats[unit].npixcol,recnum,1);
	if (status < 0) return status;
	status =  IBISColumnWrite(ibis,(char *)&nband,stats[unit].nbandcol,recnum,1);
	if (status < 0) return status;
	
	if (means)
	{
		status = IBISRecordWrite(stats[unit].meanrec,(char *)means,recnum);
		if (status < 0) return status;
	}
	if (covar)
	{
		status = IBISRecordWrite(stats[unit].covarrec,(char *)covar,recnum);
		if (status < 0) return status;
	}
	
	return status;
}


int ISTATRecordRead( unit, recnum, name, npix, nbands, means, covar)
int unit;
int recnum;
char *name;
int *npix;
int *nbands;
float *means;
float *covar;
{
	int status=1;
	int ibis = stats[unit].ibis;
	
	if (name)
	{
		status =  IBISColumnRead(ibis,name,stats[unit].classnamecol,recnum,1);
		if (status < 0) return status;
	}
	if (npix)
	{
		status =  IBISColumnRead(ibis,(char *)npix,stats[unit].npixcol,recnum,1);
		if (status < 0) return status;
	}
	if (nbands)
	{
		status =  IBISColumnRead(ibis,(char *)nbands,stats[unit].nbandcol,recnum,1);
		if (status < 0) return status;
	}	
	if (means)
	{
		status = IBISRecordRead(stats[unit].meanrec,(char *)means,recnum);
		if (status < 0) return status;
	}
	if (covar)
	{
		status = IBISRecordRead(stats[unit].covarrec,(char *)covar,recnum);
		if (status < 0) return status;
	}
	
	return status;
}

/* return the record number of the named stat class */
int ISTATRecordFind(unit, classname)
int unit;
char *classname;
{
	int status=1;
	int row;
	int ibis = stats[unit].ibis;
	int nrow = stats[unit].nclasses;
	int colnum = stats[unit].classnamecol;
	char name[9];

	for (row=1; row<=nrow; row++)
	{
		status =  IBISColumnRead(ibis,name,colnum,row,1);
		if (status < 0) return 0;
		if (!_s_strcmp_nocase(name,classname)) break;
	}
	
	return (row <= nrow) ? row : 0;
}


/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/


void FTN_NAME2_(istat_file_open, ISTAT_FILE_OPEN) ( int *unit, char *mode,
			int *nclasses,
			int *nbands, char *inst, int *status, ZFORSTR_PARAM )
{
   ZFORSTR_BLOCK
   char c_mode[8],c_instance[33];
   char *instptr = (char *)0;

   zsfor2c(c_mode, 7, mode, &unit, 6, 2, 1, status);
   zsfor2c(c_instance, 32, inst, &unit, 6, 5, 2, status);
   if (c_instance[0]  && c_instance[0] != ' ') instptr = c_instance;

   *status = ISTATFileOpen(*unit,c_mode,*nclasses,*nbands,instptr);
}

void FTN_NAME2_(istat_signal, ISTAT_SIGNAL) ( unit,status,abendflag)
int *unit;
int *status;
int *abendflag;
{
	ISTATSignal( *unit, *status, *abendflag);
}

void FTN_NAME2_(istat_file_close, ISTAT_FILE_CLOSE) ( unit,status)
int *unit;
int *status;
{
	*status = ISTATFileClose( *unit );
}

void FTN_NAME2_(istat_file_info, ISTAT_FILE_INFO) (unit,nclasses,nbands,ibis) 
int *unit;
int *nclasses;
int *nbands;
int *ibis;
{
	ISTATFileInfo( *unit, nclasses, nbands, ibis );
}

void FTN_NAME2_(istat_record_write, ISTAT_RECORD_WRITE) (int *unit,
		int *recnum, char *name,
		int *npix, int *nbands, float *means, float *covar, int *status,
		ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[33];

   zsfor2c(c_name, 32, name, &unit, 8, 3, 1, status);
   
   *status = ISTATRecordWrite(*unit,*recnum,c_name,*npix,*nbands,means,covar);
}

void FTN_NAME2_(istat_record_read, ISTAT_RECORD_READ) (int *unit, int *recnum,
	char *name, int *npix,
	int *nbands, float *means, float *covar, int *status, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[33];

   *status = ISTATRecordRead(*unit,*recnum,c_name,npix,nbands,means,covar);

   zsc2for(c_name, 0, name, &unit, 8,3,1, status);
}

int FTN_NAME2_(istat_record_find, ISTAT_RECORD_FIND) (int *unit,
					char *classname, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_name[33];

   zsfor2c(c_name, 32, classname, &unit, 2, 2, 1, classname);
   return ISTATRecordFind(*unit,c_name);
}





$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create istatfile.imake
#define SUBROUTINE istatfile

#define P2_SUBLIB
#define USES_ANSI_C
#define FTN_STRING

#define MODULE_LIST istatfile.c 
$ Return
$!#############################################################################
$Test_File:
$ create tstistatfile.pdf
procedure
refgbl $echo
refgbl $autousage
body
  let $echo="y"
  let $autousage="none"
  tistatfile fred.stat 4 6
  ibis-list fred.stat 'group nr=5 nc=10 csiz=8
  label-list fred.stat
  tistatfile fred.stat 50 12 
  ibis-list fred.stat 'group nr=5 nc=10 csiz=8+
     cols=(2,4,6,8,10,12,14,16,18,20)
  label-list fred.stat
end-proc

$!-----------------------------------------------------------------------------
$ create tistatfile.pdf
process
parm out string count=1
parm nclass integer count=1
parm nband integer count=1
parm INST keyword valid=(USE_INST, NO_INST) default=USE_INST
end-proc

$!-----------------------------------------------------------------------------
$ create tistatfile.c
/*
 * Test program for ISTAT library
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"

void FTN_NAME(c_test)()
{
	int unit, status;
	
	zvmessage("*** C - TEST ***", " ");
	status = zvunit(&unit,"OUT",1,0);
	if (status < 0) zvsignal(unit, status, 1);
	
	CreateStatsFile(unit);
	ReadStatsFile(unit);
}

CreateStatsFile(unit)
int unit;
{
	int status,def;
	int nclasses;
	int nbands;
	char *inst=(char *)0;
	
	zvp("NCLASS",&nclasses,&def);
	zvp("NBAND",&nbands,&def);
	if (zvptst("USE_INST")) inst = "Testing";
	
	status = ISTATFileOpen(unit,IMODE_WRITE,nclasses,nbands,inst);
	if (status<0) ISTATSignal(unit, status, 1);
	
	WriteStatsData(unit,nclasses,nbands);
	
	status = ISTATFileClose(unit);
	if (status<0) ISTATSignal(unit, status, 1);
	
}

WriteStatsData(unit,nclasses,nbands)
int unit;
int nclasses;
int nbands;
{
	int status;
	int row;
	char classname[9];
	float covariance[1000];
	float means[1000];
	int npix;
	
	memset(covariance,0,sizeof(covariance));
	memset(means,0,sizeof(means));
	for (row=1;row<=nclasses;row++)
	{
		/* just make some junk up */
		sprintf(classname,"CLASS%03d",row);
		npix = row*row;
		means[0] = (float)row/2;
		covariance[0] = (float)row*2;
		
		/* and write it out */
		status = ISTATRecordWrite(unit,row, classname,npix,
				nbands,means,covariance);
		if (status < 0) ISTATSignal(unit,status,1);
	}
}

ReadStatsFile(unit)
int unit;
{
	int status;
	int nbands,nclasses;
	int ncor;
	float *means,*covariance;
	char classname[9];
	char message[80];
	int npix,nb,row;

	status = ISTATFileOpen(unit,IMODE_READ,0,0,0);
	if (status<0) ISTATSignal(unit, status, 1);
	
	/* find out how big the file is and allocate memory */
	
	ISTATFileInfo(unit, &nclasses,&nbands,0);
	ncor = nbands*(nbands +1)/2;
	means = (float *)malloc(sizeof(float)*nbands);
	covariance = (float *)malloc(sizeof(float)*ncor);

	/* print out some records */

	sprintf(message,"%9s  %10s  %6s %6s %8s %8s",
	  "ROW","CLASSNAME","NPIX","NB","MEANS","COVAR");
	zvmessage(message," ");
	
	for (row=1;row<=nclasses;row++)
	{
		status = ISTATRecordRead(unit,row, classname,&npix,
				&nb,means,covariance);
		if (status < 0) ISTATSignal(unit,status,1);
		
		sprintf(message,"%9d \"%10s\" %6d %6d %8.3f %8.3f",
			row,classname,npix,nb,means[0],covariance[0]);
		zvmessage(message," ");
	}

	zvmessage("--- Searching for Class003 ---"," ");	
	row = ISTATRecordFind(unit,"class003");
	if (row > 0) 
	{
	  status = ISTATRecordRead(unit,row, classname,&npix,
			&nb,means,covariance);
	  if (status < 0) ISTATSignal(unit,status,1);
	
	  sprintf(message,"%9d \"%10s\" %6d %6d %8.3f %8.3f",
		row,classname,npix,nb,means[0],covariance[0]);
	  zvmessage(message," ");
	}
	else zvmessage("  Not Found. "," ");

	zvmessage("--- Searching for Class Wookie ---"," ");	
	row = ISTATRecordFind(unit,"class Wookie");
	if (row < 1 )  zvmessage ("  Not Found. "," ");

	status = ISTATFileClose(unit);
	if (status<0) ISTATSignal(unit, status, 1);
}

$!-----------------------------------------------------------------------------
$ create tistatfile_main.f
C Fortran Test file for ISTATFILE module
C

	include 'VICMAIN_FOR'

	
	subroutine main44
	implicit none
	call c_test
	call fortran_test
	return
	end
	

	subroutine fortran_test
	implicit none
	integer unit, status 
	
	call xvmessage('*** FORTRAN - TEST ***', ' ') 
	call xvunit(unit,'OUT',1,status,' ') 
	if (status .lt. 0) call xvsignal(unit, status, 1) 
	
	call create_stats_file(unit) 
	call read_stats_file(unit) 
	return
	end


	subroutine create_stats_file(unit)
	implicit none
	integer unit

	integer status,def 
	integer nclasses 
	integer nbands 
	character *40 inst
	logical xvptst
	
	call xvp('NCLASS',nclasses,def) 
	call xvp('NBAND',nbands,def) 
	if (xvptst('USE_INST')) then
	   inst = 'Testing' 
	else
	   inst = ' ' 
	endif
	
	call istat_file_open(unit,'write',nclasses,nbands,inst,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 
	
	call write_stats_data(unit,nclasses,nbands) 
	
	call istat_file_close(unit,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 
		
	return
	end


	
	subroutine write_stats_data(unit,nclasses,nbands)
	implicit none
	integer unit
	integer nclasses
	integer nbands

	integer status,i 
	integer row 
	character*8 classname
	real*4 covariance(1000) 
	real*4 means(1000) 
	integer npix 
	
	do i=1,1000
	  covariance(i) = 0
	  means(i) = 0
	enddo
	
	do row=1,nclasses	
		! just make some junk up 
		write(classname, '(A,I3.3)') 'Class',row
		npix = row*row 
		means(1) = float(row)/2.
		covariance(1) = float(row)*2 
		
		! and write it out 
		call istat_record_write(unit,row, classname,npix,
     +				nbands,means,covariance,status) 
		if (status .lt. 0) call istat_signal(unit,status,1) 
	enddo
	return
	end
	
	subroutine read_stats_file(unit)
	implicit none
	integer unit

	integer status 
	integer nbands,nclasses 
	real*4 means(20),covariance(400) 
	character*8 classname
	character*80 message
	integer npix,nb,row,ibis
	integer istat_record_find  !function declaration

	call istat_file_open(unit,'read',0,0,0,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 
	
	! find out how big the file is and allocate memory 
	
	call istat_file_Info(unit, nclasses,nbands,ibis) 

	! print out some records 
	write (message,'(A10,A11,2A7,2A9)')
     +     'ROW','CLASSNAME','NPIX','NB','MEANS','COVAR'
	call xvmessage(message,' ') 
		
	do row=1,nclasses
		call istat_record_read(unit,row, classname,npix,
     +			nb,means,covariance,status) 
		if (status .lt. 0) call istat_signal(unit,status,1) 
		
		write (message,'(I10,A11,2I7,2F9.3)')
     +             row,classname,npix,nb,means(1),covariance(1)
		call xvmessage(message,' ') 
	enddo
	
	call xvmessage('--- Searching for Class003 ---',' ') 	
	row = istat_record_find(unit,'Class003') 
	if (row .lt. 0) then
		message = '  Not Found. ' 
	else 
		write (message, '(A,I)') 'Found at row: ',row
	endif 
	call xvmessage(message,' ')
	
	call istat_record_read(unit,row, classname,npix,
     + 		nb,means,covariance,status) 
	if (status .lt. 0) call istat_signal(unit,status,1) 
	
	write (message,'(I10,A11,2I7,2F9.3)')
     +      row,classname,npix,nb,means(0),covariance(0)
	call xvmessage(message,' ') 
	
	call istat_file_close(unit,status) 
	if (status.lt.0) call istat_signal(unit, status, 1) 

	return
	end

$!-----------------------------------------------------------------------------
$ create tistatfile.imake
#define PROGRAM tistatfile

#define MODULE_LIST tistatfile_main.f tistatfile.c
#define MAIN_LANG_FORTRAN
#define USES_C
#define USES_FORTRAN
#define FTN_STRING

#define TEST

#define LIB_P1SUB
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL

$ Return
$!#############################################################################
