$!****************************************************************************
$!
$! Build proc for MIPL module ibisgr
$! VPACK Version 1.9, Friday, March 19, 2010, 13:38:24
$!
$! Execute by entering:		$ @ibisgr
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
$ write sys$output "*** module ibisgr ***"
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
$ write sys$output "Invalid argument given to ibisgr.com file -- ", primary
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
$   if F$SEARCH("ibisgr.imake") .nes. ""
$   then
$      vimake ibisgr
$      purge ibisgr.bld
$   else
$      if F$SEARCH("ibisgr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisgr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisgr.bld "STD"
$   else
$      @ibisgr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisgr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisgr.com -mixed -
	-s ibisgr.c ibisgr_bridge.c -
	-i ibisgr.imake -
	-t tstibisgr.pdf tibisgr.pdf tibisgr_for.f tibisgr_c.c tibisgr.imake -
	-o ibisgr.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisgr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************
 *  ibisgr.c  -- C Interface to IBIS GRAPHICS-1 routine, using
 *               IBIS-2 subroutine library.
 *
 **********************************************************************
 *
 * 	Revision history:
 * 
 * 	New:		February 1986	KFE
 * 
 * 	2		April 1986	KFE
 * 		Extended format to handle any dimension between 1 and 40
 * 		instead of only 2 or 3.
 * 
 * 	3		June 1987	MKT
 * 		Added subroutine SETGR.
 *
 *      4		October 1994   NDR
 *              Rewritten in C and ported to Unix, using IBIS2 routines.
 *              Added C/Fortran Bridges
 *              Added z/signalgr routine for error handling.
 *              Added z/updategr routine for updating graphics files.
 *              Added IBIS-2 forward compatibility.
 *              Can handle up to 1024 dimension files.
 *
 *      5		Jan 1995   NDR
 *              Fix to handle bad labels for large files
 *              Fixed bridge to UPDATEGR
 *              Added RDGR_UNIT,WRGR_UNIT.
 * 
 ************************************************************************/

/************************************************************************
 *                              Includes
 ************************************************************************/

#include "ibisfile.h"
#include "ibiserrs.h"
#include "zvproto.h"
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/************************************************************************
 *                              Defines
 ************************************************************************/

#define MAX_FILES 40
#define GR_BLOCK 128
#define ROW_INC 128000000L
#define BAD_NUM(num) ((num) < 1 || (num) > MAX_FILES)
#define CHECK_INIT() if (first_time) init_gr()
#define CHECK_NUM(num) if (BAD_NUM(num)) return IBIS_INVALID_PARM
#define CHECK_CLOSED(num) if (_gr_files[num].flags & FLAG_FILE_OPEN) \
	return IBIS_FILE_ALREADY_OPENED
#define CHECK_OPEN(num) if (!(_gr_files[num].flags & FLAG_FILE_OPEN)) \
	return IBIS_FILE_NOT_OPEN
#define CHECK_WRITE(num) if (!(_gr_files[num].flags & FLAG_FILE_WRITE)) \
	return IBIS_FILE_OPENED_READONLY

typedef enum {
	FLAG_FILE_OPEN     =1,
	FLAG_FILE_WRITE    =2,
	FLAG_FILE_NEW      =4
} gr_flags;

/************************************************************************
 *                              Structures
 ************************************************************************/

typedef struct GraphFile {
	int flags;  /* status flags           */
	int ibis;   /* IBIS file              */
	int record; /* IBIS record            */
	int dim;    /* dimension              */
	int size;   /* current physical size  */
	int read_row;/* current read row      */
	int write_row;/* current write row      */
} GraphFile;

/************************************************************************
 *                              Private Globals
 ************************************************************************/

static float _zero[1024];
static float temp_buf[1024];
static GraphFile _gr_files[MAX_FILES+1]; /* index 0 not used */
static int first_time=1;

static void init_gr();
static int OpenGraph();
static int SetUpNewGraph();
static int SetUpOldGraph();

/************************************************************************
 *                              Public Routines
 ************************************************************************/

/*
 *  zrdgr -- open an existing graphics file for reading
 */

int zrdgr(int instance,int num,int dimension)
{
	int unit,status;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_CLOSED(num);
	
	status = zvunit(&unit,"inp",instance,NULL);
	if (status !=1) return status;

	return OpenGraph(unit,IMODE_READ,num,dimension,0);	
}


/*
 *  zrdgr_unit -- open an existing graphics file unit for read
 */

int zrdgr_unit(int unit,int num,int dimension)
{
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_CLOSED(num);

	return OpenGraph(unit,IMODE_READ,num,dimension,0);	
}

/*
 *  zwrgr -- create a new graphics file for writing
 */

int zwrgr(int instance,int num,int dimension)
{
	int is_new=1;
	int unit,status;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_CLOSED(num);
	
	status = zvunit(&unit,"out",instance,NULL);
	if (status !=1) return status;
	
	/* create file with appropriate sizes,oriented by row.*/
	/* unported sub will have this as IBIS-1 (OWRITE) format */

	status = OpenGraph(unit,IMODE_OWRITE,num,dimension,is_new);
	if (status != 1) return status;
	
	_gr_files[num].flags |= FLAG_FILE_WRITE;
	return 1;
}

/*
 *  zwrgr_unit -- open an existing graphics file unit for write
 */

int zwrgr_unit(int unit,int num,int dimension)
{
	int is_new=1;
	int status;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_CLOSED(num);
		
	/* create file with appropriate sizes,oriented by row.*/
	/* unported sub will have this as IBIS-1 (OWRITE) format */

	status = OpenGraph(unit,IMODE_OWRITE,num,dimension,is_new);
	if (status != 1) return status;
	
	_gr_files[num].flags |= FLAG_FILE_WRITE;
	return 1;
}


/*
 *  zupdategr -- open an existing graphics file unit for update
 */

int zupdategr(int unit,int num,int dimension)
{
	int status=1;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_CLOSED(num);
	
	status = OpenGraph(unit,IMODE_UPDATE,num,dimension,0);
	if (status != 1) return status;
	
	_gr_files[num].flags |= FLAG_FILE_WRITE;
	return 1;
}

/*
 *  zgetgr -- read in a coordinate row
 */

int zgetgr(int num,int *zero1,int *eof,float* first_c,float* second_c,float* other_c)
{
	GraphFile *graph;
	int status,dim;
	int i;
	float *val;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_OPEN(num);
	
	graph = _gr_files + num;
	if (graph->read_row > graph->size)
	{
		*zero1=1;
		*eof =1;
		return 1;
	}
	*eof = 0;
	
	status = IBISRecordRead(graph->record, (char *)temp_buf, graph->read_row);
	if (status != 1) return status;
	
	dim = graph->dim;
	*first_c = temp_buf[0];
	if (second_c && dim>1) 
		*second_c = temp_buf[1];
	if (other_c && dim>2) 
		memcpy(other_c,temp_buf+2,(dim-2)*sizeof(float));
	
	/* test for zero */
	for (i=0,val=temp_buf;i<dim;i++,val++)
		if (*val) break;
		
	*zero1 = (i==dim);
	
	graph->write_row=graph->read_row;
	graph->read_row++;
	
	return 1;
}

/*
 *  znextgr -- skips to next non-zero coordinate set, or eof if none.
 */

int znextgr(int num,int *eof,float* first_c,float* second_c,float* other_c)
{
	int status=1,eof1=0,zero=1;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_OPEN(num);
	
	/* go until you hit zero */
	while (zero && !eof1 && status==1)
	{
		status = zgetgr(num,&zero,&eof1,first_c,second_c,other_c);
	}
	
	*eof = eof1;
	return status;
}

/*
 *  zputgr -- writes the next coordinate set out to file (if writeable).
 *  The arguments were originally float instead of double, but that caused
 *  issues with prototypes...  rgd 3/2010
 */


int zputgr(int num,double first_c,double second_c,float* other_c)
{
	GraphFile *graph;
	int status=1,dim;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_OPEN(num);
	CHECK_WRITE(num);
	
	graph = _gr_files + num;
	if (graph->write_row > graph->size)
	{
		int newsize = graph->size+ROW_INC;
		
		/* Extend file */
		status = IBISFileSet(graph->ibis,IFILE_NR,(char *)(uintptr_t) newsize,0);
		if (status != 1) return status;
		graph->size = newsize;
	}

	dim = graph->dim;
	temp_buf[0] = (float)first_c;
	if (dim>1) temp_buf[1] = (float)second_c;
	if (dim>2) memcpy(temp_buf+2,other_c,(dim-2)*sizeof(float));
		
	status = IBISRecordWrite(graph->record, (char *)temp_buf, graph->write_row);
	if (status != 1) return status;
	
	graph->write_row++;
	graph->read_row=graph->write_row;
	
	return 1;
}

/*
 *  zendgr -- writes an end-of-polygon delimiter to the file.
 *     This may be modified for future compatibility with
 *     more "object-oriented" graphics-1 extensions. Currently
 *     only the "all-zero" delimiter implementation is used.
 */

int zendgr(int num)
{
	return zputgr(num,0.0,0.0,_zero);
}

/* 
 * zsetgr -- seek operation for reading.
 * go to row "row". If a new file, this is a no-op
 * in the original code. A file open for writing
 * with update can seek.
 */

int zsetgr(int num, int row)
{
	GraphFile *graph;

	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_OPEN(num);

	graph = _gr_files + num;

	if ((graph->flags & FLAG_FILE_NEW) || graph->size<row) return 1;
	graph->write_row = graph->read_row = row;
	return IBISRecordSet(_gr_files[num].record,IRECORD_ROW,row);

}

/* close up shop */

int zclgr(int num)
{
	GraphFile graph;
	int status=1,status2=1;

	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_OPEN(num);
	
	/* store and then clear the file entry */
	graph=_gr_files[num];
	memset(_gr_files+num,0,sizeof(GraphFile));

	status = IBISRecordClose(graph.record);
	if (status != 1) status2=status;  /* but continue */
	
	if (graph.flags & FLAG_FILE_NEW)
	{
		int rows_left = (graph.write_row-1)%GR_BLOCK;
	
		if (rows_left)
		{
		  /* make sure that all other rows at end are zero. */
		  status = IBISRowClear(graph.ibis,graph.write_row,rows_left);
		  if (status != 1) status2=status; 
		}
		status = IBISFileSet(graph.ibis,IFILE_NR,(char *) (uintptr_t) (graph.write_row-1),0);
		if (status != 1) status2=status;  /* but continue */
	}
	
	status = IBISFileClose(graph.ibis,0);
	if (status != 1) return status;
	
	return status2;
}

void zsignalgr(int num,int status,int abendflag)
{
	/* 
	 * eventually we may have some more graphics-specific errors,
	 * but for now it's mostly just IBIS-2 and VICAR errors.
	 */
	 
	 if (BAD_NUM(num))
	 {
	 	char msg[80];
		sprintf(msg,"Invalid Graphics file-number:%d",num);
	 	zvmessage(msg, " ");
		if (abendflag) zabend();
	 }
	 else
	 {
		int ibis = _gr_files[num].ibis;
		if (ibis)
		 	IBISSignal(ibis,status,abendflag);
		else
		 	IBISSignalU(ibis,status,abendflag);
			
	 }
}


/************************************************************************
 *                              Private Routines
 ************************************************************************/

static void init_gr()
{
	/* not much to do, here */
	first_time=0;
	memset(_gr_files,0,sizeof(_gr_files));
	memset(_zero,0,sizeof(_zero));
}

/*
 *  Common interface -- open a graphics file from VICAR unit
 */

static int OpenGraph(unit,mode,num,dimension,newflag)
int unit;
char *mode;
int num;
int dimension;
int newflag;
{
	int ibis=0,record=0,nr=GR_BLOCK,status=1;
	int cols[1024],nc_file;
	GraphFile graph;
	
	CHECK_INIT();	
	CHECK_NUM(num);
	CHECK_CLOSED(num);
	
	status = IBISFileOpen(unit,&ibis,mode,dimension,nr,0,IORG_ROW);
	if (status != 1) goto fail;

	status = IBISFileGet(ibis,IFILE_NC,&nc_file,1,1,0);
	if (status != 1) goto fail;

	if (newflag)
		status = SetUpNewGraph(unit,ibis,cols,dimension);
	else
		status = SetUpOldGraph(ibis,cols,dimension);
	if (status != 1) goto fail;

	status = IBISFileGet(ibis,IFILE_NR,&nr,1,1,0);
	if (status != 1) goto fail;
	
	status = IBISRecordOpen(ibis,&record,0,cols,dimension,IFMT_REAL);
	if (status != 1) goto fail;

	/* everything worked; set up gr  */
	graph.flags = FLAG_FILE_OPEN;
	if (newflag) graph.flags |= FLAG_FILE_NEW;
	graph.ibis = ibis;
	graph.dim = dimension;
	graph.size = nr;
	graph.record = record;
	graph.write_row = graph.read_row = 1;
	_gr_files[num] = graph;
	return 1;

fail:
	/* if (ibis) IBISFileClose(ibis,0); */
	return status;
}

static int SetUpNewGraph(unit,ibis,cols,dimension)
int unit;
int ibis;
int cols[];
int dimension;
{
	int status,i;
	
	/*
	 *  Override the 'TABULAR' file type.
	 */
	
	status = IBISFileSet(ibis,IFILE_TYPE,"GRAPHICS-1",0);
	if (status != 1) return status;
	/* prevent the IBIS routines from initializing file */
	status = IBISFileSet(ibis,IFILE_AUTO_INIT,IINIT_OFF,0);
	if (status != 1) return status;

	status = IBISFileSet(ibis,IFILE_NR,(void*) (uintptr_t) ROW_INC,0);
	if (status != 1) return status;


 	status=zldel( unit, "system","type",NULL);
	if (status != 1) return status;
	status=zladd( unit,"system","type","GRAPH1","format","string", NULL);
	if (status != 1) return status;

	for (i=0;i<dimension;i++) cols[i]=i+1;

	/* Install the columns into the C_POSITION class */
	IBISGroupNew(ibis,ITYPE_GROUP,"C_POSITION",cols,dimension,0);
	IBISGroupNew(ibis,ITYPE_GROUP,"C_ROOT",cols,dimension,0);
	
	return 1;
}

static int SetUpOldGraph(ibis,cols,dimension)
int ibis;
int cols[];
int dimension;
{
	int i,nc,nc_file;
	int status;
	
	/* Look for columns in C_POSITION. If none, then take everybody */
	nc = IBISColumnFind( ibis, ITYPE_ANY, 
	          "group:C_POSITION & group:C_ROOT", 0, 1, 0 );

	status = IBISFileGet(ibis,IFILE_NC,&nc_file,1,1,0);
	if (status != 1) return status;

	/* In anticipation of future extensions to GRAPHICS files, */
	/* We shall look just for columns in group POSITION, and if */
	/* none then just take all the numeric columns */

	if (nc>0)
	{
		IBISColumnFind( ibis, ITYPE_ANY, 
	          "group:C_POSITION & group:C_ROOT", cols, 1, nc );
	}
	else
	{
		/* get all columns which are not ASCII */
		for (i=0;i<nc_file;i++) cols[i]=i+1;
		IBISGroupNew(ibis,ITYPE_LOCAL,"$all",cols,nc_file,0);
		nc = IBISColumnFind(ibis,ITYPE_ANY,
			"$all - format:ascii",cols,1,0);
	}
	if (nc < dimension) return IBIS_NO_SUCH_COLUMN;
	return 1;
}


/* End of module */




$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisgr_bridge.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 *  ibisgr_bridge.c -- FORTRAN bridges to IBIS Graphics-1 code
 *
 *  NOTE!!!: Ported C code should call the "z" routines rather
 *   than the fortran bridges below. Only FORTRAN programs can
 *   call these routines.
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ibisfile.h"

int FTN_NAME2(rdgr, RDGR) (int *instance,int *num,int *dimension)
{
	return zrdgr(*instance,*num,*dimension);
}

int FTN_NAME2_(rdgr_unit, RDGR_UNIT) (int *unit,int *num,int *dimension)
{
	return zrdgr_unit(*unit,*num,*dimension);
}

int FTN_NAME2(wrgr, WRGR) (int *instance,int *num,int *dimension)
{
	return zwrgr(*instance,*num,*dimension);
}

int FTN_NAME2_(wrgr_unit, WRGR_UNIT) (int *unit,int *num,int *dimension)
{
	return zwrgr_unit(*unit,*num,*dimension);
}

int FTN_NAME2(updategr, UPDATEGR) (int *unit,int *num,int *dimension)
{
	return zupdategr(*unit,*num,*dimension);
}

int FTN_NAME2(getgr, GETGR) (int *num,int *zero,int *eof,
			float *first_c,float *second_c,float *other_c)
{
	return zgetgr(*num,zero,eof,first_c,second_c,other_c);
}

int FTN_NAME2(nextgr, NEXTGR) (int *num,int *eof,
		float *first_c,float *second_c,float *other_c)
{
	return znextgr(*num,eof,first_c,second_c,other_c);
}

int FTN_NAME2(putgr, PUTGR) (int *num,
		float *first_c,float *second_c,float *other_c)
{
	return zputgr(*num,*first_c,*second_c,other_c);
}

int FTN_NAME2(endgr, ENDGR) (int *num)
{
	return zendgr(*num);
}

int FTN_NAME2(setgr, SETGR) (int *num, int *row)
{
	return zsetgr(*num, *row);
}

int FTN_NAME2(clgr, CLGR) (int *num)
{
	return zclgr(*num);
}

void FTN_NAME2(signalgr, SIGNALGR) (int *num,int *status,int *abortflag)
{
	(void) zsignalgr(*num,*status,*abortflag);
}



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibisgr.imake
#define SUBROUTINE ibisgr
#define USES_C
#define MODULE_LIST ibisgr.c ibisgr_bridge.c
#define FTN_STRING
#define P1_SUBLIB
$ Return
$!#############################################################################
$Test_File:
$ create tstibisgr.pdf
procedure
refgbl $autousage
body
let $autousage="none"
!
! Create a large graphics file (280 values; 140 2d coordinate sets)
ibis-gen int1 nc=2 nr=46 datacol=(1,2) data=( +
    1,1, 5,5,   44.32,2.214, 30,12.5, 2,2,  0,0,  1,1,  20,1,  20,20, 1,20, +
    1,1, 32,13, 78., 23.29,  43,83,   48,1, 0,0,  2,2,  19,2,  19,19, 2,19, +
    2,2, 38,3,  11,23,       1,5,     35,2, 33,2, 0,0,  44,2,  2,3,   53,3, +
    2,3, 45,4,  4,4,         34,5,    3,4,  45,5, 33,2, 23,23, 34,33, 3,2, +
    1,1, 69,90, 8,9,         5,6,     0,4,  8,7)
ibis-gen int2 nc=2 nr=93 datacol=(1,2) data=(5,9, 3,8, 7,4, +
    3,4, 5,9, 8,7, 8,3, 6,7, 5,8, 9,7, 1,3, 2,1, 3,6, +
    2,4, 5,9, 3,7, 2,4, 0,2, 1,9, 3,8, 4,9, 7,8, 3,4, +
    5,1, 7,4, 7,3, 1,9, 0,8, 7,5, 4,7, 0,9, 8,6, 0,9, +
    5,4, 7,8, 7,0, 5,9, 2,8, 4,3, 5,7, 4,5, 1,8, 7,3, +
    4,9, 8,3, 6,7, 5,8, 7,2, 1,3, 8,5, 7,9, 5,8, 7,6, +
    8,9, 6,8, 4,7, 1,6, 8,5, 6,3, 4,5, 8,9, 3,4, 6,7, +
    5,7, 8,6, 1,0, 7,4, 6,7, 8,3, 1,4, 6,0, 7,3, 6,5, +
    7,8, 4,1, 6,3, 7,8, 0,5, 6,4, 1,3, 9,8, 7,3, 4,8, +
    8,7, 4,3, 1,8, 5,7, 1,3, 4,1, 3,7, 4,1, 3,7, 4,7)
ibis-catenate INP=(int1,int2) OUT=inta
ibis-copy inta polya 'ibis-1 'row
label-list polya
ibis-list polya gr1dim=2 'nohead
!
! The first test will transfer the file from (x,y) to (l,s), with (x=0,y=0)
! equivalent to (l=50,s=1)

tibisgr-convert polya  out=polyao 
label-list polyao
ibis-list polyao gr1dim=2 'nohead

! Make sure that big files are ok (FR#85711)
ibis-gen c nc=2 nr=100000 index=1
tibisgr-convert c  out=c1
ibis-list c1 gr1=2 nr=10

!
! Create a smaller graphics file to test repeats
ibis-gen polyb 'ibis-1 'row datacol=(1,2) data=( +
    4,5, 6,7, 22,1, 42,1, 8,3.3, 44.221,47.34)
!
! Try the second and third tests
tibisgr-convert polyb repeat=5 out=polybo 
ibis-list polyb gr1dim=2 'nohead
tibisgr-convert polyb repeat=5 rstart=3 out=polybo 
ibis-list polybo gr1dim=2 'nohead
!
! Try one of the tests again, this time trying SETGR on an output file
tibisgr-convert polyb  out=polybo 'write
label-list polybo
ibis-list polybo gr1dim=2 'nohead
!
!
! The fifth test will transfer the file from (x,y) to (l,s), with (x=0,y=0)
! equivalent to (l=50,s=1), but treating the input and output files as 5d
! files.
tibisgr-convert polya dimen=5 out=polyao2
label-list polyao2
ibis-list polyao2 gr1dim=5 'nohead
!
! Display several coordinates from a graphics file
tibisgr-print polya dimen=2 coord=6
tibisgr-print polya dimen=2 coord=3
tibisgr-print polya dimen=2 coord=10
tibisgr-print polya dimen=2 coord=40
tibisgr-print polya dimen=2 coord=-1
tibisgr-print polya dimen=2 coord=15
tibisgr-print polya dimen=2 coord=14 extra=4
tibisgr-print polya dimen=2 coord=62 extra=6
tibisgr-print polya dimen=2 coord=124 extra=4
tibisgr-print polya dimen=2 coord=130 extra=12
!
! Change the perceived dimension of the file and try again -- IBIS-1 only!
tibisgr-print polya dimen=3 coord=6
tibisgr-print polya dimen=3 coord=3
tibisgr-print polya dimen=3 coord=40
tibisgr-print polya dimen=3 coord=9
tibisgr-print polya dimen=3 coord=7 extra=4
tibisgr-print polya dimen=3 coord=46 extra=4
tibisgr-print polya dimen=3 coord=92 extra=4
tibisgr-print polya dimen=3 coord=86 extra=4
!
! Create a lot of graphic files for test #6, which takes 18 input files
ibis-gen f 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(6,206)
ibis-gen g 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(7,207)
ibis-gen h 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(8,208)
ibis-gen i 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(9,209)
ibis-gen j 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(10,2010)
ibis-gen k 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(11,2011)
ibis-gen l 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(12,2012)
ibis-gen m 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(13,2013)
ibis-gen n 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(14,2014)
ibis-gen o 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(15,2015)
ibis-gen p 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(16,2016)
ibis-gen q 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(17,2017)
ibis-gen r 'ibis-1 'row nc=2 nr=1  datacol=(1,2) data=(18,2018)
tibisgr-bigcopy  +
    inp=(polya,polyao,polyb,polybo,polyao2,f,g,h,i,j,k,l,m,n,o,p,q,r) +
    out=(out1,out2)
label-list out1
ibis-list out1 gr1=2 'nohead
label-list out2
ibis-list out2 gr1=2 'nohead
!
! Test new UPDATE mode (uses C interface)
ibis-list polya gr1=2 'nohead nr=10
tibisgr-update  polya  dim=2
ibis-list polya gr1=2 'nohead nr=10
!
end-proc

$!-----------------------------------------------------------------------------
$ create tibisgr.pdf
PROCESS

  SUBCMD CONVERT
	  PARM INP	TYPE=STRING COUNT=1
	  PARM OUT	TYPE=STRING  COUNT=1
	  PARM DIMEN	TYPE=INTEGER DEFAULT=2
	  PARM REPEAT	TYPE=INTEGER DEFAULT=-1
	  PARM RSTART	TYPE=INTEGER DEFAULT=1
	  PARM WRITE	TYPE=KEYWORD COUNT=(0:1) DEFAULT=-- VALID=WRITE
  END-SUB

  SUBCMD PRINT
	  PARM INP	TYPE=STRING COUNT=1
	  PARM DIMEN	TYPE=INTEGER DEFAULT=2
	  PARM COORD	TYPE=INTEGER DEFAULT=1
	  PARM EXTRA	TYPE=INTEGER DEFAULT=0
  END-SUB

  SUBCMD BIGCOPY
	  PARM INP	TYPE=STRING COUNT=(1:18)
	  PARM OUT	TYPE=STRING  COUNT=(1:2)
  END-SUB

  SUBCMD UPDATE
	  PARM INP	TYPE=STRING COUNT=1
	  PARM DIMEN	TYPE=INTEGER DEFAULT=2
  END-SUB

END-PROC

$!-----------------------------------------------------------------------------
$ create tibisgr_for.f

	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE

! This is a program to test the IBIS graphics-1 subroutines, with 
! subcommands used to specify the particular test to run.
!
! Available subcommands:  -CONVERT   -PRINT   -BIGCOPY -UPDATE
!

	integer count
	character*80 message
	character*20 subcommand
	character*1 cmdchar

! Get the sub command
	call xvp ('_SUBCMD', subcommand, count)
	write (message, 5) subcommand
 5	format (' Subcommand ',A)
	call xvmessage (message, ' ')

!  Now do it	
	cmdchar = subcommand(1:1)
	if (cmdchar.eq.'C') then
		call convert_file
	else if (cmdchar.eq.'P') then
		call print_file
	else if (cmdchar.eq.'B') then
		call copy_files
	else if (cmdchar.eq.'U') then
		call update_file
	endif
	
 	return
	end  ! tibisgr

c------------------------------------------------------------------------
!
!    1.	-CONVERT: Converts the input IBIS graphics-1 file in (x,y) format into a 
!	(line,sample) format for output.
!
!       REPEAT=5  causes a repeat of the first four coordinate
!	sets in the file. For example, given the following graphics file:
!	    a, b, c, d, e, f
!	where a,...,f are each coordinate sets, test 2 will produce the
!	following output coordinate sets:
!	    a, b, c, d, a, b, c, d, e, f
!	If keyword 'WRITE is supplied, then the SETGR call for the
!	repeat is applied to the write file rather than to the read file;
!	the repeat should have no effect--the output file should match 
!	the input file.
!       RSTART=3  causes the 3rd and 4th coordinate sets 
!	to be repeated. Using the same input file as shown above, test #3
!	will produce:
!	    a, b, c, d, c, d, e, f
!	If keyword 'WRITE is supplied, then the SETGR call for the
!	repeat is applied to the write file rather than to the read file;
!	the repeat should have no effect--the output file should match 
!	the input file.
	
	subroutine convert_file
	implicit none
	real x, y, extra_coords(8)
	logical zero, eof, set_write, xvptst
	integer count,repeat_trigger,repeat_start
	integer num_dimen
	integer maxY
	parameter (maxY = 50)
	
	call xvp('REPEAT',repeat_trigger,count)
	call xvp('RSTART',repeat_start,count)
 	call xvp ('DIMEN', num_dimen, count)

! Open and create the files
  	call rdgr (1, 1, num_dimen)
	call wrgr (1, 2, num_dimen)

	set_write = xvptst ('WRITE')

!     Copy the coordinate sets, repeating some coordinates if the count 
!     equals repeat_trigger
	count = 1
	eof = .false.
	do while (.not. eof)
	    call nextgr (1, eof, x, y,extra_coords) ! get the start of the linestring
	    zero = .false.
	    do while (.not. zero .and. .not. eof) ! read until end of linestring
		call putgr (2, maxY-y, x+1,extra_coords)
		call getgr (1, zero, eof, x, y,extra_coords)

!	  Check for repeat of coordinates
		count = count + 1
		if (count .eq. repeat_trigger) then
		    if (set_write) then
			call setgr (2, repeat_start)
		    else
			call setgr (1, repeat_start)
		    endif
		endif
	    enddo
	    if (zero) call putgr (2, 0, 0,extra_coords)
	enddo

	call clgr (1)
	call clgr (2)
	
	return
	end
	
c------------------------------------------------------------------------
!    2.	-PRINT Prints a coordinate from a graphics file. The coordinate is specified
!	via the COORD parameter. The dimensions of the graphics file are
!	specified via the DIMEN parameter, which defaults to 2. (A maximum of
!	8 coordinates may be in a set.) The EXTRA option is used to 
!	specify how many additional coordinate sets are to be displayed 
!	following the specified coordinate set; it defaults to zero.
!


	subroutine print_file
	implicit none
	character*80 message
	real x, y, extra_coords(8)
	integer count
	integer num_dimen,k,coord_num,extra,i
	logical zero, eof
	
 	call xvp ('DIMEN', num_dimen, count)
	
	call rdgr (1, 1, num_dimen)

	call xvp ('COORD', coord_num, count)
	call setgr (1, coord_num)
	call getgr (1, zero, eof, x, y, extra_coords)

	write (message, 40) coord_num, x, y, 
     +            (extra_coords(k),k=1,num_dimen-2)
 40	format (' Coordinate #',I3,': ',8F7.2)
	call xvmessage (message,' ')

! Do we need to print some trailing coordinate sets?
	call xvp ('EXTRA', extra, count)
	do i = 1, extra
	    call getgr (1, zero, eof, x, y, extra_coords)
	    if (eof) go to 460
	    write (message, 40) coord_num+i, x, y, 
     +		(extra_coords(k),k=1,num_dimen-2)
	    call xvmessage (message,' ')
	enddo

 460 	call clgr (1)
	
	return
	end

c------------------------------------------------------------------------
!    3.	-BIGCOPY Write the first coordinate set from the 9 odd-numbered input files
!	to output file #1, and the first coordinate set from the 9 even-
!	numbered input files to output file #2. This test tries out the
!	maximum of 20 graphics files.

	subroutine copy_files
	implicit none
	real x, y
	integer i
	logical zero, eof
	

! use lots of 2-D input files
 	do i = 1, 18
	    call rdgr (i, i, 2)
	end do
	call wrgr (1, 19, 2)
	call wrgr (2, 20, 2)

! Write the first coordinate in the odd-numbered input files to the first
! output file
	do i = 1, 17, 2
	    call getgr (i, zero, eof, x, y,0)
	    call putgr (19, x, y,0)
	end do

! Write the first coordinate in the even-numbered input files to the second
! output file
	do i = 2, 18, 2
	    call getgr (i, zero, eof, x, y,0)
	    call putgr (20, x, y,0)
	end do

	do i = 1, 20
	    call clgr (i)
	end do
	
	return
	end

$!-----------------------------------------------------------------------------
$ create tibisgr_c.c
/*
 *  tibisgr_c.c
 *
 *  A C subroutine which increments the Y coordinate of
 *   a VICAR/IBIS graphics file by one. Used to test the C interface
 *   to the IBISGR graphics library.
 */

#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(update_file)()
{
	float x, y, extra_coords[8];
	int zero, eof;
	int status,count,unit;
	int num_dimen;
	
 	status = zvp ("DIMEN", &num_dimen, &count);
	status = zvunit(&unit,"INP",1,0);
  	status = zupdategr (unit, 1, num_dimen);
	if (status != 1) zsignalgr(1,status,1);

/*     Update the coordinate sets */
	eof = 0;
	while (!eof)
	{
	    status = znextgr (1, &eof, &x, &y, extra_coords);
	    if (status != 1) zsignalgr(1,status,1);
	    zero = 0; /* false */
	    while (!zero && !eof) 
	    {
		status = zputgr (1, x, y+1.0,extra_coords);
	 	if (status != 1) zsignalgr(1,status,1);
		status = zgetgr (1, &zero, &eof, &x, &y,extra_coords);
	 	if (status != 1) zsignalgr(1,status,1);
	    }
	    if (zero) status = zendgr (1);
	    if (status != 1) zsignalgr(1,status,1);
	}

	status = zclgr (1);
	if (status != 1) zsignalgr(1,status,1);
	
	return;
}


$!-----------------------------------------------------------------------------
$ create tibisgr.imake
#define PROGRAM tibisgr
#define MODULE_LIST tibisgr_for.f tibisgr_c.c
#define USES_FORTRAN
#define USES_C
#define FTN_STRING
#define MAIN_LANG_FORTRAN
#define TEST
#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB

$ Return
$!#############################################################################
$Other_File:
$ create ibisgr.hlp
1  IBISGR
    These are the standard IBIS subroutines that perform buffered I/O with
IBIS graphics-1 files:

	z/rdgr/_unit to open a file for reading
	z/wrgr/_unit to open a file for writing
	z/updategr to open a file-unit for update
	z/getgr	   to get a coordinate set (pair or triplet) from the file
	z/nextgr   to get the beginning coordinate set of the next line string
	z/putgr	   to put a coordinate set into the file
	z/endgr	   to put an end-of-polygon terminator (0,0,0..) into the file
	z/setgr	   to specify the next coordinate set to be read by getgr
	z/clgr	   to close (and flush, for writing) the file
	z/signalgr to check the error status of the last i/o call.

The opening routines set up a structure that contains all of the
necessary information about the files.  The I/O routines only perform
serial I/O, although the position in pre-existing files (read/update)
can be modified by setgr.  There can be at most 40 graphics
files open at a time. The graphics files can have any number of
dimensions (the number of real coordinates per set) from 1 to 1024. 


2  CALLING SEQUENCES

  FORTRAN:
	status = rdgr (instance, num, dimension)
	status = wrgr (instance, num, dimension)
	status = rdgr_unit (unit, num, dimension)
	status = wrgr_unit (unit, num, dimension)
	status = updategr (unit, num, dimension)
	status = getgr ( num, zero, eof, first_c, second_c, third_c)
	status = nextgr (num, eof, first_c, second_c, third_c)
	status = putgr (num, first_c, second_c, third_c)
	status = endgr (num)
	status = setgr (num, coord)
	status = clgr (num)
	call signalgr (num,signal,abortflag)

  C:
	status = zrdgr (instance, num, dimension);
	status = zwrgr (instance, num, dimension);
	status = zrdgr_unit (unit, num, dimension)
	status = zwrgr_unit (unit, num, dimension)
	status = zupdategr (unit, num, dimension);
	status = zgetgr ( num, &zero, &eof, &first_c, &second_c, &third_c);
	status = znextgr (num, &eof, &first_c, &second_c, &third_c);
	status = zputgr (num, first_c, second_c, &third_c);
	status = endgr (num);
	status = zsetgr (num, coord);
	status = zclgr (num);
	(void) zsignalgr (num,signal,abortflag);

    The num parameter is the internal file number that these set of
routines use to reference the graphics files;  thus the number that
was used to open the file (with rdgr updategr or wrgr) must be used in
subsequent calls for that file.

    If the file is opened with only one or two dimensions then the
unnecessary coordinate parameters (second_c, third_c ) can be passed
null pointers (in C) or zeroes (FORTRAN). There are no variable arguments
in these routines, and so portable programs must not omit these parameters.

If the dimension is four or greater than the third_c parameter is
actually a real array containing the appropriate number of elements (n-2).
In any case for C routines it is always passed by reference, even
though first_c and second_c may be passed by value.

The routines all return an integer "status" value, which =1 if successful.
If not, the status refers to a graphics error status which may be passed to
z/signalgr(). To access the status value in FORTRAN the routines must be
declared as INTEGER functions in the calling program.

2  rdgr

	status = rdgr (instance, num, dimension)
	status = zrdgr (instance, num, dimension);
	status = rdgr_unit (unit, num, dimension)
	status = zrdgr_unit (unit, num, dimension);

		rdgr opens an IBIS graphics file for reading
		    and prepares the common block. rdgr_unit operates
		    similarly, but takes a VICAR unit (from xvunit).

	    Parameters:
		instance    integer	input	  which Vicar input file
		unit        integer	input	  which Vicar file unit
		num	    integer	input	  which IBIS graphics file
		dimension   integer     input	  number of coords per set

2  wrgr

	status = wrgr (instance, num, dimension)
	status = zwrgr (instance, num, dimension);
	status = wrgr_unit (unit, num, dimension)
	status = zwrgr_unit (unit, num, dimension);

		wrgr creates an IBIS graphics file for writing
		    and prepares the common block. wrgr_unit operates
		    similarly, but takes a VICAR unit (from xvunit).

	    Parameters:
		instance    integer	input	  which Vicar output file
		unit        integer	input	  which Vicar file unit
		num	    integer	input	  which IBIS graphics file
		dimension   integer     input	  number of coords per set


2  updategr

	status = updategr (unit, num, dimension)
	status = zupdategr (unit, num, dimension);

		updategr opens an existing IBIS graphics file for reading
		    and/or writing, and prepares the common block

	    Parameters:
		unit	    integer	input	  VICAR unit from z/xvunit
		num	    integer	input	  which IBIS graphics file
		dimension   integer     input	  number of coords per set

2  getgr

	status = getgr ( num, zero, eof, first_c, second_c, third_c)
	status = zgetgr ( num, &zero, &eof, &first_c, &second_c, &third_c);

		getgr gets a coordinate set from a graphics file

	    Parameters:
		num	 integer   input	which IBIS graphics file
		zero	 logical   output	true if all coords are zero
		eof	 logical   output	true if at the end of file
		first_c  real	   output	the first coordinate got
		second_c real	   output	the second coordinate got
		third_c  real arr. output	the rest of the coordinates

	    If eof is true then the coordinates are garbage.

2  nextgr

	status = nextgr (num, eof, first_c, second_c, third_c)
	status = znextgr (num, &eof, &first_c, &second_c, &third_c);

		nextgr reads coordinate sets from the graphics file
		    until coming to a non zero

	    Parameters:
		num	 integer   input	which IBIS graphics file
		eof	 logical   output	true if at the end of file
		first_c  real	   output	the first coordinate to get
		second_c real	   output	the second coordinate to get
		third_c  real arr. output	the rest of the coordinates

	    If eof is true then the coordinates are garbage.

2  putgr

	status = putgr (num, first_c, second_c, third_c)
	status = zputgr (num, first_c, second_c, &third_c);

		putgr puts a coordinate set out in the 
			graphics file

	    Parameters:
		num	 integer   input	which IBIS graphics file
		first_c  real	   input	the first coordinate to be put
		second_c real	   input	the second coordinate to be put
		third_c  real arr. input	the rest of the coordinates


2  endgr

	status = endgr (num)
	status = zendgr (num);

		putgr puts an end-of-polygon delimiter into the 
			graphics file

	    Parameters:
		num	 integer   input	which IBIS graphics file

2  setgr

	status = setgr (num, coord)
	status = zsetgr (num, coord);

		setgr specifies the next coordinate set (row) to be read 
		by getgr/putgr. This only has an effect for files opened
		with either z/rdgr or z/updategr.

	    Parameters:
		num	 integer   input	which IBIS graphics file
		coord	 integer   input	next coordinate set in
						graphics file

2  clgr

	status = clgr (num)
	status = zclgr (num);

		clgr closes the graphics file
		    if the file is an output/update file then it flushes 
		    the buffer and updates the IBIS label

	    Parameters:
		num	integer	   input	which IBIS graphics file

2  signalgr

	call signalgr (num,status,abendflag)
	(void) zsignalgr (num,status,abendflag);

		signalgr reports an informative error message if any, and
		   if abendflag is nonzero, will abort program.

	    Parameters:
		num         integer	   input	which IBIS graphics file
		status      integer	   input	error status
		abendflag   integer	   input	abort if non-zero.



2  OPERATION

    This set of IBIS graphics I/O routines uses a static structure
that keeps track of the IBIS-2 records and file control information for
up to forty open graphics files.  The routines that open the files (rdgr/_unit
wrgr/_unit and updategr) set up the structure.  


2 FILE FORMAT

   The graphics-1 file format is a list of coordinates defining the vertices
of polygons.  For two-dimensional (2-D) files each coordinate consists of 
a pair of real numbers, with the polygon terminator currently being a set of 
zeros.

For 3-D files each coordinate consists of a triplet of real numbers, with
the terminator being a triplet of zeros.  Usually the coordinate pairs
are in (line,sample) format, and the coordinate triplets in (line,sample,
elevation) format, although they could refer to any coordinate space.  A 
greater number of dimensions are allowed, up to 1024, to enable nominal data
to be carried along with the coordinate data. The file may contain extra 
terminators (pairs or  triplets of zeros) that must be skipped over when 
reading through the file.

Note: For future compatibility it is suggested that the IBISGR 
routines be used (via the 'zero' indicator) to determine when a polygon 
delimiter is encountered, rather than relying on this specific "all-zero"
implementation. Use 'endgr' to guarantee that a valid delimiter is
inserted into the file, instead of using 'putgr' to place zero valued
coordinates into file.

The graphics files are stored on disk in a IBIS-2 compatible file. Formerly
there were specific implementations in terms of number of bytes per record,
etc., but this is no longer the case. Any IBIS-2 file with numerical values
in the "columns" may be used as a GRAPHICS file. For future compatibility
if the IBISGR routines find that the groups "C_POSITION" and "C_ROOT" have
been defined (e.g. by the ICL routines), then it will use ONLY those
columns which belong to both of those groups. Otherwise it will use all of
the columns which contain numerical values.


2  EXAMPLES

3   FORTRAN


The following is a sample Fortran program using these calls:

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

C	This program converts the input IBIS graphics-1 file in (x,y) format
C	into a (line,sample) format for output.  
C	For 2-D files.

	INTEGER	NL, COUNT
	REAL	X, Y
	LOGICAL zero, eof
	INTEGER status, rdgr,wrgr,nextgr,putgr,getgr,clgr

	call XVP ('NL', NL, COUNT)
	status = rdgr (1, 1, 2)
	if (status.ne.1) call signalgr(1,status,1)
	status =  wrgr (1, 2, 2)
	if (status.ne.1) call signalgr(2,status,1)

	eof = .FALSE.
	DO WHILE (.NOT. eof)
	    status = nextgr (1, eof, X, Y, 0)     ! get the start of the poly
	    if (status.ne.1) call signalgr(1,status,1)
	    zero = .FALSE.
	    DO WHILE (.NOT. zero .AND. .NOT. eof) ! read until end of poly
		status = putgr (2, NL-Y, X, 0)
	        if (status.ne.1) call signalgr(2,status,1)
		status = getgr (1, zero, eof, X, Y, 0)
	        if (status.ne.1) call signalgr(1,status,1)
	    ENDDO
	    ! In place of 'call putgr(2,0,0,0)' call the following:
	    call endgr (2) !  End-of-polygon
	ENDDO

	status = clgr (1)
	if (status.ne.1) call signalgr(1,status,1)
	status = clgr (2)
	if (status.ne.1) call signalgr(2,status,1)

	RETURN
	END

3  C

  Here is an example of a portable C program to update a graphics file
  by adding "1" to the y coordinate:
  
   
   #include "vicmain_c"

   main44()
   {
	float x, y, extra_coords[8];
	int zero, eof;
	int status,count,unit;
	int num_dimen;
	
 	status = zvp ("DIMEN", &num_dimen, &count);
	status = zvunit(&unit,"INP",1,0);
  	status = zupdategr (unit, 1, num_dimen);
	if (status != 1) zsignalgr(1,status,1);

   /*     Update the coordinate sets */
	eof = 0;
	while (!eof)
	{
	    status = znextgr (1, &eof, &x, &y, extra_coords);
	    if (status != 1) zsignalgr(1,status,1);
	    zero = 0; /* false */
	    while (!zero && !eof) 
	    {
		status = zputgr (1, x, y+1.0,extra_coords);
	 	if (status != 1) zsignalgr(1,status,1);
		status = zgetgr (1, &zero, &eof, &x, &y,extra_coords);
	 	if (status != 1) zsignalgr(1,status,1);
	    }
	    if (zero) status = zendgr (1);
	    if (status != 1) zsignalgr(1,status,1);
	}

	status = zclgr (1);
	if (status != 1) zsignalgr(1,status,1);
	
	return;
   }




2  RESTRICTIONS

	There can be at most 40 graphics files open at a time.
	The number of dimensions must be between 1 and 1024 inclusive.
     
2  HISTORY

     IBISGR
     Original Programmer:     Frank Evans       February 1986
     Cognizant Programmer:    Niles Ritter      October 1994
     Documentation Author:    Frank Evans and Niles Ritter
     Revision:          5     February 1995      NDR

$ Return
$!#############################################################################
