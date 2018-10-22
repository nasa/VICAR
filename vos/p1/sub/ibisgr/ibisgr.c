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




