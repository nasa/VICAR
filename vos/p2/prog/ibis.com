$!****************************************************************************
$!
$! Build proc for MIPL module ibis
$! VPACK Version 1.9, Monday, December 07, 2009, 16:28:13
$!
$! Execute by entering:		$ @ibis
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
$!   PDF         Only the PDF file is created.
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
$ write sys$output "*** module ibis ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ibis.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ibis.imake") .nes. ""
$   then
$      vimake ibis
$      purge ibis.bld
$   else
$      if F$SEARCH("ibis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibis.bld "STD"
$   else
$      @ibis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibis.com -mixed -
	-s ibis_main.c ibis_gen.c ibis_list.c ibis_copy.c ibis_catenate.c -
	   ibis_groups.c ibis_local.h -
	-p ibis.pdf -
	-i ibis.imake -
	-t tstibis.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibis_main.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 **  ibis.c: Basic IBIS-2 file utilities
 **      Uses new IBIS-2 Subroutine Library.
 **/

#include "vicmain_c"
#include "ibis_local.h"
#include <string.h>

static char *subcmd[]={
	"gen",
	"list",
	"copy",
	"catenate",
	"group",
	"xxxx"
};

typedef enum {
	cmdGEN=0,
	cmdLIST,
	cmdCOPY,
	cmdCATENATE,
	cmdGROUP,
	cmdLAST
} cmdType;

/* Global variable - Num Cols (dim) for Graphics-1 files */
int gr1dim; 

void main44(void)
{
    int count,def,cmd;
    char command[20],*str;
    
    /* get the global parm */
    zvp( "gr1dim", &gr1dim, &def);
    
    /* determine subcommand */
    zvparm("_SUBCMD",command,&count,&def,0,0);
    for (str=command;*str;str++) *str = tolower(*str);
    for (cmd=cmdGEN;cmd<=cmdLAST;cmd++)
    	if (!strcmp(command,subcmd[cmd])) break;

    switch (cmd)
    {
        case cmdGEN :      new_file();  break;
		   		
        case cmdLIST :     list_file(); break;
		   		
        case cmdCOPY :     copy_file(); break;
	
        case cmdCATENATE : catenate_files(); break;
	
        case cmdGROUP :    modify_group(); break;
	
	/* This should never happen: */   		
	default: zvmessage("Unknown Subcommand"," "); zabend();
    }
}


void pre_format(int ibis)
{
	int i,status;
	int intcols[40],a4cols[40],num_intcol,num_a4col;
	
	zvp( "intcols", intcols, &num_intcol );
        if (!intcols[0]) num_intcol=0;
	for (i=0;i<num_intcol;i++)
	{
		status=IBISColumnSet(ibis,
			ICOLUMN_FORMAT,IFMT_FULL,intcols[i]);
		if (status!=1) IBISSignal(ibis,status,1);
	}
	zvp( "a4cols", a4cols, &num_a4col );
        if (!a4cols[0]) num_a4col=0;
	for (i=0;i<num_a4col;i++)
	{
		status=IBISColumnSet(ibis,ICOLUMN_FORMAT,"A4",a4cols[i]);
		if (status!=1) IBISSignal(ibis,status,1);
		status=IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"A5",a4cols[i]);
		if (status!=1) IBISSignal(ibis,status,1);
	}
}

char *new_format_string(nc,deffmt,str)
int nc;
char *deffmt;
char *str;
{
    char *fmt=(char *)0,*ptr;
    int i;
    
    if (!deffmt) deffmt=IFMT_REAL;
    
    if (str) fmt=str;
    else
    {
	    fmt = (char *)calloc(1L, IFMT_SIZE * nc);
	    if (!fmt) return fmt;
    }
    
    for (i=0,ptr=fmt;i<nc;i++,ptr+=IFMT_SIZE)
    	strcpy(ptr,deffmt);
    
    return fmt;
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_gen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>
#include <stdlib.h>

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-GEN                               ***
 ***                                                                  ***
 ************************************************************************/
 
int new_file(void)
{
	int outunit,status;
	int out;
	int nc, nr,i;
	int count, def;
	int fmt_cols[100];
	char org[10];
	char type[201];
	char def_fmt[20];
	char colformat[MAX_COLUMN][IFMT_SIZE];
	char format_buf[MAX_FMT][IFMT_SIZE];
	char *fmt_ptr=(char *)0;
	char *mode;
	
 	status=zvunit( &outunit, "out", 1, NULL);
	if (status!=1) zvsignal( outunit, status, 1);

	zvp("nr", &nr, &def ); if (!nr) nr=10;
	zvp("nc", &nc, &def ); if (!nc) nc=10;
	zvp("deffmt", def_fmt, &def );
	if (!def_fmt[0] || def_fmt[0]==' ') strcpy(def_fmt,IFMT_REAL);

	/* Create the Formatting string */
	format_buf[0][0]='\0';
	new_format_string(MAX_COLUMN,def_fmt,colformat);
	zvparm("format", colformat, &count, &def, MAX_COLUMN, IFMT_SIZE);
	if (!def && count && strlen((char *) colformat)) 
	{
		fmt_ptr=(char *)colformat;
		zvp("fmtcols", fmt_cols, &def );
		if (count > nc) nc = count;
		if (fmt_cols[0] || count < nc)
		{
			/* format specified columns; all others default */
			new_format_string(nc,def_fmt,format_buf);
			fmt_ptr=(char *)format_buf;
			if (fmt_cols[0])
			{
			  for (i=0;i<count;i++)
			   if (fmt_cols[i]<(nc+1))
				strcpy(format_buf[fmt_cols[i]-1], colformat[i]);
			}
			else
			   memcpy(format_buf,colformat,(long)IFMT_SIZE*count);
		}
	}
	
	if (zvptst("ibis-2")) mode=IMODE_WRITE;
	else mode=IMODE_OWRITE;
  
	/* Create the output file unit, or abort on error */
	zvpone( "org", org, 1, 0 );

	status = IBISFileUnit(outunit, &out, mode, nc, nr, fmt_ptr, org);
	if (status!=1) IBISSignalU( outunit, status, 1);

	/* Set up subfile type */
	zvp("type",type,&def);
	if (type[0] && type[0]!=' ')
		IBISFileSet(out,IFILE_TYPE,type,0);

	/* Install Default-Format, if any */
	if (def_fmt[0] && def_fmt[0]!=' ')
	{
	   status=IBISFileSet(out,IFILE_FMT_DEFAULT,def_fmt,0);
	   if (status!=1) IBISSignalU( outunit, status, 1);
	}

	/* Create and open the file */
	status = IBISFileUnitOpen(out);
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	/* Write out data ? */
	status = write_data(out,nr,nc);
	if (status!=1) IBISSignalU( outunit, status, 1);

	/* close up shop */
	
	status = IBISFileClose( out, 0 );
	if (status != 1) IBISSignalU( outunit, status, 1);
}

int write_data(ibis)
int ibis;
{
	int status=1;
	int ncols,def,indexcol;
	int record;
	int cols[MAX_DATA];
	int nvals,nrows,row;

	zvp("indexcol",&indexcol,&def);
	if (indexcol)
	{
		int data[512],nr,i,rowsnow,rowsleft;
		
		IBISFileGet(ibis,IFILE_NR,&nr,1,1,0);
		status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,
				IFMT_FULL,indexcol);
		if (status !=1) goto failure;
		
		/* write out the INDEX values */
		for (row=1,rowsleft=nr;rowsleft>0;rowsleft-=rowsnow)
		{
		   rowsnow = rowsleft < 512? rowsleft : 512;
		   for (i=0;i<rowsnow;i++)
		   	data[i] = row+i;
		   status = IBISColumnWrite(ibis,(char *)data,indexcol,
								row,rowsnow);
		   if (status !=1) goto failure;
		   row += rowsnow;
		}
		
	}
	
	zvparm("datacols",cols,&ncols,&def,0,0);
	if (ncols && cols[0])
	{
		float *data,*dptr;
		
		zvpcnt("data",&nvals);
		nrows = (nvals + ncols-1)/ncols;
		if (!nvals || !nrows) goto next;
		
		data=(float *)calloc(1L,sizeof(float)*nvals);
		zvp("data",data,&def);

		status=IBISRecordOpen(ibis,&record,0,cols,ncols,IFMT_REAL);
		if (status !=1) goto failure;
		
		for (row=1,dptr=data;row<=nrows;row++,dptr+=ncols)
		{
		   status = IBISRecordWrite(record,(char *)dptr,row);
		   if (status !=1) goto failure;
		}
		
		status=IBISRecordClose(record);
		if (status !=1) goto failure;

		free (data);
	}
next:	
	zvparm("strcols",cols,&ncols,&def,0,0);
	if (ncols && cols[0])
	{
		char *data,*dptr;
		int size=81;
		
		zvpcnt("string",&nvals);
		nrows = (nvals + ncols-1)/ncols;
		if (!nvals || !nrows) goto end;
		
		data=(char *)calloc(1L,size*nvals);
		zvparm("string",data,&nvals,&def,0,size);

		status=IBISRecordOpen(ibis,&record,0,cols,ncols,"A80");
		if (status !=1) goto failure;
		
		for (row=1,dptr=data;row<=nrows;row++,dptr+=ncols*size)
		{
		   status = IBISRecordWrite(record,dptr,row);
		   if (status !=1) goto failure;
		}
		
		status=IBISRecordClose(record);
		if (status !=1) goto failure;

		free (data);
	}
end:	
	return 1;
failure:
	return status;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_list.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis_local.h"
#include <string.h>
#include <stdio.h>
#include <zvproto.h>
#include <stdlib.h>
#include <string.h>

void outmessage(char* msg,char* dummy);
void pre_format(int ibis);

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-LIST                              ***
 ***                                                                  ***
 ************************************************************************/

static FILE* fd=0;

#define IBISPRINT( str, ptr, fmtchar, fmtStr ) \
	switch (tolower(fmtchar)) { \
		case 'b': sprintf( str, fmtStr,    *(char *)  (ptr));  break; \
		case 'h': sprintf( str, fmtStr,    *(short *) (ptr));  break; \
		case 'f': sprintf( str, fmtStr,    *(long *)  (ptr));  break; \
		case 'r': sprintf( str, fmtStr,    *(float *)  (ptr));  break; \
		case 'd': sprintf( str, fmtStr,    *(double *)(ptr));  break; \
		case 'c': sprintf( str, fmtStr,    ((float*)ptr)[0],((float*)ptr)[1] );break; \
		case 'a': sprintf( str, fmtStr,    ptr );break; \
		default:  sprintf( str, fmtStr,   "BAD FORMAT!" );break; \
	}

#define IBISFORMAT( str, fmtchar, colsize ) \
	switch (tolower(fmtchar)) { \
		case 'b': \
		case 'h': \
		case 'f': sprintf( str, "%%%dd",  (colsize));  break; \
		case 'r': sprintf( str, "%%%d.2f",  (colsize));  break; \
		case 'd': sprintf( str, "%%%d.2lf", (colsize));  break; \
		case 'c': sprintf( str, " (%%-%d.2f,%%%d.2f)", \
			((colsize)-4)/2, colsize - (4 + ((colsize)-4)/2) );\
			 break; \
		case 'a': sprintf( str, "%%%ds", (colsize) );break; \
		default:  sprintf( str, "%%%ds", (colsize));break; \
	}


static void parse_user_format();
static void show_header();
static int show_groups();
static void show_preamble();

int list_file(void)
{
	int inunit,status;
	int in;
	int nc_file,nr_file,nr,nc, rinc, rows_left, scol, col,i;
	int row, row_inc;
	int cols[1024], def,ccnt;
	int colm;
	int space;
	int total;
	int colnum;
	int srow,srow1;
	int csize[MAXCOLPERROW],colperrow,rowsize=132;
	int colsize[MAXCOLPERROW],coffset[MAXCOLPERROW];
	int disp_header,disp_groups,disp_units,disp_formats;
	int user_format=0,col_header;
	double buffer[MAXCOLPERROW][DBUFSIZE];
	char *format;
	char *dataPtr[MAXCOLPERROW];
	char headerStr[ROWSIZE+2];
	char formatStr[ROWSIZE+2], fmtelemt[MAXCOLSIZE+1];
	char groupStr[ROWSIZE+2], grpelemt[MAXCOLSIZE+1];
	char unitStr[ROWSIZE+2], unitelemt[MAXCOLSIZE+1];
	char lineStr[ROWSIZE+2],lineelemt[MAXCOLSIZE+1];
	char valueStr[ROWSIZE+2], valueelemt[MAXCOLSIZE+1];
	char headerfmt[20],formatfmt[20];
	char valuefmt[MAXCOLPERROW][20];
	char userfmt[200],cfmt[MAXCOLPERROW][20];
	char outStr[ROWSIZE+2];
	char version[10];
	char colformat[10];
	char outfile[255];
	
	/* open the input IBIS file, abort on error */
	
	zvunit( &inunit, "inp", 1, NULL);
	status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
	if (status!=1)  IBISSignalU( inunit, status, 1);
	
	/* Find out if there is an ASCII text file */
	zvp("outfile", outfile, &def );
	if (outfile[0]!=' ')
	{
		fd = fopen(outfile,"w");
		if (!fd)
		{
			zvmessage("cant open output file"," ");
			zabend();
		}
	}

	/* get the input row & column counts */
	
	IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
	IBISFileGet( in, IFILE_NR, &nr_file,  1, 1, 0 );
	
	disp_groups = zvptst("groups");
	disp_units = zvptst("units");
	disp_header = zvptst("header");
	col_header = zvptst("colhead");
	disp_formats = zvptst("formats");
	zvp("space", &space, &def );

	show_preamble(); /* user preamble */
	
	IBISFileGet( in, IFILE_VERSION, version,  1, 1, 9 );
	if (disp_header) 
	{
		show_header(in,nr_file,nc_file,version);
		if (disp_groups) show_groups(in,"Group \'%s\':",IFILE_GROUPS,ITYPE_GROUP);
		if (disp_units) show_groups(in,"Unit \'%s\':",IFILE_UNITS,ITYPE_UNIT);	
	}
	
	/* get the input format string */
	
	format = (char *)malloc( sizeof(char)*IFMT_SIZE* (nc_file+1) );
	if (!format)
	{
		outmessage( "unable to allocate format string", " ");
		zabend();
	}
	
	zvp( "cformat",userfmt,&def);
	memset(csize,0,sizeof(csize));
	if (strlen(userfmt))
	{
		user_format = 1;
		parse_user_format(userfmt,cfmt,csize);
	}
	else
	{
		zvp( "csize", csize, &def );
		if (!csize[0]) csize[0] = 15;
	}
	zvp( "screen", &rowsize, &def );
	if (!rowsize) rowsize=80;

	/* which & how many rows to read */
	
	zvp( "sr", &srow, &def );
	zvp( "nr", &nr, &def );
	if (!nr) nr=nr_file;
	if (srow > nr_file) srow=nr_file;
	if (srow + nr - 1 > nr_file)
		nr = nr_file + 1 - srow;
	row_inc = nr;
	if (row_inc > 30) row_inc=30;
	
	/* which & how many columns to read */	
	
	zvp("sc", &scol, &def );
	zvp("nc", &nc, &def );
	if (!nc) nc = nc_file;
	
	zvp( "cols", cols, &ccnt );
	if (cols[0]) nc=ccnt;
	else ccnt = 0;
	
	if (scol > nc_file) scol=nc_file;
	if (scol + nc - 1 > nc_file)
		nc = nc_file + 1 - scol;
	
	/* pre-format old IBIS files */
	if (!strcmp(version,IVERSION_1)) pre_format(in);
	
	/* Set up the column formatting strings */

	/* This used to be %%-%dd and %%-%ds but csize and csize-2	*/
	/* are pointers so carto changed them to %p.  The whole thing	*/
	/* makes no sense, but appears to be unused anyway. rgd 3/2010	*/
	sprintf(headerfmt, "C:%%-%pd", csize-2);
	sprintf(formatfmt, "%%-%ps", csize);
	
	if (!csize[1]) 
		for (colnum=1;colnum<nc;colnum++) 
			csize[colnum]=csize[0];
	csize[nc+1]=1;

	for (i=0;i<MAXCOLSIZE;i++) lineelemt[i]='-';
	lineelemt[MAXCOLSIZE]='\0';
	

	/* The big loop */

	for (col=0; col<nc; col+=colperrow)
	{
		/* figure out how many cols this time */
		coffset[0]=0;
		for (colperrow=0,total=csize[col];
			total<rowsize && col+colperrow<nc;colperrow++)
		{
			coffset[colperrow+1]=total;
			total+=csize[col+colperrow+1];
		}
		
		/* draw some dashed lines */
		for (colnum = 0; colnum<colperrow; colnum++)
		{
			strcpy(lineStr+coffset[colnum],"+");
			strncpy( lineStr+coffset[colnum]+1,
				lineelemt,csize[col+colnum]-1);
			lineStr[coffset[colnum+1]] = '\0';
		}
		/* set up the header */
		
		headerStr[0]=formatStr[0]='\0';
		
		for (colnum = 0; colnum<colperrow; colnum++)
		{
			colm = ccnt ?  cols[col+colnum] : col+scol+colnum;
			IBISColumnGet( in, ICOLUMN_FORMAT,colformat, colm );
			IBISColumnGet( in, ICOLUMN_U_FORMAT, format+IFMT_SIZE*colm, colm );
			IBISColumnGet( in, ICOLUMN_U_SIZE, colsize+colnum, colm);
			
			strcpy(grpelemt," -- ");
			IBISGroupFind( in, ITYPE_GROUP, colm, grpelemt, 1,1,sizeof(grpelemt));
			sprintf(groupStr+coffset[colnum],
				"%-*s",csize[col+colnum], grpelemt);

			strcpy(unitelemt," -- ");
			IBISGroupFind( in, ITYPE_UNIT, colm, unitelemt, 1,1,sizeof(unitelemt));
			sprintf(unitStr+coffset[colnum],
				"%-*s",csize[col+colnum], unitelemt);
			if (user_format)
				strcpy(valuefmt[colnum], cfmt[colnum+col]);
			else {
			  IBISFORMAT( valuefmt[colnum],format[IFMT_SIZE*colm], csize[col+colnum] )
			}
			sprintf(fmtelemt,"C:%-d", colm);
			sprintf(headerStr+coffset[colnum],"%*s",csize[col+colnum], fmtelemt);

			sprintf(fmtelemt,"%*s",csize[col+colnum], colformat);
			strcat(formatStr, fmtelemt );
		}

		srow1=srow;
		for (rows_left=nr; rows_left>0; rows_left-=rinc)
		{

			rinc = row_inc;
			if (rows_left < rinc) rinc = rows_left;

			if (col_header)
			{
				outmessage( " ", " ");
				sprintf( outStr, "Rows: %-d:%-d", srow1, srow1+rinc-1 );
				outmessage( outStr, " ");
				outmessage( lineStr, " " );
				outmessage( headerStr, " ");
				if (disp_formats) outmessage( formatStr, " ");
				if (disp_groups) outmessage( groupStr, " ");
				if (disp_units) outmessage( unitStr, " ");
				outmessage( lineStr, " " );
			}
			/* get the column values */
	
			for (colnum = 0; colnum<colperrow && col+colnum<nc; colnum++)
			{
				colm = ccnt ?   cols[col+colnum] : col+scol+colnum ;
				IBISColumnRead( in, (char*) buffer[colnum], colm, srow1, rinc );
				dataPtr[colnum] = (char *)buffer[colnum];
			}
			
	
			/* print the column values */
	
			for (row=0; row<rinc; row++)
			{

				/* strcpy(valueStr," "); */
				valueStr[0]='\0';
				for (colnum = 0; colnum<colperrow && col+colnum<nc; colnum++)
				{
					colm = ccnt ? cols[col+colnum] : col+scol+colnum ;
					IBISPRINT ( valueelemt, 
					   dataPtr[colnum], format[IFMT_SIZE*colm], valuefmt[colnum])
					valueelemt[csize[col+colnum]]='\0';
					strcat(valueStr, valueelemt);
					dataPtr[colnum] += colsize[colnum];
				}
				/* make sure no bad chars */
				for (i=0;i<rowsize && valueStr[i];i++)
				   if (!isprint(valueStr[i]))
					valueStr[i]='.';
				outmessage( valueStr, " ");
				if (space && !((srow1 + row)%space)) outmessage(" "," ");
			}
			srow1 += rinc;
		}
	}

	/* close up shop */

	IBISFileClose( in, 0 );
	if (fd) fclose(fd);
}

/* go through C-string format and grab each '%' element */

static void parse_user_format(userfmt,cfmt,csize)
char *userfmt;
char cfmt[MAXCOLPERROW][20];
int *csize;
{
	int len,index=0;
	char *ptr,*endpt,*charpt,*start;
	char fmt[80],test[100];
	
	for (ptr=userfmt; *ptr; ptr=endpt,csize++,index++)
	{
		start = ptr;
		endpt = strchr(ptr,'%'); /* find this % */
		if (!endpt) endpt = ptr+strlen(ptr);
		else endpt++; /* go past it */
                charpt = strchr(ptr,'s'); /* also, find s */
		ptr = endpt;
		endpt = strchr(ptr,'%'); /* find next % */
		if (!endpt) endpt = ptr+strlen(ptr);
		len = endpt - start;	
		strncpy(fmt,start,len);
		fmt[len]='\0';
                if (!charpt || charpt>endpt) sprintf(test,fmt,0.0);
                else sprintf(test,fmt,"woof");
		*csize = strlen(test);
		strcpy(cfmt[index],fmt);
	}
}


static void show_header(ibis,nr,nc,version)
int ibis;
int nr;
int nc;
char *version;
{
	char org[10],type[201];
	char outStr[200];

	IBISFileGet( ibis, IFILE_ORG, org,  1, 1, 9 );
	IBISFileGet( ibis, IFILE_TYPE, type,  1, 1, 200 );
	if (!type[0]) strcpy(type,"NONE");
	
	outmessage( " ", " ");
	sprintf( outStr, "Number of Rows:%-d  Number of Columns: %-8d", nr, nc );
	outmessage( outStr, " ");
	sprintf( outStr, "File Version:%s  Organization:%s  SubType:%s",
		 version, org, type );
	outmessage( outStr, " ");
}


static int show_groups(ibis,fmtstr,filegrp,type)
int ibis;
char *fmtstr;
char *filegrp;
char *type;
{
	char groupname[MAXGRPNAME+1];
	int ngcols,ngroups,group,i;
	int cols[1024];
	char valueelemt[200],outStr[200];

	ngroups = IBISFileGet( ibis, filegrp, groupname,  1, 0, MAXGRPNAME );
	if (ngroups<0) return ngroups;
	for (group=0;group<ngroups;group++)
	{
		ngcols = IBISFileGet( ibis, filegrp, groupname, group+1, 1, MAXGRPNAME );
		sprintf( outStr, fmtstr, groupname );
		ngcols=IBISColumnFind( ibis, type, groupname, cols, 1, 1024);
		if (ngcols<0) 
		{
			IBISSignal( ibis, ngcols, 0);
			continue;
		}
		for (i=0;i<ngcols;i++)
		{
			sprintf(valueelemt, " %d", cols[i]);
			strcat(outStr, valueelemt);
		}
		outmessage( outStr, " ");
	}
	
	return ngroups;
}

#define MAX_PRE_LINES 5
#define MAX_PRE_LENGTH 200

static void show_preamble()
{
	char preamble[MAX_PRE_LINES][MAX_PRE_LENGTH];
	int count,def,i;

	/* print out any user preamble first */
	zvparm("preamble", preamble, &count, &def, MAX_PRE_LINES, MAX_PRE_LENGTH);
	if (!def && strlen(preamble[0])>0)
	{
		for (i=0;i<count;i++)
			outmessage(preamble[i]," ");
	}

}


void outmessage(char* msg,char* dummy)
{
	if (fd)
	{
		fprintf(fd,"%s\n",msg);
	}
	else zvmessage(msg,dummy);
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_copy.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>


/************************************************************************
 ***                                                                  ***
 ***                           IBIS-COPY                              ***
 ***                                                                  ***
 ************************************************************************/
 

int copy_file()
{
	int inunit,outunit,status;
	int cols[1024],outcols[1024];
	int in, out,def;
	int incol,outcol;
	int nc_file,nr_file,nc=1, nr=1, sr,sc,col,ncout;
	int colsize=1;
	int row, row_inc, nrow;
	int ccnt,occnt,old_in,old_out;
	char type[201];
	char *format,*outformat=(char *)0, org[10],*mode;
	char inversion[10],cform[20];
	double buffer[DBUFSIZE]; /* force alignment */

	/* open the input IBIS file, abort on error */
	
	zvunit( &inunit, "inp", 1, NULL);
	status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
	if (status!=1)  IBISSignalU( inunit, status, 1);
	
	/* get the input row & column counts */
	
	IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
	IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );

	/* pre-format 0ld IBIS */
	IBISFileGet( in, IFILE_VERSION, inversion,  1, 1, 9 );
	old_in = !strcmp(inversion,IVERSION_1);
	old_out = zvptst(IVERSION_1);
	if (old_in) pre_format(in);
	

	zvp( "sr", &sr, &def );
	zvp( "nr", &nr, &def );
	if (!nr) nr=nr_file;
	if (sr > nr_file) sr=nr_file;
	if (sr + nr - 1 > nr_file)
		nr = nr_file + 1 - sr;
		
	zvp( "sc", &sc, &def );
	zvp( "nc", &ncout, &def );
	if (!ncout) ncout=nc_file;
	nc = ncout;
	zvp( "incols", cols, &ccnt );
	if (cols[0]) nc=ccnt; 
	else ccnt=0;
	
	zvp( "outcols", outcols, &occnt );
	if (!outcols[0]) occnt = 0;

	if (ccnt && occnt && occnt!=ccnt)
	{
		zvmessage("#incols and #outcols are unequal"," ");
		zabend();
	}
	if (sc > nc_file) sc=nc_file;
	if (sc + nc - 1 > nc_file)
		nc = nc_file + 1 - sc;

	/* get the input format string */
	format = new_format_string(nc_file,IFMT_REAL,0);
	if (!format)
	{
		zvmessage( "unable to allocate format string", 0);
		zabend();
	}
	IBISFileGet( in,  IFILE_FORMATS, format, 1, nc_file, IFMT_SIZE );

	/*
	 *  The only case where we have to be careful about
	 *  formatting the output file is when the input is
	 *  IBIS-2 and the out is IBIS-1; in this case we
	 *  we the "intcol" and "a4col" parms to set up the file.
	 */
	if (!(!old_in && old_out))
	{
		outformat = new_format_string(ncout,IFMT_REAL,0);
		if (!outformat)
		{
			zvmessage( "unable to allocate outformat string", 0);
			zabend();
		}
		for (col=0;col<nc;col++)
		{
			incol = ccnt ? cols[col]-1: col;
			outcol = occnt ? outcols[col]-1: col;
			strcpy(outformat+(outcol*IFMT_SIZE),
				format + (incol*IFMT_SIZE) );
		}
	}
	
	/* open the output file, or abort on error */
	
	zvpone( "org", org, 1, 0 );
	mode = old_out ? IMODE_OWRITE : IMODE_WRITE;
	zvunit( &outunit, "out", 1, NULL);
	status = IBISFileOpen( outunit, &out, mode, ncout, nr, outformat, org );
	if (status!=1)  IBISSignalU( outunit, status, 1);
	
	if (!old_in && old_out) pre_format(out);

	/* Set the File Type */	
	zvp("type",type,&def);
	if (type[0] && type[0]!=' ')
		IBISFileSet(out,IFILE_TYPE,type,0);

	
	/* 
	 * copy the columns
	 */

	for (col=0; col<nc; col++)
	{
		incol = ccnt ? cols[col] : col+1;
		outcol = occnt ? outcols[col] : col+1;

		if (old_out) /* The output determines buffer */
		{
		IBISColumnGet( out, ICOLUMN_U_FORMAT, cform, outcol );		
		status = IBISColumnSet( in, ICOLUMN_U_FORMAT, cform, incol );		
		if (status !=1 ) IBISSignal(in,status,1);
		}
		else  /* The input determines buffer */
		{
		IBISColumnGet( in, ICOLUMN_U_FORMAT, cform, incol );		
		status = IBISColumnSet(out, ICOLUMN_U_FORMAT, cform, outcol );		
		if (status !=1 ) IBISSignal(out,status,1);
		}
		IBISColumnGet( in, ICOLUMN_U_SIZE, &colsize, incol ); /* size of element */
		
		/* Compute # rows that will fit into buffer : */
		row_inc = (IBUFSIZE + colsize  - 1)/colsize ;
		
		for (row = 0; row<nr; row += row_inc)
		{
			nrow = MIN( nr-row , row_inc);
			
			status = IBISColumnRead( in, (char *)buffer, incol, row+sr, nrow );
			if (status !=1 ) IBISSignal(in,status,1);
			status = IBISColumnWrite( out, (char *)buffer, outcol, row+1, nrow );
			if (status !=1 ) IBISSignal(out,status,1);
			
		}
	}

	if (zvptst("gcopy") && !old_out)
	{
		status=IBISGroupTransfer( in, out, ITYPE_GROUP, cols, outcols, nc);
		if (status < 0) 
		{
			IBISSignal(in,status,0);
			zvmessage("Non-fatal; continuing..."," ");
		}
	}
	if (zvptst("ucopy") && !old_out)
	{
		status=IBISGroupTransfer( in, out, ITYPE_UNIT, cols, outcols, nc);
		if (status < 0) 
		{
			IBISSignal(in,status,0);
			zvmessage("Non-fatal; continuing..."," ");
		}
	}

	/* close up shop */

	IBISFileClose( in, 0 );
	status = IBISFileClose( out, 0 );
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	return 1;
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_catenate.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-CATENATE                          ***
 ***                                                                  ***
 ************************************************************************/
 
int catenate_files()
{
	int inunit,outunit,status;
	int in, out,def;
	int incol,outcol,numout;
	int nc=0, nr=0,nr_file,nc_file, col;
	int colsize=1;
	int row, row_inc, nrow,cur_col,i;
	int srow[MAX_FILE],scol[MAX_FILE];
	int by_row;
	char type[201];
	char *format, org[10];
	char cform[20];
	char *mode;
	double buffer[DBUFSIZE]; /* force alignment */

	zvpcnt("INP",&numout);
	by_row = zvptst("BYROW");

	/* open the input IBIS file, abort on error */
	
	srow[0]=1;
	scol[0]=1;	
	if (by_row)
	{
		zvunit( &inunit, "inp", 1, NULL);
		status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
		if (status!=1)  IBISSignalU( inunit, status, 1);
		
		/* get the input column counts */
		
		IBISFileGet( in, IFILE_NC, &nc, 1, 1, 0 );
		
		/* get the input format string */
		
		format = new_format_string(nc,IFMT_REAL,0);
		if (!format)
		{
			zvmessage( "unable to allocate format string", 0);
			zabend();
		}
		IBISFileGet( in,  IFILE_FORMATS, format, 1, nc, IFMT_SIZE );
		IBISFileClose(in,0);
	
		/* Determine total number of rows from all files */
		for (i=1,nr=0;i<=numout;i++)
		{
			zvunit( &inunit, "inp", i, NULL);
			status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
			if (status!=1)  IBISSignalU( inunit, status, 1);
			IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );
			nr += nr_file;
			IBISFileClose(in,0);
			scol[i]=1;
			srow[i]=srow[i-1]+nr_file;
		}

	}
	else
	{
		/* allocate the input format string */		
		format = new_format_string(1024,IFMT_REAL,0);
		if (!format)
		{
			zvmessage( "unable to allocate format string", 0);
			zabend();
		}
	
		/* Determine file format string and number of Rows & Cols */
		for (i=1,cur_col=1;i<=numout;i++)
		{
			zvunit( &inunit, "inp", i, NULL);
			status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
			if (status!=1)  IBISSignalU( inunit, status, 1);
			IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );
			IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
			IBISFileGet( in,  IFILE_FORMATS,
				format+IFMT_SIZE*(cur_col-1), 1, nc_file, IFMT_SIZE );
			cur_col += nc_file;
			nc += nc_file;
			nr = (nr > nr_file) ? nr : nr_file;
			srow[i]=1;
			scol[i]=scol[i-1]+nc_file;
			IBISFileClose(in,0);
		}

	}
	
	/* open the output file, or abort on error */
	
	zvpone( "org", org, 1, 0 );
	zvunit( &outunit, "out", 1, NULL);
	if (zvptst("ibis-2")) mode=IMODE_WRITE;
	else mode=IMODE_OWRITE;

	status = IBISFileOpen( outunit, &out, mode, 
		nc, nr, format, org );
	if (status!=1)  IBISSignalU( outunit, status, 1);
	
	/* Set the File Type */	
	zvp("type",type,&def);
	if (type[0] && type[0]!=' ')
		IBISFileSet(out,IFILE_TYPE,type,0);
	
	/* 
	 * copy the columns of all files
	 */

	for (i=1;i<=numout;i++)
	{
		zvunit( &inunit, "inp", i, NULL);
		status = IBISFileOpen( inunit, &in, IMODE_READ, gr1dim,0,0,0);
		if (status!=1)  IBISSignalU( inunit, status, 1);
		IBISFileGet( in, IFILE_NR, &nr_file, 1, 1, 0 );
		IBISFileGet( in, IFILE_NC, &nc_file, 1, 1, 0 );
	
		for (col=0; col<nc_file; col++)
		{
			outcol = col+scol[i-1]; incol = col+1;
			IBISColumnGet( out, ICOLUMN_FORMAT, cform, col+scol[i-1] );		
			status = IBISColumnSet(in, ICOLUMN_U_FORMAT, cform, col+1 );		
			if (status !=1 ) IBISSignal(out,status,1);
			IBISColumnGet( in, ICOLUMN_U_SIZE, &colsize, col+1 );
			
			/* Compute # rows that will fit into buffer : */
			row_inc = (IBUFSIZE + colsize  - 1)/colsize ;
			
			/* Copy the data of a single column */
			for (row = 0; row<nr_file; row += row_inc)
			{
				nrow = MIN( nr_file-row , row_inc);
				
				status = IBISColumnRead( in, (char *)buffer,
					incol, row+1, nrow );
				if (status !=1 ) IBISSignal(in,status,1);
				status = IBISColumnWrite( out, (char *)buffer,
					outcol, row+srow[i-1], nrow );
				if (status !=1 ) IBISSignal(out,status,1);
				
			}

			/* transfer the group memberships */
			if (zvptst("gcopy") )
			{
				status=IBISGroupTransfer( in, out, ITYPE_GROUP, &incol, &outcol, 1);
				if (status < 0) IBISSignal(in,status,0);
			}
			if (zvptst("ucopy") )
			{
				status=IBISGroupTransfer( in, out, ITYPE_UNIT, &incol, &outcol, 1);
				if (status < 0) IBISSignal(in,status,0);
			}

		}
	
		IBISFileClose( in, 0 );
		
	} /* end of file loop */
	
	/* close up shop */
	status = IBISFileClose( out, 0 );
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	return 1;
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_groups.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "ibis_local.h"
#include <zvproto.h>
#include <string.h>

/************************************************************************
 ***                                                                  ***
 ***                           IBIS-GROUP                             ***
 ***                                                                  ***
 ************************************************************************/ 

modify_group()
{
	int outunit;
	int status,def,count;
	int ibis;
	int cols[20],ncols;
	int *colptr=(int*)0;
	char type[10];
	char name[33];
	char expr[80];
	char *exprptr=(char *)0;
	
	status=zvunit( &outunit, "inp", 1, NULL);
	if (status!=1) zvsignal( outunit, status, 1);
	
	/* open the output file, or abort on error */
	
	status = IBISFileOpen(outunit, &ibis, IMODE_UPDATE, gr1dim, 0, 0, 0);
	if (status!=1) IBISSignalU( outunit, status, 1);
	
	zvp("type", type, &def );
	zvp("name", name, &def );
	
	if (zvptst("create"))
	{
		zvparm("cols",cols,&ncols,&def,0,0);
		if (!def && ncols && cols[0]) colptr=cols;
		zvparm("expr",expr,&count,&def,0,0);
		if (!def && expr[0] && expr[0]!=' ') exprptr = expr;
		
		count=IBISGroupNew( ibis, type, name, colptr, ncols , exprptr);
		if (count<0) IBISSignal( ibis, count, 0);
	}
	else
	{
		status=IBISGroupDelete(ibis, type, name );
		if (status!=1) IBISSignalU( outunit, status, 1);
	}
	
	/* close up shop */
	
	status = IBISFileClose( ibis, 0 );
	if (status != 1) IBISSignalU( outunit, status, 1);
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibis_local.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * ibis_local.h: Local Include file for program IBIS
 */

#ifndef _ibis_local_h
#define  _ibis_local_h 1

/* INCLUDES */
#include "ibisfile.h"
#include "ibiserrs.h"
#include <ctype.h>

/* DEFINES */
#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#define MAX_FMT 1024
#define MAX_COLUMN 100
#define MAXGRPNAME 32
#define MAX_DATA 100
#define MAX_FILE 20
#define DBUFSIZE 1000
#define IBUFSIZE (DBUFSIZE*sizeof(double))
#define MAXCOLSIZE 80
#define ROWSIZE 250
#define MAXCOLPERROW 50

/* DECLARES */
extern int gr1dim;
char *new_format_string(/* int nc,char * deffmt,char *str */);

#endif /* _ibis_local_h */

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ibis.pdf
process help=*

subcmd GEN
	parm out string count=1
	parm nr integer def=10
	parm nc integer def=10
	parm org keyword valid=(row,column) default=column
	parm format (string,4)  count=1:100 default=" "
	parm fmtcols integer count=1:100 valid=0:1024 default=0
	parm deffmt (string,4) count=1 default="REAL"
	parm type (string,200)  default=" "
	parm version keyword valid=(ibis-1,ibis-2) def=ibis-2
	parm data real count=1:600 def=0
	parm datacols integer count=1:100 default=0
	parm string (string,80) count=1:200 def=" "
	parm strcols integer count=1:100 default=0
	parm indexcol integer count=1 default=0
end-sub

subcmd LIST
	parm inp string count=1
	parm sr integer def=1
	parm sc integer def=1
	parm nr integer count=1 def=0
	parm nc integer count=1 def=0
	parm cols integer valid=0:1024 count=1:20 def=0
	parm screen integer valid=80:250 count=1 def=80
	parm space integer count=1 def=0
	parm csize integer valid=3:80 count=1:50 def=12
	parm units keyword valid=(units,nounits) count=1 def=nounits
	parm groups keyword valid=(groups,nogroups) count=1 def=nogroups
	parm formats keyword valid=(formats,noformat) count=1 def=noformat
	parm header keyword valid=(header,noheader) count=1 def=header
	parm intcols integer valid=0:40 count=0:40 def=0
	parm a4cols integer valid=0:40 count=0:40 def=0
	parm cformat string default=""
	parm colhead keyword valid=(colhead,nocol) count=1 def=colhead
	parm preamble string count=1:8 def=""
	parm outfile string def=" "
end-sub

subcmd COPY
	parm inp string count=1
	parm out string count=1
	parm sr integer def=1
	parm sc integer def=1
	parm nr integer count=1 def=0
	parm nc integer count=1 def=0
	parm incols integer count=1:256 default=0
	parm outcols integer count=1:256 default=0
	parm org keyword valid=(row,column) default=column
	parm version keyword valid=(ibis-1,ibis-2) def=ibis-2
	parm type (string,200)  default=" "
	parm intcols integer valid=0:100 count=1:100 def=0
	parm a4cols integer valid=0:100 count=1:100 def=0
	parm groups keyword valid=(gcopy,nogcopy) def=gcopy
	parm units keyword valid=(ucopy,noucopy) def=ucopy
end-sub

subcmd CATENATE
	parm inp string count=1:20
	parm out string count=1
	parm org keyword valid=(row,column) default=column
	parm type (string,200)  default=" "
	parm groups keyword valid=(gcopy,nogcopy) def=gcopy
	parm units keyword valid=(ucopy,noucopy) def=ucopy
	parm mode keyword valid=(byrow,bycolumn) def=byrow
	parm version keyword valid=(ibis-1,ibis-2) def=ibis-2
end-sub


subcmd GROUP
	parm inp string count=1
	parm name string count=1
	parm cols integer count=1:20 def=0
	parm expr string count=0:1 def=--
	parm action keyword valid=(create,delete) def=create
	parm type keyword valid=(unit,group) def=group
end-sub

!Global parameter
parm gr1dim integer default=0

end-proc


.TITLE
VICAR Program IBIS
.HELP
IBIS is an IBIS-2 program, designed to provide basic file utilities
for IBIS-2 and IBIS-1 tabular and graphics files.

When IBIS is invoked on the command line, the mode of operation
is determined by the subcommand which is used.  For help on the
parameters of IBIS-GEN, type -GEN (etc.). For general help on the 
operation of the subcommand, type HELP GEN (etc.)
.page
REVISION HISTORY

   Written by:            N. D. Ritter     Mar    1994
   Cognizant Programmer:  N. D. Ritter     Mar    1994
   Latest revision:       2                Jun    1994
.LEVEL1
!
.VAR gr1dim 
GRAPHICS-1 dimension.

.SUBCMD GEN
Create a new IBIS file.
.VAR OUT -GEN
Output file name
.VAR org -GEN
Org. by ROW or COLUMN?
.VAR format -GEN
Column Formats
.VAR version -GEN
IBIS-1 or IBIS-2?
.VAR fmtcols -GEN
Which cols are formatted?
.VAR deffmt -GEN
Default Column Format
.VAR type -GEN
IBIS-2 Subtype
.VAR nr -GEN
Number of Rows
.VAR nc -GEN
Number of Columns
.VAR data -GEN
Numerical Data for cols
.VAR datacols -GEN
Columns to put data in
.VAR string -GEN
String Data for cols
.VAR strcols -GEN
Columns to put strings in
.VAR indexcol -GEN
Place Index #'s in Column

.subcmd LIST
List column values of IBIS file.
.VAR inp -LIST
Input file
.VAR sr -LIST
Starting Row
.VAR sc -LIST
Starting Column
.VAR nr -LIST
Number of Rows
.VAR nc -LIST
Number of Columns
.VAR cols -LIST
Specific Columns to list
.VAR screen -LIST
Screen Size
.VAR space  -LIST
Blank line interval
.VAR csize  -LIST
Physical column sizes
.VAR units -LIST
List UNITS ?
.VAR groups  -LIST
List GROUPS ?
.VAR formats  -LIST
List FORMATS ?
.VAR header  -LIST
List Header Info?
.VAR colhead  -LIST
List Column Header?
.VAR intcols  -LIST
IBIS-1 INTEGER Columns
.VAR a4cols  -LIST
IBIS-1 ASCII Columns
.VAR cformat  -LIST
C-formatting string.
.VAR preamble  -LIST
User-defined header.
.VAR outfile  -LIST
Optional output file.

.subcmd copy
Copy columns of IBIS file.
.VAR inp  -COPY
Input IBIS file
.VAR out   -COPY
Output IBIS file
.VAR sr  -COPY
Starting Row
.VAR sc   -COPY
Starting Column
.VAR nr   -COPY
Number of Rows
.VAR nc   -COPY
Number of Columns
.VAR cols  -COPY
Specific Columns to copy
.VAR outcols   -COPY
Destination Columns 
.VAR org -COPY
Org. by ROW or COLUMN?
.VAR version -COPY
IBIS-1 or IBIS-2?
.VAR type -COPY
IBIS-2 Subtype
.VAR groups -COPY
Copy Column Groups?
.VAR units -COPY
Copy Column Units?

.subcmd catenate
Catenate columns of several IBIS files.
.VAR inp  -CATENATE
Input IBIS file
.VAR out   -CATENATE
Output IBIS file
.VAR org -CATENATE
Org. by ROW or COLUMN?
.VAR type -CATENATE
IBIS-2 Subtype
.VAR groups -CATENATE
Copy Column Groups?
.VAR units -CATENATE
Copy Column Units?
.VAR mode -CATENATE
Catenate by ROWS or COLUMNS?
.VAR version -CATENATE
IBIS-1 or IBIS-2?

.subcmd group
Defines/Delete an IBIS GROUP.
.VAR inp -GROUP
Input IBIS-2 file
.VAR type -GROUP
Type of Group
.VAR name -GROUP
Name of group
.VAR cols -GROUP
Columns defining group
.VAR expr -GROUP
Expression defining group
.VAR action -GROUP
Create or Delete this group?

.LEVEL2

.VAR gr1dim
GRAPHICS-1 Files do not contain any NC (dimension) information. This
parameter allows the specification of the GRAPHICS-1 dimension of the
file.

.SUBCMD GEN
IBIS-GEN: Creates a new IBIS-2, IBIS-1 or GRAPHICS file.

CALLING SEQUENCE:

  IBIS-GEN OUT=OUT.IBIS PARAMS
.page
OPERATION:

  IBIS-GEN will create an IBIS interface or graphics file with
NC columns and NR rows. If this is an IBIS-2 format file (default),
then the format of each column may be specified with the FORMAT
parameter. 

  IBIS-2 files may be organized by contiguous rows or columns; this
may be specified using the 'ROW or 'COLUMN keyword value.

  To create an old IBIS-1 format tabular file, use the 'IBIS-1 keyword;
to make the file a GRAPHICS-1 format file use both the 'IBIS-1 keyword
and the 'ROW organization keyword. Note however, that the old IBIS-1
format is not a very robust format for platform-independent operation,
and so its use in future applications is discouraged.

.VAR OUT -GEN
Output file name
.VAR org -GEN
Org. by ROW or COLUMN?
.VAR format -GEN
By default all columns have the format specified by the DEFFMT
parameter; To override this formatting on a column-by-column
basis use the FORMAT parameter. If the FMTCOLS parameter is
not specified this will result in the formats of columns
(1,2,3...) being overridden.
.VAR version -GEN
IBIS-1 or IBIS-2?
.VAR nr -GEN
Number of Rows
.VAR nc -GEN
Number of Columns
.VAR type -GEN
IBIS-2 Subtype (e.g. LUT, STATISTICS, etc).
.VAR fmtcols -GEN
By default all columns have the format specified by the DEFFMT
parameter; To override this formatting on a column-by-column
basis use the FORMAT parameter. If the FMTCOLS parameter is
specified, the formats listed in FORMAT(1),FORMAT(2)... will
be assigned to columns FMTCOLS(1), FMTCOLS(2), and so on.
.VAR deffmt -GEN
Default Column Format. This is the format all columns in the
file will be assigned, unless overridden by the FORMAT parameter.
.VAR data -GEN
Numerical Data for cols
.VAR datacols -GEN
Columns to put data in
.VAR string -GEN
String Data for cols
.VAR strcols -GEN
Columns to put strings in
.VAR indexcol -GEN
Place Index #'s in Column

.subcmd LIST
IBIS-LIST lists out the values of an IBIS tabular or GRAPHICS file

CALLING SEQUENCE:

  IBIS-LIST INP=INP.IBIS PARAMS
.page
OPERATION:

  IBIS-LIST will list the column values of an IBIS tabular or
GRAPHICS file. For IBIS-1 format graphics files the NC (dimension)
of the file is not stored in the file, but must be specified using
the GR1DIM parameter.  A window of the IBIS file may be displayed
using the SR,SC,NR,NC parameters (for Starting Row/column, Number
of Rows/columns).  The COLS parameter allows the specification of
only specific columns.

  IBIS-2 files, in addition to having column values, also have
column group-names and units of measurement. These may be listed
above each column by specifying the 'GROUP and 'UNIT keywords.
The format of each column may be displayed using the 'FORMAT keyword.

.PAGE
FORMATTING

  There are several ways of adjusting the manner in which the
IBIS column data is diplayed. The 'NOHEAD key removes the information
header at the beginning, while the 'NOCOL key causes the column
header to be omitted. In its place the user may place their own
header using the PREAMBLE string parameter, which permits up to
5 lines of text to be displayed before the file is printed.
The overall column-size in characters may be modified by the CSIZE
parameter. If a single value is specified this will apply to all
of the columns, or each column-size may be specified separately. 
The SCREEN parameter specifies the width of a display-line, which
may be changed from 80 to 132 if a longer format is desired.

  By default IBIS-LIST displays the column-values using specific
floating-point and integer options, and right-justifies everything.
These formatting features may be overridden with the CFORMAT paramter,
which takes as input a "C" style data formatting string.
.page
FORMATTING

For example, the command

  ibis-list temp cform="c1=%03d %-7d %6.3f '%6.6s'" 'nohead 'nocol

Results in an output like this:

  c1=001 0        0.000 '   one'
  c1=002 0        0.000 '   two'
  ...

For those unfamiliar with C-formatting, each "%" refers, roughly,
to the corresponding output value, and is followed by formatting
parameters. For example, "%6.3f" is analogous to "F6.3" in FORTRAN.
All other characters in the string are interpreted literally.
Here is a guide to common formatting parameters:
.page
FORMATTING

         C-FORMAT    FORTRAN     Interpretation
        ----------  ----------   ------------------------
          %7d          I7        7 decimal integer
          %07d         I7.7      7 decimal integer,zero-padded
          %-7d         ----      7 decimal integer,left-justified
          %7x          Z7        7 decimal integer,in hexadecimal
          %7o          O7        7 decimal integer,in octal
          %7.2f        F7.2      7 floating pt, 2 decimal pts
          %7.2e        E7.2      7 scientific notation, 2 dec. pts
          %7.7s        A7        7 ASCII character string
          %8.7s        A8        7 char. string, padded to 8
          %-8.7s       ----      7 char. string, padded & l-justified.
.page
FORMATTING

  Notice: do not try to use the CFORMAT parameter alone to coerce
an IBIS-1 column to an ASCII or integer value. On non-vax platforms
the values are translated to native floating point first, unless
the A4COL or INTCOL parameters are specified. In that case, all
columns in the A4COL list must be passed to a "%s" format and
all INTCOL columns must be passed to a "%d","%x" or "%o" format.

.VAR inp -LIST
Input file
.VAR sr -LIST
Starting Row
.VAR sc -LIST
Starting Column
.VAR nr -LIST
Number of Rows
.VAR nc -LIST
Number of Columns
.VAR cols -LIST
Specific Columns to list
.VAR screen -LIST
Screen Size
.VAR space  -LIST
Blank line interval
.VAR csize  -LIST
Physical column size
.VAR units -LIST
List UNITS ?
.VAR groups  -LIST
List GROUPS ?
.VAR formats  -LIST
List FORMATS ?
.VAR header  -LIST
List Header Info?
.VAR colhead  -LIST
List Column Header?
.VAR intcols  -LIST
IBIS-1 Files do not contain any formatting information. This
parameter allows the specification of which columns were written
using VAX/VMS INTEGER*4 Columns.
.VAR a4cols  -LIST
IBIS-1 Files do not contain any formatting information. This
parameter allows the specification of which columns were written
using VAX/VMS CHARACTER*4 Columns.
.VAR cformat  -LIST
C-formatting string. Overrides standard formatting.
.VAR preamble  -LIST
The PREAMBLE parameter allows the user to specify up to 5 lines of
header to be displayed before any other information is put out.
.VAR outfile  -LIST
By default the listing goes to the standard output file. This
allows you to redirect the output to a text file (the "Beginning
VICAR task" message will not appear in the file).
.subcmd copy
IBIS-COPY copies the column values from one IBIS file to another.

CALLING SEQUENCE:

  IBIS-COPY INP=INP.IBIS OUT=OUT.IBIS PARAMS
.page
OPERATION:

  IBIS-COPY will list the specified column values of an IBIS tabular or
GRAPHICS file to another IBIS file, converting file formats, and file
organization in the process. A window of the IBIS file may be copied
using the SR,SC,NR,NC parameters (for Starting Row/column, Number
of Rows/columns).  The COLS parameter allows the copying of only specific
columns.

  This program has the capability to copy to and from old IBIS-1 format
tabular and GRAPHICS files, allowing for backward compatibility with old
programs and procedures. NOTE: the NC (dimension) parameter is not stored
in GRAPHICS-1 files, but must be explicitly supplied by the user with the
GR1DIM parameter.

  The format of the output columns is determined by the format of the
input columns in most cases. For IBIS-1 input files, since column format
information is not stored, the INTCOLS and A4COLS parameters may be used
to impose a non-REAL format on a column. If the output is also IBIS-1, then
the correct formatting is done there, as well. If the input is IBIS-2,
and the output is IBIS-1, then you must again use the INTCOLS and A4COLS
parameters to set up the desired column format of the output.

.VAR inp  -COPY
Input IBIS file
.VAR out   -COPY
Output IBIS file
.VAR sr  -COPY
Starting Row
.VAR sc   -COPY
Starting Column
.VAR nr   -COPY
Number of Rows
.VAR nc   -COPY
Number of Columns
.VAR cols  -COPY
Specific Columns to copy
.VAR outcols   -COPY
Destination Columns 
.VAR org -COPY
Org. by ROW or COLUMN?
.VAR version -COPY
IBIS-1 or IBIS-2?
.VAR type -COPY
IBIS-2 Subtype (e.g. LUT, STATISTICS, etc).
.VAR groups -COPY
This keyword controls whether the column groups of the new
file are inherited from the old file. 
.VAR units -COPY
This keyword controls whether the column units of the new
file are inherited from the old file. 

.subcmd catenate
IBIS-CATENATE catenates columns of several IBIS files into
a new IBIS file. The concatenation may be by ROWS or COLUMNS.

CALLING SEQUENCE:

  IBIS-CATENATE INP=(IN1,IN2...IN10) OUT=OUT.IBIS PARAMS
.page
OPERATION:

  IBIS-CATENATE will merge all of the rows and columns of
  the input files into the output file. In the default
  'BYROW mode the concatenation is by ROWS, so that successive
  rows of each file are place below the preceding. The number
  of columns does not change in this case. In 'BYCOLUMN mode
  the successive columns of each file are placed to the
  right of the columns of the preceding files. In this
  case the number of columns is the sum of the columns
  in each input file.

.page
OPERATION:

  In 'BYCOLUMN mode the total number of columns may not exceed
  1024, but there are no other restrictions. In 'BYROW mode the
  number of columns is determined by the primary input, and all
  other files are required to be compatible with the column
  formats of the primary input; that is, numerical columns may
  not be concatenated with ASCII columns and vis versa.

  IBIS-1 files may be included in the input, but they will be
  assumed to be REAL format. If this is not the desired effect,
  the IBIS-1 file must be converted to IBIS-2 using IBIS-COPY
  to establish the column formatting. All files output by
  IBIS-CATENATE are IBIS-2 format.

.VAR inp  -CATENATE
Input IBIS file
.VAR out   -CATENATE
Output IBIS file
.VAR org -CATENATE
Org. by ROW or COLUMN?
.VAR type -CATENATE
IBIS-2 Subtype
.VAR groups -CATENATE
Copy Column Groups?
.VAR units -CATENATE
Copy Column Units?
.VAR mode -CATENATE
Catenate by ROWS or COLUMNS?
.VAR version -CATENATE
Create IBIS-1 or IBIS-2 formatted file?

.subcmd group
IBIS-GROUP defines and installs a GROUP definition into an IBIS tabular
or GRAPHICS file.

CALLING SEQUENCE:

  IBIS-GROUP INP=INP.IBIS NAME=Name TYPE=<UNIT,GROUP> COLUMNS=(C1,C2...)
  IBIS-GROUP INP=INP.IBIS NAME=Name TYPE=<UNIT,GROUP> EXPRESSION=<Expr.>

.page
OPERATION:

  IBIS-GROUP will define a named group of columns using either an explicit
  list of columns or a group-expression using pre-existing groups, units,
  or formats. The columns may be explictly specified using the COLUMNS
  parameter, or implicitly defined using the currently defined groups
  in the file (at the very least, FORMATS are always defined in each file).
  An example of a group expression is:
  
  	line | group:sample - [unit:(kg*m)/sec^2]	

  which means: take all columns in group "line" or "sample", but then
  take away all columns which use "(kg*m)/sec^2" as a unit.

  A group may also be deleted using the 'DELETE keyword.
  
  Note that 'format' types may not be created or destroyed.

 
.VAR inp -GROUP
Input IBIS-2 file
.VAR type -GROUP
Type of Group
.VAR name -GROUP
The name given to a group or a unit must begin with an alphanumeric character,
but otherwise the characters following the first character may be ANY printable
character except colons, up to 32 characters total. The complete list of characters
that may be used are:

  [a-z][A-Z][0-9] ~!@ # $ % ^ & * _ + - = (){}[]<> | ? ; , . " ' ` \/ 

Here are some valid examples:

	kg*m/sec^2   
	2  
	a_long_Long_LOng_LONg_LONG_name! 
	has_[brackets]_and_{braces}(etc)!@#$%^&*}\/?=
	A   name    with  spaces

Here are some invalid examples:

	%starts_with_NON_alphanumeric
	has:colons:in:its:name
	a_long_Long_LOng_LONg_LONG_Whoops!_TOO_long_name!
	
.VAR cols -GROUP
Columns defining group
.VAR expr -GROUP
Expression defining group

  An example of a group expression is:

        line | group:sample - [unit:(kg*m)/sec^2]

  which means: take all columns in group "line" or "sample", but then
  take away all columns which use "(kg*m)/sec^2" as a unit.

.VAR action -GROUP
Create or Delete this group?

.VAR inp -GROUP
Input IBIS-2 file for installing/deleting groups.
.VAR type -GROUP
Type of Group to create/delete. 
Note that 'format' types may not be created or destroyed.

.VAR name -GROUP
Name of group
.VAR cols -GROUP
Columns defining group
.VAR expr -GROUP
Expression defining group.

The expression string EXPR consists of a simple series of relative or 
"full-pathname" group names, separated by operators. If a given groupname
appearing in an expression contains any characters other than alphanumeric,
dollar$signs and under_score (and the colon for full-pathnames), then they
must be quoted.  To quote a name you may nest it between (parentheses),
[brackets], {curly brackets}, 'single quotes' or "double quotes". The choice
of quotation marks is only driven by the condition that the matching end-quote
character not occur within the name being quoted. Here are some examples of
group names as used within an expression:
	
	This_name_doesnt_need_quotes
	(This   one  has   spaces  so  it  does)
	[This one has (parens) in its name]
	"$%^*(){} in double quotes"

The currently supported operators for group expressions are (&, |, and -)  for
ordered intersection, concatenization, and set-differencing (A but not B)
respectively. The ( *  and + ) characters may be used as an aliases for
( &  and | ), respectively.

The expressions are evaluated immediately, from left to right, and no
expression-grouping is (currently) permitted. If the group name is not in
full-pathname format, the group will be searched for in the standard order.

Some examples of group-expressions:

	format:BYTE & group:Image1

resulting group: All BYTE format columns belonging to group named "Image1"

	line | group:sample - [unit:(kg*m)/sec^2]	

resulting group: take all of the columns from local group "line", and all of group 
"sample", but then subtracting away any column which uses "(kg*m)/sec^2" as a
unit of measurement. Note that the unit name was placed between brackets, as
it contains the characters "*/^()".

Although both upper and lower case letters may be used in a group name, recognition
of names during searches is case-insensitive.
.VAR action -GROUP
Create or Delete this group?
.END
$ Return
$!#############################################################################
$Imake_File:
$ create ibis.imake

#define PROGRAM ibis

#define R2LIB

#define MODULE_LIST \
	ibis_main.c \
	ibis_gen.c \
	ibis_list.c \
	ibis_copy.c \
	ibis_catenate.c \
	ibis_groups.c
#define INCLUDE_LIST ibis_local.h
#define MAIN_LANG_C
#define USES_C

#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL

$ Return
$!#############################################################################
$Test_File:
$ create tstibis.pdf
procedure
refgbl $autousage
refgbl $syschar
body
local listcmd string init="dcl type"

let $autousage="none"
let _onfail="continue"

if ($syschar(1) = "UNIX")
  let listcmd="ush cat"
end-if

write "*********************************************"
write "************ Error Checking Tests       *****"
write "*********************************************"
write "**** All of these tests should safely abend *"
write "**** with an [IBIS-KEYWORD] explanation   ***"
write "*********************************************"
 ibis-gen oldibis nc=200 nr=100 'ibis-1 'column
 ibis-gen temp nc=5000 nr=100 'ibis-2 'column
 ibis-gen temp  nr=30 form=("BYTE","HALF","BAD")

let _onfail="return"   !No failures after this point

write "*********************************************"
write "************ IBIS-1 COLUMNS (INTERFACE) *****"
write "*********************************************"

 ibis-gen oldibis nc=10 nr=6 'ibis-1 'column +
   format=(FULL,A4) fmtcol=(2,4) datacols=(2,3,5) +
   data=(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5) strcol=4 +
   string=(THIS,IS,AN,ASCI,COLM) indexcol=1
 ibis-list oldibis a4col=4 intcol=2 'formats 
 ibis-copy oldibis newibis a4col=4 intcol=2
 ibis-list newibis 'format 

  !Test new output file option
 ibis-list newibis 'format  outfile=tmp.txt
write "*********************************************"
write "This next command should give the same result as above"
write "*********************************************"
 &listcmd tmp.txt

 ibis-copy newibis oldibis2 a4col=4 intcol=2 'ibis-1
 ibis-list oldibis2 a4col=4 intcol=2 

  ibis-gen oldibis nc=3 nr=6 datacol=(1,2,3) +
    data=(1,2,3,4,5,6,7,8,9)
  ibis-gen oldibis2 nc=3 nr=6 datacol=(1,2,3) +
    data=(1,2,3,4,5,6,7,8,9)
  ibis-catenate (oldibis, oldibis2) oldibis3
  ibis-list oldibis3 
  ibis-catenate (oldibis, oldibis2) oldibis3  'bycolum
  ibis-list oldibis3 

write "*********************************************"
write "************ IBIS-1 ROW (GRAPHICS) **********"
write "*********************************************"

 ibis-gen oldibis nc=20 nr=30 'ibis-1 'row
 ibis-list oldibis gr1dim=20 

write "*********************************************"
write "************ IBIS-2 COLUMNS *****************"
write "*********************************************"

 ibis-gen temp nc=1024 nr=1 'column 
 ibis-gen temp nc=20 nr=30 type=LOOKUP 'column
 ibis-list temp 
 ibis-gen temp  nc=8 nr=30 'column +
   form=("FULL","BYTE","HALF","A20","DOUB", "FULL","HALF","HALF") +
   indexcol=1 datacols=(2,3,5) +
   data=(1,2,3,1,2,3,1,2,3,4,5,6,4,5,6,4,5,6) strcol=4 +
   string=("THIS IS AN","ASCII COLUMN","The End.")   
 ibis-list temp csize=(6,5,5,16,7,6,5,5) 'formats 
 ibis-copy temp temp1 'row
 ibis-list temp1 csize=(6,5,5,16,7,6,5,5) 'formats 
 ibis-copy temp temp1  incol=(1,2,3,4) outcol=(4,3,2,1)
 ibis-list temp1 'formats 
 ibis-copy temp temp1  incol=(1,2,3,4,5) outcol=(5,4,3,2,1) +
 	a4col=2 intcol=(5,4,3) 'ibis-1
 ibis-list temp1 'formats intcol=(5,4,3) a4col=2 
 ibis-copy temp temp1  incol=(1,2,3,5) 'row
 ibis-list temp1 'formats gr1=4 

  ibis-gen temp nc=3 format=(BYTE,REAL,A10) nr=6 datacol=(1,2) +
    data=(1,2,3,4,5,6,7,8,9) strcol=3 string=(This,is,lots,of,data)
  ibis-gen temp1 nc=3 format=(BYTE,REAL,A10) nr=6 datacol=(1,2) +
    data=(1,2,3,4,5,6,7,8,9) strcol=3 string=(This,is,lots,of,data)
  ibis-catenate (temp, temp1) temp2
  ibis-list temp2 
  ibis-catenate (temp, temp1) temp2 'bycolumn
  ibis-list temp2 

write "*********************************************"
write "************ IBIS-2 ROWS ********************"
write "*********************************************"

 ibis-gen temp nc=1024 nr=1 'row 
 ibis-gen temp nc=20 nr=10 'row
 ibis-list temp 
 ibis-gen temp  nc=8 nr=30 'row +
   form=("FULL","BYTE","HALF","A20","DOUB", "FULL","HALF","HALF") +
   indexcol=1 datacols=(2,3,5) +
   data=(1,2,3,1,2,3,1,2,3,4,5,6,4,5,6,4,5,6) strcol=4 +
   string=("THIS IS AN","ASCII COLUMN","The End.")   
 ibis-list temp csize=(6,5,5,16,7,6,5,5) 'formats 
 ibis-copy temp temp1 'column
 ibis-list temp1 csize=(6,5,5,16,7,6,5,5) 'formats 
 ibis-copy temp temp1  incol=(1,2,3,4) outcol=(4,3,2,1)
 ibis-list temp1 'formats 
 ibis-copy temp temp1  incol=(1,2,3,4,5) outcol=(5,4,3,2,1) +
 	a4col=2 intcol=(5,4,3) 'ibis-1
 ibis-list temp1 'formats intcol=(5,4,3) a4col=2 
 ibis-copy temp temp1  incol=(1,2,3,5) 'row
 ibis-list temp1 'formats gr1=4 

  ibis-gen temp nc=3 format=(BYTE,REAL,A10) nr=6 datacol=(1,2) 'row +
    data=(1,2,3,4,5,6,7,8,9) strcol=3 string=(This,is,lots,of,data)
  ibis-gen temp1 nc=3 format=(BYTE,REAL,A10) nr=6 datacol=(1,2) 'row +
    data=(1,2,3,4,5,6,7,8,9) strcol=3 string=(This,is,lots,of,data)
  ibis-catenate (temp, temp1) temp2
  ibis-list temp2 
  ibis-catenate (temp, temp1) temp2 'bycolumn
  ibis-list temp2 


write "*********************************************"
write "************ IBIS-2 GROUP TESTS *************"
write "*********************************************"

! Test out group computations
 ibis-gen temp  nr=30 +
   form=("BYTE","HALF","FULL","REAL","DOUB", "FULL","HALF","HALF")
 ibis-list temp 'formats 
 ibis-group temp  name="Evens" cols=(2,4,6,8) 'group
 ibis-group temp  name="Odds" cols=(1,3,5,7) 'group
 ibis-group temp  name="My Bytes" expr="format:byte" 'group
 ibis-group temp +
   name="(kg*m)/sec^2" expr="format:half | real" 'unit
 ibis-group temp +
   name="Even Newtons" expr="[(kg*m)/sec^2] * Evens" 'group
 ibis-list temp 'groups 'units 
 ibis-group temp  name="Odds" 'delete
 ibis-list temp 'groups 'units 

!Test ibis-copy with groups
  ibis-copy temp temp1
  ibis-list temp1 'groups 'units 
  ibis-copy temp temp1 incol=(1,3,5,7) outcol=(4,3,2,1)
  ibis-list temp1 'groups 'units 
  ibis-copy temp temp1 'noucopy 'nogcopy
  ibis-list temp1 'groups 'units 

!Make sure that IBIS can delete the last group FR#82932
  ibis-gen temp nc=5 nr=5
  ibis-group temp One 1
  ibis-list temp 'group 
  ibis-group temp One 'delete
  ibis-list temp 'group 

write "*********************************************"
write "************ IBIS-2 LIST TESTS *************"
write "*********************************************"

!Test new header options
  ibis-gen  temp nr=5 nc=4 format=(BYTE,FULL,REAL,A10) +
	index=1 strcol=4 string=(one,two,three,four,five)
  ibis-list temp 'nocol
  ibis-list temp 'nohead
  ibis-list temp preamble=("Here is the first line of Preamble",+
  			   "   .. and here is the second",+
  			   "   .. and here is the third",+
  			   "   .. and here is the fourth",+
  			   "   .. and here is the last") 'nohead 'nocol
 ibis-list temp cform="c1=%03d %-7d %6.3f '%6s'" 'nohead 'nocol

write "*********************************************"
write "************ IBIS-2 COPY TESTS *************"
write "*********************************************"

  ! check that we can copy a few columns into a larger
  ! file with defaulted other columns
  ibis-gen temp format=(BYTE,HALF,A5)
  ibis-copy temp temp1 nc=5 incol=(1,2,3)  nr=5
  ibis-list temp1
  ibis-gen temp nc=6 format=(byte,half,full,real,a10,comp) index=1 +
   datac=(2,3,4,6) data=(1,2,3,4,5,6,7,8,9,8,7,6,2,3,4,5) strc=5 +
   string=(one,two,three,four,five) nr=5
  ibis-list temp 'format
  ibis-copy temp temp1  nc=6 incol=(1,5,6) nr=6 outc=(2,4,5)
  ibis-list temp1 'format
end-proc
$ Return
$!#############################################################################
