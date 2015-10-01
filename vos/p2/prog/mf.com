$!****************************************************************************
$!
$! Build proc for MIPL module mf
$! VPACK Version 1.9, Thursday, November 15, 2012, 14:18:49
$!
$! Execute by entering:		$ @mf
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
$ write sys$output "*** module mf ***"
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
$ write sys$output "Invalid argument given to mf.com file -- ", primary
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
$   if F$SEARCH("mf.imake") .nes. ""
$   then
$      vimake mf
$      purge mf.bld
$   else
$      if F$SEARCH("mf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mf.bld "STD"
$   else
$      @mf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mf.com -mixed -
	-s mf.c -
	-i mf.imake -
	-p mf.pdf -
	-t tstmf.pdf tstmf.log_rjb tstmf.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mf.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*************************************************************************
 *
 *  mf.c:  Portable IBIS-2 module.
 *
 * Blurb from the original FORTRAN code:
 *
  MF ALLOWS THE USER TO CREATE FORTRAN LIKE EXPRESSIONS TO PERFORM
  GENERAL MATHEMATICAL OPERATIONS ON ONE OR MORE IBIS INTERFACE FILE
  THE EXPRESSIONS ARE CODED IN A PARAMETER STRING.  THE STRING IS
  INTERPRETED TO DETERMINE THE INPUT AND OUTPUT COLUMNS AND
  OPERATIONS TO BE PERFORMED.  THE VARIABLES REPRESENTING A DATA
  COLUMN ARE WRITTEN AS KEYWORDS TO ALLOW THE USER FLEXIBILITY IN WRITI
  THE EXPRESSION.  THE FUNCTIONS AVAILABLE ARE: SQRT,ALOG,ALOG10,
  AINT, SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, ABS, MIN OR AMIN1,
  MAX OR AMAX1, MOD OR AMOD, ALONG WITH STANDARD BINARY OPERATIONS
  +-* / AND ** AND LOGIC OPERATIONS .AND. .OR. .XOR. AND .NOT.
  ALL OPERATIONS OPERATE AS IN FORTRAN IV WITH THE EXCEPTION OF MIN AND
  MAX WHICH ARE BINARY ONLY.  A SPECIAL FEATURE ALLOWS FOR THE CALCULAT
  OF COLUMN STATISTICS.

  USER PARAMETERS:

  FUNCTION,"ARITHMETIC FUNCTION STRING" - THE STRING SPECIFIES THE FUNC
            BE PERFORMED.

 Revision history

  11/13/94     ndr    Added Graphics capability, "POLY" variable to
  				Obsolete old "GF" function.

  1/5/94       ndr    Ported to Unix, with IBIS-2 library calls.

  1/5/87       ejb    fixed bug in sci. not. conversion that
                      mistook '.E'  for a '.EQ.' logical operation

  12/6/87      ejb    fixed to convert to upper case before
                      parsing and compiling the functions
                      also changed CVSCIN to do Sci. Not. conversion
                      for a more general case

 *************************************************************************/
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>             /*64-bit def of NULL*/
 
#include "vicmain_c.h"
#include "applic.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zmabend.h"
#include "zifmessage.h"

typedef enum {
        OpNone=0,
	OpFunction,
	OpMean,
	OpSigma,
	OpMin,
	OpMax,
	OpFirst,
	OpSum,
	OpRunSum,
	OpRunDiff,
	OpEnd
} optype;

static char *OpNames[] = {
	"xxx",
	"xxx",
	"mean",
	"sigma",
	"min",
	"max",
	"first",
	"sum",
	"rsum",
	"rdiff",
	"xxx"
};

#define MAXSTRING  1024
#define MAXCOL 1024
#define NROWS 1024
#define MAXPOLYS 100000L

/* prototypes */
void process_function(int ibis, int function, int code, int nc);
void find_zeroes(int ibis);
char *nextcolnum(char *string, int *val, int *len, int inc);
int function_type(char *instring);
int parse_function_string(char *instring, char *fstring, int *pincol,
                 int *ncol, int *outcol, int *opnum, int nc);
int replace_variable(char *fstring, char *varname, int column);
void convert_fstring(char *instring, char *fstring, int *pincol, int *ncol, int nc);
void do_function_op(int ibis, char *fstring, int *pincol, int ncol, int *outcol, int code);
int check_zero(float *buf, int ncol);
void do_column_op(int ibis, int opnum, int *pincol, int outcol );
void compute_stats(int ibis, int incol, int nrow, float *mean, float *sig);
void compute_minmax(int ibis, int incol, int nrow, float *minval, float *maxval);
void write_column_value(int ibis, int outcol, int nrow, float value);
void write_column_sum(int ibis, int incol, int outcol, int nrow);
void write_column_diff(int ibis, int incol, int outcol, int nrow);
int zknuth(char *buf, int *fbuf);
void zxknuth(float *buf, float *fbuf);
void zknuth_dump(float *fbuf);

static int indexcol=0;
static int polycol=0;
static int graphics_mode=0;
static int *zero_rows=(int *)0;

void main44(void)
{
	int num_funcs;
	int function;
	int status;
	int unit;
	int ibis,nr,nc;
	int gr1dim,count; /* dimension for GRAPHICS-1 */
	int code;

	zifmessage("mf version 05-May-2011");

	zvpcnt("function",&num_funcs);
	zvunit(&unit,"inp",1,NULL);                 /*64-bit*/
	zvp("gr1dim",&gr1dim,&count);
	graphics_mode = zvptst("SKIP"); /* skip zeroes */

	code=0;
	zvp ("code",&code,&count);	
/*printf ("IBISFileOpen(unit, (main44) \n");*/
	status = IBISFileOpen(unit,&ibis,IMODE_UPDATE,gr1dim,0,0,0);
	if (status != 1) IBISSignalU(unit,status,1);
/*printf ("after\n");*/
	if (graphics_mode) find_zeroes(ibis);
	
    status = IBISFileGet(ibis,IFILE_NR,&nr,1,1,0);
    if (status!=1) IBISSignal(ibis,status,1);
    status = IBISFileGet(ibis,IFILE_NC,&nc,1,1,0);
    if (status!=1) IBISSignal(ibis,status,1);

    if (nr == 0) {
        zmabend("??E Input file has 0 rows");
    }
    if (nc == 0) {
        zmabend("??E Input file has 0 columns");
    }

	for (function=0;function<num_funcs;function++)
		process_function(ibis,function,code,nc);
	
	status = IBISFileClose(ibis,0);
	if (status != 1) IBISSignalU(unit,status,1);
}

/**************************************************************/
/* process_function - process the input function */
/* add nc for max columns in file               */
void process_function(int ibis,int function,int code,int nc)
{
	char instring[MAXSTRING+1];
	char fstring[MAXSTRING+1];
/*    char outmsg[130]; */
	int incol[MAXCOL],outcol,opnum,ncol,status;

	zvpone("function",instring,function+1,MAXSTRING);
	parse_function_string(instring,fstring,incol,&ncol,&outcol,&opnum,nc);

/*    printf ("IBISColumnSet (process_function) outcol = %d   ncol = %d\n",outcol, ncol);*/
	status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,IFMT_REAL,outcol);
	if (status!=1) IBISSignal(ibis,status,1);
/*
void do_function_op(int ibis, char *fstring, int *incol, int ncol, int *outcol, int code);
void do_column_op(int ibis, int opnum, int *incol, int outcol );
*/	
	if (opnum==OpFunction) {
/*        sprintf (outmsg,"do_function_op\n");
        zvmessage(outmsg," "); 
*/
		do_function_op( ibis, fstring, (int *)&incol, ncol, (int *)&outcol,code);
	} else {
/*    printf ("do_column-op (process_function) incol[0] = %d\n",incol[0]); */
		do_column_op( ibis, opnum,(int *)&incol, outcol );
    }
}

/****************************************************************/

/* find_zeroes - This routine finds all zeroes (polygon ends)   */
/*              in IBIS graphics mode                           */
void find_zeroes(int ibis)
{
	int record,status,nc,nr,row;
	int cols[MAXCOL],count,i,*rptr,*endptr;
	float rval[MAXCOL];
	
	if (zero_rows) return;

	zero_rows = (int *)calloc(1L, MAXPOLYS * sizeof(int));
	if (!zero_rows)
	{
		zvmessage("??E Error allocating space for polygon array"," ");
		zabend();
	}
	rptr = zero_rows;
	endptr = rptr + MAXPOLYS;
	
	status = IBISFileGet(ibis,IFILE_NR,&nr,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);
	status = IBISFileGet(ibis,IFILE_NC,&nc,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);

    if (nr == 0) {
        zmabend("??E Input file has 0 rows");
    }
    if (nc == 0) {
        zmabend("??E Input file has 0 columns");
    }
	
	/* Check for new Graphics groups, if any */
	count = IBISColumnFind(ibis,ITYPE_ANY,"C_POSITION & C_ROOT",cols,1,0);
	if (count == 0)
	{
		/* find all non-ASCII columns */
		for (i=0;i<nc;i++) cols[i] = i+1;
		IBISGroupNew(ibis,ITYPE_LOCAL,"$all",cols,nc,0);
		count = IBISColumnFind(ibis,ITYPE_ANY,"$all - format:ASCII",cols,1,0);
	}
	
	if (count <=0)
	{
		zvmessage("??E Failed to find any non-ASCII columns in file", " ");
		zabend();
	}
	status = IBISRecordOpen(ibis,&record,0,cols,count,IFMT_REAL);
	if (status < 0) IBISSignal(ibis,status,1);
	
	for (row = 1; row <= nr; row++)
	{
/* arg2 = char *buffer (float rval) needs to be a float so it is cast (char*) */
		status = IBISRecordRead(record,(char*)&rval,row);
		if (status < 0) IBISSignal(ibis,status,1);
		
		for (i=0;i<nc;i++)
			if (rval[i] != 0.0) break;
		if (i==nc)
		{
			if (rptr==endptr)
			{
				zvmessage("??E Max # polygons exceeded"," ");
				zabend();
			}
			*rptr++ = row;
		}
	}
	
	IBISRecordClose(record);
}

/*****************************************************************************
 *****************************************************************************
 *                        String Parsing Section                             *
 *****************************************************************************
 *****************************************************************************/
/*  nextcolnum -
 *  Grab the next 'Cnnn' or 'Xnnn' expression and length.
 *  The C or X must the first alpha encountered (or else AMAX1
 *  would be mis-interpreted).
 */
char *nextcolnum(char *string,int *val,int *len,int nc)
{
	int length;
	char *str;
	int found=0;
    char message[130];
	
	while (!found && *string)
	{
		while (!isalpha(*string) && *string) string++;
		if (!*string) goto bad;
		
		switch (*string++)
		{
			case 'c':
			case 'x':
				found = isdigit(*string); 
				if (found) break;
				/* else fall through */
			default:
				/* skip rest of alpha word and move on */
				while (isalpha(*string)) string++;
				continue; 
		}
	}
	if (!found) goto bad;
	
	/* If we got here, we found a good variable */
	
	*val = atoi(string);
    if (*val == 0) {
        zvmessage ("??E Column value = 0"," ");
        goto bad;
    }
    if (*val > nc) {
        sprintf (message,"??E Col value = %d greater than file value = %d",*val,nc);
        zvmessage (message," ");
        goto bad;
    }         
	length=0;
	for (str=string; isdigit(*str); str++) length++;
	*len = length;
	
	return string;
bad:
	*val=0;
	return (char *)0;
	
}

/**************************************************************/

/* function _type - Go thru instring and
 * determine if string is a local #COMMAND format
 * function (and what type), or else a KNUTH function string.
 */
 
int function_type(char *instring)
{
	int op,opfound,len;
	char *str;
	
	op = OpFunction;
	str = (char *)strchr(instring,'#');
	if (str)
	{
		str++;
		opfound=0;
		/* determine which column operation is flagged */
		for (op=OpMean;op<OpEnd;op++)
		{
			len = (int)strlen(OpNames[op]);             /* cast - May 05, 2011 */
			if (!strncmp(OpNames[op],str,(size_t)len))  /* cast - May 05, 2011 */
			{
				opfound = op;
				break;
			}
		}
		return opfound;
	}

	return OpFunction;
}

/****************************************************************/
/*  parse string for legality - get columns to be used          */
int parse_function_string(char *instring,char *fstring,int *pincol,int *ncol,int *outcol,int *opnum,int nc)
{
	char *str;
	int len;
	

	/* convert to lowercase */
	for (str=instring;*str;str++) *str = tolower(*str);
	
	/* get output column number */
    /* the way this was written - this message didnt work well */
	instring = nextcolnum(instring, outcol, &len, nc);
	if (!instring) {
        zvmessage("??E *** Bad Function String - Non-existent column ***"," ");
        zabend();
    }
	instring += len;
	while (isspace(*instring)) instring++;
	
	/* scan past '=' */
	if (*instring++ != '=') {
        zvmessage("??E *** Bad Function String - No equals sign ***"," ");
        zabend();
    }
	while (isspace(*instring)) instring++;

	/* Determine whether this is a knuth or #local function */
	*opnum = function_type(instring);
	if (!*opnum) {
        zvmessage("??E *** Bad Function String - Non-existent opnum ***"," ");
        zabend();
    }

	convert_fstring(instring,fstring,pincol,ncol,nc);

	indexcol = replace_variable(fstring,"index",*ncol+1);
	polycol = replace_variable(fstring,"poly",*ncol+2);

	return 1;
}

/**************************************************************/
/* indicate whether index or poly is in function string          */
int replace_variable(char *fstring,char *varname,int column)
{
	char colstring[30];
	char *str;
	int namelen=(int)strlen(varname);           /* cast - May 05, 2011 */
	int foundcol=0;

	/* convert varname to column indexin */
	for (str = fstring; *str; str++)
	{
		if (!strncmp(str,varname,(size_t)namelen))      /* cast - May 05, 2011 */
		{
			if (!foundcol)
			{
				foundcol = column;
				sprintf(colstring,"c%-d        ",column);
			}
			strncpy(str,colstring,(long unsigned int)namelen);      /* cast - May 05, 2011 */
			str += namelen-1;
		}
	}
	return foundcol;
}

/****************************************************************/
/* get number of columns, ncol, and column ids in array, incol  */
/* from instring and place rest of string in fstring            */
void convert_fstring(char *instring,char *fstring,int *pincol,int *ncol,int nc)
{
	int col;
	int len;
	int ncols;
	int colnum[MAXCOL];
	char *outstr;
	char *str;
	char *next;

	memset(colnum,0,sizeof(colnum));

	/* scan and count Cnnn columns and convert */
	str = instring;
	outstr = fstring;
	ncols = 0;
	while (*str)
	{
		/* find next col number */
		next = nextcolnum(str, &col,&len,nc);
		if (next)
		{
			/* copy everything up to next */
			while (str < next) *outstr++ = *str++;
			
			/* skip Cnnn number */
			str += len;
			
			/* check for new column reference */
			if (!colnum[col])
			{
				pincol[ncols] = col;
				ncols++;
				colnum[col] = ncols;
			}
			
			/* write out new column number */
			sprintf(outstr,"%-d",colnum[col]);
			outstr += strlen(outstr);
		}
		else while (*str) *outstr++ = *str++;
	}
	*outstr='\0';       /* append nul just to be safe */
	*ncol=ncols;
}

/*****************************************************************************
 *****************************************************************************
 *                        Knuth Function Processing                          *
 *****************************************************************************
 *****************************************************************************/
void do_function_op(int ibis, char *fstring, int *pincol, int ncol, int *outcol,int code)
{
	int ier,nrow,row,outrow,rows_now,nc;
	int poly=0;
	int status;
	int record;
	int skip;
	int was_zero=1;
	int gmode = graphics_mode || polycol;
	int next_zero=0,*zptr=0;
	char message[80];
	float fbuf[300];
	float outbuf[NROWS];
	
	status = IBISFileGet(ibis,IFILE_NR,&nrow,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);

    if (nrow == 0) {
        zmabend("??E Input file has 0 rows");
    }
    status = IBISFileGet(ibis,IFILE_NC,&nc,1,1,0);
    if (status!=1) IBISSignal(ibis,status,1);

    if (nc == 0) {
        zmabend("??E Input file has 0 columns");
    }

/* If ibis graphics file - find zero points */	
	if (gmode)
	{
		if (!zero_rows) find_zeroes(ibis);
		zptr = zero_rows;
		next_zero = *zptr;
	}

/* int zknuth(char *string, int *buf) */
	
	ier=zknuth( fstring, (int *)&fbuf);
        if (ier)
	{
	   switch (ier)
	   {
		case 1: sprintf(message,"??E LINE,SAMP, or BAND in Function string:"); break;
		case 2: sprintf(message,"??E Bad Function:"); break;
		case 3: sprintf(message,"??E Function evaluation error:"); break;
	   }
	   zvmessage(message," ");
	   zvmessage(fstring," ");
	   zabend();
	}
    if (code) {
        zknuth_dump(fbuf);
	}
/* ncol is number of columns on right side of = sign */
	if (ncol>0)
	{
		status = IBISRecordOpen(ibis,&record,0,pincol,ncol,IFMT_REAL);
		if (status!=1) IBISSignal(ibis,status,1);
	}
	
	outrow=1;
	rows_now=0;
	for (row=1;row<=nrow;row++)
	{
		skip = (gmode && row==next_zero);
		if (skip)
		{
			/* graphics mode - outbuf is zero */
			next_zero = *++zptr;
			was_zero = 1;
			outbuf[rows_now]=0.0;
		}
		else /* need to compute outbuf */
		{
		   if (ncol>0)
		   {
/* arg2 = char *buffer (float rval) needs to be a float so it is cast (char*) */
/*          status=IBISRecordRead(record,(char*)&fbuf,row);     */
			status=IBISRecordRead(record,(char *)fbuf,row);
			if (status!=1) IBISSignal(ibis,status,1);

		   }

		   if (indexcol) fbuf[indexcol-1] = (float)row;         /* cast - May 05, 2011 */
		   if (polycol)
		   {
			if (was_zero) poly++;
			fbuf[polycol-1] = (float)poly;                      /* cast - May 05, 2011 */
			was_zero = 0;
		   }
/* void zxknuth(float *buf,float *result);   */		
		   zxknuth(fbuf,&outbuf[rows_now]);

		}
		rows_now++;
		if (rows_now==NROWS)
		{
/* arg 2 =  char *buffer (float outbuf) needs to be a float so outbuf is cast (char*)       */ 
/* arg 3 =  int column (int *outcol)  works as shown                                        */
			status=IBISColumnWrite(ibis,(char*)&outbuf,*outcol,outrow,rows_now);
			if (status!=1) IBISSignal(ibis,status,1);
			outrow += rows_now;
			rows_now = 0;
		}
	}
	if (rows_now) /* some rows left over */
	{
/* arg 2 =  char *buffer (float outbuf) needs to be a float so outbuf is cast (char*)       */
/* arg 3 =  int column (int *outcol)   works as shown                                       */
/*            printf ("outcol = %d for IBISColumnWrite in do_function_op 2\n",*outcol);     */
		status=IBISColumnWrite(ibis,(char*)&outbuf,*outcol,outrow,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
	}
	
	if (ncol>0) IBISRecordClose(record);
}
/**************************************************************/

/* check_zero  */
int check_zero(float *buf,int ncol)
{
	register int i;
	
	for (i=0;i<ncol;i++)
		if (*buf++) return 0;
	
	return 1;
}

/*****************************************************************************
 *****************************************************************************
 *                        #Column Operation Processing                       *
 *****************************************************************************
 *****************************************************************************/

 
void do_column_op(int ibis, int opnum,int *pincol,int outcol )
{
	int nrow,nc;
	int status;
	float mean,sig,minval,maxval,first,xx;
	
	status = IBISFileGet(ibis,IFILE_NR,&nrow,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);

    if (nrow == 0) {
        zmabend("??E Input file has 0 rows");
    }
    status = IBISFileGet(ibis,IFILE_NC,&nc,1,1,0);
    if (status!=1) IBISSignal(ibis,status,1);
    if (nc == 0) {
        zmabend("??E Input file has 0 columns");
    }
	
/* arg 4 =  int column (int *pincol) needs to be a long (not int) so pincol is cast (long) */
/*    printf ("IBISColumnSet (do_column_op) (long)pincol[0] = %d\n",pincol[0]);*/
    status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,IFMT_REAL,(long)pincol[0]);
    
	if (status!=1) IBISSignal(ibis,status,1);
	/* preprocessing step */
	switch (opnum)
	{
/*
void compute_stats(int ibis, int incol, int nrow, float *mean, float *sig);
void compute_minmax(int ibis, int incol, int nrow, float *minval, float *maxval);
void write_column_value(int ibis, int outcol, int nrow, float value);
void write_column_sum(int ibis, int incol, int outcol, int nrow);
void write_column_diff(int ibis, int incol, int outcol, int nrow);
*/

		case OpMean: case OpSigma: case OpSum:
			compute_stats(ibis,(long)pincol[0],nrow,&mean,&sig);
			break;
		case OpMin: case OpMax:
			compute_minmax(ibis,(long)pincol[0],nrow,&minval,&maxval);
			break;
		case OpFirst:
/* arg 2 =  char *buffer (float first) needs to be a float so it is cast (char*) */
/* arg 3 =  int column (int *pincol) needs to be a long (not int) so it is cast (long) */ 
/*            printf ("IBISColumnRead (do_column_op) (long)pincol = %d\n",pincol[0]);*/
			status=IBISColumnRead(ibis,(char *)&first,(long)pincol[0],1,1);
			if (status!=1) IBISSignal(ibis,status,1);
			break;
	}

	/* processing step */
	switch (opnum)
	{
		case OpMean: write_column_value(ibis,outcol,nrow,mean); break;
		case OpSigma:write_column_value(ibis,outcol,nrow,sig); break;
/*		case OpSum:  write_column_value(ibis,outcol,nrow,mean*(float)nrow); break; /* cast - May 05, 2011 */
		case OpSum: xx=mean*(float)nrow; write_column_value(ibis,outcol,nrow,xx); break;
		case OpMin:  write_column_value(ibis,outcol,nrow,minval); break;
		case OpMax:  write_column_value(ibis,outcol,nrow,maxval); break;
		case OpFirst:write_column_value(ibis,outcol,nrow,first); break;
		
		case OpRunSum: write_column_sum(ibis,(int)pincol[0],outcol,nrow); break;
		case OpRunDiff:write_column_diff(ibis,(int)pincol[0],outcol,nrow); break;
	}
}


/**************************************************************/
void compute_stats(int ibis,int incol,int nrow,float *mean,float *sig)
{
	register float meanval,diff,sigval=0,sum=0;
	float inbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int status;
	
	/* first loop -- compute sum and mean value */
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
/* arg 2 =  char *buffer (float inbuf) needs to be a float so inbuf is cast (char*) */
/*    printf ("IBISColumnRead (compute_stats) incol = %d\n",incol);*/
		status = IBISColumnRead(ibis,(char *)&inbuf,(long)incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
			sum += inbuf[thisrow];
		rows_left -= rows_now;
	}
	meanval = sum/(float)nrow;                                  /* cast - May 05, 2011 */
	
	/* second loop -- compute sigma */
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
/* arg 2 =  char *buffer (float inbuf) needs to be a float so inbuf is cast (char*) */
/*    printf ("IBISColumnRead (compute_stats - 2) incol = %d\n",incol);*/
		status = IBISColumnRead(ibis,(char *)&inbuf,(long)incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			diff = inbuf[thisrow]-meanval;
			sigval += diff*diff;
		}
		rows_left -= rows_now;
	}
	sigval = (float)(sqrt(sigval/(float)(nrow<2 ? 1 : nrow-1)));        /* cast - May 05, 2011 */    
	
	*mean = meanval;
	*sig = sigval;
}

/**************************************************************/
void compute_minmax(int ibis,int incol,int nrow,float *minval,float *maxval)
{
	register float minv=0,maxv=0;
	float inbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int first=1;
	int status;
	
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
/* arg 2 =  char *buffer (float inbuf) needs to be a float so inbuf is cast (char*) */
/*            printf ("IBISColumnRead (compute_minmax ) incol = %d\n",incol);*/

		status = IBISColumnRead(ibis,(char *)&inbuf,(long)incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		if (first) {minv=maxv=inbuf[0];first=0;}
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			if (inbuf[thisrow] < minv)
				minv = inbuf[thisrow];
			else if (inbuf[thisrow] > maxv)
				maxv = inbuf[thisrow];
		}
		rows_left -= rows_now;
	}
	*minval = minv;
	*maxval = maxv;
}

/**************************************************************/
void write_column_value(int ibis,int outcol,int nrow,float value)
{
/*	register float minv,maxv;       */
	float outbuf[NROWS];
	int row,rows_left,rows_now;
	int status;
	
	/* same value, over and over again */
	for (row=0;row<NROWS;row++) outbuf[row] = value;

	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
/* arg 2 =  char *buffer (float outbuf) needs to be a float so outbuf is cast (char*) */
		status = IBISColumnWrite(ibis,(char*)&outbuf,outcol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		rows_left -= rows_now;
	}
}

/**************************************************************/
void write_column_sum(int ibis,int incol,int outcol,int nrow)
{
	register float sum=0;
	float inbuf[NROWS];
	float outbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int status;
	
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
/* arg 2 =  char *buffer (float inbuf) needs to be a float so inbuf is cast (char*) */
/*            printf ("IBISColumnRead (write_column_sum ) incol = %d\n",incol);*/
		status = IBISColumnRead(ibis,(char*)&inbuf,(long)incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			sum += inbuf[thisrow];
			outbuf[thisrow] = sum;
		}
/* arg 2 =  char *buffer (float outbuf) needs to be a float so outbuf is cast (char*) */
		status = IBISColumnWrite(ibis,(char*)&outbuf,outcol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		rows_left -= rows_now;
	}
}


/**************************************************************/
void write_column_diff(int ibis,int incol,int outcol,int nrow)
{
	register float prev=0,diff;
	float inbuf[NROWS];
	float outbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int status;
	
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
/* arg 2 =  char *buffer (float inbuf) needs to be a float so inbuf is cast (char*) */
/*            printf ("IBISColumnRead (write_column_dif) incol = %d\n",incol);*/

		status = IBISColumnRead(ibis,(char*)&inbuf,(long)incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			diff = inbuf[thisrow] - prev;
			prev = inbuf[thisrow];
			outbuf[thisrow] = diff;
		}
/* arg 2 =  char *buffer (float outbuf) needs to be a float so outbuf is cast (char*) */
		status = IBISColumnWrite(ibis,(char*)&outbuf,outcol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		rows_left -= rows_now;
	}
}
/**************************************************************/

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mf.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM mf    

   To Create the build file give the command:

                % vimake mf                           (Unix)


************************************************************************/
#define PROGRAM mf

#define MODULE_LIST mf.c

#define MAIN_LANG_C
#define R2LIB

/* Comment this out before delivery.
#define DEBUG
*/

#define USES_ANSI_C

#define LIB_CARTO
#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB
/************************* End of Imake file ***************************/

$ Return
$!#############################################################################
$PDF_File:
$ create mf.pdf
PROCESS        HELP=*
! MF PDF - VICAR/IBIS SOFTWARE
PARM INP TYPE=STRING
PARM FUNCTION TYPE=(STRING,200),COUNT=(1:50)
PARM GR1DIM TYPE=INTEGER COUNT=1 DEFAULT=0
PARM ZEROES  TYPE=KEYWORD VALID=(SKIP,INCLUDE) DEF=INCLUDE
PARM CODE  TYPE=INTEGER COUNT=1 DEF=0
END-PROC
.TITLE
VICAR/IBIS Program MF
.HELP
PURPOSE

     MF   allows   the  user  to  create  FORTRAN or C -like 
     expressions to perform general mathematical operations on 
     one  or more IBIS/graphics file columns.   The  expressions 
     are  written as a parameter string.   The parameter  is 
     interpreted  to determine the input and output  columns 
     and   operations  to  be  performed.    The   variables 
     representing  a data column are written as keywords  to 
     allow  the user flexibility in writing the  expression.  
     The functions available are:  SQRT, ALOG, ALOG10, AINT, 
     SIN,  COS TAN,  ASIN,  ACOS,  ATAN,  ATAN2, ABS, MIN or 
     AMIN1,  MAX  or  AMAX1,  and MOD or  AMOD,  along  with 
     standard binary operations +,  -,  *,  /, **, and logic 
     operations,   .AND.,   .OR.,   .XOR.,  and  .NOT.   All 
     operations operate as in FORTRAN IV and C.   A  special 
     feature   allows   for   the  calculation   of   column 
     statistics.

TAE COMMAND LINE FORMAT

     MF INP=int PARAMS

     where

     int                 is a random access file.  Since it
                         is used for both input and  output, 
                         no output file is specified.

     PARAMS              is   a  standard  VICAR   parameter 
                         field.

    The GR1DIM specifies the (Graphics-1) Dimension.

    The ZEROES parameters specifies whether to process rows
    with all zeroes. The permitted options are: SKIP and
    INCLUDE. INCLUDE is the default.

    The CODE parameter will show the pseudo instructions for
    math and logic functions. The nmenomics are the same
    as for the CODE parameter for mf but have different
    operands. No pseudo instructions are generated for
    column opearations.

.PAGE
METHOD

     MF performs arithmetic operations on an interface file.  
     The  program  uses  two  library  routines KNUTH  and 
     XKNUTH,   to   compile  and  interpret   FORTRAN or C  like 
     expressions  entered by the parameters in an expression 
     such as:

                     C35 = (100*C34)/C4

     In this expression,  C34 and C4 are the input  columns.  
     KNUTH    compiles   the   expression   into    machine 
     instructions.   The  expression is applied to the input 
     column in XKNUTH to produce the output column, C35. For
     compatibility with program GF, the variable names X5,
     etc, may be used in place of C5, etc.


RESTRICTIONS

     Maximum number of columns in one execution is 50.
.PAGE
EXAMPLE

     MF INP=FILE.INT FUNCTION=("C5 = C2/C3+100+SQRT(C2)")

     In this example,  C2 is divided by C3 and added to  100 
     plus the square root of C2.   The results are placed in 
     C5.  Further examples of allowable functions follow:

                FUNCTION=("C5 = !(C3  || C2)")

     logical   operations  are  performed  bitwise  on   the 
     operands. The  logical values T and F are converted to 1.  and 0. 
     for storage in column C5

                FUNCTION=("X5 = X3.LE.INDEX")

     Column 5 is 1.0 if column 3 has a value < its row value (INDEX).
     
                FUNCTION=("X5 = POLY * 2")

     the operator POLY returns the current polygon number,
     where polygons are delimited by rows of all zero values.

                FUNCTION=("C5 = #MEAN(C3)")

     In this example, the mean of column 3 is calculated and 
     that  value is placed in every row entry in  column  5.  
     This  operation  is different than the  arithmetic  and 
     logic operations given earlier because it operates on a 
     vertical  column instead of horizontally across a  row.  
     These  operations  cannot  be  used  in  an  arithmetic 
     expression  such as C5 = #MEAN(C3)*10.   The  allowable 
     column operations are:

                       C2 = #MEAN(C4)
                       C2 = #SIGMA(C4)
                       C2 = #MIN(C4)
                       C2 = #MAX(C4)
                       C2 = #FIRST(C4)
                       C2 = #SUM(C4)
                       C2 = #RSUM(C4) (running sum)
                       C2 = #RDIFF(C4) (running diff)
					   
	See the FUNCTION help for more examples.

Original Programmer:  A. L. Zobrist, 15 December 1976

Cognizant Programmer:  N. D. Ritter

Revision:  3,        
        05 January 1987 - E.J. Barragy  -  fixed bug in sci. not. conversion that
                      mistook '.E'  for a '.EQ.' logical operation
        06 June 1987  - E.J. Barragy  - fixed to convert to upper case before
                      parsing and compiling the functions
                      also changed CVSCIN to do Sci. Not. conversion
                      for a more general case
        05 January 1994 - N. D. Ritter - Ported to Unix, with IBIS-2 library calls.    
        13 November 1994 N. D. Ritter - Graphics Functions
        18 October 2007 - R. J. Bambery - Add CODE parameter
                        to show symbolic dump like mf3 and f2.
        28 Feb 2008 - R. J. Bambery - - Fixes for ANSI_C compiler 
                        in Linux 
        24 Mar 2008 - R. J. Bambery - fixed error in arg 3 in IBISColumnWrite
                        call
        28 Mar 2008 - R. J. Bambery - fixed xknuth call (prototype in 
                        cartoVicarProtos.h was wrong),  added check for
                        columns and rows in input file. added check for
                        function columns exceeding those in input file
                        (required some reorganization of logical flow)

        02 Dec 2009 - R. J. Bambery - made compatible with 64-bit linux
                        removed cartoVicarProtos.h (Makefile.mf)

        28 Jan 2010 - R. J. Bambery - Made compatible with 64-bit afids Build 793
                            Linux, MacOSX (both Intel/PowerPC)
        12 Aug 2010 - R. J. Bambery - got rid of warning:
                /usr/local/afids/include/vicmain_c:63: warning: function declaration isn't a prototype
                #include "vicmain_c" to #include "vicmain_c.h"
        05 May 2011 - R. J. Bambery - Removed all warning messages generated from gcc 4.4.4
                                    Build 1009
        15 Nov 2012 - L.W.Kamp - added data types to function declarations for compatibilty
                                 with ANSI C on Solaris.

.LEVEL1
.VARIABLE INP
Input IBIS interface file
.VARIABLE FUNCTION
Specifies function and columns
.VARIABLE GR1DIM
Dimension (Graphics-1 only)
.VARIABLE ZEROES
Process rows with all zeroes?
.VARIABLE CODE
Set 1 to see pseudo code

.LEVEL2
.VARIABLE INP
            Specifies IBIS interface file. There
            is no output file. Results of MF are
            written in INP. Graphics-1 files may
			also be processed, using the "GR1DIM"
			parameter.
.VARIABLE FUNCTION
     FUNCTION            
	 
	 this keyword specifies the function to be applied,  and the columns  to 
	 which  it applies.   Functions  are delimited  by  double  quotes.  "C" 
	 followed  by a number indicates the  columns  used as input  or  output.  
	 Up  to nineteen columns can be used  as  input.   The term INDEX can  be 
	 used  in the arithmetic  expression to  introduce row number  into  the 
	 calculation   (see  examples).    A   special notation is used for column 
	 statistics   (see  examples).    By using   ","   separator,    several 
	 functions can be placed in one call (i.e. FUNC=("C1=INDEX","C2=C1*C1").
	 Maximum number of columns in one execution is 50. 
	 
	 Also, to support GRAPHICS "GF" capabilities, the "POLY" operator may
	 also be used. This function returns the polygon number of the
	 current row (which starts at 1), or a zero if end-of-polygon. To
	 further support MF as a replacement for GF, the columns may be
	 referenced by "Xnnn" rather than "Cnnn" to indicate coordinate value.
	 
.VARIABLE GR1DIM
GRAPHICS-1 files do not have any explicit dimensions in the
file label. This allows the user to properly dimension a
graphics file so that it may be used by this program.
IBIS-2 format graphics files do not need or use this.
.VARIABLE ZEROES
This keyword indicates whether to SKIP or INCLUDE rows in which all
values are zero. This is primarily used for GRAPHICS files,
in which an all-zero row signals "pen-up" or next polygon.
NOTE: the #MEAN type column operators do not honor this keyword,
and will write a value in every row. The POLY function
always skips zeroes, while the INDEX function honors this
keyword.
.VARIABLE CODE
Set 1 to see pseudo code

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmf.pdf
procedure
refgbl $autousage
refgbl $echo
! Jun 24, 2012 - RJB
! TEST SCRIPT FOR MF     
! tests IBIS tabular files
!
! Vicar Programs:
!       ibis-gen ibis-list  
! 
! parameters:
!   <none>
!
! Requires NO external test data: 

body
let $autousage="none"
let $echo="yes"
let _onfail="stop"
! Create a new IBIS-2  file, and throw in an ASCII column
!
ibis-gen a nc=5 nr=10 format=(FULL,REAL,DOUB,REAL,A4)
!
! Test various KNUTH functions with C and FORTRAN constructs
!
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
ibis-list a
!
! Test Column Operations
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
ibis-list a
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
ibis-list a
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
ibis-list a

! Create an old IBIS-1  file and test same options
!
ibis-gen a nc=5 nr=10 'IBIS-1 'COLUMN
!
! Test various KNUTH functions with C and FORTRAN constructs
!
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
ibis-list a
!
! Test Column Operations
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
ibis-list a
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
ibis-list a
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
ibis-list a

! Create an old GRAPHICS-1  file and test same options
! (must use GR1DIM, as dimension is not built-into file)
!
ibis-gen a nc=5 nr=10 'IBIS-1 'ROW
!
! Test various KNUTH functions with C and FORTRAN constructs
!
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" ) GR1DIM=5
ibis-list a GR1DIM=5
!
! Test Column Operations
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)") GR1DIM=5
ibis-list a GR1DIM=5
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)") GR1DIM=5
ibis-list a GR1DIM=5
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)") GR1DIM=5
ibis-list a GR1DIM=5

!Test new graphics handling, including Xnnn parsing:
ibis-gen a nc=3 nr=12 'IBIS-1 'ROW datacol=(1,2) +
  data=(1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0)

mf inp=a function=("x3=poly*2") GR1DIM=3
ibis-list a GR1DIM=3 nr=12

mf inp=a function=("x3=AMAX1(x1,x2)+1") GR1DIM=3 'skip
ibis-list a GR1DIM=3 nr=12

mf inp=a function=("x3=x1+1") GR1DIM=3
ibis-list a GR1DIM=3 nr=12

mf inp=a function=("x3=INDEX") GR1DIM=3 'skip
ibis-list a GR1DIM=3 nr=12
let $echo="no"

! clean up:
ush rm -f a

end-proc
$!-----------------------------------------------------------------------------
$ create tstmf.log_rjb
tstmf
let _onfail="stop"
ibis-gen a nc=5 nr=10 format=(FULL,REAL,DOUB,REAL,A4)
Beginning VICAR task ibis
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1        1.00        0.10        0.00            
           2        4.00        0.20        0.00            
           3        9.00        0.30        0.00            
           4       16.00        0.39        0.00            
           5       25.00        0.00        0.00            
           6       36.00        0.00        0.00            
           7       49.00        0.00        0.00            
           8       64.00        0.00        0.00            
           9       81.00        0.00        0.00            
          10      100.00        0.00        0.00            
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1        5.50        3.03        1.00            
           2        5.50        3.03        1.00            
           3        5.50        3.03        1.00            
           4        5.50        3.03        1.00            
           5        5.50        3.03        1.00            
           6        5.50        3.03        1.00            
           7        5.50        3.03        1.00            
           8        5.50        3.03        1.00            
           9        5.50        3.03        1.00            
          10        5.50        3.03        1.00            
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1       10.00        1.00       55.00            
           2       10.00        1.00       55.00            
           3       10.00        1.00       55.00            
           4       10.00        1.00       55.00            
           5       10.00        1.00       55.00            
           6       10.00        1.00       55.00            
           7       10.00        1.00       55.00            
           8       10.00        1.00       55.00            
           9       10.00        1.00       55.00            
          10       10.00        1.00       55.00            
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1        1.00        1.00       55.00            
           2        3.00        1.00       55.00            
           3        6.00        1.00       55.00            
           4       10.00        1.00       55.00            
           5       15.00        1.00       55.00            
           6       21.00        1.00       55.00            
           7       28.00        1.00       55.00            
           8       36.00        1.00       55.00            
           9       45.00        1.00       55.00            
          10       55.00        1.00       55.00            
ibis-gen a nc=5 nr=10 'IBIS-1 'COLUMN
Beginning VICAR task ibis
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.10        0.00        0.00
        2.00        4.00        0.20        0.00        0.00
        3.00        9.00        0.30        0.00        0.00
        4.00       16.00        0.39        0.00        0.00
        5.00       25.00        0.00        0.00        0.00
        6.00       36.00        0.00        0.00        0.00
        7.00       49.00        0.00        0.00        0.00
        8.00       64.00        0.00        0.00        0.00
        9.00       81.00        0.00        0.00        0.00
       10.00      100.00        0.00        0.00        0.00
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        5.50        3.03        1.00        0.00
        2.00        5.50        3.03        1.00        0.00
        3.00        5.50        3.03        1.00        0.00
        4.00        5.50        3.03        1.00        0.00
        5.00        5.50        3.03        1.00        0.00
        6.00        5.50        3.03        1.00        0.00
        7.00        5.50        3.03        1.00        0.00
        8.00        5.50        3.03        1.00        0.00
        9.00        5.50        3.03        1.00        0.00
       10.00        5.50        3.03        1.00        0.00
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       10.00        1.00       55.00        0.00
        2.00       10.00        1.00       55.00        0.00
        3.00       10.00        1.00       55.00        0.00
        4.00       10.00        1.00       55.00        0.00
        5.00       10.00        1.00       55.00        0.00
        6.00       10.00        1.00       55.00        0.00
        7.00       10.00        1.00       55.00        0.00
        8.00       10.00        1.00       55.00        0.00
        9.00       10.00        1.00       55.00        0.00
       10.00       10.00        1.00       55.00        0.00
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        1.00       55.00        0.00
        2.00        3.00        1.00       55.00        0.00
        3.00        6.00        1.00       55.00        0.00
        4.00       10.00        1.00       55.00        0.00
        5.00       15.00        1.00       55.00        0.00
        6.00       21.00        1.00       55.00        0.00
        7.00       28.00        1.00       55.00        0.00
        8.00       36.00        1.00       55.00        0.00
        9.00       45.00        1.00       55.00        0.00
       10.00       55.00        1.00       55.00        0.00
ibis-gen a nc=5 nr=10 'IBIS-1 'ROW
Beginning VICAR task ibis
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" ) GR1DIM=5
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.10        0.00        0.00
        2.00        4.00        0.20        0.00        0.00
        3.00        9.00        0.30        0.00        0.00
        4.00       16.00        0.39        0.00        0.00
        5.00       25.00        0.00        0.00        0.00
        6.00       36.00        0.00        0.00        0.00
        7.00       49.00        0.00        0.00        0.00
        8.00       64.00        0.00        0.00        0.00
        9.00       81.00        0.00        0.00        0.00
       10.00      100.00        0.00        0.00        0.00
       11.00      121.00        0.00        0.00        0.00
       12.00      144.00        0.00        0.00        0.00
       13.00      169.00        0.00        0.00        0.00
       14.00      196.00        0.00        0.00        0.00
       15.00      225.00        0.00        0.00        0.00
       16.00      256.00        0.00        0.00        0.00
       17.00      289.00        0.00        0.00        0.00
       18.00      324.00        0.00        0.00        0.00
       19.00      361.00        0.00        0.00        0.00
       20.00      400.00        0.00        0.00        0.00
       21.00      441.00        0.00        0.00        0.00
       22.00      484.00        0.00        0.00        0.00
       23.00      529.00        0.00        0.00        0.00
       24.00      576.00        0.00        0.00        0.00
       25.00      625.00        0.00        0.00        0.00
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)") GR1DIM=5
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       13.00        7.36        1.00        0.00
        2.00       13.00        7.36        1.00        0.00
        3.00       13.00        7.36        1.00        0.00
        4.00       13.00        7.36        1.00        0.00
        5.00       13.00        7.36        1.00        0.00
        6.00       13.00        7.36        1.00        0.00
        7.00       13.00        7.36        1.00        0.00
        8.00       13.00        7.36        1.00        0.00
        9.00       13.00        7.36        1.00        0.00
       10.00       13.00        7.36        1.00        0.00
       11.00       13.00        7.36        1.00        0.00
       12.00       13.00        7.36        1.00        0.00
       13.00       13.00        7.36        1.00        0.00
       14.00       13.00        7.36        1.00        0.00
       15.00       13.00        7.36        1.00        0.00
       16.00       13.00        7.36        1.00        0.00
       17.00       13.00        7.36        1.00        0.00
       18.00       13.00        7.36        1.00        0.00
       19.00       13.00        7.36        1.00        0.00
       20.00       13.00        7.36        1.00        0.00
       21.00       13.00        7.36        1.00        0.00
       22.00       13.00        7.36        1.00        0.00
       23.00       13.00        7.36        1.00        0.00
       24.00       13.00        7.36        1.00        0.00
       25.00       13.00        7.36        1.00        0.00
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)") GR1DIM=5
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       25.00        1.00      325.00        0.00
        2.00       25.00        1.00      325.00        0.00
        3.00       25.00        1.00      325.00        0.00
        4.00       25.00        1.00      325.00        0.00
        5.00       25.00        1.00      325.00        0.00
        6.00       25.00        1.00      325.00        0.00
        7.00       25.00        1.00      325.00        0.00
        8.00       25.00        1.00      325.00        0.00
        9.00       25.00        1.00      325.00        0.00
       10.00       25.00        1.00      325.00        0.00
       11.00       25.00        1.00      325.00        0.00
       12.00       25.00        1.00      325.00        0.00
       13.00       25.00        1.00      325.00        0.00
       14.00       25.00        1.00      325.00        0.00
       15.00       25.00        1.00      325.00        0.00
       16.00       25.00        1.00      325.00        0.00
       17.00       25.00        1.00      325.00        0.00
       18.00       25.00        1.00      325.00        0.00
       19.00       25.00        1.00      325.00        0.00
       20.00       25.00        1.00      325.00        0.00
       21.00       25.00        1.00      325.00        0.00
       22.00       25.00        1.00      325.00        0.00
       23.00       25.00        1.00      325.00        0.00
       24.00       25.00        1.00      325.00        0.00
       25.00       25.00        1.00      325.00        0.00
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)") GR1DIM=5
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        1.00      325.00        0.00
        2.00        3.00        1.00      325.00        0.00
        3.00        6.00        1.00      325.00        0.00
        4.00       10.00        1.00      325.00        0.00
        5.00       15.00        1.00      325.00        0.00
        6.00       21.00        1.00      325.00        0.00
        7.00       28.00        1.00      325.00        0.00
        8.00       36.00        1.00      325.00        0.00
        9.00       45.00        1.00      325.00        0.00
       10.00       55.00        1.00      325.00        0.00
       11.00       66.00        1.00      325.00        0.00
       12.00       78.00        1.00      325.00        0.00
       13.00       91.00        1.00      325.00        0.00
       14.00      105.00        1.00      325.00        0.00
       15.00      120.00        1.00      325.00        0.00
       16.00      136.00        1.00      325.00        0.00
       17.00      153.00        1.00      325.00        0.00
       18.00      171.00        1.00      325.00        0.00
       19.00      190.00        1.00      325.00        0.00
       20.00      210.00        1.00      325.00        0.00
       21.00      231.00        1.00      325.00        0.00
       22.00      253.00        1.00      325.00        0.00
       23.00      276.00        1.00      325.00        0.00
       24.00      300.00        1.00      325.00        0.00
       25.00      325.00        1.00      325.00        0.00
ibis-gen a nc=3 nr=12 'IBIS-1 'ROW datacol=(1,2)  +
  data=(1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0)
Beginning VICAR task ibis
mf inp=a function=("x3=poly*2") GR1DIM=3
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        1.00        1.00        4.00
        1.00        1.00        4.00
        1.00        1.00        4.00
        0.00        0.00        0.00
        1.00        1.00        6.00
        1.00        1.00        6.00
        1.00        1.00        6.00
        0.00        0.00        0.00
        0.00        0.00        0.00
mf inp=a function=("x3=AMAX1(x1,x2)+1") GR1DIM=3 'skip
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        0.00        0.00        0.00
mf inp=a function=("x3=x1+1") GR1DIM=3
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        1.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        1.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        1.00
        0.00        0.00        1.00
mf inp=a function=("x3=INDEX") GR1DIM=3 'skip
Beginning VICAR task mf
mf version May 05, 2011 (64-bit) - RJB
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        1.00
        1.00        1.00        2.00
        0.00        0.00        3.00
        1.00        1.00        4.00
        1.00        1.00        5.00
        1.00        1.00        6.00
        0.00        0.00        7.00
        1.00        1.00        8.00
        1.00        1.00        9.00
        1.00        1.00       10.00
        0.00        0.00       11.00
        0.00        0.00       12.00
let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstmf.log_solos
tstmf
let _onfail="stop"
ibis-gen a nc=5 nr=10 format=(FULL,REAL,DOUB,REAL,A4)
Beginning VICAR task ibis
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1        1.00        0.10        0.00            
           2        4.00        0.20        0.00            
           3        9.00        0.30        0.00            
           4       16.00        0.39        0.00            
           5       25.00        0.00        0.00            
           6       36.00        0.00        0.00            
           7       49.00        0.00        0.00            
           8       64.00        0.00        0.00            
           9       81.00        0.00        0.00            
          10      100.00        0.00        0.00            
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1        5.50        3.03        1.00            
           2        5.50        3.03        1.00            
           3        5.50        3.03        1.00            
           4        5.50        3.03        1.00            
           5        5.50        3.03        1.00            
           6        5.50        3.03        1.00            
           7        5.50        3.03        1.00            
           8        5.50        3.03        1.00            
           9        5.50        3.03        1.00            
          10        5.50        3.03        1.00            
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1       10.00        1.00       55.00            
           2       10.00        1.00       55.00            
           3       10.00        1.00       55.00            
           4       10.00        1.00       55.00            
           5       10.00        1.00       55.00            
           6       10.00        1.00       55.00            
           7       10.00        1.00       55.00            
           8       10.00        1.00       55.00            
           9       10.00        1.00       55.00            
          10       10.00        1.00       55.00            
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
           1        1.00        1.00       55.00            
           2        3.00        1.00       55.00            
           3        6.00        1.00       55.00            
           4       10.00        1.00       55.00            
           5       15.00        1.00       55.00            
           6       21.00        1.00       55.00            
           7       28.00        1.00       55.00            
           8       36.00        1.00       55.00            
           9       45.00        1.00       55.00            
          10       55.00        1.00       55.00            
ibis-gen a nc=5 nr=10 'IBIS-1 'COLUMN
Beginning VICAR task ibis
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.10        0.00        0.00
        2.00        4.00        0.20        0.00        0.00
        3.00        9.00        0.30        0.00        0.00
        4.00       16.00        0.39        0.00        0.00
        5.00       25.00        0.00        0.00        0.00
        6.00       36.00        0.00        0.00        0.00
        7.00       49.00        0.00        0.00        0.00
        8.00       64.00        0.00        0.00        0.00
        9.00       81.00        0.00        0.00        0.00
       10.00      100.00        0.00        0.00        0.00
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        5.50        3.03        1.00        0.00
        2.00        5.50        3.03        1.00        0.00
        3.00        5.50        3.03        1.00        0.00
        4.00        5.50        3.03        1.00        0.00
        5.00        5.50        3.03        1.00        0.00
        6.00        5.50        3.03        1.00        0.00
        7.00        5.50        3.03        1.00        0.00
        8.00        5.50        3.03        1.00        0.00
        9.00        5.50        3.03        1.00        0.00
       10.00        5.50        3.03        1.00        0.00
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       10.00        1.00       55.00        0.00
        2.00       10.00        1.00       55.00        0.00
        3.00       10.00        1.00       55.00        0.00
        4.00       10.00        1.00       55.00        0.00
        5.00       10.00        1.00       55.00        0.00
        6.00       10.00        1.00       55.00        0.00
        7.00       10.00        1.00       55.00        0.00
        8.00       10.00        1.00       55.00        0.00
        9.00       10.00        1.00       55.00        0.00
       10.00       10.00        1.00       55.00        0.00
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        1.00       55.00        0.00
        2.00        3.00        1.00       55.00        0.00
        3.00        6.00        1.00       55.00        0.00
        4.00       10.00        1.00       55.00        0.00
        5.00       15.00        1.00       55.00        0.00
        6.00       21.00        1.00       55.00        0.00
        7.00       28.00        1.00       55.00        0.00
        8.00       36.00        1.00       55.00        0.00
        9.00       45.00        1.00       55.00        0.00
       10.00       55.00        1.00       55.00        0.00
ibis-gen a nc=5 nr=10 'IBIS-1 'ROW
Beginning VICAR task ibis
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" ) GR1DIM=5
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.10        0.00        0.00
        2.00        4.00        0.20        0.00        0.00
        3.00        9.00        0.30        0.00        0.00
        4.00       16.00        0.39        0.00        0.00
        5.00       25.00        0.00        0.00        0.00
        6.00       36.00        0.00        0.00        0.00
        7.00       49.00        0.00        0.00        0.00
        8.00       64.00        0.00        0.00        0.00
        9.00       81.00        0.00        0.00        0.00
       10.00      100.00        0.00        0.00        0.00
       11.00      121.00        0.00        0.00        0.00
       12.00      144.00        0.00        0.00        0.00
       13.00      169.00        0.00        0.00        0.00
       14.00      196.00        0.00        0.00        0.00
       15.00      225.00        0.00        0.00        0.00
       16.00      256.00        0.00        0.00        0.00
       17.00      289.00        0.00        0.00        0.00
       18.00      324.00        0.00        0.00        0.00
       19.00      361.00        0.00        0.00        0.00
       20.00      400.00        0.00        0.00        0.00
       21.00      441.00        0.00        0.00        0.00
       22.00      484.00        0.00        0.00        0.00
       23.00      529.00        0.00        0.00        0.00
       24.00      576.00        0.00        0.00        0.00
       25.00      625.00        0.00        0.00        0.00
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)") GR1DIM=5
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       13.00        7.36        1.00        0.00
        2.00       13.00        7.36        1.00        0.00
        3.00       13.00        7.36        1.00        0.00
        4.00       13.00        7.36        1.00        0.00
        5.00       13.00        7.36        1.00        0.00
        6.00       13.00        7.36        1.00        0.00
        7.00       13.00        7.36        1.00        0.00
        8.00       13.00        7.36        1.00        0.00
        9.00       13.00        7.36        1.00        0.00
       10.00       13.00        7.36        1.00        0.00
       11.00       13.00        7.36        1.00        0.00
       12.00       13.00        7.36        1.00        0.00
       13.00       13.00        7.36        1.00        0.00
       14.00       13.00        7.36        1.00        0.00
       15.00       13.00        7.36        1.00        0.00
       16.00       13.00        7.36        1.00        0.00
       17.00       13.00        7.36        1.00        0.00
       18.00       13.00        7.36        1.00        0.00
       19.00       13.00        7.36        1.00        0.00
       20.00       13.00        7.36        1.00        0.00
       21.00       13.00        7.36        1.00        0.00
       22.00       13.00        7.36        1.00        0.00
       23.00       13.00        7.36        1.00        0.00
       24.00       13.00        7.36        1.00        0.00
       25.00       13.00        7.36        1.00        0.00
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)") GR1DIM=5
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       25.00        1.00      325.00        0.00
        2.00       25.00        1.00      325.00        0.00
        3.00       25.00        1.00      325.00        0.00
        4.00       25.00        1.00      325.00        0.00
        5.00       25.00        1.00      325.00        0.00
        6.00       25.00        1.00      325.00        0.00
        7.00       25.00        1.00      325.00        0.00
        8.00       25.00        1.00      325.00        0.00
        9.00       25.00        1.00      325.00        0.00
       10.00       25.00        1.00      325.00        0.00
       11.00       25.00        1.00      325.00        0.00
       12.00       25.00        1.00      325.00        0.00
       13.00       25.00        1.00      325.00        0.00
       14.00       25.00        1.00      325.00        0.00
       15.00       25.00        1.00      325.00        0.00
       16.00       25.00        1.00      325.00        0.00
       17.00       25.00        1.00      325.00        0.00
       18.00       25.00        1.00      325.00        0.00
       19.00       25.00        1.00      325.00        0.00
       20.00       25.00        1.00      325.00        0.00
       21.00       25.00        1.00      325.00        0.00
       22.00       25.00        1.00      325.00        0.00
       23.00       25.00        1.00      325.00        0.00
       24.00       25.00        1.00      325.00        0.00
       25.00       25.00        1.00      325.00        0.00
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)") GR1DIM=5
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=5
Beginning VICAR task ibis
 
Number of Rows:25  Number of Columns: 5       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:25
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        1.00      325.00        0.00
        2.00        3.00        1.00      325.00        0.00
        3.00        6.00        1.00      325.00        0.00
        4.00       10.00        1.00      325.00        0.00
        5.00       15.00        1.00      325.00        0.00
        6.00       21.00        1.00      325.00        0.00
        7.00       28.00        1.00      325.00        0.00
        8.00       36.00        1.00      325.00        0.00
        9.00       45.00        1.00      325.00        0.00
       10.00       55.00        1.00      325.00        0.00
       11.00       66.00        1.00      325.00        0.00
       12.00       78.00        1.00      325.00        0.00
       13.00       91.00        1.00      325.00        0.00
       14.00      105.00        1.00      325.00        0.00
       15.00      120.00        1.00      325.00        0.00
       16.00      136.00        1.00      325.00        0.00
       17.00      153.00        1.00      325.00        0.00
       18.00      171.00        1.00      325.00        0.00
       19.00      190.00        1.00      325.00        0.00
       20.00      210.00        1.00      325.00        0.00
       21.00      231.00        1.00      325.00        0.00
       22.00      253.00        1.00      325.00        0.00
       23.00      276.00        1.00      325.00        0.00
       24.00      300.00        1.00      325.00        0.00
       25.00      325.00        1.00      325.00        0.00
ibis-gen a nc=3 nr=12 'IBIS-1 'ROW datacol=(1,2)  +
  data=(1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0)
Beginning VICAR task ibis
mf inp=a function=("x3=poly*2") GR1DIM=3
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        1.00        1.00        4.00
        1.00        1.00        4.00
        1.00        1.00        4.00
        0.00        0.00        0.00
        1.00        1.00        6.00
        1.00        1.00        6.00
        1.00        1.00        6.00
        0.00        0.00        0.00
        0.00        0.00        0.00
mf inp=a function=("x3=AMAX1(x1,x2)+1") GR1DIM=3 'skip
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        0.00
        0.00        0.00        0.00
mf inp=a function=("x3=x1+1") GR1DIM=3
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        1.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        1.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        1.00        1.00        2.00
        0.00        0.00        1.00
        0.00        0.00        1.00
mf inp=a function=("x3=INDEX") GR1DIM=3 'skip
Beginning VICAR task mf
mf version 05-May-2011
ibis-list a GR1DIM=3 nr=12
Beginning VICAR task ibis
 
Number of Rows:42  Number of Columns: 3       
File Version:IBIS-1  Organization:ROW  SubType:NONE
 
Rows: 1:12
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        1.00        1.00
        1.00        1.00        2.00
        0.00        0.00        3.00
        1.00        1.00        4.00
        1.00        1.00        5.00
        1.00        1.00        6.00
        0.00        0.00        7.00
        1.00        1.00        8.00
        1.00        1.00        9.00
        1.00        1.00       10.00
        0.00        0.00       11.00
        0.00        0.00       12.00
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
