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

