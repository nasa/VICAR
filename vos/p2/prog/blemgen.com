$!****************************************************************************
$!
$! Build proc for MIPL module blemgen
$! VPACK Version 1.9, Monday, December 07, 2009, 16:00:53
$!
$! Execute by entering:		$ @blemgen
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
$ write sys$output "*** module blemgen ***"
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
$ write sys$output "Invalid argument given to blemgen.com file -- ", primary
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
$   if F$SEARCH("blemgen.imake") .nes. ""
$   then
$      vimake blemgen
$      purge blemgen.bld
$   else
$      if F$SEARCH("blemgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake blemgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @blemgen.bld "STD"
$   else
$      @blemgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create blemgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack blemgen.com -mixed -
	-s blemgen.c -
	-i blemgen.imake -
	-p blemgen.pdf -
	-t tstblemgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create blemgen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <string.h>
#include "vicmain_c"


#define TRUE 1
#define FALSE 0
#define MAX_INPUTS 5
#define MAX_BLEMS 4090
#define MAX_DN 4095
#define OUT_OF_RANGE(A,B,C) ((A < B) || (A > C))
#define EQUAL(x,y,z) (strncmp(x,y,z)==0)
#define TLEN 9
/*		***** Global variables *****				*/
    int instances[100];
    char tasks[100][TLEN];      /* See under ULEN,Section 4.1 of Porting Guide*/
    int dcode, elements;        /* prnt - dcode & number of elements    */
    int n_inputs;		/* number of input data sets		*/
    int picscale;		/* dark-current picture scale		*/
    int minsat,maxerr,maxrms;	/* limits on sat, err, and rms		*/
    float minslope,maxslope;	/* limits on slope term			*/
    float mindc,maxdc;		/* limits on offset term (dark-current) */
    double sum_slopes;		/* sum of slope terms (good pixels only)*/
    double sum_slopes_squared;  /* sum of squares of slopes             */
    double sum_dc;		/* sum of dark-currents			*/
    double sum_dc_squared;	/* sum of squares of dark-currents	*/
    int bc;                     /* blemish class=0, blemish criteria=1  */
    char message[80];		/* message buffer for zvmessage		*/
    char units[9];
    int blemish_total;		/* total number of blemishes found	*/
    struct BLEMISH
    {
	short line;		/* line number of blemish		*/
	short samp;		/* sample number of blemish		*/
	short class;		/* blemish classification		*/
        short sat_dn;		/* DN at which pixel becomes saturated.	*/
    } blemishes[MAX_BLEMS];

void main44(void)
{
char STRING[120];
    int low, high, nspike, mode;
    int i,j,k;			/* temporary increment variable		*/
    int in_unit[MAX_INPUTS];	/* table of input unit numbers		*/
    int out_unit;		/* output unit number			*/
    int stat;			/* status return for subr calls		*/
    int nl,ns;			/* number of lines, number of samples	*/
    int npblem=0;		/* number of permanent blemishes	*/
    int nsat=0;			/* number of low-full-well pixels	*/
    int nunclass=0;		/* number of unclassified blemishes	*/
    int ndcblem=0;		/* number of double column blemishes	*/
    int *intptr;                /* integer pointer for his, ...         */
    int his[MAX_DN+1];		/* histogram of low-full-well pixels	*/
    int class,isat;		/* temporary class and saturation dn    */
    double r,mean,sigma;	/* temporary statistical variables	*/
    float cal[1024];		/* slopes from GALGEN (one image line)	*/
    short sat[1024];		/* saturation dns from GALGEN		*/
    short err[1024];		/* maximum fit error from GALGEN	*/
    short rms[1024];		/* rms fit error from GALGEN		*/
    short dc[1024];		/* dark-current from GALGEN		*/
    char format[5];		/* DC data format			*/
    int size;                   /* size of his buffer                   */
    int num1=0;                 /* no. of blems failing each criteria   */
    int num2=0;
    int num4=0;
    int num5=0;
    int num6=0;
    int num7=0;
/*  ==================================================================  */
    zifmessage("Blemgen version 28-Jul-97");

    stat = zvpcnt("INP",&n_inputs);

    for (i = 1; i <= n_inputs; i++)
        {
	stat = zvunit(&in_unit[i-1],"INP",i,NULL);
	stat = zvopen(in_unit[i-1],"OPEN_ACT","SA","IO_ACT","SA",NULL);
        }


    stat = zvget(in_unit[0],"NL",&nl,"NS",&ns,NULL);	/* Get NL,NS */

    for (i = 1; i < n_inputs; i++)
        {
        stat = zvget (in_unit[i], "NL",&j,"NS",&k,NULL); /* Check for consistency*/
        if (j != nl || k != ns )
  	    {
	     zvmessage("***Invalid input files","");
	     zabend();
	    }
        }

/* Get DC scale if entered (DC must be entered 5th) */
    if (n_inputs == 5)
	{
	getlabval(&in_unit[4],"PICSCALE",&picscale,&stat);
        if (stat != 1)
		{
                picscale = 1;
		stat = zvget(in_unit[4],"FORMAT",format,NULL);
		if (strcmp(format,"BYTE") != 0)
			{
			zvmessage ("Scale of DC frame ambiguous","");
			zabend();
			}
		}
	}

    init_params();
    blemish_total = 0;

/* Read lines from all inputs (up to 5) and process */

    for (i = 1; i <= nl; i++)
    {
	switch (n_inputs)
	{
	    case 5 :	stat = zvread(in_unit[4], dc, NULL);
	    case 4 :	stat = zvread(in_unit[3], rms, NULL);
	    case 3 :	stat = zvread(in_unit[2], err, NULL);
	    case 2 :	stat = zvread(in_unit[1], sat, NULL);
			stat = zvread(in_unit[0], cal, NULL);
	}

/* Check pixels vs. criteria and accumlate sums of slope and DC */
	flag_blemishes(&stat,cal,sat,err,rms,dc,ns,i);
        if (stat == 4) break;
    }

    sprintf(message,"Total of %d blemishes found",blemish_total);
    zvmessage (message,"");

    r = (nl*ns) - blemish_total;	/* total # of good pixels	*/
    mean = sum_slopes/r;
    sigma = sqrt((double)(sum_slopes_squared/r)-(mean*mean));

    if (EQUAL(units,"LUMINANC",8))
      zvmessage("Energy unit = relative-foot-lambert-milliseconds", "");
    else
      zvmessage("Energy unit = picoamp-milliseconds", "");

    sprintf(message,"Slope -- Mean=%-0.10f Energy unit/DN   Sigma=%-0.4f",
            mean,sigma);
    zvmessage (message,"");

/* Put mean value of slope into slope file label */

    if (EQUAL(units,"LUMINANC",8)) 
      sprintf(message,"RELATIVE-FOOT-LAMBERT-MILLISECONDS");
    else
      sprintf(message,"PICOAMP-MILLISECONDS");

    stat = zvclose(in_unit[0],NULL);
    stat = zvopen(in_unit[0],"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",NULL);
    stat = zladd(in_unit[0],"HISTORY","ENERGY_UNIT",message,
                "FORMAT","STRING","MODE","REPLACE",NULL);
    if (stat != 1) zmabend("Error in zladd");

    if (mean == 0.0) sprintf(message,"1.0/0.0 DN/ENERGY_UNIT");
    else sprintf(message,"%-0.10f DN/ENERGY_UNIT",1./mean);

    stat = zladd(in_unit[0],"HISTORY","MEANSLOP",message,
		 "FORMAT","STRING","MODE","REPLACE",NULL);
    if (stat != 1) zmabend("Error in zladd");
    stat = zvclose(in_unit[0],NULL);

    if (n_inputs == 5)   /*  If DC supplied  */
	{
	mean = sum_dc/(r*picscale);
	sigma= sqrt((double)(sum_dc_squared/r)-(mean*mean));
        sprintf(message," DC -- Mean=%-0.10f DN   Sigma=%-0.4f",mean,sigma);
        zvmessage (message,"");
	sprintf(message,"%-0.10f DN",mean);
	stat = zvclose(in_unit[4],NULL);
	stat = zvopen(in_unit[4],"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",NULL);
	stat = zladd(in_unit[4],"HISTORY","MEANDC",message,
		     "FORMAT","STRING","MODE","REPLACE",NULL);
        if (stat != 1) zmabend("Error in zladd");
	stat = zvclose(in_unit[4],NULL);
	}

    zvmessage ("Classifying blemishes...","");
    i = blemish_total + 1;
    blemishes[i].line = blemishes[i].samp = 32767; /* flag end of buffer */

/* Change criteria entries of blemish buffer into classification */

    if (bc == 0)
        {
        for (i=0; i<blemish_total; i++) classify_blemish(i,nl,ns);
        }

    memset (&his[0], '\0', sizeof(his));

/* Get numbers for summarization */

    for (i=0; i<blemish_total; i++)
	{
	class = blemishes[i].class;
	isat  = blemishes[i].sat_dn;
        if (class == 0) ++nunclass;
        if (class == 1) ++num1;
        if (class == 2) ++num2;
        if (class == 4) ++num4;
        if (class == 5) ++num5;
        if (class == 6) ++num6;
        if (class == 7) ++num7;
        if (class > 15) ++ndcblem;
        if (isat > 0)
            {
            if (isat > MAX_DN) isat = MAX_DN;
            if (isat < 0) isat = 0;
            ++nsat;
            ++his[isat];
	    }
	}

/*  Summarize  */
    npblem = blemish_total - nsat;
    dcode = 4;
    elements = 1;

    zprnt(dcode,elements,&npblem,"Number of permanent blemishes=.");
    zprnt(dcode,elements,&nsat,"Number of low-full-well-pixels=.");

    if (bc != 0)
       {
       zprnt(dcode,elements,&ndcblem,"Number of double column blemishes=.");
       zprnt(dcode,elements,&nunclass, "Number of unclassified blemishes=.");
       zvmessage(" ", "");
       zvmessage(
        "Blemishes found by each criteria (slope,dc,sat,err,rms,lfwp)","");
       sprintf(message," %d %d %d %d %d %d\n",num1,num2,num4,num5,num6,num7);
       zvmessage(message, "");
       }
    zprnt(dcode,elements,&blemish_total,"Total number of blemishes=.");
    zvmessage("Histogram of low-full-well pixels","");

    if (nsat > 0) {
       /* The call to zphist originally had only two parameters; his & nsat ... 
       added 4 default values when porting BLEMGEN to UNIX. */
       low = 0;
       high = MAX_DN;
       nspike = 0;
       mode = 0;
       zphist(&his[0],nsat,low,high,nspike,mode);
    }

    stat = zvunit(&out_unit,"OUT",1,NULL);
    ns = blemish_total * 4;
    stat = zvopen(out_unit,"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",
	"U_FORMAT","HALF","O_FORMAT","HALF","U_NL",1,"U_NS",ns,NULL);
    if (bc == 1)
	{
	stat = zladd(out_unit,"HISTORY","BLEMGEN",
	    "BLEMISH CRITERIA ***WARNING: NOT A VALID BLEMISH FILE",
	    "FORMAT","STRING",NULL);
	}
    else
	{
	 stat = zladd(out_unit,"HISTORY","FILE","BLEMISH FILE",
	    "FORMAT","STRING",NULL);
	}
    stat = zvwrit(out_unit, blemishes, NULL);
    stat = zvclose(out_unit, NULL);
    zvmessage("BLEMGEN done.","");
}

init_params()
{
    int count;		/* TAE COUNT from zvparm			*/

    zvp("MINSLOPE",&minslope,&count);
    zvp("MAXSLOPE",&maxslope,&count);
    zvp("MINDC",&mindc,&count);
    zvp("MAXDC",&maxdc,&count);
    zvp("MINSAT", &minsat, &count);
    zvp("MAXERR", &maxerr, &count);
    zvp("MAXRMS", &maxrms, &count);
    bc = 0;
    if (zvptst("BC")) bc=1;
    mindc = mindc*picscale;		/* Adjust for input dc scale	*/
    maxdc = maxdc*picscale;
    zvp("UNITS", &units, &count);
    return;
}

flag_blemishes(ind, cal, sat, err, rms, dc, ns, line)

int *ind;
int ns,line;
float cal[];
short sat[],err[],rms[],dc[];
{
int samp;		/* current sample				*/
int sat_val;		/* saturation point of current pixel		*/
int is_blem;		/* flag indicating if blem was found		*/
double tmp,sum_sq;	/* temporary variable				*/

sum_sq = 0.0;

for (samp=0; samp<ns; samp++)
    {
    switch (n_inputs)
	{
	case 5:
		is_blem = OUT_OF_RANGE(dc[samp],mindc,maxdc);
		if (is_blem)
			{
			store_blemish(ind,line,samp,2,0);
			goto SKIP;
			}
	case 4:
		if (rms[samp] > maxrms)
			{
			store_blemish(ind,line,samp,6,0);
			goto SKIP;
			}
	case 3:
		if (err[samp] > maxerr)
			{
			store_blemish(ind,line,samp,5,0);
			goto SKIP;
			}
	case 2:
		sat_val = sat[samp];
		if (sat_val < minsat)	/* this includes -1, perm blem	*/
			{
			store_blemish(ind,line,samp,4,0);
			goto SKIP;
			}
	case 1:
		is_blem = OUT_OF_RANGE(cal[samp],minslope,maxslope);
		if (is_blem)
			{
			store_blemish(ind,line,samp,1,0);
			goto SKIP;
			}
	}

    if (sat_val != 32767)		/* low-full-well pixel*/
		{
		store_blemish(ind,line,samp,7,sat_val);
		goto SKIP;
		}

	/* Here if good pixel.  Compute mean and sigmas... */
    tmp = (double)(cal[samp]);
    sum_slopes += tmp;
    sum_slopes_squared += tmp*tmp;
    tmp = (double)(dc[samp]);
    sum_dc += tmp;
    sum_sq += tmp*tmp;
    SKIP:;
    }
    sum_dc_squared += sum_sq/(picscale*picscale);
return;
}

store_blemish(ind,line,samp,criteria,sat_val)

    int *ind;			/* return indicator=0 normal, =4 full   */
    int line;			/* line of blemish found		*/
    int samp;			/* sample number of blemish found	*/
    int criteria;		/* blemish criteria which failed        */
    int sat_val;		/* saturation value of blemish		*/

{
    struct BLEMISH *blem_ptr;

    *ind = 0;
    samp++;		/* change from array index to sample number	*/

    if (blemish_total >= MAX_BLEMS)
    {
	zvmessage("***Blemish total exceeds allocated storage","");
	sprintf(message,"On line %d, sample %d",line,samp);
	zvmessage(message,"");
        if (bc == 0) zabend();
        *ind = 4;
        return;
    }

    blem_ptr = &blemishes[blemish_total];
    blem_ptr->line = line;
    blem_ptr->samp = samp;
    blem_ptr->class = criteria;
    blem_ptr->sat_dn = sat_val;
    blemish_total++;
    return;
}

classify_blemish(index,nl,ns)

    int nl,ns;		/* number of lines, number of samples in image	*/
    int index;		/* index into blemishes array of current blemish*/
{
    short class;	/* "class" of blemish.  see help file		*/
    int line,samp;	/* line & samp of current blemish		*/

    class = 0x000f;	/* isolated pixel				*/
    line = blemishes[index].line;
    samp = blemishes[index].samp;

    if ((samp == 1) || (line == 1) || (samp == ns) || (line == nl))
    {
        blemishes[index].class = 0;
	return;
    }

    if (is_blemish(line-1,samp) || is_blemish(line+1,samp))
	class = 0x000d;

    if (is_blemish(line-1,samp-1) || is_blemish(line+1,samp+1))
        class = class & 0x000e;

    if (is_blemish(line-1,samp+1) || is_blemish(line+1,samp-1))
        class = class & 0x000b;

    if (is_blemish(line,samp-1) || is_blemish(line,samp+1))
        class = class & 0x0007;

    if (class==0)
    {			/* flags and classifies double column blemishes	*/
	if (is_double_column(line,samp,ns,index)) return;
        if (line!= nl-1 && samp!=2)
        {
           sprintf(message,
              "***** Blemish at line %d, sample %d is not classified *****",
              line, samp);
              zvmessage(message,"");
        }
    }
    blemishes[index].class = class;
    return;
}

is_double_column(line,samp,ns,index)
    int line,samp;		/* line sample of blemish		*/
    int ns,index;		/* index into blemishes array		*/
{
    short class;		/* class of blemish (interp scheme)	*/
    int left;			/* good pixel column to left of blemish	*/
    int right;			/* good pixel column to right of blemish*/

    if (samp < ns-1)
    {
    class = 0x001f;		/* First, assume blemish is on left side*/
    left = samp - 1;		/* of double-column blemish...		*/
    right = samp + 2;
    if (is_blemish(line-1,left) || is_blemish(line+1,right))
        class = class & 0x001e;
    if (is_blemish(line,left) || is_blemish(line,right))
        class = class & 0x001d;
    if (is_blemish(line+1,left) || is_blemish(line-1,right))
        class = class & 0x001b;
    if ((class & 0x0007) != 0) goto SUCCESS;
    }
    if (samp > 2)
    {
    class = 0x0017;		/* If that failed, assume blemish is	*/
    left = samp - 2;		/* on right side of double-column	*/
    right = samp + 1;		/* blemish...				*/
    if (is_blemish(line-1,left) || is_blemish(line+1,right))
        class = class & 0x001e;
    if (is_blemish(line,left) || is_blemish(line,right))
        class = class & 0x001d;
    if (is_blemish(line+1,left) || is_blemish(line-1,right))
        class = class & 0x001b;
    if ((class & 0x0007) != 0)	goto SUCCESS;
    }
    class = 0;		/* Here if no good pixels found */
    return FALSE;

		 	/* Here if we have found good pixels interpolate*/
SUCCESS: blemishes[index].class = class;
    return TRUE;
}

is_blemish(line,samp)	/* returns true if point is in blemishes array	*/

    int line,samp;	/* line & samp of proposed blemish		*/
{
    struct BLEMISH *current_blem;	/* pointer to current blemish	*/
					/* in blemishes array		*/

    for (current_blem = blemishes; current_blem->line < line; current_blem++);

    if (current_blem->line > line) return (FALSE);

    for ( ; current_blem->samp < samp; current_blem++)
    {
	if (current_blem->line > line) return (FALSE);
    }	
    if (current_blem->line > line) return (FALSE);

    return (current_blem->samp == samp);
}

/* Return last value (VALUE) of a label item (KEY)
 * Outputs: VALUE, IND
 * Upon return, IND=1 if item is found, =0 otherwise.
 */
getlabval(iunit,key,value,ind)
    int *iunit,*value,*ind;
    char key[];
{ 
    int icnt, j;

    memset (&instances[0], '\0', sizeof (instances));
    memset (&tasks[0][0], '\0', sizeof (tasks));
    icnt = 20;

    *ind = zlhinfo(*iunit,(char *)tasks,instances,&icnt,"ULEN",TLEN,NULL);

    for (j=icnt-1; j>=0; j--)
	{
        *ind = zlget(*iunit,"HISTORY",key,value,"HIST",
               tasks[j],"INSTANCE",instances[j],"NELEMENT",1,NULL);
        if (*ind == 1) return;
	}

    *ind = 0;
    return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create blemgen.imake
#define PROGRAM  blemgen

#define MODULE_LIST blemgen.c

#define MAIN_LANG_C
#define TEST
#define USES_FORTRAN 
#define USES_C
#define R2LIB 
#define LIB_RTL
#define LIB_FORTRAN
#define FTN_STRING 


#define LIB_TAE
#define LIB_P2SUB

$ Return
$!#############################################################################
$PDF_File:
$ create blemgen.pdf
PROCESS HELP=*
  PARM INP	TYPE=STRING		COUNT=2:5
  PARM OUT	TYPE=STRING		COUNT=1
  PARM MINSLOPE	TYPE=REAL		COUNT=1		DEFAULT=-9999
  PARM MAXSLOPE	TYPE=REAL		COUNT=1		DEFAULT=9999
  PARM MINDC	TYPE=REAL		COUNT=1		DEFAULT=-9999
  PARM MAXDC	TYPE=REAL		COUNT=1		DEFAULT=9999
  PARM MINSAT	TYPE=INTEGER		COUNT=1		DEFAULT=0
  PARM MAXERR	TYPE=INTEGER		COUNT=1		DEFAULT=1000
  PARM MAXRMS	TYPE=INTEGER		COUNT=1		DEFAULT=1000
  PARM BC	TYPE=KEYWORD 	COUNT=0:1   VALID=BC	DEFAULT=--
  PARM UNITS    TYPE=KEYWORD    COUNT=(0:1)             DEFAULT=RADIANCE +
                                            VALID=(RADIANCE,LUMINANC)
          
END-PROC
.help
PURPOSE:

   BLEMGEN is a radiometric calibration program which automatically
identifies camera blemishes and generates a Blemish File.  The Blemish
File is a required input to program GALSOS which radiometrically corrects
the flight image data.  The blemishes may be displayed via program BLEMPIC.

   BLEMGEN is specific to CCD cameras with a linear light-transfer function
and is currently used for the Galileo SSI and Cassini ISS.

References: 

D-4264  MIPL Software Structural Design for the Instrument
        Calibration of GLL SSI Science Processing.

D-tbd   Software Design Document for Instrument Calibration -
        Cassini ISS, C. Avis.

.page
EXECUTION STATEMENT:

    BLEMGEN INP=(CAL,SAT,ERR,RMS,DC) OUT=BLEM user-parameters...  

The input files are generated by the radiometric calibration program GALGEN,
and consist of statistical data measuring the linearity at each pixel:

    CAL is the Radiometric File containing the slope terms of the linear
	light-transfer function (REAL*4).
    SAT contains the saturation DN for each low-full-well-pixel (16-bit data).
    ERR contains the maximum absolute difference (in DN) between sample
	points and the fitted line (16-bit data).
    RMS contains the rms error (in DN) from the linear fit (16-bit data).
    DC  is the Dark-Current File containing the offset terms of the linear
	light-transfer function (16-bit data).

These input files are images the same size as that output by the
camera (depends upon summation mode).  Note that if the slope model is
used in GALGEN, then a raw dark-current file or a PICSUM of multiple
dark-current files may be used in place of DC.

The output (BLEM) is a Blemish File containing the (line,sample) location,
classification number, and (for low-full-well pixels) saturation DN of
each identified blemish (16-bit integer).

.page
OPERATION:

The output Blemish File (BLEM) defines each blemish as a vector of the form
(LINE,SAMP,CLASS,SATDN), where LINE and SAMP are the picture coordinates
where the blemish occurs, SATDN is the DN value at which the pixel saturates
at full-well, and CLASS describes the interpolation algorithm to be used to
remove the blemish. The vectors are sorted first on LINE, then on SAMP.

Two types of blemishes are stored in the Blemish File: 1) permanent blemishes
and 2) low-full-well pixels.  Low-full-well pixels are treated as blemishes
only when the DN value exceeds the full-well capacity of the pixel.  Per-
manent blemishes are identified by vectors with SAT=0.  If SAT>0, then the
vector represents a low-full-well pixel.

.page
BLEMISH IDENTIFICATION:

A permanent blemish is any pixel which has one of the following character-
istics:

    o  a light-transfer function whose slope or offset is excessively
       high or low.

    o  a lower-than-average full-well.

    o  light-transfer function nonlinearities greater than specified
       thresholds.

Blemishes are identified by applying user-specified thresholds to the
slope, offset, saturation DN, maximum error, and rms associated with
each pixel of the CCD (see parameters MINSLOPE,MAXSLOPE,MINDC,MAXDC,
MINSAT,MAXERR, and MAXRMS) as follows:

    1) Let z and dc represent the slope and offset terms of the inverse
       light-transfer function
		e = z(d-dc)
       as extracted from input files CAL and DC, respectively.  Then
       z and dc are acceptable if:
		MINSLOPE < z < MAXSLOPE
       and
		   MINDC < dc < MAXDC
       Otherwise, the pixel is identified as a permanent blemish.

    2) Let SATDN be the DN value at which full-well is reached, as
       extracted from the input file SAT.  Then if
		   SATDN < MINSAT
       the pixel is identified as a permanent blemish.  Else if
		   SATDN < 32767
       the pixel is identified as a low-full-well pixel.

    3) Let EMAX represent the maximum absolute difference between the
       light-transfer data points and the fitted curve, as extracted
       from the input file ERR.  Then if
		    EMAX > MAXERR
       the pixel is identified as a permanent blemish.

    4) Let ERMS represent the rms error resulting from the fit, as
       extracted from the input file RMS.  Then if
		    ERMS > MAXRMS
       the pixel is identified as a permanent blemish.

.page
BLEMISH CLASSIFICATION:

The blemish classification is used by GALSOS (GLL) and DECAL (CAS) to 
determine the interpolation algorithm to use in removing the blemishes.  
Blemishes are classified according to which of the surrounding pixels are 
good and hence available for interpolation.  In order to limit the possible 
number of permutations, the surrounding pixels are paired, leaving four 
pairs. If we represent the blemish as a B, and each pair as a number 1-4, 
then we have the following diagram:

                        1   2   3

                        4   B   4

                        3   2   1
  
The  four  least significant bits in the CLASS halfword represent each of
these pairs in order, such that CLASS = 15 would mean to use all 4 pairs,
CLASS = 5 would mean to use pairs 3 and 1, etc.  This results in a range
of values for CLASS from 0-15.  If CLASS=0, then no neighboring pixels
are available for interpolation (i.e. it is surrounded by other blemishes
or the edges of the image.)

The classification scheme is extended to include double-column blemishes
B of the form:

	1 b b 3			 1 b b 3
	2 B b 2		or	 2 b B 2
	3 b b 1			 3 b b 1

where the b represent neighboring blemishes.  The resulting CLASS is
represented in binary as 10xxx and 11xxx for the right and left double-
column blemishes, respectively, where the x's are 1 or 0 depending on the
presence or absence of good pixels at the corresponding locations.  This
results in a range of values for CLASS from 17-23 and 25-31.

All blemishes of CLASS=0 (termed unclassified blemishes) are set equal to
0 by GALSOS or DECAL.  If a blemish lies on one of the edges of the image, 
it is assigned CLASS=0.

Note that this blemish classification scheme is oriented towards the
removal of single pixel blemishes.  The only extended blemishes treated
are line blemishes a single pixel in width, or column blemishes no more
that two pixels in width.

If the keyword BC is specified, then the blemish class is replaced by a
code which indicates which blemish criteria was used in flagging each
blemish:

	1 = slope term out of range
	2 = offset term out of range
	3 = unused
	4 = pixel saturation level exceeds MINSAT
	5 = maximum error exceeds MAXERR
	6 = rms error exceeds MAXRMS  
	7 = low-full-well pixel

The criteria tests are performed in series and a pixel will not be
tested after it fails one test.  The tests are performed in the 
following order (using the above criteria numbers):

            2, 6, 5, 4, 1, 7

WARNING: The BC keyword should be used for diagnostic purposes only.
The resulting output is NOT a valid blemish file.

BLEMGEN prints out the following statistical data:

	1) Mean and standard deviation of slope and offset coefficients
	   for all good pixels (i.e. excluding blemishes).
	2) Number of permanent blemishes.
	3) Number of low-full-well-pixels.
	4) Number of unclassified blemishes.
	5) Number of double column blemishes.
	6) Total number of blemishes (=permanent blemishes+low-full-well
	   pixels).
	7) Histogram of DN values at which low-full-well pixels saturate.
	8) The number of pixels failing each criteria

Note that if isolated unclassified blemishes exist away from the edges of
the image there is a problem.  To display the blemishes, see BLEMPIC.

.page
EXAMPLE:
  galgen INP=(D0,D1,D2,D3,...,Dn)	     !Input light-transfer sequence
    OFFSETS=OFFSETS.DAT +		     !Input Shutter-Offset File
    OUT=(CAL,SAT,ERR,RMS,DC) +		     !Output files
    EXPO=(0,133.22,200,266.67,400,533.33) +  !Commanded exposure times (in msec)
    GAIN=100 +				     !Camera gain-state
    LC=3.54 +
    ERROR=(0,20) SKIP=4

  blemgen INP=(CAL,SAT,ERR,RMS,DC + 	      !GALGEN outputs input to BLEMGEN
    OUT=BLEM +				      !Output blemish file
    MINSLOPE=0.13 MAXSLOPE=18.2 +	      !Limits on slope terms (CAL)
    MINDC=3 MAXDC=95 +			      !Limits on dark-current (DC)
    MINSAT=15 MAXERR=9 MAXRMS=5               !Limits on SAT, ERR, and RMS

  In this example, GALGEN and BLEMGEN are run to generate a Radiometric File
  (CAL), Dark-Current File (DC), and Blemish File (BLEM).  These files,
  together with the Shutter-Offset File (OFFSETS.DAT) are required as inputs
  to GALSOS, which radiometrically corrects Galileo flight images:

  galsos A1 OUT CAL=CAL DC=DC BLEM=BLEM OFFSETS=OFFSETS.DAT +
    IOF=10000. HPER=0.5

.page
HISTORY:

 ORIGINAL PROGRAMMER: Dan Stanfill, Nov 85
 CURRENT COGNIZANT PROGRAMMER: Gary Yagi
 REVISIONS:
  28 Jul 97   S. Pohorsky......Merged Cassini calibration version into MIPS.
                               Per Charlie Avis, changed Cassini units to
                               picoamp-milliseconds.  (Note this changes the
                               default units in preparation for Cassini.  This
                               is minor; only affects the 'Energy units =' 
                               message.)
  08 Jan 95   J. Yoshimizu.....Added UNITS
  10 Oct 94   J. Turner(CRI)...Made portable for UNIX
  26 Aug 94   C. Avis..........Made input buffers 1024 for Cassini
  01 Apr 90   G. Yagi..........Delete spurious messages in BC mode.
  13 Oct 89   G. Yagi..........Fix processing of column 2.
  12 Oct 89   G. Yagi..........Fix double-column blemish routine
  22 Sep 89   G. Yagi..........Switch codes for bad slope and offset
  18 Oct 88   G. Yagi..........Add test in call to PHIST
  28 Apr 88...G. Yagi..........Minor changes to help file.
  12 Dec 87   G. Yagi..........Interface to new Radiometric File format
  20 May 87...G. Yagi..........Compute sigmas in double precision
  14 Aug 86...G. Yagi..........Added BC parameter

.level1
.variable INP
Input calibration data
(CAL,SAT,ERR,RMS,DC)
.variable OUT
Output Blemish File
.variable MINSLOPE
Minimum valid value
of slope term from CAL
.variable MAXSLOPE
Maximum valid value
of slope term from CAL
.variable MINDC
Minimum valid value
of dark-current (DC)
.variable MAXDC
Maximum valid value
of dark-current (DC)
.variable MINSAT
Minimum acceptable
saturation value
(from SAT file)
.variable MAXERR
Maximum allowable
absolute error in
ERR file.
.variable MAXRMS
Maximum allowable
rms error in RMS file.
.variable BC
KEYWORD--OPTIONAL
Store blemish criteria
which failed.
***WARNING***
Use this option for
debugging only.  The
resulting output is
NOT a valid Blemish
File.
.variable UNITS
KEYWORD--OPTIONAL
specifies whether the
energy units are in 
radiance or luminance.
.LEVEL2
.variable INP
Input calibration data: Radiometric slope file, Saturation file, Error file,
RMS error file and the Dark-current file.  All are outputs from the program
GALGEN.  However, the dark-current may be supplied from another source.  In 
addition, the dark-current may be a PICSUMd version or raw.
.variable OUT
The output Blemish File is a file of vectors (one per blemish).  The vector
consists of the components LINE, SAMP, CLASS, and SATURATION DN.  See OPERATION
for the description of each component.
.variable MINSLOPE
REAL - OPTIONAL - Default=-9999.
Minimum valid value of slope term from Radiometric slope file.
.variable MAXSLOPE
REAL - OPTIONAL - Default=9999.
Maximum valid value of slope term from Radiometric slope file.
.variable MINDC
REAL - OPTIONAL - Default=-9999.
Minimum valid value of dark-current from the Dark-current file.  This value has no
scale factors included.
.variable MAXDC
REAL - OPTIONAL - Default=9999.
Maximum valid value of dark-current from the Dark-current file.  This value has no
scale factors included.
.variable MINSAT
INTEGER - OPTIONAL - Default=0
Minimum acceptable saturation value (from Saturation file).  Low-full-well pixels will
have a saturation value less than the maximum DN for the sensor (4095 for Cassini ISS).
.variable MAXERR
INTEGER - OPTIONAL - Default=1000
Maximum allowable absolute error in Error file.  This is the maximum deviation from the 
computed value of any one point used in the GALGEN fit.
.variable MAXRMS
INTEGER - OPTIONAL - Default=1000
Maximum allowable RMS error in RMS error file.  This is the RMS error resulting from 
GALGEN fitting a linear funtion to light transfer points.
.variable BC
KEYWORD--OPTIONAL
Specifies that the output file contain the number of the failed criteria
instead of the CLASS component.  The numbers are:
	1 = slope term out of range
	2 = offset term out of range
	3 = unused
	4 = pixel saturation level exceeds MINSAT
	5 = maximum error exceeds MAXERR
	6 = rms error exceeds MAXRMS  
	7 = low-full-well pixel
***WARNING*** Use this option for debugging only.  The resulting output is NOT a valid 
Blemish File.
.variable UNITS
KEYWORD--OPTIONAL
Specifies whether the energy units are in LUMINANC (Relative-foot-lambert-
milliseconds) or RADIANCE (picoamp-milliseconds).
(UNITS should be LUMINANC for Galileo and RADIANCE for Cassini).
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstblemgen.pdf
procedure
refgbl $echo
refgbl $becho
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
!!!!!!!!!
  local g1                type=string
  local g2                type=string
  local g3                type=string
  local g4                type=string
  local g5                type=string
  local g6                type=string
  local g7                type=string
  local g8                type=string
  local g9                type=string
  local g10               type=string
  local goff              type=string
  local s1                type=string
  local s2                type=string
  local s7                type=string
  local s8                type=string
  local s14               type=string
  local s15               type=string
  local s24               type=string
  local s25               type=string
  local s36               type=string
  local s37               type=string
  local sol               type=string

  let $echo="no"
  if ($syschar(1) = "VAX_VMS")
    let g1   = "wms_test_work:[testdata.mipl.gll]galt1.dat"
    let g2   = "wms_test_work:[testdata.mipl.gll]galt2.dat"
    let g3   = "wms_test_work:[testdata.mipl.gll]galt3.dat"
    let g4   = "wms_test_work:[testdata.mipl.gll]galt4.dat"
    let g5   = "wms_test_work:[testdata.mipl.gll]galt5.dat"
    let g6   = "wms_test_work:[testdata.mipl.gll]galt6.dat"
    let g7   = "wms_test_work:[testdata.mipl.gll]galt7.dat"
    let g8   = "wms_test_work:[testdata.mipl.gll]galt8.dat"
    let g9   = "wms_test_work:[testdata.mipl.gll]galt9.dat"
    let g10  = "wms_test_work:[testdata.mipl.gll]galt10.dat"
    let goff = "wms_test_work:[testdata.mipl.gll]goffsets.img"
  else ! Unix
    let g1   = "/project/test_work/testdata/mipl/gll/galt1.dat"
    let g2   = "/project/test_work/testdata/mipl/gll/galt2.dat"
    let g3   = "/project/test_work/testdata/mipl/gll/galt3.dat"
    let g4   = "/project/test_work/testdata/mipl/gll/galt4.dat"
    let g5   = "/project/test_work/testdata/mipl/gll/galt5.dat"
    let g6   = "/project/test_work/testdata/mipl/gll/galt6.dat"
    let g7   = "/project/test_work/testdata/mipl/gll/galt7.dat"
    let g8   = "/project/test_work/testdata/mipl/gll/galt8.dat"
    let g9   = "/project/test_work/testdata/mipl/gll/galt9.dat"
    let g10  = "/project/test_work/testdata/mipl/gll/galt10.dat"
    let goff = "/project/test_work/testdata/mipl/gll/goffsets.img"
  end-if

!!!!!!!!!
let $echo="yes"
!                     GENERATE CALIBRATION FILES
galgen INP=(&g1,&g2,&g3,&g4 +
            &g5,&g6,&g7,&g8 +
            &g9,&g10)+
 out=(cal.100, sat.100, fit.100, rms.100, dc.100)+
 offsets=&goff +
 gain=100 light=3.54 shutter=(0,7,8,9,10,11,12,13,14,15)+
 'lfwpt error=(0,7) skip=4 numb=(3,3,3,3,3,3,3,3,3,3)
!
!                     DETECT AND CLASSIFY BLEMISHES
blemgen (cal.100,sat.100,fit.100,rms.100,dc.100) blem.100. +
  minslope=.52 maxslope=72.8 mindc=-113.0 maxdc=17.08 +
  minsat=15 maxerr=3 maxrms=1 'luminanc


  if ($syschar(1) = "VAX_VMS")
    let s1   = "wms_test_work:[testdata.cassini.iss]sum2.1"
    let s2   = "wms_test_work:[testdata.cassini.iss]sum2.2"
    let s7   = "wms_test_work:[testdata.cassini.iss]sum2.7"
    let s8   = "wms_test_work:[testdata.cassini.iss]sum2.8"
    let s14  = "wms_test_work:[testdata.cassini.iss]sum2.14"
    let s15  = "wms_test_work:[testdata.cassini.iss]sum2.15"
    let s24  = "wms_test_work:[testdata.cassini.iss]sum2.24"
    let s25  = "wms_test_work:[testdata.cassini.iss]sum2.25"
    let s36  = "wms_test_work:[testdata.cassini.iss]sum2.36"
    let s37  = "wms_test_work:[testdata.cassini.iss]sum2.37"
    let sol  = "wms_test_work:[testdata.cassini.iss]sol.dat"
  else ! Unix
    let s1   = "/project/test_work/testdata/cassini/iss/sum2.1"
    let s2   = "/project/test_work/testdata/cassini/iss/sum2.2"
    let s7   = "/project/test_work/testdata/cassini/iss/sum2.7"
    let s8   = "/project/test_work/testdata/cassini/iss/sum2.8"
    let s14  = "/project/test_work/testdata/cassini/iss/sum2.14"
    let s15  = "/project/test_work/testdata/cassini/iss/sum2.15"
    let s24  = "/project/test_work/testdata/cassini/iss/sum2.24"
    let s25  = "/project/test_work/testdata/cassini/iss/sum2.25"
    let s36  = "/project/test_work/testdata/cassini/iss/sum2.36"
    let s37  = "/project/test_work/testdata/cassini/iss/sum2.37"
    let sol  = "/project/test_work/testdata/cassini/iss/sol.dat"
  end-if
!-----------------------------------------------------------
!CASSINI DATA
picsum (&s1 &s2 ) cgdc
!picsum (&s7 &s8 ) cg1
! (jul2010 / lwk) s8 is missing, so try just using s7 twice:
picsum (&s7 &s7 ) cg1
picsum (&s14 &s15 ) cg2
picsum (&s24 &s25 ) cg3
picsum (&s36 &s37 ) cg4
galgen (cg1,cg2,cg3,cg4) out=(cal,sat,err,rms,dc) dc=cgdc +
        offset=&sol light=5.1

let $echo="no"
let $becho="no"
write "Find none this time"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=0 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Find some based on slope"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat,err,rms,dc) blem +
  minslope=5 maxslope=20 mindc=-100 maxdc=500 +
  minsat=0 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Based on DC"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-10 maxdc=30. +
  minsat=0 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Based on ERR"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=0 maxerr=30 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Based on RMS"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=0 maxerr=1000 maxrms=30 'lumin


let $echo="no"
let $becho="no"
write "Does it find Low Full Well pixels ok?"
write "Make 512 oddballs in the SAT file."
let $echo="yes"
let $becho="yes"
f2 sat sat2 func="(LINE.EQ.SAMP)*(LINE/10-32766.5) + IN1"
list sat2 (5,5,12,12)


let $echo="no"
let $becho="no"
write "Based on SAT" 
let $echo="yes"
let $becho="yes"
blemgen (cal,sat2,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=25 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Zero out all elements of blemish file but the CLASS" 
write "convert to byte and histogram to see kinds of classes"
let $echo="yes"
let $becho="yes"

f2 blem b 'byte func="(MOD(SAMP,4).EQ.3)*IN1"
hist b

let $echo="no"
let $becho="no"
write "Find just the LFWP"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat2,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=0 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Use all above criteria and output codes in blem file"
write "then do histogram of the codes to check the count of each"
let $echo="yes"
let $becho="yes"
blemgen (cal,sat2,err,rms,dc) blem +
  minslope=5 maxslope=20 mindc=-10 maxdc=30 +
  minsat=25 maxerr=30 maxrms=30 'bc  'lumin

!zero out all but the words with the codes in them (3rd of 4 for each blem)
f2 blem bx func="(MOD(SAMP,4).EQ.3)*IN1" 'byte
hist bx

let $echo="no"
let $becho="no"
write "In above histogram:  level 0 - ignore"
write "                     level 1 - bad slopes"
write "                     level 2 - bad dc"
write "                     level 4 - bad SAT"
write "                     level 5 - bad ERR"
write "                     level 6 - bad RMS"
write "                     level 7 - Low Full Well"
write "Note:  some pixels may have satisfied more than on criteria, but"
write "       are finally bookkept in only one place"
write " "
write "Does it handle Low Full Well pixels in the halfword range?"
write "Make 512 oddballs in the SAT file ranging from 4 to 2048"
write "MINSAT=1028 will make 256 of them LFW pixels."
let $echo="yes"
let $becho="yes"

f2 sat sat2 func="(LINE.EQ.SAMP)*(LINE*4-32767) + IN1"
list sat2 (1,1,5,5)

blemgen (cal,sat2,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=1028 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "Check blemish classifying.  Generating blemishes with known"
write "locations and therefore known classes.  After BLEMGEN, the"
write "classes are printed in order of blemish number."
let $echo="yes"
let $becho="yes"

copy  cal c (101,101,11,7)
copy  sat s (101,101,11,7)
copy  err e (101,101,11,7)
copy  rms r (101,101,11,7)
copy  dc  d (101,101,11,7)
genthis g nl=11 ns=7 +           !make 20 blems various classes
dn=(1,1,1,1,1,1,1,+
    1,9,1,9,1,1,1,+
    1,1,1,9,1,1,1,+
    1,1,9,9,9,9,1,+
    1,1,9,1,9,1,1,+
    1,1,9,9,9,9,1,+
    1,1,9,1,1,1,1,+
    1,1,9,1,9,1,1,+
    1,1,1,9,1,1,1,+
    1,9,9,1,9,1,1,+
    1,1,1,1,1,1,1)

let $echo="no"
let $becho="no"
write "List out where the blemishes will be for class checking"
write "The 9's will become blemishes, the 1's won't"
let $echo="yes"
let $becho="yes"

list g 'dump

let $echo="no"
let $becho="no"
write "The classes for these blemish locations should be:"
write "      15     13"
write "              8"
write "          1  25  4  3"
write "          8      8"
write "          5   2  5  6"
write "          9"
write "         12     11"
write "             10"
write "       7  3     14"
write " "
write "or when put in order of blemish number:"
write " 15,13,8,1,25,4,3,8,8,5,2,5,6,9,12,11,10,7,3,14"
write " "
let $echo="yes"
let $becho="yes"

f2 (c,g) cal2 func=in1*in2

blemgen (cal2,s,e,r,d) blem.tmp +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=0 maxerr=1000 maxrms=1000 'lumin

let $echo="no"
let $becho="no"
write "weed out all but class from blem file and list"
let $echo="yes"
let $becho="yes"

size blem.tmp t zoom=-2 'noin
local nsx integer
form t ns=nsx
let nsx=nsx-1
copy t u (1,2,1,&nsx)
size u v zoom=-2 'noin
list v 'dump

let $echo="no"
let $becho="no"
write "Check out 'RADIANCE"
let $echo="yes"
let $becho="yes"

blemgen (cal,sat,err,rms,dc) blem +
  minslope=0 maxslope=50 mindc=-100 maxdc=500 +
  minsat=0 maxerr=1000 maxrms=1000 

end-proc

$ Return
$!#############################################################################
