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
