
/*  Welcome to GIACONDA */
/*  (Generator of Images in Accurate Color On Numerous Digital Apparatus). */

/*  Revision History:
    5 September 1994 ... CRI ... MSTP S/W Conversion (VICAR Porting).
                                 In porting GIACONDA, the contents of the 
                                 include file 'colors.h' was incorporated 
                                 in-line within this file.  The two structures
                                 ILLUMINANT & MATCH were defined as static.
                                 
*/

#include <math.h>
#include <ctype.h>
#include <stdio.h>
#include "vicmain_c"


#define SSIZE 50					/*spectrum size*/
#define DELTA_LAMBDA 10					/*lambda increment(nm)*/
#define START_LAMBDA 290				/*first lambda (nm)*/
#define NCOLORS 100					/*colors in file*/
#define LCOLOR 10					/*length of names*/
#define LCOMMENT 52					/*length of comments*/
#define MFILTERS 8					/*max filters*/
#define MCOLORS 10					/*max special colors*/
#define MLINES 2000
#define MSAMPLES 2000
#define EPSILON 1E-36					/*small float*/
#define TVEXP 2.6					/*tv intensity = */
  							/*  dn ** TVEXP  */

#define D55 1						/*ILLUMINANT[D55]=sun*/
#define FLR 2						/*ILLUMINANT[FLR]=   */
  							/* flourescent light */
#define RED 0						/*index to arrays*/
#define GREEN 1
#define BLUE 2
#define BLACK 3

#define mm(i,j) matrix[(i) * size + (j)]		/*for minverse(),mdet()*/
#define mi(i,j) inverse[(i) * size + (j)]		/*for minverse()*/
#define mc(i,j) comatrix[(i) * (size - 1) + (j)]	/*for minverse()*/

struct color {						/*color description*/
  char name[LCOLOR + 1];
  char comment[LCOMMENT + 1];
  int light;
  float intensity[SSIZE];				/*spectrum*/
  float tristim[3];					/*tristimulus values*/
};

int   int_ts;                           /* integer for translation */
float real1_ts;                         /* float for translation */
float real2_ts;                         /* float for translation */
int   int_conv[12];                     /* translation buffer */
int   real_conv[12];                    /* translation buffer */
int   int_size, real_size;              /* Pixel sizes */

static double ILLUMINANT[][SSIZE] = {				/*illuminants*/
  {1,1,1,1,1,1,1,1,1,1,					/*white*/
   1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1},
  {0,.02,2.1,11.2,20.6,23.9,27.8,30.6,34.3,32.6,	/*D55*/
   38.1,61.0,68.6,71.6,67.9,85.6,98.0,100.5,99.9,102.7,
   98.1,100.7,100.7,100.0,104.2,102.1,103.0,100.0,97.2,97.7,
   91.4,94.4,95.1,94.2,90.4,92.3,88.9,90.3,93.9,90.0,
   79.7,82.8,84.8,70.2,79.3,85.0,71.9,52.8,75.9,71.8},
  {0,0,0,0,0,0,0,0,0,7.5,				/*std flour. cool white*/
   12.5,17.7,23.1,27.4,33.4,38.7,44.3,46.7,50.2,49.7,
   50.0,48.4,45.7,47.4,53.2,64.7,81.6,100.0,115.0,120.8,
   116.6,103.1,84.8,69.8,52.7,40.5,30.5,22.0,15.5,9.5,
   5.0,2.0,.5,.2,.1} 
};

/*CIE standard observer: indices = x/y/z,lamda*/

static double MATCH [3][SSIZE] = {
  { 0,0,0,0,0,0,0,0,0,.0014,
    .0042,.0143,.0435,.1344,.2839,.3483,.3362,.2908,.1954,.0956,
    .032,.0049,.0093,.0633,.1655,.2904,.4334,.5945,.7621,.9163,
    1.0263,1.0622,1.0026,.8544,.6424,.4479,.2835,.1649,.0874,.0468,
    .0227,.0114,.0058,.0029,.0014,.0007,.0003,.0002,.0001,0},
  { 0,0,0,0,0,0,0,0,0,0.,
    .0001,.0004,.0012,.004,.0116,.023,.038,.06,.091,.139,
    .208,.323,.503,.71,.862,.954,.995,.995,.952,.87,
    .757,.631,.503,.381,.265,.175,.107,.061,.032,.017,
    .0082,.0041,.0021,.001,.0005,.0002,.0001,.0001,0.,0},
  { 0,0,0,0,0,0,0,0,0,.0065,
    .0201,.0679,.2074,.6456,1.3856,1.7471,1.7721,1.6692,1.2876,.813,
    .4652,.272,.1582,.0782,.0422,.0203,.0087,.0039,.0021,.0017,
    .0011,.0008,.0003,.0002,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0}
};

#define CAM 5						/*for able77v2()*/
#define FILT 3	
#define SCID 18
#define LSIZE 19

#define BIT0 1

#define max(a,b) ((a) > (b)) ? (a) : (b)		/*maximum*/
#define repeat(index,low,high) for (index = low; index < high; index++)
/* mm(i,j), mi(i,j), & mc(i,j) are defined in colors.h */

int  status;
int  input_unit[MFILTERS];
char colors[110],dev[10];
int  nin,ncolors,ocolor[MCOLORS],lcolor[MCOLORS],nweights,nout,scale;
int  nvalues,def;
int  nlines,nsamples;
char alpha_buf[8 * 133];
float weight[MCOLORS];
double itr[3][MFILTERS],offset[3];

struct color *file;	
struct color *pfile[MCOLORS];
double *response[MFILTERS];
enum {VGR,GLL} project[MFILTERS];
double exponent;

double minvert(),mdet(),mcof(),fabs(),pow();



double V4_FILTERS[][SSIZE] = {				/*SN 4*/
  {0},							
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,2,29,234,522,505,503,492,415,
   334,281,240,198,139,98,24,9,4,3,
   1},
  {0,0,0,0,0,0,0,0,0,5,					/*clear*/
   34,112,243,405,515,580,595,563,529,449,
   363,307,274,250,265,301,314,316,296,253,
   202,153,108,68,37,12,6},
  {0,0,0,0,0,0,0,0,0,4,					/*violet*/
   32,104,222,377,481,415,247,56,13,4},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 6190*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,
   0,0,2,39,8},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,3,
   5,15,36,84,206,283,272,275,271,243,
   198,148,103,64,34,11,5},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 5410*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,122,29},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,2,4,11,31,61,
   110,134,97,57,31,11,5}
};  



double V5_FILTERS[][SSIZE] = {				/*SN 5*/
  {3,14,41,85,122,154,191,239,275,321,			/*clear*/
   319,358,406,415,450,470,468,447,418,393,
   370,344,321,299,304,307,299,279,247,212,
   172,130,92,63,37,19,11,6},
  {0,0,0,0,0,0,13,71,177,267,				/*violet*/
   298,334,382,392,380,282,129,25,10,4},
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,2,13,73,341,422,368,362,368,
   354,321,284,213,187,100,37,14,6,3,
   1},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,1,2,4,9,22,57,126,179,
   158,120,86,61,36,19,11,6},
  {3,14,41,85,122,154,191,239,275,321,			/*clear*/
   319,358,406,415,450,470,468,447,418,393,
   370,344,321,299,304,307,299,279,247,212,
   172,130,92,63,37,19,11,6},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   8,32,59,135,253,281,262,255,238,209,
   171,129,89,60,34,18,11,6},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   7,21,40,105,237,283,274,261,238,207,
   170,128,87,59,36,18,11,6}
};  



double V6_FILTERS[][SSIZE] = {				/*SN 6*/
  {0},
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,4,40,281,558,560,583,555,486,
   451,421,384,343,228,110,43,18,8,3,
   1},
  {0,0,0,0,0,0,0,0,0,5,					/*clear*/
   42,143,295,441,557,636,657,648,595,524,
   487,459,430,430,443,511,567,595,566,506,
   409,317,238,178,122,81,55,36,12},
  {0,0,0,0,0,0,0,0,0,4,					/*violet*/
   39,134,271,410,525,474,318,81,21,
   5},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 6190*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,
   0,0,5,102,27},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,0,
   6,19,61,144,339,480,502,532,529,492,
   402,308,227,164,111,75,52,35,12},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 5410*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,655,52},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,2,6,18,51,118,
   229,278,213,152,105,74,51,34,11}
};



double V7_FILTERS[][SSIZE] = {				/*SN 7*/
  {3,16,42,70,101,135,178,220,268,293,			/*clear*/
   310,333,369,398,432,449,445,442,415,388,
   361,333,314,299,303,308,300,280,251,214,
   172,138,103,66,44,23},
  {0,0,0,0,0,0,12,65,172,244,				/*violet*/
   289,311,347,377,364,270,122,24,10,4},
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,2,12,76,348,392,362,359,361,
   344,310,275,236,177,95,35,13,5,2,
   1},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,1,2,5,14,42,108,182,
   167,129,97,63,42,23},
  {3,16,42,70,101,135,178,220,268,293,			/*clear*/
   310,333,369,398,432,449,445,442,415,388,
   361,333,314,299,303,308,300,280,251,214,
   172,138,103,66,44,23},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   4,10,24,60,106,258,260,247,235,209,
   170,137,102,63,43,21},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   5,16,39,93,197,261,255,247,236,212,
   171,137,102,63,41,21}
};



double GLL_FILTERS[][SSIZE] = {
  {0,0,0,0,0,                                             /*CLEAR*/
   0,0,0,0,19730,
    850900,1225000,1206000,1095000,1082000,
   1156000,1209000,1356000,1507000,1650000,
   1836000,2039000,2132000,2268000,2333000,
   2338000,2359000,2357000,2360000,2439000,
   2610000,2774000,2678000,2577000,2508000,
   2474000,2435000,2610000,2786000,2919000,
   3007000,3094000,2709000,2290000,2018000,
   1845000,1678000,1611000,1527000,1501000},
  {0,0,0,0,0,                                             /*5600*/
   0,0,0,0,0,
   0,0,0,14230,0,
   0,0,0,0,0,
   0,0,0,115400,127600,
   1936000,2078000,1962000,1931000,1870000,
   1705000,485900,13420}, 
  {0,0,0,0,0,                                             /*6600*/
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,26850,103500,1826000,
   1848000,2194000,2356000,2352000,2427000,
   988000,345200,13750,6978,4104},
  {0,0,0,0,0,                                            /*4450*/
   0,0,0,0,15070,
   580100,853600,831400,563400,698300,
   93020,16820,4318,0,0,
   0,0,0,0,0,
   0,0,0,2384,0,
   0,0,0,0,0,
   0,0,0,0,0,
   3040,3138,2750,2326},
  {0,0,0,0,0,                                             /*7560*/
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   24420,1420000,1525000,32890,1543},
  {0},                                                    /*9680*/ 
  {0,0,0,0,0,                                             /*7270*/
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,46520,1744000,
   1879},
  {0}                                                     /*8890*/
};

struct camera {
  int sn;						/*serial number*/
  double (*pfilter)[SSIZE];				/*pointer to filters*/
};
struct camera V1_CAMERAS[] = {{6,V6_FILTERS},
  			      {7,V7_FILTERS},
  			      {0}
};
struct camera V2_CAMERAS[] = {{4,V4_FILTERS},
  			      {5,V5_FILTERS},
  			      {0}
};
struct camera GLL_CAMERAS[] = {{1,GLL_FILTERS},
  			       {0}
};

struct {
  int id;						/*"spacecraft id"   */
  struct camera *pcamera;				/*  AS IN ABLE77V2()*/
} CRAFT[] = {{2,V2_CAMERAS},
  	     {1,V1_CAMERAS},
  	     {77,GLL_CAMERAS},
  	     {0}
};



void main44(void)
{

  int  colors_unit, count;
  int  iinput,instance;
  int  nl_latest,ns_latest;
  char format[5];
  char scf_name [80];

  zifmessage ("GIACONDA version 5-Sept-1994");

  status = zvp ("SCFNAME", scf_name, &count);
  check_status(1,0);

  status = zvunit(&colors_unit,"NONE",1,"U_NAME",scf_name, NULL);
  check_status(1,1);

  status = zvopen(colors_unit,"ADDRESS",&file, NULL);	/*open color file*/
  if (!(status & BIT0)) {				/*if can't find file*/
    zvmessage ("ASSIGN SPECIAL_COLORS TO A COLOR LIBRARY",0);
    return;
  }
  check_status(1,1);

  status = zvtrans_inu (int_conv, "FULL", "FULL", colors_unit);
  check_status(1,2);
  status = zvtrans_inu (real_conv,"REAL", "REAL", colors_unit);
  check_status(1,2);

  status = get_parms();
  if (status == -1) return;				/*FATAL USER ERROR*/

  status = zvunit(&input_unit[0],"INP",1, NULL);
  check_status(1,3);

  status = zvopen(input_unit[0],"OPEN_ACT","SA", NULL);	/*open 1st input*/
  check_status(1,5);

  status = zvget(input_unit[0],				/*get nl,ns,format*/
  	"NL",&nlines,"NS",&nsamples,"FORMAT",format, NULL);	
  check_status(1,5);

  if (nlines > MLINES || nsamples > MSAMPLES) {		/*if too big*/
    sprintf(alpha_buf,					/*FATAL USER ERROR*/
  	    "IMAGES TOO LARGE -- LIMIT IS %d BY %d",
  	    MLINES,MSAMPLES);
    zvmessage (alpha_buf,0);
    return;
  }

  if (strncmp(format,"HALF",4)) {			/*if not halfword*/
    zvmessage ("ERROR -- INPUTS MUST BE HALFWORD",0);
    return;						/*FATAL USER ERROR*/
  }

  repeat(iinput,1,nin) {				/*for other inputs*/
    instance = iinput + 1;
    status = zvunit(&input_unit[iinput],"INP",instance, NULL);
    check_status(1,6);
    status = zvopen (input_unit[iinput],"OPEN_ACT","SA",NULL);/*open the file*/
    status = zvget (input_unit[iinput],    			/*get nl,ns*/
  	  "NL",&nl_latest,"NS",&ns_latest,NULL);
    check_status(1,8);
    if (nlines != nl_latest || nsamples != ns_latest) {	/*same size as 1st?*/ 
     zvmessage ("ALL INPUT IMAGES ARE NOT THE SAME SIZE",0);/*FATAL USER ERROR*/
     return;
    }
    if (strncmp(format,"HALF",4)) {			/*if not halfword*/
      zvmessage ("ERROR -- INPUTS MUST BE HALFWORD",0);
      return;						/*FATAL USER ERROR*/
    }
  }

  repeat(iinput,0,nin) {				/*for each input*/
    status = get_project(iinput);
    if (status == 0) {					/*not found*/
      return;
    }
    get_response(iinput);
  }

  status = ioverf_to_rgb();
  if (status == -1) return;				/*FATAL USER ERROR*/
  if (nout) {						/*if outputs specified*/
    create_output();
  }
}



get_project(input)
/*This function assigns to project[input], a number describing the project
indicated by the image label.  A 1 is returned if the project is determined, a 0
otherwise.*/ 

int input;
{
  char task[9],craft[133];
  int nhist,instances[1];

  nhist = 1;
  status = zlhinfo(input_unit[input],task,instances,&nhist, NULL);
  check_status(4,1);
  status = zlget(input_unit[input],"HISTORY",		/*look in LAB02*/
  	"LAB02",craft,"HIST",task, NULL);
  if (status != CANNOT_FIND_KEY) {
    check_status(4,2);
    if (!strncmp(craft,"VGR",3)) {			/*if VGR image*/
      project[input] = VGR;
      return 1;
    }    
  }

  status = zlget(input_unit[input],"HISTORY",		/*look in LAB01*/
  	"LAB01",craft,"HIST",task, NULL);
  if (status != CANNOT_FIND_KEY) {
    check_status(3,4);
    if (!strncmp(craft,"GLL",3)) {			/*if GLL image*/
      project[input] = GLL;
      return 1;
    }
  }

  zvmessage("ERROR -- INPUTS MUST BE VGR OR GLL IMAGES",0);/*none of the above*/
  return 0;
}



/*This function assigns to response[input], a pointer to an array describing
the response of the filter described in the image label.*/

get_response(input)
int input;
{
  int isc,icamera;
  int label[LSIZE];
  label[0] = LSIZE;					/*for able77v2()*/
  switch (project[input]) {
    case VGR:
      zable77v2(&status,input_unit[input],label);	/*get label info*/
      status += 1;					/*normal = 0*/
      check_status(3,3);
      for (isc = 0;					/*find spacecraft*/
	   CRAFT[isc].id != label[SCID] && CRAFT[isc].id;
	   isc++);
      for (icamera = 0;					/*find camera*/
	   (CRAFT[isc].pcamera + icamera)->sn != label[CAM] &&
	   (CRAFT[isc].pcamera + icamera)->sn;
	   icamera++);
      response[input] =				/*point to data*/
	(CRAFT[isc].pcamera + icamera)->pfilter[label[FILT]];
      break;

    case GLL:
      zable86(&status,input_unit[input],label);	/*get label info*/
      status += 1;
      check_status(3,5);
      response[input] = GLL_CAMERAS[0].pfilter[label[FILT]];
      break;
    default:
      break;
  }
}



/* The get_parms function gets the input parameters and check for their 
   consistency. get+_parms returns a -1 for a fatal user error, a 1 otherwise.*/

get_parms()
{

  int icolor, ifile;

  zvparm ("INP",alpha_buf,&nin,&def,MFILTERS,133); /* Length */
  zvparm ("OUT",alpha_buf,&nout,&def,3,133); /* Length */
  zvparm ("COLORS",colors,&ncolors,&def,MCOLORS,0);
  zvsptr (colors, ncolors, ocolor, lcolor);
  zvparm ("WEIGHTS",weight,&nweights,&def,MCOLORS,0); /* Length */
  zvparm ("DEVICE",dev,&nvalues,&def,1,0);
  zvparm ("SCALE",&scale,&nvalues,&def,1,0);

  if (nin > ncolors) {
    zvmessage ("SPECIFY AT LEAST AS MANY COLORS AS THERE ARE INPUTS",0);
    return -1;
  }
  if (nweights > ncolors) {
    zvmessage ("TOO MANY WEIGHTS",0);
    return -1;
  }
  repeat(icolor,nweights,ncolors) {		/*default weight = 1*/
    weight[icolor] = 1;
  }


  repeat(icolor,0,ncolors) {			/*for each special color*/
    for (ifile = 0;				/*find in file*/
  	 strcmp(file[ifile].name,
  	        colors + ocolor[icolor] - 1) &&
  	 ifile < NCOLORS;
  	 ifile++);
    pfile[icolor] = file + ifile;			/*save the address*/

    if (ifile == NCOLORS) {				/*if not there*/
      sprintf(alpha_buf,"COLOR \"%s\" DOES NOT EXIST",
  	      colors + ocolor[icolor] - 1);
      zvmessage (alpha_buf,0);				/*tell the user*/
      return -1;
    }
  }
  return 1;
}



ioverf_to_rgb()

/*This function calculates I/F for each special color through each of the
relevant filters, and then gets the matrix which transforms those values to
the corresponding tristimulus values by doing a least-squares solution to the
resulting equations using the special colors as the data points.  The
resulting matrix is then multiplied times the device matrix (transforming
X,Y,Z to R,G,B) and, finally scaled so that the maximum possible image DN
results in a maximum output of 255.  The function returns -1 if there is the
problem is underdetermined.*/ 

{
  int mode,i,j,iinput,jinput,icolor,lamda,rgb;
  double data[MFILTERS],
  	 matrix[MFILTERS * MFILTERS],
  	 inverse[MFILTERS * MFILTERS],
  	 ioverf[MFILTERS][MCOLORS],
  	 itt[3][MFILTERS],
  	 ttr[3][3];
  float con[MFILTERS];
  double intensity,flux;
  int size;
  double determinant,adjustment,out_max[3],max_max;
  float devmat[3][3],devoff[3];
  int ndevmat,ndevoff;
  char label_buf[7200];
  size = nin;						/*for mm(),mi()*/

  repeat(iinput,0,nin) {				/*for each filter*/
    flux = 0;						/*get flux*/
    repeat (lamda,0,SSIZE) {
      flux += response[iinput][lamda] *
  	ILLUMINANT[D55][lamda];
    }
    repeat(icolor,0,ncolors) {		            /*for each special color*/
      intensity = 0;					/*get intensity*/
      repeat (lamda,0,SSIZE) {
        zvtrans (int_conv, &(pfile[icolor]->light), &int_ts, 1);
        zvtrans (real_conv, &(pfile[icolor]->intensity[lamda]), &real1_ts, 1);
        intensity += response[iinput][lamda] *
  	  ILLUMINANT[int_ts][lamda] *
  	  real1_ts;
      }
      ioverf[iinput][icolor] = intensity/flux;		/*I/F*/
    }
  }


  repeat(i,0,3) {					/*for x,y,z*/
    repeat(iinput,0,nin) {				/*get data vector*/	
      data[iinput] = 0;
      repeat(icolor,0,ncolors) {
        zvtrans (real_conv, &(pfile[icolor]->tristim[i]), &real2_ts, 1);
        data[iinput] += weight[iinput] *		
          ioverf[iinput][icolor] *
  	  real2_ts;
      }
    }

    repeat(iinput,0,nin) {				/*get msmt. matrix*/
      repeat(jinput,0,nin) {
  	mm(iinput,jinput) = 0;
        repeat(icolor,0,ncolors) {
  	  mm(iinput,jinput) +=
  	    weight[icolor] *
  	    ioverf[iinput][icolor] *
  	    ioverf[jinput][icolor];
        }
      }
    }

    determinant = minvert(matrix,inverse,nin);		/*invert matrix*/
    if (fabs(determinant) < EPSILON) {			/*no unique solution*/
      zvmessage("NO UNIQUE SOLUTION -- SEE HELP (EXECUTION)",0);
      return -1;
    }
    repeat(iinput,0,nin) {				/*data * inv.-msmt. = */
      itt[i][iinput] = 0;				/* i/f-to-tristim     */
      repeat(jinput,0,nin) {
        itt[i][iinput] +=
  	  mi(iinput,jinput) *
  	  data[jinput];
      }
    }
  }



  zvparm("DEVMAT",devmat,&ndevmat,&def,9,0);		/*user device matrix*/
  if (ndevmat) {					/*if there is one*/
    zvparm("DEVOFF",devoff,&ndevoff,&def,3,0);		/*user offset*/
    repeat (i,0,3) {
      offset[i] = devoff[i];				/*copy offset*/
      repeat (j,0,3) {
        ttr[i][j] = devmat[i][j];			/*copy matrix*/
      }
    }
    exponent = 1;					/*linear response*/
  }
  else {
    status = xyztorgb(dev,ttr,offset);			/*get rgb(x,y,z)*/
    exponent = TVEXP;					/*exponent. response*/
  } 
 
  repeat(rgb,0,3) {				/*mult. by device matrix*/
    out_max[rgb] = 0;
    repeat(iinput,0,nin) {
      itr[rgb][iinput] =				/*i/f-to-rgb*/
        ttr[rgb][0] * itt[0][iinput] +
        ttr[rgb][1] * itt[1][iinput] +
        ttr[rgb][2] * itt[2][iinput];
      out_max[rgb] += itr[rgb][iinput];			/*itr term for I/F=1*/
    }
  }

  max_max = max(out_max[0],out_max[1]);			/*max for I/F=1*/
  max_max = max(max_max,out_max[2]);

  adjustment = pow(255.,exponent) / (max_max * scale);
  repeat(iinput,0,nin) {
    if (project[iinput] == VGR) {
      /*get ficor multiplier*/
      mode = 1;
      zficor (input_unit[iinput], label_buf, &con[iinput], mode);
    }
    else {
      con[iinput] = 1.0;
    }
  }
  zvmessage (" ",0);
  zvmessage ("TRANSFORMATION MATRIX:",0);		/*hdr for xform matrix*/
  zvmessage ("INPUT           RED            GREEN           BLUE",0);
  repeat(iinput,0,nin) {
    repeat(rgb,0,3) {
      itr[rgb][iinput] *=				/*adjust to max out=255*/
        adjustment * con[iinput];
    }
    sprintf(alpha_buf,"%-d    %15.5f %15.5f %15.5f",
  	    iinput + 1,
  	    itr[RED][iinput],
  	    itr[GREEN][iinput],
  	    itr[BLUE][iinput]);
    zvmessage (alpha_buf,0);				/*print xform matrix*/
  }  
}



create_output()
/*This function multiplies the input images by the matrix itt[][], thus
creating the red, green, and blue output images*/ 

{
  int red_unit,green_unit,blue_unit;
  double red,green,blue,max_intensity;
  unsigned char red_output[MSAMPLES],green_output[MSAMPLES],
    blue_output[MSAMPLES];
  unsigned short input[MFILTERS][MSAMPLES],*pinput;
  int iinput,line,sample,lamda;

  status = zvunit(&red_unit,"OUT",1, NULL);		/*create output files*/
  check_status(2,1);
  status = zvunit(&green_unit,"OUT",2, NULL);
  check_status(2,2);
  status = zvunit(&blue_unit,"OUT",3, NULL);
  check_status(2,3);
  status = zvopen(red_unit,"OP","WRITE",
  	 "O_FORMAT","BYTE","U_FORMAT","BYTE",
  	 "OPEN_ACT","SA", NULL);
  status = zvopen(green_unit,"OP","WRITE",
  	 "O_FORMAT","BYTE","U_FORMAT","BYTE",
  	 "OPEN_ACT","SA", NULL);
  status = zvopen(blue_unit,"OP","WRITE",
  	 "O_FORMAT","BYTE","U_FORMAT","BYTE",
  	 "OPEN_ACT","SA", NULL);


  repeat(line,0,nlines) {				/*for each line*/
    repeat(iinput,0,nin) {				/*for each input*/
      status = zvread(input_unit[iinput],input[iinput], NULL);	/*read a line*/
    }
    repeat(sample,0,nsamples) {				/*for each sample*/

      red = offset[RED];				/*output=itr*input+ */
      green = offset[GREEN];				/*  offset	    */
      blue = offset[BLUE];
      for (iinput = 0, pinput = &input[0][sample];
  	   pinput < input[nin];
           iinput++, pinput += MSAMPLES) {
        red += itr[RED][iinput] * *pinput;
        green += itr[GREEN][iinput] * *pinput;
        blue += itr[BLUE][iinput] * *pinput;
      }

      max_intensity = pow(255.,exponent);			/*max possible intensity*/
      if (red < 0) {
        red_output[sample] = 0;
      }
      else if (red > max_intensity) {
        red_output[sample] = 255;
      }
      else {
       red_output[sample] = pow(red,1 / exponent);		/*CRT response*/
      }

      if (green < 0) {
        green_output[sample] = 0;
      }
      else if (green > max_intensity) {
        green_output[sample] = 255;
      }
      else {
       green_output[sample] = pow(green,1 / exponent);	/*CRT response*/
      }

      if (blue < 0) {
        blue_output[sample] = 0;
      }
      else if (blue > max_intensity) {
        blue_output[sample] = 255;
      }
      else {
       blue_output[sample] = pow(blue,1 / exponent);	/*CRT response*/
      }
    }

    status = zvwrit(red_unit,red_output, NULL);		/*write the line*/
    check_status(2,7);
    status = zvwrit(green_unit,green_output, NULL);
    check_status(2,8);
    status = zvwrit(blue_unit,blue_output, NULL);
    check_status(2,8);
  }

  status = zvclose(red_unit, NULL);			/*close output files*/
  check_status(2,9);
  status = zvclose(green_unit, NULL);
  check_status(2,10);
  status = zvclose(blue_unit, NULL);
  check_status(2,11);

  repeat(iinput,0,nin) {				/*close input files*/
    status = zvclose(input_unit[iinput], NULL);
    check_status(2,12);
  }
  sprintf(alpha_buf,"OUTPUTS HAVE BEEN CREATED WITH %d LINES AND %d SAMPLES\n",
          nlines,nsamples);
  zvmessage (alpha_buf,0);
}



/*This function prints a message if status is negative or even.*/

check_status(function,location)
int function,location;
{

  if (status < 0 || !(status & BIT0)) {
    sprintf(alpha_buf,
  	    "SYSTEM ERROR %d AT LOCATION %d.%d",
  	    status,function,location);
    zvmessage (alpha_buf,0);
  }
}
