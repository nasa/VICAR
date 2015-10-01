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

double ILLUMINANT[][SSIZE] = {				/*illuminants*/
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

double MATCH[3][SSIZE] = {
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
