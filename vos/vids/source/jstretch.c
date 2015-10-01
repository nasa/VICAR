/* jstretch.c -- code for all jstretch commands.			*/

#include "VIDSdefs.h"
#include <math.h>
int *CurrentLut(), *NewLut();
TAEVariable *GetVariable();
#define PI 3.1415927
/************************************************************************/
/* jstretch_alarm_do sets a series of values in the lookup table
 * to a user specified value.
 */
int jstretch_alarm_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int	planes[MAXPLANES];		/* List of planes to invert	*/
  int	nPlanes;			/* number of planes		*/
  int	i,j;				/* increment variables		*/
  int	*lut;				/* current lut in use		*/
  int	alarmVals[256];			/* array of DNs to alarm	*/
  int 	nVals;				/* number of values to alarm	*/
  int	newDN;				/* new DN to assign to alarmed	*/

  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  if (GetAlarmList(env, alarmVals, &nVals, &newDN) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);	/* Get the current lut		*/
    if (lut == NULL) return FAIL;
    for (j = 0; j < nVals; j++)
      lut[alarmVals[j]] = newDN;
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_comp_do inverts the named lookup tables.
 */
int jstretch_comp_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int	planes[MAXPLANES];		/* List of planes to invert	*/
  int	nPlanes;			/* number of planes		*/
  int	i,j;				/* increment variables		*/
  int	*lut;				/* current lut in use		*/

  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);	/* Get the current lut		*/
    if (lut == NULL) return FAIL;
    for (j = 0; j < 256; j++)
      lut[j] = 255 - lut[j];
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_contour_do provides DN contours on the given intervals.
 */
int jstretch_contour_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int	planes[MAXPLANES];		/* List of planes to invert	*/
  int	nPlanes;			/* number of planes		*/
  int	i,j,k;				/* increment variables		*/
  int	*lut;				/* current lut in use		*/
  int	start;				/* starting dn in contour	*/
  int	end;				/* ending dn in contour		*/
  int	interval;			/* size of contour interval	*/
  int	maxnum;				/* maximum number of contours	*/
  int	value;				/* dn to replace in table.	*/
  int	current;			/* current dn/interval to use	*/
  int	last;				/* saved dn/interval		*/
  Boolean stretched;			/* True if use stretched vals	*/

  if (GetContourParms(env, &start, &end, &interval, &maxnum, &value,
       &stretched) != SUCCESS) return FAIL;
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  if (interval <= 0)
    ABORT(FAIL,"Sorry, the INTERVAL parameter must be greater than zero","VIDS-BADPARM");
  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);	/* Get the current lut		*/
    if (lut == NULL) return FAIL;
    k = 0;
    current = stretched ? lut[0] : 0;		/* check first dn for 	*/
    if (((current - start) % interval) == 0)	/* a contour...		*/
    {
      lut[0] = value;
      k = 1;
    }
    last = (current - start) / interval;
    for (j = 1; j < 256; j++)			/* Loop through all dn	*/
    {						/* values, putting a	*/
      if (k >= maxnum) break;			/* contour every time	*/
      current = stretched ? lut[j] : j;		/* the interval is 	*/
      if (current < start) continue;		/* crossed.		*/
      if (current > end) continue;
      current = (current - start) / interval;
      if (current != last)		/* next interval, draw contour	*/
      {
        lut[j] = value;
        k++;
      }
      last = current;
    }
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_copy_do copies one stretch (lut) to another.
 */
int jstretch_copy_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int p1, p2;				/* src, dst planes		*/
  int i;				/* temp variable		*/

  if (GetStrCopyParms(env, &p1, &p2) != SUCCESS) return FAIL;
  if (NewLut(env, p2) == NULL) return FAIL;
  BlockMove(CurrentLut(env, p1), env->planes[p2].lut, 256 * sizeof(int));
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_cursor_do -- code for jstretch-cursor command.
 */
int jstretch_cursor_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  int		*lutList[MAXPLANES];	/* luts to be modified		*/
  int		*savedLuts[MAXPLANES];	/* saved copies of luts		*/
  int		lut[256];		/* a local lookup table		*/
  int		i,status;		/* increment, status holder	*/
  float		low,high;		/* low,high vals mapped to 0,255*/
  int		nlVideo,nsVideo;	/* lines/samps video available	*/
  int		unit;			/* device unit number		*/
  int		x,y,xLast,yLast;
  int		curs;			/* cursor number		*/
  ButtonAction	action;			/* accept,cancel, or reject	*/
  float		tanSlope,tanOffs;	/* to trans curs pos for tan	*/
  float		gain,offset;		/* gain,offset of actual curve	*/
  Region	*dispRgn;		/* location to display dtf	*/
  Boolean	showDTF;		/* True if DTF to be displayed	*/
  GraphColor	color;			/* color in which to display dtf*/
  Rect		dtfRect;		/* temp rectangle holder	*/
  Boolean	HasGraphics();
  int		firsttime;
  char		msg[80];

  unit = env->devUnit;
  curs = env->cursor.number;

  if (GetJstrCursParms(env, &dispRgn, &showDTF, &color) != SUCCESS) return FAIL;
  if ((HasGraphics(env) == False) && (showDTF == True))
  {
    showDTF = False;
    NotifyUser(Inform, "", 
      "No graphics plane is available, so DTF will not be displayed");
  }
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++) savedLuts[i] = NULL;
  for (i = 0; i < nPlanes; i++)	/* guarantee private lut for all planes	*/
  {
    if ((lutList[i] = NewLut(env, planes[i])) == NULL) break;
    savedLuts[i] = malloc(256 * sizeof(int));
    if (savedLuts[i] != NULL)
      BlockMove(lutList[i], savedLuts[i], 256 * sizeof(int));
  }
  if (i < nPlanes)
  {
    for (i = 0; i < nPlanes; i++)
      if (savedLuts[i] != NULL) free(savedLuts[i]);
    return FAIL;
  }

  nsVideo = VideoSamps(env);		/* fill in video size, which is	*/
  nlVideo = VideoLines(env);		/* the range of the cursor.	*/

/* Use a linear transform to define an angle, which, when fed into
 * a tangent function, gives a gain for the linear stretch.  The tan 
 * function is used because it varies from 0 to infinity on a known
 * range (0 - pi/2) with a value of one at the center point (pi/4).
 */
  tanSlope = - PI / (2.0 * (float) nlVideo);
  tanOffs = PI / 2.0 + .000001;		/* .0001 keeps gain above zero	*/
  NotifyUser(Inform,"","Cursor controlled linear stretch");
  NotifyUser(Inform,"","Use buttons 1 or 3 to terminate, 2 to cancel");
  
  x = nsVideo / 2; y = nlVideo / 2;		/* Set curs to neutral	*/
  zdcset(unit, curs, x, y);			/* position		*/
  if (showDTF)
  {
    ShowGraphics(env);
    FillRegion(env, env->grafIMP, dispRgn, 255, 0);
    PlotDTFLabel(env, dispRgn, &dtfRect, True);
  }

  xLast = yLast = 0;
  firsttime = True;
  do
  {
    status = zdclocation(unit, curs, &x, &y);	/* get cursor pos	*/
    if (status != SUCCESS) break;
    if ((x == xLast) && (y == yLast)) continue;
    xLast = x; yLast = y;
    gain = tan((float) y * tanSlope + tanOffs);
    offset = x * 256.0 / (float)nsVideo;
    offset = offset - (gain * (256.0 - offset));
    low = -offset / gain;
    high = (255.0 - offset) / gain;
    if ((showDTF==True) && !firsttime)		/* don't erase first time through */
    {
      status = PlotDTFData(env, &dtfRect, lut, color, NoColor, 0, 255);
      if (status != SUCCESS) break;
    }
    firsttime = False;
    LinearLut(lut, low, high);
    for (i = 0; i < nPlanes; i++)
      BlockMove(lut, lutList[i], 256 * sizeof(int));
    SendLuts(env);
    if (showDTF)
    {
      status = PlotDTFData(env, &dtfRect, lut, color, color, 0, 255);
      if (status != SUCCESS) break;
    }
  } while (!ButtonDown(env, &action));
  if (action == CANCEL)
  {
    for (i = 0; i < nPlanes; i++)
      if (savedLuts[i] != NULL)
        BlockMove(savedLuts[i], lutList[i], 256 * sizeof(int));
    SendLuts(env);
    NotifyUser(Inform,"","Reverted to original stretch on each plane");
  }
  else
  {
    sprintf(msg, "Applied linear stretch from %.1f to %.1f", low, high);
    NotifyUser(Inform,"",msg);
  }
  for (i = 0; i < nPlanes; i++)
    if (savedLuts[i] != NULL) free(savedLuts[i]);
  return status;
}
/************************************************************************/
/* jstretch_func_do applies a user-specified mathematical function to
 * the lookup tables.
 */
int jstretch_func_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  int		i,j;			/* increment variables		*/
  int		*lut;			/* current lut			*/
  TAEVariable	*v;			/* temp variable ptr		*/
  FunctionDef	theFunc;		/* compiled function to execute	*/
  float		ApplyFunction();

  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  v = GetVariable(env, "FUNCTION");
  if (v == NULL) return FAIL;
  UpperCase(SVAL(*v, 0), SVAL(*v, 0));
  if (ParseFunction(SVAL(*v, 0), &theFunc) != SUCCESS)
    ABORT(FAIL, "Error parsing function; use DN for pixel value or check syntax",
          "VIDS-FUNCERR");

  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);
    if (lut == NULL) return FAIL;
    for (j = 0; j < 256; j++)
    {
      lut[j] = (int) (ApplyFunction(&theFunc, (float)j) + 0.5);
      if (lut[j] < 0) lut[j] = 0;
      if (lut[j] > 255) lut[j] = 255;
    }
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_gauss_do performs an automatic gaussian stretch, that is,
 * applying a stretch so that the histogram approximates a gaussian
 * curve with the user specified mean and sigma.
 */
int jstretch_gauss_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  long	    *hists[MAXPLANES],*theHist;	/* Histograms			*/
  float		gsigma;			/* no. of sigmas across 1/2 curve*/
  float		mean;			/* desired mean of gaussian	*/
  int		low,high;		/* low/high hist vals to exclude*/
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  int		i;

  if (GetGaussParms(env, &gsigma, &mean, &low, &high) != SUCCESS) return FAIL;
  if (gsigma <= 0.0)
    ABORT(FAIL,"Sorry, the value of GSIGMA must be greater than zero",
          "VIDS-BADPARM");
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  if (GetHistList(env, planes, nPlanes, hists) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    if (NewLut(env, planes[i]) == NULL) return FAIL;
    GaussLut(CurrentLut(env, planes[i]), hists[i], gsigma, mean, low, high);
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_linear_do -- code for jstretch-linear command.
 */
int jstretch_linear_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  float		low,high;		/* to be mapped to 0,255	*/
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  int i;
  
  if (GetLinearParms(env, &low, &high) != SUCCESS) return FAIL;
  if (high == low)
    ABORT(FAIL,"Sorry, the HIGH and LOW parameters may not be equal.","VIDS-BADPARM");
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    if (NewLut(env, planes[i]) == NULL) return FAIL;
    LinearLut(CurrentLut(env, planes[i]), low, high);
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_log_do -- code for jstretch-log command.  Fits the lookup
 * table to the function dn(out) = a * log(dn(in) + c) + b.  low is 
 * mapped to zero, high is mapped to 255.
 */
int jstretch_log_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  float		low,high;		/* to be mapped to 0,255	*/
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  int i,j;
  float		a,b,c;			/* for dn=a*log(dn(in) + c) + b	*/
  int		lut[256];		/* local lut			*/
  int		*theLut;		/* a current lut ptr		*/
  TAEVariable	*v;
  
  v = GetVariable(env,"CURVE");
  if (v == NULL) return FAIL;
  c = RVAL(*v, 0);
  if (c <= 0)
  {
    NotifyUser(Inform,"","CURVE must be greater than zero, setting to 1e-30");
    c = 1e-30;
  }
  if (GetLinearParms(env, &low, &high) != SUCCESS) return FAIL;
  if (high == low)
    ABORT(FAIL,"Sorry, the HIGH and LOW parameters may not be equal.","VIDS-BADPARM");
  if ((low + c) <= 0.0)
    ABORT(FAIL,"Sorry, LOW + CURVE must be greater than zero.","VIDS-BADPARM");
  if ((high + c) <= 0.0)
    ABORT(FAIL,"Sorry, HIGH + CURVE must be greater than zero.","VIDS-BADPARM");
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  a = 255.0 / (log(high + c) - log(low + c));
  b = -a * log(low + c) + 0.5;
  for (i = 0; i < 256; i++)
  {
    lut[i] = (int) (a * log((double)i + c) + b);
    if (lut[i] < 0) lut[i] = 0;
    if (lut[i] > 255) lut[i] = 255;
  }
  for (i = 0; i < nPlanes; i++)
  {
    if ((theLut = NewLut(env, planes[i])) == NULL) return FAIL;
    BlockMove(lut, theLut, 256 * sizeof(int));
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_percent_do -- code for jstretch-percent command.  Performs an
 * automatic linear stretch.
 */
int jstretch_percent_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  float		lPerc,hPerc;		/* percent to saturate at ends	*/
  int		lExclude,hExclude;	/* low/high hist vals to exclude*/
  float		low,high;		/* to be mapped to 0,255	*/
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  long	    *hists[MAXPLANES],*theHist;	/* Histograms			*/
  int		i,j,k;			/* increment variables		*/
  float		total;			/* total number of pixels	*/
  float		lTotal,hTotal;		/* Low/high sums of histogram	*/
  char		msg[80];

  if (GetPercentParms(env, &lPerc, &hPerc, &lExclude, &hExclude) != SUCCESS) return FAIL;
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  if (GetHistList(env, planes, nPlanes, hists) != SUCCESS) return FAIL;
  lPerc /= 100.0;
  hPerc /= 100.0;
  for (i = 0; i < nPlanes; i++)
  {
    theHist = hists[i];
    total = 0.0;
    for (j = lExclude; j <= hExclude; j++)
      total += (float) theHist[j];
    if (total == 0.0)
      total = 0.001;		/* avoid divide by 0 below */
    lTotal = hTotal = 0.0;
    low = lExclude;
    high = hExclude;
    for (j = lExclude, k = hExclude; j <= hExclude; j++, k--)
    {
      lTotal += (float) theHist[j];
      hTotal += (float) theHist[k];
      if ((lTotal / total) <= lPerc) low  = (float)j;
      if ((hTotal / total) <= hPerc) high = (float)k;
    }
    sprintf(msg,"Applying stretch from %.0f to %.0f on plane %d",
               low,high,planes[i]);
    NotifyUser(Inform,"",msg);
    if (high == low)
      NotifyUser(Inform,"","Sorry, invalid stretch was calculated, none applied.");
    else
    {
      if (NewLut(env, planes[i]) == NULL) return FAIL;
      LinearLut(CurrentLut(env, planes[i]), low, high);
    }
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_period_do -- code for jstretch-periodic command.  Performs a
 * periodic stretch, where the output DN is given by
 * DNout = (ampl/2) * sin(2 * pi * freq * DNin / 255 + phi) + mean
 */
int jstretch_period_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int		planes[MAXPLANES];	/* List of planes to invert	*/
  int		nPlanes;		/* number of planes		*/
  float mean,ampl,freq,phi;
  int		i,j;			/* increment variables		*/
  int		*lut;			/* current lut in use		*/

  if (GetPeriodParms(env, &mean, &ampl, &freq, &phi) != SUCCESS) return FAIL;
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  freq = 2.0 * freq * PI / 255;
  ampl = ampl / 2.0;
  mean = mean + 0.5;
  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);
    if (lut == NULL) return FAIL;
    for (j = 0; j < 256; j++)
    {
      lut[j] = (int) (ampl * sin(freq * j + phi) + mean);
      if (lut[j] < 0) lut[j] = 0;
      if (lut[j] > 255) lut[j] = 255;
    }
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_shift_do performs a bit shift on the given lookup tables.
 */
int jstretch_shift_do(env)
  VIDSEnvironment	*env;
{
  int		planes[MAXPLANES];	/* List of planes to invert	*/
  int		nPlanes;		/* number of planes		*/
  int		nbits;			/* number of bits to shift	*/
  unsigned	i,j;			/* increment variables		*/
  int		*lut;			/* current lut in use		*/
  TAEVariable	*v;

  v = GetVariable(env, "NBITS");
  if (v == NULL) return FAIL;
  nbits = IVAL(*v, 0);
  
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);	/* Get the current lut		*/
    if (lut == NULL) return FAIL;
    for (j = 0; j < 256; j++)
      lut[j] = (j << nbits) % 256;
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_smooth_do smooths the histogram by trying to fit it to a
 * uniform distribution function.  Simply to reuse code, a gaussian
 * curve with a very small gsigma is used to produce the flat line.
 */
int jstretch_smooth_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  long	    *hists[MAXPLANES],*theHist;	/* Histograms			*/
  float		gsigma;			/* no. of sigmas across 1/2 curve*/
  float		mean;			/* desired mean of gaussian	*/
  int		low,high;		/* low/high hist vals to exclude*/
  int		planes[MAXPLANES];	/* planes to modify luts on	*/
  int		nPlanes;		/* number of planes on list	*/
  int		i;
  TAEVariable	*v;
  
  v = GetVariable(env, "RANGE");
  if (v == NULL) return FAIL;
  low = IVAL(*v, 0);
  high = IVAL(*v, 1);
  gsigma = 0.0001;
  mean = 127.5;
  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  if (GetHistList(env, planes, nPlanes, hists) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    if (NewLut(env, planes[i]) == NULL) return FAIL;
    GaussLut(CurrentLut(env, planes[i]), hists[i], gsigma, mean, low, high);
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* jstretch_table_do sets a series of values in the lookup table
 * to a user specified value, interpolating between the points.
 */
int jstretch_table_do(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int	planes[MAXPLANES];		/* List of planes to invert	*/
  int	nPlanes;			/* number of planes		*/
  int	i,j,k;				/* increment variables		*/
  int	*lut;				/* current lut in use		*/
  int	inTable[20];			/* table of input DNs		*/
  int	outTable[20];			/* matching table of output DNs	*/
  int 	nVals;				/* number of values in table	*/
  float	m,b;				/* slope,offset of line		*/

  if (GetPlaneList(env, planes, &nPlanes, False) != SUCCESS) return FAIL;
  if (GetTableList(env, inTable, outTable, &nVals) != SUCCESS) return FAIL;
  for (i = 0; i < nPlanes; i++)
  {
    lut = NewLut(env, planes[i]);	/* Get the current lut		*/
    if (lut == NULL) return FAIL;
    if (nVals >= 1)
      lut[inTable[0]] = outTable[0];
    for (j = 1; j < nVals; j++)
    {
      m = (float) (outTable[j] - outTable[j - 1]) /
          (float) (inTable[j] - inTable[j - 1]);
      b = (float)outTable[j] - (m * (float)inTable[j]) + 0.5;
      for (k = inTable[j - 1]; k <= inTable[j]; k++)
        lut[k] = (int) (m * (float)k + b);
    }
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
/* GetAlarmList gets the list of DNs to alarm for jstretch_alarm_do.
 */
int GetAlarmList(env, alarmVals, nVals, newDN)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  int			alarmVals[];	/* out: array of values to alrm	*/
  int			*nVals;		/* out: number of vals to alrm	*/
  int 			*newDN;		/* out: the new DN for each val	*/
{
  TAEVariable	*v;
  char vname[8];		/* name of variables	*/
  int i,j;
  
  v = GetVariable(env, "VALUE");
  if (v == NULL) return FAIL;
  *newDN = IVAL(*v, 0);
  
  *nVals = 0;
  for (i = 1; i <= 10; i++)
  {
    if (*nVals >= 256) break;
    sprintf(vname, "RANGE%d", i);
    v = GetVariable(env, vname);
    if (v == NULL) return FAIL;
    if (v->v_count == 1)
      alarmVals[(*nVals)++] = IVAL(*v, 0);
    else if (v->v_count == 2)
      for (j = IVAL(*v, 0); (*nVals < 256) && (j <= IVAL(*v, 1)); j++)
	alarmVals[(*nVals)++] = j;
  }
  return SUCCESS;
}    
/************************************************************************/
int GetContourParms(env, start, end, interval, maxnum, value, stretched)
  VIDSEnvironment	*env;
  int			*start;		/* starting dn in contour	*/
  int			*end;		/* ending dn in contour		*/
  int			*interval;	/* size of contour interval	*/
  int			*maxnum;	/* maximum number of contours	*/
  int			*value;		/* dn to replace in table.	*/
  Boolean		*stretched;	/* True if use stretched vals	*/
{
  TAEVariable	*v;
  
  v = GetVariable(env, "START");
  if (v == NULL) return FAIL;
  *start = IVAL(*v, 0);

  v = GetVariable(env, "END");
  if (v == NULL) return FAIL;
  *end = IVAL(*v, 0);

  v = GetVariable(env, "INTERVAL");
  if (v == NULL) return FAIL;
  *interval = IVAL(*v, 0);

  v = GetVariable(env, "MAXNUM");
  if (v == NULL) return FAIL;
  *maxnum = IVAL(*v, 0);

  v = GetVariable(env, "VALUE");
  if (v == NULL) return FAIL;
  *value = IVAL(*v, 0);

  v = GetVariable(env, "STRETCH");
  if (v == NULL) return FAIL;
  *stretched = (*SVAL(*v, 0) == 'S') ? True : False;
  return SUCCESS;
}
/************************************************************************/
/* GetStrCopyParms gets parms for jstretch_copy_do.
 */
int GetStrCopyParms(env, p1, p2)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int *p1, *p2;				/* src/dst planes		*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  
  v = GetVariable(env, "PLANE1");
  if (v == NULL) return FAIL;
  if (NameToImp(env, SVAL(*v, 0), p1) != SUCCESS)
    return FAIL;

  v = GetVariable(env, "PLANE2");
  if (v == NULL) return FAIL;
  if (NameToImp(env, SVAL(*v, 0), p2) != SUCCESS)
    return FAIL;

  if ((*p1 <= 0) || (*p2 <= 0))
      ABORT(FAIL, "Image plane numbers must be positive.", "VIDS-BADPLANE");
  if ((*p1 > env->nimps) || (*p2 > env->nimps))
    ABORT(FAIL, "Sorry, one of the image plane numbers is too large.",
          "VIDS-BADPLANE");
  return SUCCESS;
}
/************************************************************************/
int GetJstrCursParms(env, dispRgn, showDTF, color)
  VIDSEnvironment	*env;
  Region		**dispRgn;
  Boolean		*showDTF;
  GraphColor		*color;
{
  TAEVariable	*v;
  int		count;
  
  if (GetRegionList(env, dispRgn, &count, "DISPRGN", 1) != SUCCESS)
    return FAIL;

  v = GetVariable(env, "DTF");
  if (v == NULL) return FAIL;
  *showDTF = (EQUAL(SVAL(*v, 0), "SHOW")) ? True : False;
  
  v = GetVariable(env, "COLOR");
  if (v == NULL) return FAIL;
  *color = StringToColor(SVAL(*v, 0));
  
  return SUCCESS;
}
/************************************************************************/
/* GetLinearParms returns the parameters for jstretch_linear_do.
 */
int GetLinearParms(env, low, high)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  float *low,*high;			/* to be mapped to 0,255	*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  
  v = GetVariable(env, "LOW");
  if (v == NULL) return FAIL;
  *low = RVAL(*v, 0);

  v = GetVariable(env, "HIGH");
  if (v == NULL) return FAIL;
  *high = RVAL(*v, 0);

  return SUCCESS;
}
/************************************************************************/
int GetPercentParms(env, low, high, lExclude, hExclude)
  VIDSEnvironment	*env;
  float			*low,*high;
  int			*lExclude,*hExclude;
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/

  v = GetVariable(env, "LOW");
  if (v == NULL) return FAIL;
  *low = RVAL(*v, 0);

  v = GetVariable(env, "HIGH");
  if (v == NULL) return FAIL;
  if (v->v_count == 0)
    *high = *low;
  else
    *high = RVAL(*v, 0);

  v = GetVariable(env, "RANGE");
  if (v == NULL) return FAIL;
  *lExclude = IVAL(*v, 0);
  *hExclude = IVAL(*v, 1);

  return SUCCESS;
}
/************************************************************************/
  GetPeriodParms(env, mean, ampl, freq, phi)
  VIDSEnvironment	*env;
  float			*mean,*ampl,*freq,*phi;
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  
  v = GetVariable(env, "MEAN");
  if (v == NULL) return FAIL;
  *mean = RVAL(*v, 0);

  v = GetVariable(env, "AMPLITUD");
  if (v == NULL) return FAIL;
  *ampl = RVAL(*v, 0);

  v = GetVariable(env, "FREQU");
  if (v == NULL) return FAIL;
  *freq = RVAL(*v, 0);

  v = GetVariable(env, "PHI");
  if (v == NULL) return FAIL;
  *phi = RVAL(*v, 0);

  return SUCCESS;
}
/************************************************************************/
int GetGaussParms(env, gsigma, mean, low, high)
  VIDSEnvironment	*env;
  float			*gsigma,*mean;
  int			*low,*high;
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/

  v = GetVariable(env, "GSIGMA");
  if (v == NULL) return FAIL;
  *gsigma = RVAL(*v, 0);

  v = GetVariable(env, "MEAN");
  if (v == NULL) return FAIL;
  *mean = RVAL(*v, 0);

  v = GetVariable(env, "RANGE");
  if (v == NULL) return FAIL;
  *low = IVAL(*v, 0);
  *high = IVAL(*v, 1);

  return SUCCESS;
}
/************************************************************************/
/* GetHistList will convert a list of lookup tables into a list of
 * histogram pointers.  Returns SUCCESS or FAIL.
 */
int GetHistList(env, imps, nimps, hists)
  VIDSEnvironment	*env;
  int			*imps[];	/* in: list of lut pointers	*/
  int			nimps;		/* in: number of luts on list	*/
  long			*hists[];	/* out: list of hist ptrs	*/
{
  int i;
  Region	*theRgn;
  long		*CurrentHist();

  GetRegionList(env, &theRgn, &i, "REGION", 1);
  if (i == 0) theRgn = NULL;
  
  for (i = 0; i < nimps; i++)
    hists[i] = CurrentHist(env, imps[i], theRgn);
  return SUCCESS;
}
/************************************************************************/
int GetTableList(env, inTable, outTable, nVals)
  VIDSEnvironment	*env;
  int			inTable[];
  int			outTable[];
  int			*nVals;
{
  char		vname[4];
  int		i;
  TAEVariable	*v;
  
  *nVals = 0;
  for (i = 1; i <= 20; i++)
  {
    sprintf(vname,"P%d",i);
    v = GetVariable(env, vname);
    if (v == NULL) return FAIL;
    if (v->v_count == 2)
    {
      inTable[*nVals] = IVAL(*v, 0);
      outTable[*nVals] = IVAL(*v, 1);
      if (*nVals > 0 && inTable[*nVals] <= inTable[*nVals-1])
        ABORT(FAIL,"The input value for all pairs must be in increasing order","VIDS-PARMERR");
      *nVals += 1;
    }
  }
  return SUCCESS;
}
/************************************************************************/
/* GaussLut will force the given histogram into roughly a gaussian 
 * curve with the given mean and sigma by tweaking the given lookup
 * table.  The histogram is ignored below "low" and above "high".
 * "This arrangement is by D. Stanfill, from an original composition
 * by J. Addington entitled 'RDISPLAY' (in D minor)".
 */
int GaussLut(lut, hist, gsigma, mean, low, high)
  int		*lut;			/* The lookup table to modify	*/
  long		hist[];			/* The corresponding histogram	*/
  float		gsigma;			/* no. of sigmas across 1/2 curve*/
  float		mean;			/* mean of curve		*/
  int		low,high;		/* low/high hist vals to exclude*/
{
  int		i;			/* increment variable		*/
  float		sig2;			/* sigma squared		*/
  float		cdf[HSIZE];

  if (gsigma == 0.0) return FAIL;
  sig2 = 127.5 / gsigma;			/* sigma		*/
  sig2 = 2.0 * sig2 * sig2;			/* 2 * sigma^2		*/

/* Now fill up the cumulative distribution function (cdf) table with the 
 * proper values so that we have a gaussian histogram.  Each value
 * in cdf[] contains the gaussian formula evaluated at that point.
 * ( y = e^(-(x-mean)^2 / (2*sigma^2))  )
 */
  for (i = 0; i < HSIZE; i++)
    cdf[i] = exp(-(i - mean) * (i - mean) / sig2);
  HistToCurve(lut, hist, cdf, low, high);
  return SUCCESS;
}
/************************************************************************/
/* HistToCurve will create a lookup table which gives a good match
 * of the histogram given by hist to the curve described by the array
 * curve.
 */
int HistToCurve(lut, hist, curve, low, high)
  int	lut[256];	/* out: lookup table to modify			*/
  long	hist[HSIZE];	/* in: histogram of image to match to curve	*/
  float	curve[HSIZE];	/* in: curve to be matched against hist		*/
  int	low,high;	/* in: low/high entries in hist to use		*/
{
  int		i,cdfPos;		/* increment variables		*/
  float	       cumSum,cumSumNext,scale;	/* Cumulative sum, scaling factor*/
  long		locHist[HSIZE];		/* local copy of histogram	*/
  float		cdf[HSIZE];		/* cumulative distribution func.*/

  BlockMove(curve, cdf, HSIZE * sizeof(float));	 /* Make local copies of  */
  BlockMove(hist, locHist, HSIZE * sizeof(long));/* arrays to be modified */

  BlockFill(0, locHist, low * sizeof(long));	    /* Zero out values 	*/
  for (i = high + 1; i < HSIZE; i++) locHist[i] = 0; /* to be excluded	*/

/* Now make the cdf table contain an actual cumulative distribution
 * function (cdf); ie, each value is the sum of the values below it.
 * scale is modified to hold a scaling factor to adjust the gaussian
 * to the actual number of pixels we have, and each value in cdf
 * is scaled and then added to the point below it, to give
 * an actual cumulative distribution function.
 */
  cumSum = 0.0;			/* Total area under the curve to be fit	*/
  for (i = 0; i < HSIZE; i++)
    cumSum += cdf[i];
  scale = 0;
  for (i = low; i <= high; i++)		/* scale is the total number of	*/
    scale += (float) locHist[i];	/* pixels in our histogram 	*/
  scale = scale / cumSum;		/* divided by the total in gauss*/
  cdf[0] *= scale;
  for (i = 1; i < HSIZE; i++)
    cdf[i] = cdf[i] * scale + cdf[i - 1];

/* Now generate the lookup table based on the shape of the 
 * cumulative distribution function.  Each output DN is that DN (location)
 * in the cdf[] array that has the accumulation (value) that is
 * closest to the accumulation (sum of values) of the input DN in
 * our histogram.
 */
  cumSum=0.0;
  i = cdfPos = 0;
  cumSumNext = locHist[0];
  while ((i < HSIZE) && (cdfPos < HSIZE))
  {
    while (fabs(cdf[cdfPos] - cumSum) >= fabs(cdf[cdfPos] - cumSumNext))
    {				/* cumulative sum not yet big enough,	*/
      lut[i] = cdfPos;		/* so store the cdf position and try	*/
      i++;			/* the next spot in the histogram.	*/
      if (i > 255) break;
      cumSum = cumSumNext;
      cumSumNext = cumSum + locHist[i];
    }
    cdfPos++;			/* match next spot in cdf	*/
  }
  for (; i <= 255; i++) lut[i] = 255;	/* saturate out rest of lut	*/
  return SUCCESS;
}

