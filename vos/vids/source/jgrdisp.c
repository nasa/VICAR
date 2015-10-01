#include "VIDSdefs.h"
/* JGRDISP.C -- subroutines for the JGRDISP command to display an IBIS
 * graphics 1 file.
 */
/************************************************************************/
/* jgrdisp_do -- code for the JGRDISP command.
 * This command will display an IBIS graphics1 file in the current
 * graphics plane.
 *
 * Originally called ibis_display, a subroutine in program IDX.
 * Original Programmer:			 B. A. McGuffie
 * Converted to C and modified for VIDS: D. F. Stanfill
 *    update history
 *    --------------
 *    revision a -  3/4/86    bam
 *      initial release  
 *
 *    revision b -  9/12/86   bam
 *      1. zoom capability added
 *      2. either a 2-d or 3-d file may be input
 *      3. 3d files - colors plotted as a function of z
 *
 *    6/88 dfs
 *      Converted to C, rewritten for use in VIDS
 */
int jgrdisp_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  char		*fileName;	/* name of file to display		*/
  float		zoom;		/* zoom factor to apply to graphics	*/
  int		gPlane;		/* the plane on which to draw		*/
  PlaneInfo	*aPlane;	/* the graphics plane structure		*/
  int		dim;		/* dimension of IBIS file		*/
  int		nPens;		/* the pens parameter			*/
  float		zscale;		/* the zscale parameter			*/
  int		status;		/* status holder			*/
  Boolean	eof, zero;	/* logical variables for ibis routines	*/
  float		slds,ssds;	/* starting line/samp on display device	*/
  float		slz,ssz;	/* starting line/samp in image file	*/
  GraphColor	initColor;	/* initial color			*/
  int		pen;		/* which pen (0-7) to use		*/
  int		last_color;	/* which pen (0-7) was used last	*/
  float		zmin, zmax;	/* min and max z values of file		*/
  float		lastz;		/* previous z value			*/
  float		x,y,z,z2;	/* x,y,z values from graphics 1 file	*/
  static GraphColor pen_color[8] = /* array of colors to use for objects*/
      {Red, Green, Blue, White, Black, Yellow, Cyan, Magenta};

  status = GetGrdispParms(env, &fileName, &gPlane, &dim, &nPens,
                        &zscale, &initColor);
  if (status != SUCCESS) return status;

  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

/* This graphics file is supposed to be registered and zoomed according */
/* to the picture the user is looking at; so we use the info for the	*/
/* bw plane if in bw mode, and the red plane if in color mode		*/
  if (env->isColor)
    aPlane = &env->planes[env->redIMP];
  else
    aPlane = &env->planes[env->bwIMP];

  slds = (float) aPlane->accessWindow.sl;
  ssds = (float) aPlane->accessWindow.ss;
  slz =  (float) aPlane->imageWindow.sl;
  ssz =  (float) aPlane->imageWindow.ss;
  zoom = (float) aPlane->softZoom;
  if (zoom < 0 ) zoom = -1.0/zoom;
  SetPenPlane(gPlane);
  SetPenColor(initColor);

  for (last_color = 0; last_color < 8; last_color++)  /* set up initial */
    if (pen_color[last_color] == initColor) break;    /* color for 3d	*/

  ibis_rdgr( fileName, 0, dim );	/* open the graphics file	*/

  if ((dim == 3) && (zscale == 0))	/* read through file		*/
  {					/* to find min/max z		*/
    zmin = 1.0e30;
    zmax = -1.0e30;

    eof = False;
    while (! eof )
    {
      ibis_getgr(0, &zero, &eof, &x, &y, &z);
      if ((! zero) && (! eof))
      {
        zmin = MIN( zmin, z );
        zmax = MAX( zmax, z );
      }
    }
    ibis_clgr(0);			/* reset by closing and opening	*/
    ibis_rdgr(fileName, 0, dim );	/* the graphics file again	*/
    zscale = nPens / ( zmax - zmin );
  }
  else
  {
    zmin = 0;
  }

  do				/* loop through graphics		*/
  {				/* scan for the beginning of an element	*/
    if ( dim == 2 )		/* get next elements or end of file	*/
      ibis_nextgr(0, &eof, &x, &y);
    else
    {
      ibis_nextgr(0, &eof, &x, &y, &z);
      lastz = z;		/* save last z	*/
    }
    x = slds + (x - slz) * zoom;	/* compensate for the	*/
    y = ssds + (y - ssz) * zoom;	/* zoom on the image	*/

    if ( eof ) break;			/* test for end of file		*/

    MoveTo((int) (y + 0.5), (int) (x + 0.5));	/* move to first point	*/

    zero = False;
    while ((! zero) && (! eof))
    {
      LineTo(env, (int) (y + 0.5), (int) (x + 0.5));
      if ( dim == 2 )				/* get next pair	*/
        ibis_getgr(0, &zero, &eof, &x, &y );
      else
      {
        ibis_getgr(0, &zero, &eof, &x, &y, &z2 );
        if ( nPens > 1 )
        {
          z = zscale * ( (lastz-zmin) + (z2-zmin) ) / 2.0;
          pen = (int) z - 1;
          if ( pen < 0 ) pen = 0;		/* for minimum problem	*/
          if ( pen > 7 ) pen = 7;		/* for maximum problem	*/
          if ( pen != last_color )		/* change graphics color*/
          {					/* if required		*/
            SetPenColor(pen_color[pen]);
            last_color = pen;
          }
        }
        lastz = z2;				/* save last z		*/
      }
      x = slds + ( x - slz ) * zoom;		/* compensate for zoom	*/
      y = ssds + ( y - ssz ) * zoom;
      if ( eof ) break;
    }
  } while (! eof);

  ibis_clgr(0);				/* close the graphics file	*/
  ClearPenPlane();
  return SUCCESS;
}
/************************************************************************/
/* GetGrdispParms collects the parameters for the GRDISP command and
 * sends them up.
 */
int GetGrdispParms(env, fileName, imp, dim, pens, zscale, color)
  VIDSEnvironment	*env;
  char			**fileName;	/* graphics 1 file name to draw	*/
  int			*imp;		/* plane in which to draw	*/
  int			*dim;		/* 2 or 3 dimensional file	*/
  int			*pens;		/* number of pens to use	*/
  float			*zscale;	/* scaling factor to apply to	*/
  					/* z of 3d graphics 1 files	*/
  GraphColor		*color;		/* initial color for display	*/
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int			status;		/* temp status variable		*/
  int			i;		/* temp variable		*/
  GraphColor	StringToColor();
  TAEVariable	*GetVariable();

  v = GetVariable(env, "INP");
  if (v == NULL) return FAIL;
  *fileName = SVAL(*v, 0);
  
  *imp = env->grafIMP;
  
  v = GetVariable(env, "DIM");
  if (v == NULL) return FAIL;
  *dim = IVAL(*v, 0);
  
  v = GetVariable(env, "PENS");
  if (v == NULL) return FAIL;
  *pens = IVAL(*v, 0);
  
  v = GetVariable(env, "ZSCALE");
  if (v == NULL) return FAIL;
  *zscale = (v->v_count == 0) ? 0 : RVAL(*v, 0);

  v = GetVariable(env, "COLOR");
  if (v == NULL) return FAIL;
  *color = StringToColor(SVAL(*v, 0));

  return SUCCESS;
}
