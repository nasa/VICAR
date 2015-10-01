#include "VIDSdefs.h"

/************************************************************************/
/* JLOAD-STRETCH loads a set of stretch lookup tables from a file.
 */
int jload_stretch_do(env)
  VIDSEnvironment	*env;		/* the VIDS environment	*/
{
  char *inp;				/* input file name		*/
  int planes[MAXPLANES];		/* image planes to load		*/
  int nplanes;				/* number of image planes	*/
  int columns[MAXPLANES];		/* file column to use for each plane */
  int ncolumns;				/* count of column parameter	*/
  int i, status, col;
  TAEVariable *v;
  TAEVariable *GetVariable();

  v = GetVariable (env,"INP");
  if (v == NULL) return FAIL;
  inp = SVAL(*v, 0);

  status = GetPlaneList(env, planes, &nplanes, True);
  if (status != SUCCESS) return status;

  v = GetVariable(env, "COLUMNS");
  if (v == NULL) return FAIL;
  ncolumns = v->v_count;
  for (i=0; i<ncolumns; i++)
    columns[i] = IVAL(*v, i);

  if (ncolumns < nplanes)		/* if not enough columns given,	*/
  {					/* fill the rest in		*/
    col = 1;					/* starting col */
    if (ncolumns != 0)
      col = columns[ncolumns-1] + 1;		/* starting col */
    for (i=ncolumns; i<nplanes; i++)
      columns[i] = col++;
  }

  status = ReadStretch(env, planes, columns, nplanes, inp);
  if (status != SUCCESS) return status;

  return SUCCESS;
}

/************************************************************************/
/* JLOAD-PSEUDO loads a pseudocolor table from a file.
 */
int jload_pseudo_do(env)
  VIDSEnvironment	*env;		/* the VIDS environment	*/
{
  char *inp;				/* input file name		*/
  int plane;				/* image plane to load		*/
  int nplanes;				/* # of planes (must be 1)	*/
  int columns[3];			/* file column to use for each plane */
  int ncolumns;				/* count of column parameter	*/
  int i, status, col;
  TAEVariable *v;
  TAEVariable *GetVariable();

  v = GetVariable (env,"INP");
  if (v == NULL) return FAIL;
  inp = SVAL(*v, 0);

  status = GetPlaneList(env, &plane, &nplanes, True);
  if (status != SUCCESS) return status;

  v = GetVariable(env, "COLUMNS");
  if (v == NULL) return FAIL;
  ncolumns = v->v_count;
  for (i=0; i<ncolumns; i++)
    columns[i] = IVAL(*v, i);

  if (ncolumns < 3)			/* if not enough columns given,	*/
  {					/* fill the rest in		*/
    col = 1;					/* starting col */
    if (ncolumns != 0)
      col = columns[ncolumns-1] + 1;		/* starting col */
    for (i=ncolumns; i<3; i++)
      columns[i] = col++;
  }

  status = ReadPseudo(env, plane, columns, inp);
  if (status != SUCCESS) return status;

  return SUCCESS;
}

/********************************************************************************/
/* JLOAD-IMAGE loads a band into an image plane, using all the current
 * settings for that plane and/or image file.
 */
int jload_image_do(env)
  VIDSEnvironment	*env;		/* the current display device environ	*/
{
  char *inp;				/* input file name			*/
  int	plane, band;			/* band will be loaded into plane	*/
  int	area[4];			/* area to be displayed			*/
  int	location[2];			/* sl, ss of plane for access window	*/
  int	status;				/* temp status holder			*/
  FileInfo	*theFile;		/* the input file			*/
  PlaneInfo	*thePlane;		/* the plane structure to load		*/
  FileInfo	*findFileSlot();

  status = GetJloadImageParms(env,&inp,&plane,&band,area,location);
  if (status != SUCCESS) return status;
  theFile = findFileSlot(env, inp);
  if (theFile == NULL) ABORT(FAIL,
        "Insufficient memory to prepare file information", "VIDS-NOMEM");
  thePlane = &(env->planes[plane]);
  TieImp(thePlane, theFile, band);
  SetFileWindow(thePlane, area[0], area[1], area[2], area[3]);
  thePlane->accessWindow.sl = location[0];
  thePlane->accessWindow.ss = location[1];
  thePlane->accessWindow.nl = thePlane->accessWindow.ns = 0;
  return LoadIMP(env, plane);
}
/********************************************************************************/
int GetJloadImageParms(env,inp,plane,band,area,location)
  VIDSEnvironment	*env;		/* the current display device environ	*/
  char			**inp;		/* input file name			*/
  int			*plane, *band;	/* band will be loaded into plane	*/
  int			area[4];	/* area to be displayed			*/
  int			location[2];	/* sl, ss of plane for access window	*/
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int			status;		/* temp status variable		*/
  int			i;		/* temp variable		*/
  TAEVariable *GetVariable();

  v = GetVariable(env, "INP");
  if (v == NULL) return FAIL;
  *inp = SVAL(*v, 0);
  
  status = GetPlaneList(env, plane, &i, False); /* depend on COUNT=1 in pdf file	*/
;
  
  v = GetVariable(env, "BAND");
  if (v == NULL) return FAIL;
  *band = IVAL(*v, 0);
  
  v = GetVariable(env, "AREA");
  if (v == NULL) return FAIL;
  if (v->v_count == 4)
  {
    area[0] = IVAL(*v, 0);	area[1] = IVAL(*v, 1);
    area[2] = IVAL(*v, 2);	area[3] = IVAL(*v, 3);
  }
  else
  {
    area[0] = area[1] = 1;	area[2] = area[3] = 0;
  }
  
  v = GetVariable(env, "LOCATION");
  if (v == NULL) return FAIL;
  location[0] = IVAL(*v, 0);	location[1] = IVAL(*v, 1);
  
  return SUCCESS;
}
