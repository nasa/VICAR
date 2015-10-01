#include "VIDSdefs.h"
#include <ctype.h>

extern struct PARBLK InParblk;

FileInfo firstFile;		/* First entry in linked list of files.	*/
				/* The list is global to all devices.	*/

/* This file contains various utility subroutines for use throughout VIDS.
 */
/************************************************************************/
int SetBWMode(env)

  VIDSEnvironment *env;
  
{
  int unit;				/* device unit number		*/
  int imp;				/* image plane number		*/
  int i;
  int *CurrentLut();
  int *theLut;
  static int bwconfig[4] = {3,-1,-1,-1};

  unit = env->devUnit;
  imp = env->bwIMP;

  zddbatch(unit, True);

  if (!env->isBW)			/* avoid flashing on jmovie */
  {
    zddconfigure(unit, bwconfig);

    zddinfo (unit, 4, 1, &env->nimps);	/* Recheck things (mainly for Jupiter)*/
    zddinfo (unit, 30, 1, &i);		/* graphics overlay available? */
    if (i == 1)					/* yes */
    {
      env->grafIMP = zdsgraph(unit);	/* ask vrdi which is graphics */
      GetGrColors(env);			/* reset the graphics colors */
    }
    else					/* no */
      env->grafIMP = 1;			/* use plane 1 for graphics   */
    NewName(env, env->grafIMP, "GRAPHICS");
  }

  env->isColor = FALSE;
  env->isPseudo = FALSE;
  env->isBW = TRUE;

  zdlconnect(unit, imp, redLUT,   1, FALSE);
  zdlconnect(unit, imp, greenLUT, 1, FALSE);
  zdlconnect(unit, imp, blueLUT,  1, FALSE);
  
  theLut = CurrentLut(env, imp);
  zdlwrite(unit, redLUT,   1, theLut);
  zdlwrite(unit, greenLUT, 1, theLut);
  zdlwrite(unit, blueLUT,  1, theLut);

  zddbatch(unit, False);
  
  env->isOn.bw = True;
  env->isOn.red = env->isOn.green = env->isOn.blue = False;

  return SUCCESS;
}
/************************************************************************/
int SetPseudoMode(env)

  VIDSEnvironment *env;
  
{
  int unit;				/* device unit number		*/
  int imp;				/* image plane number		*/
  int i;
  int *theLut;
  int tempLut[256];
  PSTable *pstbl;
  int *CurrentLut();
  PSTable *CurrentPSTable();
  static int pseudoconfig[4] = {2,-1,-1,-1};

  unit = env->devUnit;
  imp = env->bwIMP;

  zddbatch(unit, True);

  if (!env->isPseudo)			/* avoid flashing on jmovie */
  {
    zddconfigure(unit, pseudoconfig);

    zddinfo (unit, 4, 1, &env->nimps);	/* Recheck things (mainly for Jupiter)*/
    zddinfo (unit, 30, 1, &i);		/* graphics overlay available? */
    if (i == 1)					/* yes */
    {
      env->grafIMP = zdsgraph(unit);	/* ask vrdi which is graphics */
      GetGrColors(env);			/* reset the graphics colors */
    }
    else					/* no */
      env->grafIMP = 1;			/* use plane 1 for graphics   */
    NewName(env, env->grafIMP, "GRAPHICS");
  }

  env->isColor = FALSE;
  env->isPseudo = TRUE;
  env->isBW = FALSE;

  zdlconnect(unit, imp, redLUT,   1, FALSE);
  zdlconnect(unit, imp, greenLUT, 1, FALSE);
  zdlconnect(unit, imp, blueLUT,  1, FALSE);
  
  theLut = CurrentLut(env, imp);
  pstbl = CurrentPSTable(env, imp);

  for (i=0; i<256; i++)		/* combine stretch and PS table into one LUT */
    tempLut[i] = pstbl->red[theLut[i]];
  zdlwrite(unit, redLUT,   1, tempLut);
  for (i=0; i<256; i++)
    tempLut[i] = pstbl->green[theLut[i]];
  zdlwrite(unit, greenLUT, 1, tempLut);
  for (i=0; i<256; i++)		/* combine stretch and PS table into one LUT */
    tempLut[i] = pstbl->blue[theLut[i]];
  zdlwrite(unit, blueLUT,  1, tempLut);
  
  zddbatch(unit, False);

  env->isOn.bw = True;
  env->isOn.red = env->isOn.green = env->isOn.blue = False;

  return SUCCESS;
}
/************************************************************************/
SetColorMode(env)

  VIDSEnvironment *env;
  
{
  int unit;				/* device unit number		*/
  int i;
  int *CurrentLut();
  static int colorconfig[4] = {1,-1,-1,-1};
  
  unit = env->devUnit;

  zddbatch(unit, True);

  if (!env->isColor)			/* avoid flashing on jmovie */
  {
    zddconfigure(unit, colorconfig);

    zddinfo (unit, 4, 1, &env->nimps);	/* Recheck things (mainly for Jupiter)*/
    zddinfo (unit, 30, 1, &i);		/* graphics overlay available? */
    if (i == 1)					/* yes */
    {
      env->grafIMP = zdsgraph(unit);	/* ask vrdi which is graphics */
      GetGrColors(env);			/* reset the graphics colors */
    }
    else					/* no */
      env->grafIMP = 1;			/* use plane 1 for graphics   */
    NewName(env, env->grafIMP, "GRAPHICS");
  }
  
  env->isColor = TRUE;
  env->isPseudo = FALSE;
  env->isBW = FALSE;

  zdlconnect(unit, env->redIMP,   redLUT,   1, FALSE);
  zdlconnect(unit, env->greenIMP, greenLUT, 1, FALSE);
  zdlconnect(unit, env->blueIMP,  blueLUT,  1, FALSE);
  
  zdlwrite(unit, redLUT,   1, CurrentLut(env,env->redIMP));
  zdlwrite(unit, greenLUT, 1, CurrentLut(env,env->greenIMP));
  zdlwrite(unit, blueLUT,  1, CurrentLut(env,env->blueIMP));

  zddbatch(unit, False);

  env->isOn.bw = False;
  env->isOn.red = env->isOn.green = env->isOn.blue = True;

  return;
}
/************************************************************************/
Boolean HasGraphics(env)
  VIDSEnvironment *env;
{
  int	status,hasGr;

  status = zddinfo(env->devUnit, 30, 1, &hasGr);
  if (status != SUCCESS) return False;
  if (hasGr == 0) return False;
  return True;
}
/************************************************************************/
int ShowGraphics(env)
  VIDSEnvironment *env;
{
  if (zdgon(env->devUnit) != SUCCESS)
    ABORT(FAIL, "Unable to turn on graphics plane", "VIDS-VRDIERR");
  env->isOn.graf = True;

  return SUCCESS;
}
/************************************************************************/
int HideGraphics(env)
  VIDSEnvironment *env;
{
  if (zdgoff(env->devUnit) != SUCCESS)
    ABORT(FAIL, "Unable to turn off graphics plane", "VIDS-VRDIERR");
  env->isOn.graf = False;

  return SUCCESS;
}
/************************************************************************/
int GetUnit(filename)
  char	*filename;		/* in: name of the file			*/
{
  int unit,status;
  static int		instance = 1;	/* instance for zvunit-increase	*/
  
  status = zvunit(&unit, "VIDS", instance, "U_NAME", filename, 0);
  instance++;
  if (status < 0)
      return status;
  else if (status != SUCCESS)
      return FAIL;
  else
      return unit;
}
/************************************************************************/
int OpenFile(file)

  FileInfo		*file;		/* in: info for file to open	*/
{
  int			unit, status;
  unsigned char		*picPtr;
  int			nl,ns,nb,nbb,nlb; /* image sizes		*/
  char			orgStr[8],formatStr[10];
  int			i, band;	/* increment variables		*/
  int			trans[12];	/* translation buffer for test	*/

  if (file->isOpen)
    return SUCCESS;			/* file already open */

  unit = file->fileUnit;
  if (unit == noUnit)
    unit = GetUnit(file->filename);

  status = zvopen(unit, "ADDRESS", &picPtr, "io_act","s", 0); /* Try array i/o */
  if (status != SUCCESS)
  {
    status = zvopen(unit, "io_act","s", 0);  /* if array fails, try no array i/o */
    if (status != SUCCESS)
    {
      zvsignal(unit, status, FALSE);
      return status;
    }
    picPtr = NULL;
  }
  
  file->fileUnit = unit;
  status = zvget(unit, "NL", &nl, "NS", &ns, "NB", &nb, "ORG", orgStr,
        "FORMAT", formatStr, "NBB", &nbb, "NLB", &nlb, 0);
  if (status != SUCCESS)
  {
    zvsignal(unit, status, FALSE);
    status = zvclose(unit, 0);
    return status;
  }
  /* If binary labels are present, or if a host translation is	*/
  /* required, then force line I/O to avoid all the extra	*/
  /* calculations we would need.				*/

  if (picPtr != NULL)
  {
    zvtrans_inu(trans, formatStr, formatStr, unit);

    if ((nlb != 0) || (nbb != 0) || (trans[0] != 0))
    {
      zvclose(unit, 0);
      status = zvopen(unit, "io_act","s", 0);
      if (status != SUCCESS)
      {
        zvsignal(unit, status, FALSE);
        return status;
      }
      picPtr = NULL;
    }
  }

  if (picPtr == NULL)
    NotifyUser(Verbose,"","File opened for line-oriented i/o");
  else
    NotifyUser(Verbose,"","File opened for array i/o");
  
  file->addr = picPtr;
  file->isOpen = TRUE;
  file->nl = nl;
  file->ns = ns;
  file->nb = nb;
  
  switch (orgStr[2])
  {
    case 'Q' :
        file->org = BSQ;
        break;
    case 'L' :
        file->org = BIL;
        break;
    case 'P' :
        file->org = BIP;
        break;
  }
  switch (formatStr[0])
  {
    case 'B' :
        file->format = ByteFormat;
        break;
    case 'H' :
        file->format = HalfFormat;
        break;
    case 'W' :				/* WORD format... should be obsolete */
        file->format = HalfFormat;
        break;
    case 'F' :
        file->format = FullFormat;
        break;
    case 'R' :
        file->format = RealFormat;
        break;
    case 'D' :
        file->format = DoubFormat;
        break;
    case 'C' :
        file->format = CompFormat;
        break;
  }
  
  return SUCCESS;
}
/************************************************************************/
/* Open a file for output.  The file is not opened for array I/O for
 * simplicity... output files are not used that often!  The file is
 * opened with a u_format of Byte.
 */
int OpenOutFile(file)

  FileInfo		*file;		/* in: info for file to open	*/
{
  int			unit, status;

  if (file->isOpen)
    return SUCCESS;			/* file already open */

  unit = file->fileUnit;
  if (unit == noUnit)
    unit = GetUnit(file->filename);

  switch (file->org)
  {
    case BSQ:  status = zvadd(unit, "u_org", "bsq", 0);	break;
    case BIL:  status = zvadd(unit, "u_org", "bil", 0);	break;
    case BIP:  status = zvadd(unit, "u_org", "bip", 0);	break;
    default:  status = SUCCESS;
  }
  if (status != SUCCESS)
  {
    zvsignal(unit, status, FALSE);
    return status;
  }
  switch (file->format)
  {
    case ByteFormat:  status = zvadd(unit, "o_format", "byte", 0); break;
    case HalfFormat:  status = zvadd(unit, "o_format", "half", 0); break;
    case FullFormat:  status = zvadd(unit, "o_format", "full", 0); break;
    case RealFormat:  status = zvadd(unit, "o_format", "real", 0); break;
    case DoubFormat:  status = zvadd(unit, "o_format", "doub", 0); break;
    case CompFormat:  status = zvadd(unit, "o_format", "comp", 0); break;
    default:  status = SUCCESS;
  }
  if (status != SUCCESS)
  {
    zvsignal(unit, status, FALSE);
    return status;
  }

  status = zvopen(unit, "u_nl", file->nl, "u_ns", file->ns, "u_nb", file->nb,
	"op", "write", "u_format", "byte", "io_act", "s", 0);
  if (status != SUCCESS)
  {
    zvsignal(unit, status, FALSE);
    return status;
  }

  file->fileUnit = unit;
  file->addr = NULL;
  file->isOpen = TRUE;

  return SUCCESS;
}
/************************************************************************/  
/* CloseFile closes the file associated with theFile.  The file structure
 * remains in place for use by other commands.
 */
 
int CloseFile(theFile)

  FileInfo		*theFile;	/* file information structure	*/
{
  int status;

  if (!theFile->isOpen)
    return SUCCESS;
  status = zvclose(theFile->fileUnit, "CLOS_ACT", "FREE", 0);
  theFile->isOpen = FALSE;
  theFile->addr = NULL;
  theFile->fileUnit = noUnit;
  return status;
}
/************************************************************************/  
/*  InitFileList initializes the global list of files.
 */
int InitFileList()

{
  firstFile.next = NULL;	/* no other files on list; must be set	*/
  firstFile.last = NULL;	/* before first call to InitFile	*/
  InitFile(&firstFile);

  return SUCCESS;
}
/************************************************************************/  
/*  findFileSlot returns a pointer to a FileInfo structure describing the
 *  specified file.  If the file is not already pointed at, a new struct is
 *  used.  If a new struct is allocated, the host file name is copied into
 *  it and the struct initialized.
 */
FileInfo *findFileSlot(env,filename)

  VIDSEnvironment	*env;		/* current environment (device)	*/
  char			*filename;	/* user specified file name	*/
  
{
  char		hostName[STRINGSIZ+1];	/* Full host file spec		*/
  FileInfo	*next, *curr;		/* links to info structs	*/
  
  GetHostFileName(filename, hostName);
  
  next = &firstFile;
  
  do
  {
    curr = next;
    if (EQUAL(curr->filename, hostName))
        return curr;
    next = curr->next;
  } while (next != NULL);
  
  next = malloc(sizeof(FileInfo)); /* file not in use, so get a new struct */
  if (next == NULL)		   /* If there is not enough memory for a  */
  {				   /* new struct, try to re-use an old one */
    next = &firstFile;
  
    do
    {
      curr = next;
      if (curr->fileUnit == noUnit)
      {
        InitFile(curr);
        strcpy(curr->filename, hostName);
        return curr;
      }
      next = curr->next;
    } while (next != NULL);
  }
  else
  {
    next->last = curr;
    next->next = NULL;
    curr->next = next;
    InitFile(next);
    strcpy(next->filename, hostName);
    return next;
  }
  return NULL;
}
/************************************************************************/  
/*  TieImp "ties" an image memory plane to a specific band of a file.  It
 *  does not load the file into the plane, but merely provides a logical
 *  relationship.
 */
TieImp(imp, file, band)

  PlaneInfo	*imp;			/* IMP to tie to file		*/
  FileInfo	*file;			/* info of file			*/
  int		band;			/* band of file to tie to imp	*/

{
  imp->file = file;
  imp->band = band;
  return;
}
/************************************************************************/  
/*  UntieImp breaks the logical connection between a plane and a file that
 *  TieImp sets up.  The plane is left with no associated file.
 */
UntieImp(imp)

  PlaneInfo	*imp;			/* IMP to untie			*/

{
  imp->file = NULL;
  imp->band = 1;
  imp->imageWindow.sl = 1;
  imp->imageWindow.ss = 1;
  imp->imageWindow.nl = 0;
  imp->imageWindow.ns = 0;
  return;
}
/************************************************************************/  
/* Redraw will load up each file into its IMP, thus displaying it.
 *   
 */
int Redraw(env)

  VIDSEnvironment		*env;	/* current VIDS environment	*/

{
  int status;
  
  status = SUCCESS;

  if (env->isColor)
  {
    status = LoadIMP(env, env->redIMP);
    if (status != SUCCESS) return status;
    status = LoadIMP(env, env->greenIMP);
    if (status != SUCCESS) return status;
    status = LoadIMP(env, env->blueIMP);
  }
  else
  {
    status = LoadIMP(env, env->bwIMP);
  }
  return status;
}
/************************************************************************/
/* PixelSize returns the number of bytes per pixel of the specified format.
 */
int PixelSize(format)

  FileFormat	format;

{
  switch (format)
  {
    case ByteFormat : return 1;
    case HalfFormat : return 2;
    case FullFormat : return 4;
    case RealFormat : return 4;
    case DoubFormat : return 8;
    case CompFormat : return 8;
    default : return 1;			/* no other format should	*/
  }					/* exist, but just in case...	*/
}
/************************************************************************/
/* UpperCase converts a string into upper case.  It is ok for the source
 * string to be the same as the destination string.
 */

UpperCase(s, d)

  char *s;				/* source string		*/
  char *d;				/* destination string		*/
{
  while (*s != '\0')
  {
    *d = islower(*s) ? toupper(*s) : *s;
    s++; d++;
  }
  *d = '\0';
  return;
}
/************************************************************************/
/* SetFileWindow sets the desired display window associated with the
 * specific file.
 */
SetFileWindow(thePlane, sl, ss, nl, ns)

  PlaneInfo		*thePlane;	/* File whose window we will set      */
  int			sl,ss,nl,ns;	/* New window (size field) parameters */
{
  thePlane->imageWindow.sl = sl;
  thePlane->imageWindow.ss = ss;
  thePlane->imageWindow.nl = nl;
  thePlane->imageWindow.ns = ns;
  return;
}
/************************************************************************/
/* ZoomToFit looks at the desired image area, calculates the zoom needed
 * to fit the whole thing on the available display device, and sets the
 * image plane softZoom to that value.  It returns the new zoom factor.
 */

int ZoomToFit(env, theIMP)

  VIDSEnvironment	*env;	   /* the current environment		*/
  int			theIMP;	   /* the IMP in question		*/
{
  PlaneInfo		*thePlane; /* the plane for theIMP		*/
  FileInfo		*theFile;  /* the file associated with theIMP	*/
  int			zoom;	   /* the zoom factor			*/
  float			x,y;	   /* lines and samples in float format	*/
  int			nl,ns;	   /* lines and samples			*/
  
  thePlane = &env->planes[theIMP];
  theFile = thePlane->file;
  if (theFile == NULL)
  {
    zoom = 1;
  }
  else
  {
    nl = thePlane->imageWindow.nl;
    if (nl == 0) nl = theFile->nl - thePlane->imageWindow.sl + 1;
    if (nl == 0) nl = 1;		/* Won't happen unless bad input file */
    y = (float) (VideoLines(env) - thePlane->accessWindow.sl + 1) /
							(float) (nl);
    ns = thePlane->imageWindow.ns;
    if (ns == 0) ns = theFile->ns - thePlane->imageWindow.ss + 1;
    if (ns == 0) ns = 1;		/* Won't happen unless bad input file */
    x = (float) (VideoSamps(env) - thePlane->accessWindow.ss + 1) /
							(float) (ns);
    x = MIN(x,y);
    zoom = (int) ((x < 1.0) ? -(1.0/x + 0.999) : x);
  }
  thePlane->softZoom = zoom;
  return zoom;
}
/************************************************************************/
/* GetPlaneList will get a list of planes from the names and/or numbers
 * supplied by the user on the command line.  It returns the list of
 * planes as a list of integers in planeList, and the number of planes
 * in nPlanes.  planeList is assumed large enough to hold all of the
 * planes, up to a maximum of MAXPLANES.
 */
#define NEXT(S,M,K) {s_copy(M,env->message); s_copy(K,env->key); status=S; continue;}

int GetPlaneList(env, planeList, nPlanes, duplicates)

  VIDSEnvironment	*env;		/* in: the VIDS environment	*/
  int			planeList[];	/* out: the list of image planes*/
  int			*nPlanes;	/* out: the number of planes	*/
  Boolean		duplicates;	/* in : True if duplicates allowed*/
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  char			*theStr;	/* temp string ptr		*/
  int			i,j;		/* temporary increment variable	*/
  int			status;		/* current status holder	*/
  int			count;		/* TCL variable count		*/
  char		   msgbuf[STRINGSIZ+1];	/* Temp buf for messages	*/
  Boolean		onlyOne;	/* True if only one plane ok	*/
  TAEVariable		*GetVariable();	/* func to get variable		*/
  
  onlyOne = False;
  v = GetVariable(env, "PLANES");	/* allow either PLANES or PLANE	*/
  if (v == NULL)			/* so that we can use this	*/
  {					/* routine even when only one	*/
    v = GetVariable(env, "PLANE");	/* plane is allowed.		*/
    if (v == NULL)
    {
      *nPlanes = 0;
      return SUCCESS;
    }
    onlyOne = True;	/* if PLANE parameter, only allow one plane	*/
  }
  count = onlyOne ? 1 : v->v_count;
  if (count <= 0)
  {
    *nPlanes = 0;
    return SUCCESS;
  }
  status = SUCCESS;
  *nPlanes = 0;
  for (i = 0; i < count; i++)
  {
    theStr = SVAL(*v, i);
    UpperCase(theStr, theStr);
/************  special case "ALL" ***************************************/
    if (strcmp(theStr, "ALL") == 0)	/* check for special case "ALL"	*/
    {
      if (onlyOne)
        ABORT(FAIL, "Only one image plane may be specified for this command",
            "VIDS-ONEPLANE");
      for (j = 0; j < env->nimps; j++)
           planeList[j] = j + 1;	/* first IMP is 1, not 0	*/
      *nPlanes = env->nimps;
      break;
    }
/************  special case "VISIBLE" ***********************************/
    if (strcmp(theStr, "VISIBLE") == 0)	/* check for special case 	*/
    {					/* "VISIBLE"; all image planes	*/
      if (onlyOne)			/* which are visible            */
        ABORT(FAIL, "Only one image plane may be specified for this command",
            "VIDS-ONEPLANE");
      if (env->isColor)
      {
        if (env->isOn.red)   planeList[(*nPlanes)++] = env->redIMP;
        if (env->isOn.green) planeList[(*nPlanes)++] = env->greenIMP;
        if (env->isOn.blue)  planeList[(*nPlanes)++] = env->blueIMP;
      }
      else
        if (env->isOn.bw) planeList[(*nPlanes)++] = env->bwIMP;
      if (env->isOn.graf) planeList[(*nPlanes)++] = env->grafIMP;
      continue;
    }
/************  special case "COLOR" *************************************/
/* allow british spelling in honor of Lucas Kamp!!			*/
    if ((strcmp(theStr, "COLOR") == 0) || (strcmp(theStr, "COLOUR") == 0))
    {
      if (strcmp(theStr, "COLOUR") == 0)
        NotifyUser(Inform,"","\"COLOUR\", eh?  Are you British?");
      if (onlyOne)
        ABORT(FAIL, "Only one image plane may be specified for this command",
            "VIDS-ONEPLANE");
      planeList[(*nPlanes)++] = env->redIMP;
      planeList[(*nPlanes)++] = env->greenIMP;
      planeList[(*nPlanes)++] = env->blueIMP;
      continue;
    }
/************  special case "IMAGE" *************************************/
    if (strcmp(theStr, "IMAGE") == 0)
    {
      if (env->isColor)
      {
        if (onlyOne)
          ABORT(FAIL, "Only one image plane may be specified for this command",
              "VIDS-ONEPLANE");
        planeList[(*nPlanes)++] = env->redIMP;
        planeList[(*nPlanes)++] = env->greenIMP;
        planeList[(*nPlanes)++] = env->blueIMP;
      }
      else
      {
        planeList[(*nPlanes)++] = env->bwIMP;
      }
      continue;
    }
/************  No special case ******************************************/
    status = NameToImp(env, theStr, &planeList[*nPlanes]);
    if (status != SUCCESS)
      continue;
    if (planeList[*nPlanes] <= 0)
      NEXT(FAIL, "Image plane number must be positive.", "VIDS-BADPLANE");
    if (planeList[*nPlanes] > env->nimps)
    {
      sprintf(msgbuf, "Image plane %d not available; maximum is %d.",
              planeList[*nPlanes], env->nimps);
      NEXT(FAIL, msgbuf, "VIDS-BADPLANE");
    }
    *nPlanes += 1;
  }
  if (!duplicates)
    WeedArray(planeList, nPlanes);
  return status;
}
/************************************************************************/
/* PixelAddress returns a pointer to a specific pixel in an array I/O file.
 * This routine is always given band, line, and sample positions.  Differing
 * organizations are taken into account.
 */
unsigned char *PixelAddress(file, band, line, samp)
  FileInfo *file;
  int band, line, samp;
{
  int base;

  if (file->addr == NULL)
    return NULL;                /* Not array I/O! (or file not opened yet) */

  switch (file->org)
  {
    case BSQ:
      base = ((band-1)*file->nl + line-1)*file->ns + samp-1;
      break;
    case BIL:
      base = ((line-1)*file->nb + band-1)*file->ns + samp-1;
      break;
    case BIP:
      base = ((line-1)*file->ns + samp-1)*file->nb + band-1;
      break;
    default:
      base = 0;                         /* just in case... */
  }

  return file->addr + base * PixelSize(file->format);
}
/************************************************************************/
/* SetSize will set the size of the image memory planes and video output
 * for the current device.  If either size is 0 or not a valid value,
 * the device will not be changed.
 */
int SetSize(env, vidnl, vidns, impnl, impns)
  VIDSEnvironment	*env;	/* vids environment			*/
  int   vidnl, vidns;		/* in: lines/samps of video for output	*/
  int	impnl, impns;		/* in: lines/samps of video for input	*/
{
  int	config[4];		/* configuration array for zddconfigure	*/
  long	i;			/* temporary variable			*/
  int	unit;			/* device unit number			*/
  int	x,y;			/* x,y location holders			*/
  int   oldvl, oldvs;		/* old video lines/samples		*/
  unsigned char *BigMalloc();

  unit = env->devUnit;

  oldvl = VideoLines(env);
  oldvs = VideoSamps(env);

  for (i=0; i<4; i++)
    config[i] = -1;		/* use current values */

  if      (vidnl==512  && vidns==0)	config[2] = 1;
  else if (vidnl==1024 && vidns==0)	config[2] = 2;
  else if (vidnl==480  && vidns==0)	config[2] = 3;
  else if (vidnl==512  && vidns==512)	config[2] = 1;
  else if (vidnl==1024 && vidns==1024)	config[2] = 2;
  else if (vidnl==480  && vidns==640)	config[2] = 3;
  else if (vidnl==512  && vidns==640)	config[2] = 4;
  else if (vidnl==512  && vidns==1024)	config[2] = 5;
  else if (vidnl==1024 && vidns==1280)	config[2] = 6;
  else if (vidnl!=0    || vidns!=0)		/* error unless both 0 */
    ABORT(FAIL, "Invalid configuration for video", "VIDS-BADPARAM");

  if      (impnl==512  && impns==0)	config[1] = 1;
  else if (impnl==1024 && impns==0)	config[1] = 2;
  else if (impnl==480  && impns==0)	config[1] = 3;
  else if (impnl==2048 && impns==0)	config[1] = 7;
  else if (impnl==512  && impns==512)	config[1] = 1;
  else if (impnl==1024 && impns==1024)	config[1] = 2;
  else if (impnl==480  && impns==640)	config[1] = 3;
  else if (impnl==512  && impns==640)	config[1] = 4;
  else if (impnl==512  && impns==1024)	config[1] = 5;
  else if (impnl==1024 && impns==1280)	config[1] = 6;
  else if (impnl==2048 && impns==2048)	config[1] = 7;
  else if (impnl==4096 && impnl==4096)	config[1] = 8;
  else if (impnl!=0    || impns!=0)		/* error unless both 0 */
    ABORT(FAIL, "Invalid configuration for image planes", "VIDS-BADPARAM");

  if (zddconfigure(unit, config) != SUCCESS)
    ABORT(FAIL, "Device cannot be set to that size", "VIDS-VRDIERR");

/* Now that the device is in a new configuration, we have to reload	*/
/* a lot of saved information, and reset settings that may have changed	*/
/* (many of these have been added because of the peculiarities of 	*/
/* individual devices, particularly the Adage)				*/

  zddinfo (unit, 4, 1, &env->nimps);	/* Now reload all the saved	*/
  env->nlMax = zdsnl(unit);		/* size information.		*/
  env->nsMax = zdsns(unit);

  i = (long) env->nlMax * env->nsMax;
  if (i > env->bufSize)
  {
    BigFree(env->buffer);
    env->buffer = BigMalloc(i);
    if (env->buffer == NULL)
      ABORT(FAIL, "Insufficient memory for image display buffer.",
                  "VIDS-INSUFMEM");
    env->bufSize = i;
  }

  zddinfo (unit, 30, 1, &i);		/* graphics overlay available? */
  if (i == 1)					/* yes */
    env->grafIMP = zdsgraph(unit);	/* ask vrdi which is graphics */
  else						/* no */
    env->grafIMP = 1;			/* use plane 1 for graphics   */
  NewName(env, env->grafIMP, "GRAPHICS");

  if (env->isColor)		/* Device may have been put into color	*/
    SetColorMode(env);		/* mode or vice-versa, so put it back.	*/
  else if (env->isBW)
    SetBWMode(env);
  else if (env->isPseudo)
    SetPseudoMode(env);

  if (oldvl != VideoLines(env) || oldvs != VideoSamps(env))
    SetDefaultRegions(env);	/* video changed, reset default display rgns */

  return SUCCESS;
}
/************************************************************************/
/* ShowSize will display the current size attributes of the display
 * device on the user terminal.
 */
int ShowSize(env)
  VIDSEnvironment	*env;
{
  int	config[4];		/* configuration array for zddconfigure	*/
  int	status;			/* status holder			*/
  int	nl,ns;			/* no. lines/samps video		*/
  int   unit;

  unit = env->devUnit;

  NotifyUser(Inform,"","Video output is \t%dx%d",zdsvnl(unit), zdsvns(unit));
  NotifyUser(Inform,"","No. of memory planes is \t%d", env->nimps);
  NotifyUser(Inform,"","No. of lines per plane is \t%d", env->nlMax);
  NotifyUser(Inform,"","No. of samps per plane is \t%d", env->nsMax);
  return SUCCESS;
}
/************************************************************************/
/* FloatZoom will return as a floating point number a useable zoom
 * by which a number may be divided to scale for zoom.
 */
float FloatZoom(zoom)
  int zoom;					/* in: zoom factor	*/
{
  if (zoom == 0) return 1.0;
  if (zoom < 0) return (-1.0 / ((float) zoom));
  return ((float) zoom);
}
/************************************************************************/
/* Zoom applies a zoom to number and returns the result as an int.
 * It returns what you would expect for a single number, i.e., a positive
 * zoom enlarges the number.
 */
int Zoom(n, zoom)
  int		n;		/* number to zoom	*/
  int		zoom; 		/* zoom factor		*/
{
  if (zoom == 0) zoom = 1;
  if (zoom < 0)
  {
    if (n >= 0)
      return (-(n - zoom - 1) / zoom);	/* Correct for int trunc*/
    else
      return ((-n - zoom - 1) / zoom);	/* handle negatives correctly */
  }
  return (n * zoom);
}
/************************************************************************/
/* StringToColor takes a string and returns the associated GraphColor.
 */
GraphColor StringToColor(string)
  char *string;			/* the input string	*/
{
  GraphColor color;

  switch (*string)	/* check only first char to allow abbreviations */
  {
    case 'R' : color = Red;	 break;
    case 'G' : color = Green;	 break;
    case 'B' : 
      if (*(string+2) == 'U')
        color = Blue;
      else
        color = Black;
      break;
    case 'W' : color = White;	 break;
    case 'Y' : color = Yellow;	 break;
    case 'C' : color = Cyan;	 break;
    case 'M' : color = Magenta;  break;
    case 'T' : color = NoColor;  break;		/* transparent */
    default  : color = NoColor;  break;
  }

  return color;
}
/************************************************************************/
/* WeedArray will go through an array of integers and eliminate
 * any duplicate elements.
 */
int WeedArray(array, n)
  int array[];				/* in/out: array to be weeded	*/
  int *n;				/* in/out: number of elements	*/
{
  register int i,j,k,total;
  
  total = *n - 1;
  for (i = 0; i < total; i++)
  {
    for (j = i + 1; j <= total; j++)
    {
      if (array[j] == array[i])
      {
        for (k = j; k < total; k++) array[k] = array[k + 1];
        total--;
      }
    }
  }
  *n = total + 1;
  return SUCCESS;
}
/************************************************************************/
/* GetVariable returns a pointer to the variable struct in the input
 * parblock for the given variable name.  If the variable is not found,
 * an error message is filled in and NULL is returned.
 */
TAEVariable *GetVariable(env, varname)
  VIDSEnvironment	*env;
  char			*varname;
{
  TAEVariable *v;
  TAEVariable *p_find();
  
  v = p_find (&InParblk, varname);
  if (v == NULL)
  {
    sprintf(env->message,
           "Sorry, %s parameter was not sent to VIDS; check PDF file.", varname);
    s_copy("VIDS-MISSING", env->key);
  }
  return v;
}
/************************************************************************/
/* SaveImageRegion reads a region of the image plane into a memory buffer
 * supplied by the caller.  The VRDI status is returned.
 */
int SaveImageRegion(env, imp, rect, buf)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int		imp;			/* Image plane to use		*/
  Rect		*rect;			/* Rectangle to save		*/
  unsigned char	*buf;			/* Buffer to use		*/
{
  int size;
  int status;

  size = (rect->right - rect->left + 1) * (rect->bottom - rect->top + 1);

  status = zdiawset(env->devUnit, imp, rect->left, rect->top,
				     rect->right, rect->bottom);
  if (status != SUCCESS)
    ABORT(FAIL, "Error setting access window", "VIDS-VRDIERR");
  status = zdiawread(env->devUnit, imp, size, buf);
  zdiawset(env->devUnit, imp, 1, 1, env->nsMax, env->nlMax);
  if (status != SUCCESS)
    ABORT(FAIL, "Error reading image data from device", "VIDS-VRDIERR");

  return SUCCESS;
}

/************************************************************************/
/* RestoreImageRegion writes a region of memory to the image plane.
 * Companion to SaveImageRegion().  The VRDI status is returned.
 */
int RestoreImageRegion(env, imp, rect, buf)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int		imp;			/* Image plane to use		*/
  Rect		*rect;			/* Rectangle to save		*/
  unsigned char	*buf;			/* Buffer to use		*/
{
  int size;
  int status;

  size = (rect->right - rect->left + 1) * (rect->bottom - rect->top + 1);

  status = zdiawset(env->devUnit, imp, rect->left, rect->top,
				     rect->right, rect->bottom);
  if (status != SUCCESS)
    ABORT(FAIL, "Error setting access window", "VIDS-VRDIERR");
  status = zdiawwrite(env->devUnit, imp, size, buf);
  zdiawset(env->devUnit, imp, 1, 1, env->nsMax, env->nlMax);
  if (status != SUCCESS)
    ABORT(FAIL, "Error writing image data to device", "VIDS-VRDIERR");

  return SUCCESS;
}

/************************************************************************/
/* UpdateVRDIState is called by dovids() to check the VRDI flags to see if
 * anything has changed since the last VIDS invocation.  If so, it updates
 * the environment block and other things appropriately.
 */
int UpdateVRDIState(env)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
{
  int unit;
  int nluts;
  int i;
  int mode;
  int imp;
  int *lut, templut[256];
  PSTable *pstable;
  unsigned char *BigMalloc();
  int *NewLut();
  PSTable *NewPSTable();

  unit = env->devUnit;
  if (unit == noUnit)
    return SUCCESS;			/* device not open yet */

  if (zdfconfig(unit))			/* configuration changed */
  {
    zddinfo (unit, 4, 1, &env->nimps);	/* Now reload all the saved	*/
    env->nlMax = zdsnl(unit);		/* size information.		*/
    env->nsMax = zdsns(unit);

    i = (long) env->nlMax * env->nsMax;
    if (i > env->bufSize)
    {
      BigFree(env->buffer);
      env->buffer = BigMalloc(i);
      if (env->buffer == NULL)
        ABORT(FAIL, "Insufficient memory for image display buffer.",
                    "VIDS-INSUFMEM");
      env->bufSize = i;
    }

    zddinfo (unit, 30, 1, &i);		/* graphics overlay available? */
    if (i == 1)					/* yes */
      env->grafIMP = zdsgraph(unit);	/* ask vrdi which is graphics */
    else					/* no */
      env->grafIMP = 1;			/* use plane 1 for graphics   */
    NewName(env, env->grafIMP, "GRAPHICS");

    mode = zdsmode(unit);		/* get the display mode */

    if (mode == 1)			/* color */
    {
      env->isColor = TRUE;
      env->isBW = FALSE;
      env->isPseudo = FALSE;
      env->redIMP = zdsimp(unit,1);
      env->greenIMP = zdsimp(unit,2);	/* returns 1 on err, so don't need */
      env->blueIMP = zdsimp(unit,3);	/* to check # of lookup tables */
      NewName(env, env->redIMP, "RED");		/* Ignore status on these */
      NewName(env, env->greenIMP, "GREEN");	/* to do as many as possible */
      NewName(env, env->blueIMP, "BLUE");
    }
    else if (mode == 2)			/* pseudocolor */
    {
      env->isColor = FALSE;
      env->isBW = FALSE;
      env->isPseudo = TRUE;
      env->bwIMP = zdsimp(unit,1);	/* all LUTs connect to same plane */
      NewName(env, env->bwIMP, "BW");
    }
    else if (mode == 3)			/* black & white */
    {
      env->isColor = FALSE;
      env->isBW = TRUE;
      env->isPseudo = FALSE;
      env->bwIMP = zdsimp(unit,1);	/* all LUTs connect to same plane */
      NewName(env, env->bwIMP, "BW");
    }
  }

  if (zdfimage(unit,0))			/* have any image planes changed? */
  {
    for (i=1; i<=env->nimps; i++)
    {
      if (zdfimage(unit,i))
      {
        UntieImp(&env->planes[i]);	/* no file for this plane any more */
        InvalHist(&env->planes[i]);	/* kill the histogram for this plane */
      }
    }
  }

  if (zdflut(unit,0))			/* have any LUTs changed? */
  {
    zddinfo(unit, 3, 1, &nluts);	/* get # of LUTs */
    mode = zdsmode(unit);
    if (mode == 1)			/* full color */
    {
      for (i=1; i<=nluts; i++)
      {
        if (zdflut(unit,i))
        {
          lut = NewLut(env, zdsimp(unit,i));	/* get LUT for this plane */
	  zdlread(unit,i,0,templut);
	  if (i == 1)
	    CheckLUT(templut, lut, &env->isOn.red);
	  if (i == 2)
	    CheckLUT(templut, lut, &env->isOn.green);
	  if (i == 3)
	    CheckLUT(templut, lut, &env->isOn.blue);
        }
      }
    }
	/* Pseudocolor doesn't need to worry about plane on/off status, 'cuz */
	/* if plane is off, LUTs all=0, so equal each other, so it's bw mode */
    else if (mode == 2)			/* pseudocolor */
    {
      imp = zdsimp(unit,1);		/* all LUTs connect to same plane */
      lut = NewLut(env, imp);		/* ramp the stretch */
      RampLut(lut);
      pstable = NewPSTable(env, imp);
      if (nluts >= 1)			/* and read in the pstable */
        zdlread(unit,1,0,pstable->red);
      if (nluts >= 2)
        zdlread(unit,2,0,pstable->green);
      if (nluts >= 3)
        zdlread(unit,3,0,pstable->blue);
    }
    else				/* monochrome */
    {
      lut = NewLut(env, zdsimp(unit,1));    /* all LUTs connect to same plane */
      zdlread(unit,1,0,templut);	    /* and should be the same	      */
      CheckLUT(templut, lut, &env->isOn.bw);
    }
  }

  if (zdfglut(unit))			/* did the graphics LUT change? */
  {
    GetGrColors(env);			/* reset the drawing colors */
  }

  return SUCCESS;
}

/************************************************************************/
/* CheckLUT is called by UpdateVRDIState to check the on/off status of a
 * plane.  If the input LUT is all 0, then the on/off flag is set False (off),
 * and the lut is not copied.  If it is not all 0, the on/off flag is set True
 * (on), and the lut is copied to the output LUT.
 */
int CheckLUT(inlut, outlut, ison)
  int *inlut, *outlut;			/* input & output LUTs		*/
  int *ison;				/* out: set TRUE or FALSE	*/
{
  int i;

  for (i=0; i<256; i++)
  {
    if (inlut[i] != 0)
    {
      *ison = TRUE;
      BlockMove(inlut, outlut, 256*sizeof(*outlut));
      return SUCCESS;
    }
  }

  *ison = FALSE;
  return SUCCESS;
}

