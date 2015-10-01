#include "VIDSdefs.h"
/************************************************************************/  
/* LoadIMP loads the file associated with theIMP from disk if there is one.
 */
int LoadIMP(env, theIMP)

  VIDSEnvironment	*env;		/* current VIDS environment	*/
  int			theIMP;		/* the imp in question		*/

{
  PlaneInfo		*thePlane;	/* description of the plane	*/
  FileInfo		*theFile;	/* description of the file	*/
  int			status;		/* status indicator		*/
  int			devUnit;	/* the device unit		*/
  FileOrg		org;		/* org of the file (BSQ, etc)	*/
  FileFormat		format;		/* the data format of the file	*/
  int			slDisp,ssDisp;	/* sl, ss in file to display	*/
  int			nlDisp,nsDisp;	/* nl, ns in file to display	*/
  int		  nlFile,nsFile,nbFile;	/* nl, ns, nb actually in file	*/
  int			slAW,ssAW,sb;	/* sl, ss, sb of access window	*/
  int			nsOut,nlOut;	/* dim of final array for output*/
  int			zoom;		/* zoom factor			*/
  int			pixIncr;	/* pixels to skip for zoom, BIP	*/
  int			lineIncr;	/* lines to skip for zoom	*/
  int			fileIncr;	/* incr in file for big moves	*/
  int			repeat;		/* repeat factor for zoom	*/
  int			line, i, j;	/* loop control variables	*/
  unsigned char	     *start,*fileStart;	/* start of local, file buffers	*/
  float			slope,offset;	/* slope, offset for scaling	*/
  Boolean		opened;		/* TRUE if opened the file here	*/
  struct
  {					/* number of times to replicate	*/
    int	left;				/* pixel before imageWindow.ss	*/
    int	top;
  } subPix;
  unsigned char *PixelAddress();
  char msg[80];
  
  thePlane = &env->planes[theIMP];
  theFile = thePlane->file;
  devUnit = env->devUnit;
  opened = FALSE;
  
  if (theFile == NULL)			/* if no file is associated	*/
    return SUCCESS;			/* with this plane, do nothing.	*/

  if (! theFile->isOpen)
  {
    status = OpenFile(theFile);
    if (status != SUCCESS)
        ABORT(FAIL, "Sorry, could not open the input file", "VIDS-OPENFAIL");
    opened = TRUE;
  }

  sb = thePlane->band;
  if ((sb < 1) || (sb > theFile->nb))
  {
    sprintf(env->message,
           "Cannot load band %d; band must be from 1 to %d",sb,theFile->nb);
    strcpy(env->key, "VIDS-BADBAND");
    return FAIL;
  }
  zoom = thePlane->softZoom;
  format = theFile->format;
  org = theFile->org;
  slope = theFile->scale.slope;
  offset = theFile->scale.offset;
  if (slope == 0.0 && offset == 0.0)	/* need to use default slope & offset */
    GetDefDataRange(env, &slope, &offset);
  slDisp = thePlane->imageWindow.sl;
  ssDisp = thePlane->imageWindow.ss;
  subPix.left = thePlane->subPixel.left;
  if (subPix.left == zoom) subPix.left = 0;
  subPix.top  = thePlane->subPixel.top;
  if (subPix.top == zoom) subPix.top = 0;
  nlFile = theFile->nl;
  nsFile = theFile->ns;
  nbFile = theFile->nb;
  nlDisp = MIN(thePlane->imageWindow.nl, (nlFile - slDisp + 1));
  if (nlDisp == 0) nlDisp = nlFile - slDisp + 1;
  nsDisp = MIN(thePlane->imageWindow.ns, (nsFile - ssDisp + 1));
  if (nsDisp == 0) nsDisp = nsFile - ssDisp + 1;
  thePlane->imageWindow.nl = nlDisp;	/* save these for reference	*/
  thePlane->imageWindow.ns = nsDisp;
  slAW = thePlane->accessWindow.sl;
  ssAW = thePlane->accessWindow.ss;
  nsOut = thePlane->accessWindow.ns;
  if (nsOut == 0) nsOut = env->nsMax - ssAW + 1;
  nlOut = thePlane->accessWindow.nl;
  if (nlOut == 0) nlOut = env->nlMax - slAW + 1;
  NotifyUser(Inform, "", "Loading band number %d", sb);
  NotifyUser(Inform, "", "Applied zoom is %d", zoom);
  sprintf(msg, "Displayed DN = DN * %f + %f", slope, offset);
  NotifyUser(Verbose, "", msg);		/* Can't send doubles to NotifyUser */
  NotifyUser(Verbose, "", "File window is (%d, %d, %d, %d)",
      slDisp, ssDisp, nlDisp, nsDisp);
  if (zoom < 0)
  {
    nsOut = MIN((nsDisp / -zoom), nsOut);
    nlOut = MIN((nlDisp / -zoom), nlOut);
    pixIncr = (org == BIP) ? (nbFile * -zoom) : -zoom;
    lineIncr = -zoom;
    fileIncr = ((org == BSQ) ? nsFile : (nsFile * nbFile)) * -zoom
                * PixelSize(format);
    repeat = 1;
  }
  else
  {
    nsOut = MIN((nsDisp * zoom), nsOut);
    nlOut = MIN((nlDisp * zoom), nlOut);
    pixIncr = (org == BIP) ? nbFile : 1;
    lineIncr = 1;
    fileIncr = ((org == BSQ) ? nsFile : (nsFile * nbFile)) * PixelSize(format);
    if (subPix.top == 0) subPix.top = zoom;
    repeat = zoom;
  }

  if ((ssDisp == 1) &&			/* Special case, just a		*/
      (nsOut == nsFile) &&		/* straight data move from the	*/
      (format == ByteFormat) &&		/* file to the IMP		*/
      (zoom == 1) &&
      (theFile->addr != NULL) &&
      (org == BSQ))
  {
    start = theFile->addr + ((slDisp - 1) * nsFile) +
            ((sb - 1) * nsFile * nlFile);
  }
  else				/* must transfer to intermediate buffer	*/
  {
    start = env->buffer;
    if (theFile->addr != NULL)	/* if array i/o, set up the increments	*/
    {				/* needed, and chunk over the data.	*/
      fileStart = PixelAddress(theFile, sb, slDisp, ssDisp);
/* the loops are separated here just to ensure that the "if" is not repeated	*/
/* each time through the loop (remember, we need optimal speed here...)		*/
      if (format == ByteFormat)
      {
        if (zoom <= 1)
        {
          for (line = 0; line < nlOut; line++)	/* chunk bytes over	*/
          {					/* straight.		*/
            IncreMove(fileStart, start, nsOut, pixIncr);
            fileStart += fileIncr;
            start += nsOut;
          }
        }
        else	/* zoom > 1 */
        {
          line = nlOut;
          for (i = 0; (i < subPix.top) && (line > 0); i++)
          {						/* First do sub	*/
            ExpandMove(fileStart,start,nsOut,nsDisp,	/* pixeling for	*/
            	       pixIncr,zoom,subPix.left);	/* first input 	*/
            line--;					/* line.	*/
            start += nsOut;
          }
          fileStart += fileIncr;
          while (line > 0)
          {
            for (i = 0; (i < repeat) && (line > 0); i++)
            {
              ExpandMove(fileStart,start,nsOut,nsDisp,pixIncr,zoom,subPix.left);
              line--;
              start += nsOut;
            }
            fileStart += fileIncr;
          }
        }
      }
      else			/* not byte data, so translate to byte	*/
      {				/* and scale to user spec. range	*/
        if (zoom <= 1)
        {
          for (line = 0; line < nlOut; line++)
          {
            IncreTrans(fileStart,start, nsOut, pixIncr,
                       format, slope, offset);
            fileStart += fileIncr;
            start += nsOut;
          }
        }
        else	/* zoom > 1 */
        {
          line = nlOut;
          for (i = 0; (i < subPix.top) && (line > 0); i++)
          {						/* First do sub	*/
            ExpandTrans(fileStart,start,nsOut,nsDisp,	/* pixeling for	*/
            	       pixIncr,zoom,subPix.left,format,	/* first input 	*/
            	       slope,offset);			/* line.	*/
            line--;
            start += nsOut;
          }
          fileStart += fileIncr;
          while (line > 0)
          {
            for (i = 0; (i < repeat) && (line > 0); i++)
            {
              ExpandTrans(fileStart,start, nsOut, nsDisp, pixIncr, zoom,
              	          subPix.left, format, slope, offset);
	      line--;
              start += nsOut;
            }
            fileStart += fileIncr;
          }
        }
      }
    }
    else			/* for line oriented i/o, we must read	*/
    {				/* the lines into a separate buffer	*/
        			/* and translate it ourselves.		*/
      unsigned char *tmpBuf;
          
      tmpBuf = malloc(nsDisp * PixelSize(format));
      if (tmpBuf == NULL)
      {
        if (opened) CloseFile(theFile);
	ABORT(FAIL, "Not enough memory for line buffer", "VIDS-INSUFMEM");
      }

      for (line = slDisp, j = 0; j < nlOut; line += lineIncr)
      { 				 /* transfer one line at a time	*/
        status = zvread(theFile->fileUnit, tmpBuf, "LINE", line,
                 "SAMP", ssDisp, "BAND", sb, "NSAMPS", nsDisp,
                 "NBANDS", 1, 0);
        if (status != SUCCESS)
        {
          if (opened) CloseFile(theFile);
          free(tmpBuf);
	  ABORT(FAIL, "Unable to read line from image file", "VIDS-EXECERR");
        }
        if (zoom <= 1)
        {
          if (format == ByteFormat)
            IncreMove(tmpBuf, start, nsOut, pixIncr);
          else
            IncreTrans(tmpBuf,start, nsOut, pixIncr,
                        format, slope, offset);
          j++;
          start += nsOut;
        }
        else				/* if zoom > 1, re-copy the	*/
        {				/* same line repeatedly		*/
          for (i = 0; (i < repeat) && (j < nlOut); i++)
          {
            if (format == ByteFormat)
              ExpandMove(tmpBuf, start, nsOut, nsDisp, pixIncr, zoom, subPix.left);
            else
              ExpandTrans(tmpBuf,start, nsOut, nsDisp, pixIncr, zoom,
             	          subPix.left, format, slope, offset);
            j++;
            start += nsOut;
          }
        }
      }
      free(tmpBuf);
    }
    start = env->buffer;		/* for move to device below.	*/
  }
  i = nsOut + ssAW - 1;
  j = nlOut + slAW - 1;
  status = zdiawset(devUnit, theIMP, ssAW, slAW, i, j);
  if (status == SUCCESS)
  {
    thePlane->accessWindow.nl = nlOut;
    thePlane->accessWindow.ns = nsOut;
    i = nlOut * nsOut;
    status = zdiawwrite(devUnit, theIMP, i, start);
    InvalHist(thePlane); /* any collected histograms are no longer valid*/
  }
  zdiawset(devUnit, theIMP, 1, 1, env->nsMax, env->nlMax);
  if (opened) CloseFile(theFile);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to write the image", "VIDS-VRDIERR");

  return SUCCESS;
}
