#include "VIDSdefs.h"

extern struct PARBLK OutParblk;

/*****************************************************************************/
/* JGET-CURSOR reports the position of the cursor and the DN value at that
 * position relative to the plane and/or image file.
 *
 * Display format (if 'PRINT is on):

Line 200, Sample 300
  Display:  Line  Samp  Value     File:   Line  Samp  Value (Real) (Imaginary)
Plane   1:   512   100    255              512   100  15.336731    23.677425
Plane   2:     1   100     30             1000   640  255
Plane  15:  <<Out of window>>            <<No file for this plane>>
Plane  15:    42    32    222            <<Cursor outside file area>>

 * Or, if 'FILE is specified (i.e., no display):

Line   2, Sample  30
     File:   Line  Samp  Value (Real) (Imaginary)
Plane   1:    512   100  15.336731    23.677425
Plane   2:   1000   640  255
Plane  15:  <<No file for this plane>>

 */
int jget_cursor_do(env)
  VIDSEnvironment	*env;		/* the current display device environ */
{
  char *from;				/* FROM parameter (DISPLAY,FILE,BOTH) */
  int print;				/* PRINT parameter (TRUE==PRINT)      */
  int cursorNum;			/* CURSOR parameter (cursor number)   */
  int planeList[MAXPLANES];		/* list of planes to look at	      */
  int nPlanes;				/* number of planes to look at	      */
  int unit;				/* device unit number		      */
  int rawSamp, rawLine;			/* raw cursor position (rel to screen)*/
  int IMPSamp, IMPLine;			/* cursor position rel. to IMP	      */
  int fileSamp, fileLine;		/* cursor position rel. to file       */
  unsigned char dn;			/* DN value of IMP at cursor	      */
  int bigdn[2];				/* DN from file (must hold COMPLEX)   */
  char line[STRINGSIZ],temp[STRINGSIZ]; /* Line buffers for prints to terminal*/
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  struct PARBLK		*oparb;		/* pointer to output parm block	*/
  int			status;		/* temp status variable		*/
  int			i;		/* temp variable		*/
  TAEINT pIMPSamp[MAXPLANES],pIMPLine[MAXPLANES];	/* parameters	*/
  TAEINT pfileSamp[MAXPLANES],pfileLine[MAXPLANES];	/* thru PARBLK	*/
  TAEINT pIMPDN[MAXPLANES];				/* out to caller */
  TAEFLOAT pfileDN[MAXPLANES], pfileiDN[MAXPLANES];
  TAEVariable	*GetVariable();

  oparb = &OutParblk;

  v = GetVariable(env, "CURSOR");
  if (v == NULL) return FAIL;
  cursorNum = IVAL(*v, 0);
  
  v = GetVariable(env, "FROM");
  if (v == NULL) return FAIL;
  from = SVAL(*v, 0);

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = EQUAL(SVAL(*v, 0), "PRINT");

  unit = env->devUnit;
  status = GetPlaneList(env, planeList, &nPlanes, False);
  if (status != SUCCESS) return status;

  status = zdclocation(unit, cursorNum, &rawSamp, &rawLine);
  if (status != SUCCESS)
      ABORT(status, "Sorry, couldn't read the cursor position from the device",
		  "VIDS-VRDIERR");

  if (print)				/* Print header */
  {
      sprintf(line, "Line %3d, Sample %3d", rawLine, rawSamp);
      NotifyUser(Inform,"", line);
      strcpy(line, "");
      if (EQUAL(from,"DISPLAY") || EQUAL(from,"BOTH"))
	  strcat(line, "  Display:  Line  Samp  Value");
      if (EQUAL(from,"FILE") || EQUAL(from,"BOTH"))
	  strcat(line, "     File:   Line  Samp  Value (Real) (Imaginary)");
      NotifyUser(Inform,"", line);
  }

  status = q_intg(oparb, "PRAWSAMP", 1, &rawSamp, P_ADD);    /* Send to TAE */
  if (status != SUCCESS)
      ABORT(status, "Error sending RAWSAMP parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PRAWLINE", 1, &rawLine, P_ADD);
  if (status != SUCCESS)
      ABORT(status, "Error sending RAWLINE parameter back to TAE", "VIDS-TAEERR");

  for (i = 0; i < nPlanes; i++)
  {
      if (print)
	  sprintf(line, "Plane %3d:  ", planeList[i]);
      if (EQUAL(from,"DISPLAY") || EQUAL(from,"BOTH"))
      {
	  CursRaw2IMP(env, planeList[i], rawSamp, rawLine,
						  &IMPSamp, &IMPLine);
	  status = ReadPixDisplay(env, planeList[i],
			        IMPSamp, IMPLine, &dn);
	  if (status != SUCCESS)
	      return status;
	  if (print)
	  {
	      sprintf(temp, "%4d  %4d   %4d            ",
				IMPLine, IMPSamp, dn);
	      strcat(line, temp);
	  }

	  pIMPSamp[i] = IMPSamp;	/* Save values to send to TAE */
	  pIMPLine[i] = IMPLine;
	  pIMPDN[i] = dn;
      }
      else
	  pIMPSamp[i] = pIMPLine[i] = pIMPDN[i] = 0;

      if (EQUAL(from,"FILE") || EQUAL(from,"BOTH"))
      {
	  if (env->planes[planeList[i]].file == NULL)
	  {
	      if (print)
	      {
		  sprintf(temp, "<<No file for this plane>>");
		  strcat(line, temp);
	      }
	      pfileSamp[i] = pfileLine[i] = 0;
	      pfileDN[i] = pfileiDN[i] = 0;
	  }
	  else
	  {
	      CursRaw2IMP(env, planeList[i], rawSamp, rawLine,
						      &IMPSamp, &IMPLine);
	      CursIMP2File(&env->planes[planeList[i]], IMPSamp, IMPLine,
						       &fileSamp, &fileLine);
	      status = ReadPixFile(env, planeList[i],
				 fileSamp, fileLine, bigdn);
	      if (status != SUCCESS && status != CLIPPED)
		  return status;

	      pfileSamp[i] = fileSamp;	/* Save values to send to TAE */
	      pfileLine[i] = fileLine;
	      pfileiDN[i] = 0;

	      if (status == CLIPPED)
	      {
		  if (print)
		  {
		      sprintf(temp, "<<Cursor outside file area>>");
		      strcat(line, temp);
		  }
		  pfileDN[i] = 0;
	      }
	      else
	      {
		  if (print)
		  {
		      sprintf(temp, "%5d %5d  ", fileLine, fileSamp);
		      strcat(line, temp);
		  }
		  switch (env->planes[planeList[i]].file->format)
		  {
		    case ByteFormat:
			if (print)
			{
			    sprintf(temp, "%3d", *(unsigned char *)bigdn);
			    strcat(line, temp);
			}
			pfileDN[i] = (TAEFLOAT) *(unsigned char *)bigdn;
			break;
		    case HalfFormat:
			if (print)
			{
			    sprintf(temp, "%6d", *(short int *)bigdn);
			    strcat(line, temp);
			}
			pfileDN[i] = (TAEFLOAT) *(short int *)bigdn;
			break;
		    case FullFormat:
			if (print)
			{
			    sprintf(temp, "%12d", *(int *)bigdn);
			    strcat(line, temp);
			}
			pfileDN[i] = (TAEFLOAT) *(int *)bigdn;
			break;
		    case RealFormat:
			if (print)
			{
			    sprintf(temp, "%12f", *(float *)bigdn);
			    strcat(line, temp);
			}
			pfileDN[i] = (TAEFLOAT) *(float *)bigdn;
			break;
		    case DoubFormat:
			if (print)
			{
			    sprintf(temp, "%24f", *(double *)bigdn);
			    strcat(line, temp);
			}
			pfileDN[i] = (TAEFLOAT) *(double *)bigdn;
			break;
		    case CompFormat:
			if (print)
			{
			    sprintf(temp, "%12f %12f", *(float *)bigdn,
						       *(((float *)bigdn)+1));
			    strcat(line, temp);
			}
			pfileDN[i] = (TAEFLOAT) *(float *)bigdn;
			pfileiDN[i] = (TAEFLOAT) *(((float *)bigdn)+1);
			break;
		  }
	      }
	  }
      }
      else
	  pfileSamp[i] = pfileLine[i] = pfileDN[i] = pfileiDN[i] = 0;

      if (print)
	  NotifyUser(Inform,"", line);
  }

/* Send information back to the TAE caller */

  status = q_intg(oparb, "PIMPSAMP", nPlanes, pIMPSamp, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending IMPSAMP parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PIMPLINE", nPlanes, pIMPLine, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending IMPLINE parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PIMPDN", nPlanes, pIMPDN, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending IMPDN parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PFSAMP", nPlanes, pfileSamp, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending FILESAMP parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PFLINE", nPlanes, pfileLine, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending FILELINE parameter back to TAE", "VIDS-TAEERR");
  status = q_real(oparb, "PFDN", nPlanes, pfileDN, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending FILEDN parameter back to TAE", "VIDS-TAEERR");
  status = q_real(oparb, "PFIDN", nPlanes, pfileiDN, P_ADD);
  if (status != SUCCESS)
     ABORT(status, "Error sending FILEIDN parameter back to TAE",
		 "VIDS-TAEERR");

  return SUCCESS;
}

/******************************************************************************/
/* Gets the DN value at a given cursor location.  Location is given relative
 * to the image plane.  Returns either SUCCESS or vrdi status.
 */
int ReadPixDisplay(env, plane_num, samp, line, dn)
  VIDSEnvironment *env;		/* the current display device environ */
  int plane_num;		/* image plane to work on		*/
  int samp, line;		/* cursor position			*/
  unsigned char *dn;		/* DN value under cursor		*/
{
  PlaneInfo *plane;		/* Pointer to current plane info struct	*/
  int status;

  plane = &env->planes[plane_num];

  status = zdiawset(env->devUnit, plane_num, 1, 1, env->nsMax, env->nlMax);
  status = zdipixelread(env->devUnit, plane_num, samp, line, dn);
  if (status != SUCCESS)
      ABORT(status, "Sorry, couldn't read the pixel from the device",
		  "VIDS-VRDIERR");
                  
  return SUCCESS;
}


/********************************************************************************/
/* Gets the DN value from the file at a given location.  Location is given
 * relative to the file.  Returns either SUCCESS, or CLIPPED if the coordinates
 * are outside the file.
 */
int ReadPixFile(env, planenum, samp, line, dn)
  VIDSEnvironment *env;		/* the current display device environ */
  int planenum;			/* image plane to work on		*/
  int samp, line;		/* cursor position relative to file	*/
  int *dn;			/* DN value under cursor		*/
{
  FileInfo *file;		/* FileInfo structure for this file	*/
  PlaneInfo *plane;		/* PlaneInfo structure for this plane	*/
  unsigned char *addr;		/* Address of pixel if array I/O	*/
  int status;
  int unit;			/* VICAR unit number			*/
  unsigned char *PixelAddress();

  plane = &env->planes[planenum];
  file = plane->file;			/* get FileInfo structure */
  if (file == NULL)
      return CLIPPED;		/* should be checked for by caller!! */

/* Check the file window */

  if (samp < plane->imageWindow.ss ||
      line < plane->imageWindow.sl)
	return CLIPPED;

  if (samp > (plane->imageWindow.ns + plane->imageWindow.ss - 1) ||
      line > (plane->imageWindow.nl + plane->imageWindow.sl - 1))
	return CLIPPED;

  if (file->isOpen && file->addr != NULL)
  {
      addr = PixelAddress(file, plane->band, &line, &samp);
      BlockMove(addr, dn, PixelSize(file->format));
  }
  else
  {
      if (!file->isOpen)
      {
	  unit = GetUnit(file->filename);
	  status = zvopen(unit, "op", "read", "open_act", "s", "io_act", "s", 0);
	  if (status != SUCCESS)
	      ABORT(status, "Sorry, couldn't open the file", "VIDS-EXECERR");
      }
      status = zvread(unit, dn, "nsamps", 1,
	     "band", plane->band, "line", line, "samp", samp, 0);
      if (status != SUCCESS)
      {
	  if (!file->isOpen)
	      zvclose(unit, "clos_act", "free", 0);
	  ABORT(status, "Sorry, couldn't read the pixel from the file",
		      "VIDS-EXECERR");
      }
      if (!file->isOpen)
      {
	  status = zvclose(unit, "clos_act", "free", 0);
	  if (status != SUCCESS)
	      ABORT(status, "Sorry, couldn't close the file", "VIDS-EXECERR");
      }
  }

  return SUCCESS;
}

