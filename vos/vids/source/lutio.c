/* LUTIO.c contains utilities to handle reading and writing of lookup
 * tables and pseudocolor tables to and from files.
 * 
 *  Modified June 1995 by Niles Ritter:
 *     Now uses portable IBIS-2 versions of IBIS file I/O 
 */
#include "VIDSdefs.h"

/************************************************************************/
/* WriteStretch writes the stretch LUT for a number of planes to the
 * given file.
 */
int WriteStretch(env, planes, nplanes, file)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			planes[];	/* list of planes to write	*/
  int			nplanes;	/* number of planes to write	*/
  char			*file;		/* filename to use		*/
{
  int unit, i, status;
  int *CurrentLut();

  status = ibis_wrfilu(&unit, file, 256, nplanes);
  if (status != SUCCESS)
    ABORT(FAIL, "Error opening output file", "VIDS-IBISERR");

  for (i=0; i<nplanes; i++)
  {
    status = ibis_putcol(unit, i+1, 256, CurrentLut(env, planes[i]));
    if (status != SUCCESS)
    {
      status = ibis_closefil(unit);
      ABORT(FAIL, "Error writing to output file", "VIDS-IBISERR");
    }
  }
  status = ibis_closefil(unit);
  if (status != SUCCESS)
    ABORT(FAIL, "Error closing output file", "VIDS-IBISERR");

  return SUCCESS;
}

/************************************************************************/
/* ReadStretch reads the stretch LUT for a number of planes from the
 * given file in the given columns.
 */
int ReadStretch(env, planes, columns, nplanes, file)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			planes[];	/* list of planes to write	*/
  int			columns[];	/* column to use for each plane	*/
  int			nplanes;	/* number of planes to write	*/
  char			*file;		/* filename to use		*/
{
  int unit, i, status;
  int *lut;
  int collen, ncols;
  char text[STRINGSIZ+1];
  int *NewLut();

  status = ibis_rdfilu(&unit, file, &collen, &ncols);
  if (status != SUCCESS)
    ABORT(FAIL, "Error opening input file", "VIDS-IBISERR");

  for (i=0; i<nplanes; i++)
  {
    NotifyUser(Verbose, "", "Loading stretch for plane %d from column %d",
		planes[i], columns[i]);
    if (columns[i] > ncols || columns[i] <= 0)
    {
      SendLuts(env);
      status = ibis_closefil(unit);
      sprintf(text, "Column %d is invalid.  The file has only %d columns.",
		columns[i], ncols);
      ABORT(FAIL, text, "VIDS-BADCOL");
    }
    lut = NewLut(env, planes[i]);
    if (lut == NULL)
    {
      SendLuts(env);
      status = ibis_closefil(unit);
      return FAIL;
    }
    status = ibis_getcol(unit, columns[i], MIN(256,collen), lut);
    if (status != SUCCESS)
    {
      SendLuts(env);
      status = ibis_closefil(unit);
      ABORT(FAIL, "Error reading from input file", "VIDS-IBISERR");
    }
  }
  status = ibis_closefil(unit);
  if (status != SUCCESS)
    ABORT(FAIL, "Error closing input file", "VIDS-IBISERR");

  SendLuts(env);

  return SUCCESS;
}

/************************************************************************/
/* WritePseudo writes the pseudocolor table for a single plane to the
 * given file.  The pseudocolor table is passed through the given LUT first.
 */
int WritePseudo(env, plane, file, lut)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			plane;		/* the plane to write		*/
  char			*file;		/* filename to use		*/
  int			lut[];		/* LUT to use before pstable	*/
{
  int unit, j, status;
  int temp_lut[256];
  PSTable *pstbl;
  PSTable *CurrentPSTable();

  status = ibis_wrfilu(&unit, file, 256, 3);
  if (status != SUCCESS)
    ABORT(FAIL, "Error opening output file", "VIDS-IBISERR");

  pstbl = CurrentPSTable(env, plane);
  for (j=0; j<256; j++)			/* rearrange LUT for red */
    temp_lut[j] = pstbl->red[lut[j]];
  status = ibis_putcol(unit, 1, 256, temp_lut);
  if (status == SUCCESS)
  {
    for (j=0; j<256; j++)		/* rearrange LUT for green */
      temp_lut[j] = pstbl->green[lut[j]];
    status = ibis_putcol(unit, 2, 256, temp_lut);
  }
  if (status == SUCCESS)
  {
    for (j=0; j<256; j++)		/* rearrange LUT for blue */
      temp_lut[j] = pstbl->blue[lut[j]];
    status = ibis_putcol(unit, 3, 256, temp_lut);
  }

  if (status != SUCCESS)
  {
    status = ibis_closefil(unit);
    ABORT(FAIL, "Error writing to output file", "VIDS-IBISERR");
  }

  status = ibis_closefil(unit);
  if (status != SUCCESS)
    ABORT(FAIL, "Error closing output file", "VIDS-IBISERR");

  return SUCCESS;
}

/************************************************************************/
/* ReadPseudo reads the pseudocolor table for a plane from the given
 * file in the given columns.
 */
int ReadPseudo(env, plane, columns, file)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			plane;		/* plane to write		*/
  int			columns[3];	/* column to use for each color	*/
  char			*file;		/* filename to use		*/
{
  int unit, status;
  int collen, ncols;
  char text[STRINGSIZ+1];
  PSTable *pstbl;
  PSTable *NewPSTable();

  pstbl = NewPSTable(env, plane);
  if (pstbl == NULL)
    return FAIL;

  status = ibis_rdfilu(&unit, file, &collen, &ncols);
  if (status != SUCCESS)
    ABORT(FAIL, "Error opening input file", "VIDS-IBISERR");

  if ((columns[0] > ncols || columns[0] <= 0) ||
      (columns[1] > ncols || columns[1] <= 0) ||
      (columns[2] > ncols || columns[2] <= 0))
  {
    status = ibis_closefil(unit);
    sprintf(text, "Columns %d, %d, or %d are invalid.  The file has only %d columns.",
		columns[0], columns[1], columns[2], ncols);
    ABORT(FAIL, text, "VIDS-BADCOL");
  }

  NotifyUser(Verbose, "", "Loading red pseudocolor LUT from column %d",
		columns[0]);
  status = ibis_getcol(unit, columns[0], MIN(256,collen), pstbl->red);
  if (status != SUCCESS)
  {
    SendLuts(env);
    status = ibis_closefil(unit);
    ABORT(FAIL, "Error reading from input file", "VIDS-IBISERR");
  }

  NotifyUser(Verbose, "", "Loading green pseudocolor LUT from column %d",
		columns[1]);
  status = ibis_getcol(unit, columns[1], MIN(256,collen), pstbl->green);
  if (status != SUCCESS)
  {
    SendLuts(env);
    status = ibis_closefil(unit);
    ABORT(FAIL, "Error reading from input file", "VIDS-IBISERR");
  }

  NotifyUser(Verbose, "", "Loading blue pseudocolor LUT from column %d",
		columns[2]);
  status = ibis_getcol(unit, columns[2], MIN(256,collen), pstbl->blue);
  if (status != SUCCESS)
  {
    SendLuts(env);
    status = ibis_closefil(unit);
    ABORT(FAIL, "Error reading from input file", "VIDS-IBISERR");
  }

  status = ibis_closefil(unit);
  if (status != SUCCESS)
    ABORT(FAIL, "Error closing input file", "VIDS-IBISERR");

  SendLuts(env);

  return SUCCESS;
}
