/************************************************************************/
/*	IBISFIL contains a set of four subroutines that perform		*/
/*	  I/O on IBIS-2 (tablular) files.				*/
/*									*/
/* Converted To IBIS-2 compatibility Jun 1995 by Niles Ritter		*/
/*      Added ibis_closefil to clean up IBIS descriptors		*/
/*      Writes out IBIS-2 format tabular files, Backward-compatible	*/
/*									*/
/* Converted from FORTRAN Nov. 1988 by Bob Deen				*/
/* Slight modifications have been made for VIDS:			*/
/*	The error action is set to "S", instead of "SA", since VIDS	*/
/*	   can't abort							*/
/*	The status is returned by the function instead of as an argument*/
/*	The filename parameter is assumed to be a C string, not FORTRAN	*/
/*	Only a subset of the IBIS routines are included			*/
/*	Removed the close, open update from wrfilu since it is no	*/
/*	   longer needed with the VICAR RTL (since zvread isn't called)	*/
/*	Used the TABULAR file type for output files.			*/
/*									*/
/************************************************************************/

#include "VIDSdefs.h"

/************************************************************************
 * ibis_rdfilu opens an existing file with an explicit file name
 * as a read-only file, and returns IBIS descriptor.
 */
int ibis_rdfilu(unit, file, clen, ncol)
  int		*unit;		/* output: unit number of file		*/
  char		*file;		/* input: name of input file		*/
  int		*clen;		/* output: length of the columns	*/
  int		*ncol;		/* output: number of columns		*/
{
  int vicunit,status;

  status = zvunit(&vicunit, "NONE", 1, "u_name", file, 0);
  if (status != SUCCESS)
    return status;

  status = IBISFileOpen( vicunit, unit, "READ", 0,0,0,0 );
  if (status != SUCCESS)
  {
    	NotifyUser(Inform, "", "Failed opening IBIS file");
  	return FAIL;
  }
  IBISFileGet( *unit, "NR", clen, 1, 0, 0 );
  IBISFileGet( *unit, "NC", ncol, 1, 0, 0 );

  return SUCCESS;
}

/************************************************************************
 * ibis_wrfilu creates and opens a new file with an explicit file name.
 */
int ibis_wrfilu(unit, file, clen, ncol)
  int		*unit;		/* output: unit number of file		*/
  char		*file;		/* input: name of output file		*/
  int		clen;		/* input: length of the columns		*/
  int		ncol;		/* input: number of columns		*/
{
  int vicunit,status;

  status = zvunit(&vicunit, "NONE", 1, "u_name", file, 0);
  if (status != SUCCESS)
    return status;

    /* Write native tables. If VMS is needed, can always convert */

  status = IBISFileOpen( vicunit, unit, "WRITE", ncol,clen,0,0 );
  if (status != SUCCESS)
  {
    	NotifyUser(Inform, "", "Failed creating IBIS file");
  	return FAIL;
  }

  return SUCCESS;
}

/************************************************************************
 * ibis_closefil closes the IBIS descriptor associated with file.
 */
int ibis_closefil(unit)
{
  int status;
  
  status = IBISFileClose(unit,0);
  if (status != SUCCESS) return FAIL;
  return SUCCESS;
}

/************************************************************************
 * ibis_getcol reads a column from a file
 */
int ibis_getcol(unit, icol, clen, data)
  int		unit;		/* input: unit number of file		*/
  int		icol;		/* input: column number to get		*/
  int		clen;		/* input: length of column		*/
  char		*data;		/* buffer for data			*/
{
  int  status;

  status = IBISColumnSet(unit,"U_FORMAT","FULL",icol);
  if (status != SUCCESS)
  	return FAIL;
  status =  IBISColumnRead(unit,data,icol,1,clen);
  if (status != SUCCESS)
  	return FAIL;

  return SUCCESS;
}

/************************************************************************
 * ibis_putcol writes a column to a file
 */
int ibis_putcol(unit, icol, clen, data)
  int		unit;		/* input: unit number of file		*/
  int		icol;		/* input: column number to put		*/
  int		clen;		/* input: length of column		*/
  char		*data;		/* buffer for data			*/
{
  int  status;

  status = IBISColumnSet(unit,"U_FORMAT","FULL",icol);
  if (status != SUCCESS)
  	return FAIL;
  status =  IBISColumnWrite(unit,data,icol,1,clen);
  if (status != SUCCESS)
  	return FAIL;

  return SUCCESS;
}
