/************************************************************************/
/*	IBISGR contains a set of seven subroutines that perform		*/
/*	  I/O on IBIS graphics 1 files.					*/
/*									*/
/*	Revision history:						*/
/*									*/
/*	New:		February 1986	KFE				*/
/*									*/
/*	2		April 1986	KFE				*/
/*		Extended format to handle any dimension between 1 and 40*/
/*		    instead of only 2 or 3.				*/
/*									*/
/*	3		June 1987	MKT				*/
/*		Added subroutine SETGR.					*/
/*									*/
/*	4		June 1988	DFS				*/
/*		Converted to C and modified for VIDS.			*/
/*									*/
/*	5		June 1995	NDR				*/
/*		Front-end to P2SUB/P1SUB IBISGR calls.			*/
/*									*/
/*									*/
#include "VIDSdefs.h"


/************************************************************************/
/*		RDGR opens an IBIS graphics 1 file for reading		*/
/*		    and prepares the common block			*/
/*									*/
int ibis_rdgr (name, num, dimension)
  char		*name;		/* input: name of input file		*/
  int		num;		/* input: which IBIS graphics file	*/
  int		dimension;	/* input: number of coords per set	*/
{
  int		unit, status;

/*		open input graphics file				*/
  status = zvunit (&unit, "IBIS", num, "U_NAME", name, 0);
  if (status != SUCCESS) return status;
  status = zrdgr_unit(unit,num+1,dimension);

  return status==SUCCESS ? SUCCESS : FAIL;
}

/************************************************************************/
int ibis_wrgr (name, num, dimension)
/*		WRGR creates an IBIS graphics 1 file for writing	*/
/*		    and prepares the common data			*/
/*									*/
/*	    Parameters:							*/
  char	*name;		/* input: name of output file			*/
  int	num;		/* input: which IBIS graphics file		*/
  int	dimension;	/* input: number of coords per set		*/
{
  int	unit,status;

/*		create output graphics file				*/
  status = zvunit (&unit, "IBIS", num, "U_NAME", name, 0);
  if (status != SUCCESS) return status;
  status = zwrgr_unit(unit,num+1,dimension);

  return status==SUCCESS ? SUCCESS : FAIL;
}


/************************************************************************/
int ibis_getgr ( num, zero, eof, first_c, second_c, third_c)
/*		GETGR gets a coordinate set from a graphics 1 file	*/
/*									*/
/*	    Parameters:							*/
  int		num;		/* input: which IBIS graphics file	*/
  Boolean	*zero;		/* output: true if all coords are zero	*/
  Boolean	*eof;		/* output: true if at the end of file	*/
  float		*first_c;	/* output: the first coordinate got	*/
  float		*second_c;	/* output: the second coordinate got	*/
  float		*third_c;	/* output: the rest of the coordinates	*/
{
  int	status = zgetgr(num+1,zero,eof,first_c,second_c,third_c);
  return status==SUCCESS ? SUCCESS : FAIL;
}


/************************************************************************/
int ibis_nextgr (num, eof, first_c, second_c, third_c)
/*		NEXTGR reads coordinate sets from the graphics 1 file	*/
/*		    until coming to a non zero				*/
/*									*/
/*	    Parameters:							*/
  int		num;		/* input: which IBIS graphics file	*/
  Boolean	*eof;		/* output: true if at the end of file	*/
  float		*first_c;	/* output: the first coordinate to get	*/
  float		*second_c;	/* output: the second coordinate to get	*/
  float		*third_c;	/* output: the rest of the coordinates	*/
{
  int status = znextgr(num+1,eof,first_c,second_c,third_c);
  return status==SUCCESS ? SUCCESS : FAIL;
}


/************************************************************************/
ibis_putgr (num, first_c, second_c, third_c)
/*		PUTGR puts a coordinate set out in the 			*/
/*			graphics 1 file					*/
/*									*/
/*	    Parameters:							*/
  int	num;		/* input: which IBIS graphics file		*/
  float	first_c;	/* input: the first coordinate to be put	*/
  float	second_c;	/* input: the second coordinate to be put	*/
  float	*third_c;	/* output: the rest of the coordinates		*/
{
  int	status=zputgr(num+1,first_c,second_c,third_c);
  return status==SUCCESS ? SUCCESS : FAIL;
}


/************************************************************************/
int ibis_setgr (graphics_num, coord_set)
  int	graphics_num, coord_set;
/*									*/
/* SETGR is used to specify the next coordinate set to be read by GETGR.*/
/*   see the standar IBISGR.COM for description				*/
/*									*/
{
  int status = zsetgr(graphics_num+1, coord_set);
  return status==SUCCESS ? SUCCESS : FAIL;
}


/************************************************************************/
int ibis_clgr (num)
/*		CLGR closes the graphics 1 file				*/
/*		    if the file is an output file then it flushes 	*/
/*		    the buffer and updates the VICAR label		*/
/*									*/
/*	    Parameters:							*/
  int	num;		/* input: which IBIS graphics file		*/
{
  int	status = zclgr(num+1);
  return status==SUCCESS ? SUCCESS : FAIL;
}
