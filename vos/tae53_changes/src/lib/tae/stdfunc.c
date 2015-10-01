/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TDM CHECKOUT FILE_TIME=12-APR-1984 11:22 DUA0:[TAEV1.OLB]STDFUNC.C;4 */
/* 	stdfunc. Functions related to standard output, needed by FORTRAN
 *		 programs.
 *
 *	CHANGE LOG:
 *
 *	22-aug-84	Change function names, update for UNIX...dm
 *	03-apr-88	Apollo conversion: FOR_NAME macro...ljn
 *
 */

#include	"taeconf.inp"
#include	"forstr.inp"
#include "taeintproto.h"


#ifdef UNIX
#define	GETLUN_TAE	FOR_NAME2_(getlun_tae, GETLUN_TAE)
#define	SETLUN	FOR_NAME2(setlun, SETLUN)
#define	GETSTD	FOR_NAME2(getstd, GETSTD)
#define SETSTD	FOR_NAME2(setstd, SETSTD)
#else			/* for VMS 	*/	
#define	GETLUN_TAE	getlun_tae
#define	SETLUN	setlun
#define	GETSTD	getstd
#define SETSTD	setstd
#endif

    static  struct  {
		long   lun;
		long   termstd;
		   } xzsavl = { -1, -1 };

/*
 *	getlun_tae. Get the lun number assigned to standard output
 */

FUNCTION  VOID  GETLUN_TAE
(
 int		*lun		/* in: lun number		*/	
)    
    {
    *lun = xzsavl.lun; 		/* -1 means not assigned	*/
    return;
    }

FUNCTION  VOID  getlun_tae
(
 int		*lun		/* in: lun number		*/	
)    
    {
    *lun = xzsavl.lun; 		/* -1 means not assigned	*/
    return;
    }
    

/*
 *	setlun. Set the lun number assigned to standard output
 */

FUNCTION  VOID  SETLUN
(
 int		*lun		/* out: lun number		*/
) 
    {
    xzsavl.lun = *lun;		/* -1 means not assigned	*/
    }


/*
 *	setstd.	Set standard output type
 *	TRUE means stdout=terminal
 */
 
FUNCTION  VOID SETSTD
(
 int		*term		/* in: TRUE if terminal		*/
)
    {
    xzsavl.termstd = *term;
    }



/*
 *	getstd. Find if terminal is the standard output		*
 */

FUNCTION  VOID GETSTD
(
 int		*term		/* out: TRUE if terminal	*/
 )
    {
    *term = xzsavl.termstd;
    }
