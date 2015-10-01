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

    GLOBAL  V03stdf = 0;		/* source version number */


#ifdef UNIX
#define	GETLUN_TAE	FOR_NAME(getlun_tae)
#define	SETLUN	FOR_NAME(setlun)
#define	GETSTD	FOR_NAME(getstd)
#define SETSTD	FOR_NAME(setstd)
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

    FUNCTION  VOID  GETLUN_TAE(lun)

    long	*lun;		/* in: lun number		*/	
    
    {
    *lun = xzsavl.lun; 		/* -1 means not assigned	*/
    return;
    }
    

/*
 *	setlun. Set the lun number assigned to standard output
 */

    FUNCTION  VOID  SETLUN(lun)

    long	*lun;		/* out: lun number		*/
 
    {
    xzsavl.lun = *lun;		/* -1 means not assigned	*/
    return;
    }


/*
 *	setstd.	Set standard output type
 *	TRUE means stdout=terminal
 */
 
    FUNCTION  VOID SETSTD(term)

    FUNINT	*term;		/* in: TRUE if terminal		*/

    {
    xzsavl.termstd = *term;
    return;
    }



/*
 *	getstd. Find if terminal is the standard output		*
 */

    FUNCTION  VOID GETSTD(term)

    FUNINT	*term;		/* out: TRUE if terminal	*/

    {
    *term = xzsavl.termstd;
    return;
    }
