/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/* TPAM CHECKOUT FILE_TIME=28-SEP-1984 16:53 DUA1:[TAEV1.OLB]XI.C;1 */
/*
 *	XI (native) bridge routines.
 *
 *	These are the FORTRAN callable bridge routines to the C callable
 *	"i_" image file package written in September 1984.
 *	Previously the XI routines had been written completely in FORTRAN
 *	with no C callable interface.
 *
 *	CHANGE LOG:
 *	02-may-85	Fix xiwrit, etc., to recognize error code...palm
 *	19-jul-85	Declare all function names in lower case for 
 *			portability...dm
 *	03-apr-88	Apollo conversion: macro for bridge names...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "pgminc.inc"
#include "taeintproto.h"




/*
 *	XICLSE - Close an image file.
 */

FUNCTION VOID BRIDGE2_NAME(xiclse) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*disp		/* in:  disposition "xdel" or "xsave"	*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/

    ifcb = (struct IFCB *) block;
    i_clse(ifcb, *disp);		/* close the file			*/
    return;
    }

/*
 *	XIHERR - Get host-dependent error code.
 */

FUNCTION VOID BRIDGE2_NAME(xiherr) 
(
    TAEINT		*block,		/* in:  image file control block	*/
    TAEINT		*hcode		/* out: host error code			*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/

    ifcb = (struct IFCB *) block;
    *hcode = i_herr(ifcb);		/* get the code				*/
    return;
    }

/*
 *	XIOPIN - Open existing image file.
 */

FUNCTION VOID BRIDGE2_NAME(xiopin) 
(
    TAEINT		*block,		/* out: image file control block	*/
    TAEINT		*lun,		/* in: logical unit no. for file access	*/
    FORSTR		*hosnam,	/* in: name of file to open		*/
    TAEINT		*type,		/* in: access: "xinput" or "xinout"	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/
    CODE		code;
    TEXT		c_name[STRINGSIZ+1];

    ifcb = (struct IFCB *) block;
    code = s_for2c(hosnam, c_name, 0);	/* convert name to C string	*/
    if (code != SUCCESS)
	{
	*status = P_FAIL;
	return;
	}
    s_strip(c_name);			/* remove trailing blanks	*/
    *status = i_opin(ifcb, *lun, c_name, *type);
    return;
    }

/*
 *	XIOPOU - Open image file for output.
 */

FUNCTION VOID BRIDGE2_NAME(xiopou) 
(
    TAEINT		*block,		/* out: image file control block	*/
    TAEINT		*lun,		/* in: logical unit no. for file access	*/
    FORSTR		*hosnam,	/* in: name of file to open		*/
    TAEINT		*org,		/* in: file organiz.: "xci" or "xcs"	*/
    TAEINT		*chans,		/* in: number of channels		*/
    TAEINT		*lines,		/* in: number of lines per channel	*/
    TAEINT		*linsiz,	/* in: bytes per line			*/
    TAEINT		*labels,	/* in: number of label records		*/
    TAEINT		*labsiz,	/* in: number of bytes per label record*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/
    CODE		code;
    TEXT		c_name[STRINGSIZ+1];

    ifcb = (struct IFCB *) block;
    code = s_for2c(hosnam, c_name, 0);	/* convert name to C string	*/
    if (code != SUCCESS)
	{
	*status = P_FAIL;
	return;
	}
    s_strip(c_name);			/* remove trailing blanks	*/
    *status = i_opou(ifcb, *lun, c_name, *org, *chans, *lines,
			*linsiz, *labels, *labsiz);	/* open the file	*/
    return;
    }

/*
 *	XIRDLB - Read image file label record.
 */

FUNCTION VOID BRIDGE2_NAME(xirdlb) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* out: receive data buffer		*/
    TAEINT		*labnum,	/* in:  number of label record to read	*/
    TAEINT		*status	/* out: "xsucc", "xnolab", or "xfail"	*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/
    GENPTR		locptr;

    ifcb = (struct IFCB *) block;
    locptr = (GENPTR) buffer;
    *status = i_rdlb(ifcb, locptr, *labnum);
    return;
    }

/*
 *	XIREAD - Initiate read from image file.
 */

FUNCTION VOID BRIDGE2_NAME(xiread) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* out: receive data buffer		*/
    TAEINT		*chan,		/* in:  chan of 1st line to read	*/
    TAEINT		*line,		/* in:  line of 1st line to read	*/
    TAEINT		*lines,		/* in:  number of lines to read		*/
    TAEINT		*status	/* out: "xsucc", etc.			*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/
    GENPTR		locptr;

    ifcb = (struct IFCB *) block;
    locptr = (GENPTR) buffer;
    *status = i_read(ifcb, locptr, *chan, *line, *lines);
    return;
    }

/*
 *	XIWAIT - Wait on image file I/O completion.
 */

FUNCTION VOID BRIDGE2_NAME(xiwait) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/

    ifcb = (struct IFCB *) block;
    *status = i_wait(ifcb);
    return;
    }

/*
 *	XIWRIT - Initiate write to an image file.
 */

FUNCTION VOID BRIDGE2_NAME(xiwrit) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* in:  data buffer			*/
    TAEINT		*chan,		/* in:  chan of 1st line to write	*/
    TAEINT		*line,		/* in:  line of 1st line to write	*/
    TAEINT		*lines,		/* in:  number of lines to write	*/
    TAEINT		*status	/* out: "xsucc", etc.			*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/
    GENPTR		locptr;

    ifcb = (struct IFCB *) block;
    locptr = (GENPTR) buffer;
    *status = i_write(ifcb, locptr, *chan, *line, *lines);
    return;
    }

/*
 *	XIWRLB - Write a label record to an image file.
 */

FUNCTION VOID BRIDGE2_NAME(xiwrlb) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* in:  data buffer			*/
    TAEINT		*labnum,	/* in:  number of label record to write*/
    TAEINT		*status	/* out: "xsucc", "xnolab", or "xfail"	*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/
    GENPTR		locptr;

    ifcb = (struct IFCB *) block;
    locptr = (GENPTR) buffer;
    *status = i_wrlb(ifcb, locptr, *labnum);
    return;
    }

/*
 *	XIXTND - Allocate new space in existing image file.
 */

FUNCTION VOID BRIDGE2_NAME(xixtnd) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*channels,	/* in:  no. channels to add to file	*/
    TAEINT		*lines,		/* in:  no. lines to add to file	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 )
    {
    struct IFCB		*ifcb;		/* image file control block		*/

    ifcb = (struct IFCB *) block;
    *status = i_xtnd(ifcb, *channels, *lines);
    return;
    }
