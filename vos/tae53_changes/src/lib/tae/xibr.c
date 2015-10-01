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



/*
 *	UNIX bridge routines for FORTRAN  callable XI (iage file) package.
 *
 *	NOTE: The FORTRAN callable XI package is completely rewritten
 *	in 'C' under TAE v1.3 and the old (TAE V1.2 and earlier) XI package
 *	which was written in FORTRAN  is now obsolete.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xiclse) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*disp		/* in:  disposition "xdel" or "xsave"	*/

 );
FUNCTION VOID BRIDGE2_NAME(xiherr) 
(
    TAEINT		*block,		/* in:  image file control block	*/
    TAEINT		*hcode		/* out: host error code			*/

 );
FUNCTION VOID BRIDGE2_NAME(xiopin) 
(
    TAEINT		*block,		/* out: image file control block	*/
    TAEINT		*lun,		/* in: logical unit no. for file access	*/
    FORSTR		*hosnam,	/* in: name of file to open		*/
    TAEINT		*type,		/* in: access: "xinput" or "xinout"	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 );
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

 );
FUNCTION VOID BRIDGE2_NAME(xirdlb) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* out: receive data buffer		*/
    TAEINT		*labnum,	/* in:  number of label record to read	*/
    TAEINT		*status	/* out: "xsucc", "xnolab", or "xfail"	*/

 );
FUNCTION VOID BRIDGE2_NAME(xiread) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* out: receive data buffer		*/
    TAEINT		*chan,		/* in:  chan of 1st line to read	*/
    TAEINT		*line,		/* in:  line of 1st line to read	*/
    TAEINT		*lines,		/* in:  number of lines to read		*/
    TAEINT		*status	/* out: "xsucc", etc.			*/

 );
FUNCTION VOID BRIDGE2_NAME(xiwait) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 );
FUNCTION VOID BRIDGE2_NAME(xiwrit) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* in:  data buffer			*/
    TAEINT		*chan,		/* in:  chan of 1st line to write	*/
    TAEINT		*line,		/* in:  line of 1st line to write	*/
    TAEINT		*lines,		/* in:  number of lines to write	*/
    TAEINT		*status	/* out: "xsucc", etc.			*/

 );
FUNCTION VOID BRIDGE2_NAME(xiwrlb) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* in:  data buffer			*/
    TAEINT		*labnum,	/* in:  number of label record to write*/
    TAEINT		*status	/* out: "xsucc", "xnolab", or "xfail"	*/

 );
FUNCTION VOID BRIDGE2_NAME(xixtnd) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*channels,	/* in:  no. channels to add to file	*/
    TAEINT		*lines,		/* in:  no. lines to add to file	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 );



/*
 *	xiclse.
 */

FUNCTION VOID BRIDGE1_NAME(xiclse) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*disp		/* in:  disposition "xdel" or "xsave"	*/

 )
    {
    BRIDGE2_NAME(xiclse) (block, disp);
    return;
    }

/*
 *	xiherr.
 */

FUNCTION VOID BRIDGE1_NAME(xiherr) 
(
    TAEINT		*block,		/* in:  image file control block	*/
    TAEINT		*hcode		/* out: host error code			*/

 )
    {
    BRIDGE2_NAME(xiherr) (block, hcode);
    return;
    }

/*
 *	xiopin.
 */

FUNCTION VOID BRIDGE1_NAME(xiopin) 
(
    TAEINT		*block,		/* out: image file control block	*/
    TAEINT		*lun,		/* in: logical unit no. for file access	*/
    TEXT		*hosnam,	/* in: name of file to open		*/
    TAEINT		*type,		/* in: access: "xinput" or "xinout"	*/
    TAEINT		*status,	/* out: "xsucc" or "xfail"		*/
    STRLEN		hosnaml	

 )
    {

    FORSTR		hosnamed;	

    hosnamed.length = GETLEN (hosnaml);
    hosnamed.pointer = hosnam;

    BRIDGE2_NAME(xiopin) (block, lun, &hosnamed, type, status);
    return;
    }

/*
 *	xiopou.
 */

FUNCTION VOID BRIDGE1_NAME(xiopou) 
(
    TAEINT		*block,		/* out: image file control block	*/
    TAEINT		*lun,		/* in: logical unit no. for file access	*/
    TEXT		*hosnam,	/* in: name of file to open		*/
    TAEINT		*org,		/* in: file organiz.: "xci" or "xcs"	*/
    TAEINT		*chans,		/* in: number of channels		*/
    TAEINT		*lines,		/* in: number of lines per channel	*/
    TAEINT		*linsiz,	/* in: bytes per line			*/
    TAEINT		*labels,	/* in: number of label records		*/
    TAEINT		*labsiz,	/* in: number of bytes per label record*/
    TAEINT		*status,	/* out: "xsucc" or "xfail"		*/
    STRLEN		hosnaml	

 )
    {
    FORSTR		hosnamed;	

    hosnamed.length = GETLEN (hosnaml);
    hosnamed.pointer = hosnam;

    BRIDGE2_NAME(xiopou) (block, lun, &hosnamed, org, chans, lines,
				linsiz, labels, labsiz, status);
    return;
    }

/*
 *	xirdlb.
 */

FUNCTION VOID BRIDGE1_NAME(xirdlb) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* out: receive data buffer		*/
    TAEINT		*labnum,	/* in:  number of label record to read	*/
    TAEINT		*status	/* out: "xsucc", "xnolab", or "xfail"	*/

 )
    {
    BRIDGE2_NAME(xirdlb) (block, buffer, labnum, status);
    return;
    }

/*
 *	xiread.
 */

FUNCTION VOID BRIDGE1_NAME(xiread) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* out: receive data buffer		*/
    TAEINT		*chan,		/* in:  chan of 1st line to read	*/
    TAEINT		*line,		/* in:  line of 1st line to read	*/
    TAEINT		*lines,		/* in:  number of lines to read		*/
    TAEINT		*status	/* out: "xsucc", etc.			*/

 )
    {
    BRIDGE2_NAME(xiread) (block, buffer, chan, line, lines, status);
    return;
    }

/*
 *	xiwait.
 */

FUNCTION VOID BRIDGE1_NAME(xiwait) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

 )
    {
    BRIDGE2_NAME(xiwait) (block, status);
    return;
    }

/*
 *	xiwrit.
 */

FUNCTION VOID BRIDGE1_NAME(xiwrit) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* in:  data buffer			*/
    TAEINT		*chan,		/* in:  chan of 1st line to write	*/
    TAEINT		*line,		/* in:  line of 1st line to write	*/
    TAEINT		*lines,		/* in:  number of lines to write	*/
    TAEINT		*status	/* out: "xsucc", etc.			*/

 )
    {
    BRIDGE2_NAME(xiwrit) (block, buffer, chan, line, lines, status);
    return;
    }

/*
 *	xiwrlb.
 */

FUNCTION VOID BRIDGE1_NAME(xiwrlb) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*buffer,	/* in:  data buffer			*/
    TAEINT		*labnum,	/* in:  number of label record to write*/
    TAEINT		*status	/* out: "xsucc", "xnolab", or "xfail"	*/

 )
    {
    BRIDGE2_NAME(xiwrlb) (block, buffer, labnum, status);
    return;
    }

/*
 *	xixtnd.
 */

FUNCTION VOID BRIDGE1_NAME(xixtnd) 
(
    TAEINT		*block,		/* in/out: image file control block	*/
    TAEINT		*channels,	/* in:  no. channels to add to file	*/
    TAEINT		*lines,		/* in:  no. lines to add to file	*/
    TAEINT		*status	/* out: "xsucc" or "xfail"		*/

)
    {
      BRIDGE2_NAME(xixtnd) (block, channels, lines, status);
    return;
    }
