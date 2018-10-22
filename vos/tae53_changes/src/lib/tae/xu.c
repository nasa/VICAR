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



/* TPEB CHECKOUT FILE_TIME=13-JUL-1983 16:01 DUA0:[TAEV1.OLB]XT.C;6 */
/*
 *  This module provides bridges between the FORTRAN-77
 *  XT routines (i.e., TAE terminal I/O 1st level bridge routines)
 *  and the corresponding C-implemented routines in the TERMINAL.C module.
 *
 *  Note that there are no bridges for t_attn and t_wait because these
 *  facilities are not available to applications programs.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	30-sep-83	Changed from XT to XU...peb
 *	30-SEP-83	Change getterm() to xugett()...peb
 *	18-jul-85	Declare name xugett() lower case for portability...dm
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "pgminc.inc"		/* C definitions of PGMINC.FIN		*/
#include "terminc.inc"
#include "eventinc.inp"		/* EVENT package			*/
#include "tmhost.inp"		/* to ger TERMINAL definition		*/
#include "taeintproto.h"

    GLOBAL COUNT	v10UBR = 0;		/* source version	*/


/* globals								*/
        TEXT	loc_string[STRINGSIZ+1];  /* local string 		*/


/*
 *	XUINIT		Bridge to t_init
 */
FUNCTION VOID BRIDGE2_NAME(xuinit) 
(
    TAEINT	*type,		/* out: type of terminal		*/
    TAEINT	*lines,		/* out: number of lines on terminal	*/
    TAEINT	*columns	/* out: number of columns 		*/

 )
    {
    COUNT	l, c;
    CODE	t;

    t_init(&l, &c, &t);		/* get from T package in locals		*/
    *type = t;			/* move to caller format		*/
    *lines = l;
    *columns = c;		
    return;
    }

/*
 *	XUCLR		Bridge to t_clear
 */
    FUNCTION VOID BRIDGE2_NAME(xuclr) (void)

    {
    t_clear();
    return;
    }

/*
 *	XUREAD		Bridge to t_read
 */
FUNCTION VOID BRIDGE2_NAME(xuread) 
(
    FORSTR	*string,	/* out: line read from terminal		*/
    TAEINT	*length,	/* out: length of line read		*/
    TAEINT	*term		/* out: terminator used			*/

 )
    {
    CODE	t;		/* local terminator type		*/

    t_read(loc_string, &t);	/* read into local string		*/
    *length = s_length(loc_string);
    if (s_c2for(loc_string, string, 0) == SUCCESS)
	*term = t;
    else
        *term = P_OVER;		/* FORTRAN string not large enough	*/
    return;
    }

/*
 *	XUWRIT		Bridge to t_write
 */
FUNCTION VOID BRIDGE2_NAME(xuwrit) 
(
    FORSTR	*string,	/* in: string to write			*/
    TAEINT	*cc		/* in: carriage control			*/
   
 )
    {
    s_for2c(string, loc_string, 0);	/* convert to C string		*/
    t_write(loc_string, *cc);
    return;
    }

/*
 *	XUBELL		Bridge for t_bell
 */
    FUNCTION VOID BRIDGE2_NAME(xubell) (void)

    {
    t_bell();
    return;
    }

/*
 *	XUOUT		Bridge for t_output
 */
FUNCTION VOID BRIDGE2_NAME(xuout) 
(
    TAEINT	*line,		/* in: line number 			*/
    TAEINT	*column,	/* in: column number			*/
    FORSTR	*string	/* out: string read from terminal	*/

 )
    {
    s_for2c(string, loc_string, 0);	/* convert to C string		*/
    t_output(*line, *column, loc_string);
    return;
    }

/*
 *	XUIN		Bridge for t_input
 */
FUNCTION VOID BRIDGE2_NAME(xuin) 
(
    TAEINT	*line,		/* in: line number where to write	*/
    TAEINT	*column,	/* in: column number 			*/
    FORSTR	*string,	/* out: FOR-77 string receiving line	*/
    TAEINT	*length,	/* out: length of FOR-77 string		*/
    TAEINT	*term		/* out: terminator			*/

 )
    {
    CODE	t;		/* local terminator code		*/

    t_input(*line, *column, loc_string, &t);	/* read into local	*/
    *length = s_length(loc_string);
    if (s_c2for(loc_string, string, 0)  ==  SUCCESS)
        *term = t;
    else
        *term = P_OVER;		/* FOR-77 string not dimensioned large	*/    
    return;
    }

/*
 *	XUPOS		Bridge for t_pos
 */
FUNCTION VOID BRIDGE2_NAME(xupos) 
(
    TAEINT	*line,		/* in: line number			*/
    TAEINT	*column	/* in: column number			*/
 )
    {
    t_pos(*line, *column);
    return;
    }

/*
 *	XUGETT.		Get name assigned to the terminal output.
 *			Note: This routine is called from library fortran 
 *			module, not directly from an application program.
 */

FUNCTION  VOID  BRIDGE2_NAME(xugett) 
(
    FORSTR	*termname		/* out: terminal output name string */
   
 )
    {
    s_c2for(TERMINAL, termname, 0);	/* convert from c to for-77 string */
    return;  
    }
