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



/* "C" bridge */

/*
 *  This module provides bridges between the FORTRAN-77
 *  XT routines (i.e., TAE terminal I/O 1st level bridge routines)
 *  and the corresponding C-implemented routines in the TERMINAL.C module.
 *
 *  Note that there are no bridges for t_attn and t_wait because these
 *  facilities are not available to applications programs.
 *
 * 	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and bridge routine names...ljn
 *	17-jun-88	Corrected mispell of GETLEN as GETLNE...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xuinit) 
(
    TAEINT	*type,		/* out: type of terminal		*/
    TAEINT	*lines,		/* out: number of lines on terminal	*/
    TAEINT	*columns	/* out: number of columns 		*/

 );
FUNCTION VOID BRIDGE2_NAME(xuclr) (void);
FUNCTION VOID BRIDGE2_NAME(xuread) 
(
    FORSTR	*string,	/* out: line read from terminal		*/
    TAEINT	*length,	/* out: length of line read		*/
    TAEINT	*term		/* out: terminator used			*/

 );
FUNCTION VOID BRIDGE2_NAME(xuwrit) 
(
    FORSTR	*string,	/* in: string to write			*/
    TAEINT	*cc		/* in: carriage control			*/
   
 );
FUNCTION VOID BRIDGE2_NAME(xubell) (void);
FUNCTION VOID BRIDGE2_NAME(xuout) 
(
    TAEINT	*line,		/* in: line number 			*/
    TAEINT	*column,	/* in: column number			*/
    FORSTR	*string	/* out: string read from terminal	*/

 );
FUNCTION VOID BRIDGE2_NAME(xuin) 
(
    TAEINT	*line,		/* in: line number where to write	*/
    TAEINT	*column,	/* in: column number 			*/
    FORSTR	*string,	/* out: FOR-77 string receiving line	*/
    TAEINT	*length,	/* out: length of FOR-77 string		*/
    TAEINT	*term		/* out: terminator			*/

 );
FUNCTION VOID BRIDGE2_NAME(xupos) 
(
    TAEINT	*line,		/* in: line number			*/
    TAEINT	*column	/* in: column number			*/
 );
FUNCTION  VOID  BRIDGE2_NAME(xugett) 
(
    FORSTR	*termname		/* out: terminal output name string */
   
 );


/*
 *	XUINIT		Bridge to t_init
 */
FUNCTION VOID BRIDGE1_NAME(xuinit) 
(
    TAEINT	*type,		/* out: type of terminal		*/
    TAEINT	*lines,		/* out: number of lines on terminal	*/
    TAEINT	*columns	/* out: number of columns 		*/

 )
    {
	BRIDGE2_NAME(xuinit) (type, lines, columns);

    return;
    }

/*
 *	XUCLR		Bridge to t_clear
 */
    FUNCTION VOID BRIDGE1_NAME(xuclr) (void)

    {
	BRIDGE2_NAME(xuclr) ();

    return;
    }

/*
 *	XUREAD		Bridge to t_read
 */
FUNCTION VOID BRIDGE1_NAME(xuread) 
(
    TEXT	*string,	/* out: line read from terminal		*/
    TAEINT	*length,	/* out: length of line read		*/
    TAEINT	*term,		/* out: terminator used			*/
    STRLEN	stringl

 )
    {
	FORSTR	stringd;

	stringd.length = GETLEN (stringl);
	stringd.pointer = string;

	BRIDGE2_NAME(xuread) (&stringd, length, term);

    return;
    }

/*
 *	XUWRIT		Bridge to t_write
 */
FUNCTION VOID BRIDGE1_NAME(xuwrit) 
(
    TEXT	*string,	/* in: string to write			*/
    TAEINT	*cc,		/* in: carriage control			*/
    STRLEN	stringl

 )
    {
	FORSTR	stringd;

	stringd.length = GETLEN (stringl);
	stringd.pointer = string;

	BRIDGE2_NAME(xuwrit) (&stringd, cc);

    return;
    }

/*
 *	XUBELL		Bridge for t_bell
 */
    FUNCTION VOID BRIDGE1_NAME(xubell) (void)

    {
	BRIDGE2_NAME(xubell) ();

    return;
    }

/*
 *	XUOUT		Bridge for t_output
 */
FUNCTION VOID BRIDGE1_NAME(xuout) 
(
    TAEINT	*line,		/* in: line number 			*/
    TAEINT	*column,	/* in: column number			*/
    TEXT	*string,	/* out: string read from terminal	*/
    STRLEN	stringl

 )
    {
	FORSTR	stringd;

	stringd.length = GETLEN (stringl);
	stringd.pointer = string;

	BRIDGE2_NAME(xuout) (line, column, &stringd);
    return;
    }

/*
 *	XUIN		Bridge for t_input
 */
FUNCTION VOID BRIDGE1_NAME(xuin) 
(
    TAEINT	*line,		/* in: line number where to write	*/
    TAEINT	*column,	/* in: column number 			*/
    TEXT	*string,	/* out: FOR-77 string receiving line	*/
    TAEINT	*length,	/* out: length of FOR-77 string		*/
    TAEINT	*term,		/* out: terminator			*/
    STRLEN	stringl

 )
    {
	FORSTR	stringd;

	stringd.length = GETLEN (stringl);
	stringd.pointer = string;

	BRIDGE2_NAME(xuin) (line, column, &stringd, length, term);

    return;
    }

/*
 *	XUPOS		Bridge for t_pos
 */
FUNCTION VOID BRIDGE1_NAME(xupos) 
(
    TAEINT	*line,		/* in: line number			*/
    TAEINT	*column	/* in: column number			*/
 )
    {
	BRIDGE2_NAME(xupos) (line, column);

    return;
    }

/*
 *	XUGETT.		Get name assigned to the terminal output.
 *			Note: This routine is called from library fortran
 *			module, not directly from an application program.
 */

FUNCTION  VOID  BRIDGE1_NAME(xugett) 
(
    TEXT	*termname,		/* out: terminal output name string */
    STRLEN	termnamel

 )
    {
	FORSTR	termnamed;

	termnamed.length = GETLEN (termnamel);
	termnamed.pointer = termname;

	BRIDGE2_NAME(xugett) (&termnamed);

    return;
    }
