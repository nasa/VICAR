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
 *	 Miscelleneous functions needed to build tm.	
 *
 *	CHANGE 	LOG:
 *
 *	16-aug-85	Add c_maxsub...dm
 *	02-dec-88	output_str...nci
 */
#include	<stdio.h>
#include	"stdh.inp"
#include	"taeconf.inp"
#include "taeintproto.h"


   FUNCTION   CODE  c_init(void)

    {
    return (SUCCESS);
    }


   FUNCTION   VOID c_maxsub(void)

    {
    return;
    }

/*	
 * 	Convert  c logical to tae logical .
 *
 */

FUNCTION  VOID c_cvtlog
(
 FUNINT	b,
 TAELOG	*tf
)
    {
    *tf = b ? 1 : 0;
    return;
    }    


/*
 *  output_str - use C fprintf to output a string to the file pointer.
 *
 *  Historical NOTE: Required for creation of ada bindings.
 */

FUNCTION void output_str
(
 FILE *fp,			/* in: file for output (stdout) */
 TEXT *str			/* in: string to output		*/	
 )
    {
    fprintf (fp,"%s\n",str);
    }
