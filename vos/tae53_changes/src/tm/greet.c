/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TDM CHECKOUT FILE_TIME= 9-NOV-1984 11:23 DUA0:[TAEV1.TM]GREET.C;2 */
/*
 *	Greeting message generator.
 *
 *
 *	CHANGE LOG:
 *
 *		        PR 884:  new greeting message...palm
 *	09-dec-84	Different greeting message for RCJM monitor...dm
 *	02-jan-85	Change RCJM greeting message...dm
 *
 ***********************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	08-mar-85	Add 'prototype' to RCJM greeting message...dm
 *
 ***********************************************************************
 *	01-dec-86	For merging RCJM into main tree, no spec'l message...nhe
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include "taeintproto.h"

/*	greet - issue TAE logon greeting message.
 *
 *	This is called once (during TM initialization).
 *	The greeting message may be changed with no
 *	impact to TAE.  If you do not have a C compiler,
 *	this may be written in assembly language; a
 *	one-instruction version (with a return) will
 *	suppress the greeting message.
 *
 */

FUNCTION VOID greet 
(
    FAST TEXT	vrsion[]		/* in:  TM version string	*/

 )
    {

    put_stdout ("");
    put_stdout ("                Welcome to VICAR");
    put_stdout("");
    return;
    }
