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



/* This file contains declarations of tutor globals.
 * Other tutor source files just reference these globals.
 *
 * CHANGE LOG:
 *
 *
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include "taeintproto.h"



    GLOBAL struct TUTCTX tutctx = { 0 }; /* tutor & micro-editor context	*/


    GLOBAL TEXT qual_notice[] = "|THIS PARAMETER HAS QUALIFIERS|";

