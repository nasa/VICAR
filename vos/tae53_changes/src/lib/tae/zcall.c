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
 *	z_call.  Application initialization.    This module
 *	plays the same role as XZCALL.FOR does for FORTRAN
 *	application programs.    See XZCALL.FOR for a
 *	description.
 *
 *	z_call is called from z_init after the initial V-block
 *	has been received from TM.  This module is typically
 *	edited by an installation.
 */

#include	"taeconf.inp"
#include	"parblk.inc"
#include "syninc.inc"
#include "taeintproto.h"


FUNCTION VOID z_call
(
    struct PARBLK	*parblk	/* in: initial V-block   */

 )
    {
    return;
    }
