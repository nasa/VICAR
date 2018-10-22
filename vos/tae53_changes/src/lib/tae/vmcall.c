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
 *	Vm_Call.  Application initialization.    This module
 *	plays the same role as XZCALL.FOR does for FORTRAN
 *	application programs and z_call for static pool parblk.
 *
 *	Vm_Call is called from Vm_Stdout after the initial V-block
 *	has been received from TM.  This module is typically
 *	edited by an installation.
 */

/* 
 * CHANGE LOG
 *
 * 22-jul-92	PR1519: Label for TAE CLASSIC..kbs
 */

#include	"taeconf.inp"
#include	"vminc.inc"
#include "taeintproto.h"


    CLASSIC_FUNCTION VOID Vm_Call (

    GENPTR    h                /* parblk data handle */
    )
    {
    return;
    }
