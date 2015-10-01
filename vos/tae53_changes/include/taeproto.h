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



/*****************************************************************************
 *
 *              Macro for function prototypes
 *  		to permit use of either a K&R or ANSI C compiler
 *
 *	CHANGE LOG
 *
 *  22-apr-93	Initial...swd
 *  03-jun-93	Combined with Xtae/Proto.h (new macro name, etc.)...kbs
 *  02-jul-93   Specifically setting -D_TAE_PROTO on a compile line caused
 *              the _TAE_PROTO macro to not be defined, so syntax errors
 *              occurred. I changed things so that _TAE_PROTO is not needed
 *              on the compile line. Rather, the _NO_PROTO handles how the
 *              _TAE_PROTO macro gets defined...krw
 *  02-jul-93   CAUTION: The  SCCS version of this file is in $TAEINC...kbs
 *  06-jul-93   Added 'what' string...kbs
 *  08-jul-93   Removed 'what' string: problem with C++ since var not used...kbs
 *
 *****************************************************************************/

/*****************************************************************************
 *
 *	C A U T I O N:  	The  SCCS version of this file is in $TAEINC.
 *
 *****************************************************************************/

        /* Make multiple includes of this file safe. */
#ifndef _TAE_Proto_h
#define _TAE_Proto_h

	/* _NO_PROTO is assumed to be defined in $TAE/config/TAEmake.tmpl */
	/* We might need to add "OR defined XTFUNCPROTO" for C++ */

#ifdef _NO_PROTO
#define _TAE_PROTO(arguments) ()
#ifndef __APPLE__	/* OS X doesn't like #define const */
#define const
#endif
#else
#define _TAE_PROTO(arguments) arguments
#endif

	/* Another approach would be to use __STDC__ a la pbmplus */
	/* NOTE: This is currently ifdef'd out */
#ifdef notdef
#if __STDC__
#define _TAE_PROTO(alist) alist
#else /*__STDC__*/
#define _TAE_PROTO(alist) ()
#define const
#endif /*__STDC__*/
#endif

/* DO NOT ADD ANYTHING AFTER THIS endif */
#endif 	/* _TAE_Proto_h */
