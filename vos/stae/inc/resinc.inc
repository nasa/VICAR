/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 *   Macros and definitions for the restricted allocation package.
 */

#ifndef	I_RESINC		/* prevent double include	*/
#define I_RESINC	0	

/*
 *   ALIGNS macro.  Given bytes, returns the number of ALIGN elements needed
 *   to hold that many bytes.
 */

/* K&R -- Integer arith involving a neg operand is machine dependent...ljn */

#define ALIGNS(bytes)  ((bytes == 0)? 0 : 1+(bytes-1)/sizeof(ALIGN))

/*
 *  STORAGE macro.  Defines storage with name "s" for use by the package.  Note
 *  that storage type may precede the macro, e.g., static STORAGE(s, 1000); 
 */

#define STORAGE(s, bytes)  ALIGN s[ALIGNS(bytes)]

/* 
 *  SET_REL macro.  Pointer p is made relative to storage s so that in another
 *  address space, pointer p can be made absolute again.
 */

#define SET_REL(p, s)  r_setrel(s, (GENPTR)&p, (GENPTR)p)

/* 
 *  SET_ABS macro.  Pointer p in storage s is made absolute.   You must supply
 *  the cast to be used for pointer p.
 */

#define SET_ABS(p, s, cast)  p=(cast)r_setabs(s, (GENPTR)&p)

#endif
