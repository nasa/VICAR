/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TERMINAL - Include file of terminal package definitions.
 *
 *	CHANGE LOG:
 *
 *	14-jan-84	New keyboard tokens for micro-editor...palm
 *	15-jun-84	New T_F1, T_F2, T_F3, T_F4...palm
 */

#ifndef	I_TERMINC		/* prevent double include		*/
#define I_TERMINC	0


#define TERM_BASE	5	/* base for arbritrary constants	*/

#define	T_NULL		5	/* T_ constants for calling terminal pkg*/
#define	T_PROMPT	6
#define	T_STDCC		7
#define	T_DOUBLE	8
#define	T_CR		9
#define	T_ESCAPE	10
#define	T_CRT		11
#define	T_NOTCRT	12
#define	T_NOTTERM	13

/*
 *	keyboard tokens for special keys:
 */

#define		T_SPECIAL	0x100		/* .GE. this means not char */
#define		T_UNKNOWN 	0x100
#define		T_UP		0x101		/* up arrow	*/
#define		T_DOWN    	0x102		/* down arrow	*/
#define		T_RIGHT   	0x103		/* right arrow	*/
#define		T_LEFT    	0x104		/* left arrow	*/
#define		T_2ESC	 	0x106		/* 2 escapes	*/
#define		T_F1		0x107		/* PF1		*/
#define		T_F2		0x108		/* PF2		*/
#define		T_F3		0x109		/* PF3		*/
#define		T_F4		0x10a		/* PF4		*/
#define		T_GOLD    	T_F1		/* GOLD		*/
#endif
