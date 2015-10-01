/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*TDM         CHECKOUT FILE_TIME=22-JAN-1983 20:19 DMA1:[TAEV1.INC]SUBDEF.INP;3 */
/*TPAM        CHECKOUT FILE_TIME=16-DEC-1982 14:21 DMA1:[TAEV1.INC]SUBDEF.INP;2 */
/*
 *	Defined constants from the TAE Prototype for use in C bridge programs.
 *
 *	These constants are required for Prototype subroutine compatibility.
 *
 *	WARNING: this file must be coordinated with SUBDEF.*
 */

/*	Version 3	*/

/*	Change log:
 *
 *	16-jun-83	Make TBD re coordination into warning...nhe
 *	31-jan-84	Do preprocessing of subroutine names for UNIX,
 *			i.e., xmput must be xmput_...palm
 */

#ifndef  I_SUBDEF			/* prevent double include	*/
#define	 I_SUBDEF	0


#define		XABORT		1
#define		XABNORMAL	2
#define		XADD		102
#define		XBADNAME	10
#define		XBADCOUNT	9
#define		XBADSIZE	4
#define		XBADTYPE	11
#define		XCCSTD		1
#define		XCCNULL		2
#define		XCCPROMPT	3
#define		XCCDBLE		4
#define		XCONT		2
#define		XCR		1
#define		XCRT		2
#define		XCRTUNKNOWN	-2
#define		XDELETE		1
#define		XEOF		2
#define 	XERROR		3
#define		XESCAPE		2
#define		XEXTEND		9999
#define		XFULLSET	101
#define		XINOUT		3
#define 	XINPUT		1
#define		XINFILE		115
#define		XKILLED		107
#define		XNOFILE		3
#define		XNOROOM		117
#define		XNOVALUE	104
#define		XOFFLINE	106
#define		XOUTFILE	116
#define 	XOUTPUT		2
#define		XPRMDIM		1000
#define 	XSAVE		2
#define		XSTRINGSIZ	133
#define		XSUBSET		100
#define		XSUCCESS	1
#define		XT4027		-1
#define		XUNKNOWN	3
#define		XUPDATE		103
#define		XVT100		1
#define		XVT52		2

#ifdef VAX_VMS
/*
 *	The following changes the names of the FORTRAN-callable
 *	C functions so that the ARGPASSER.FOR interface module will
 *	get control before calling the C functions.  For an explanation, 
 *	see comments in the ARGPASSER.FOR source module.  This is
 *	for VAX only.
 */

#define xmput 	 c_xmput
#define xpinit 	 c_xpinit 
#define xpatr 	 c_xpatr 
#define xpfile 	 c_xpfile 
#define xpfilp 	 c_xpfilp 
#define xpintg 	 c_xpintg 
#define xpreal 	 c_xpreal 
#define xpstr 	 c_xpstr 
#define xpstrp 	 c_xpstrp 
#define xqdynp 	 c_xqdynp 
#define xqfilp	 c_xqfilp  
#define xqini	 c_xqini  
#define xqintg   c_xqintg   
#define xqreal   c_xqreal   
#define xqstrp   c_xqstrp   
#define xqwrtb 	 c_xqwrtb 
#define xratr  	 c_xratr  
#define xrfilp   c_xrfilp   
#define xrinim 	 c_xrinim 
#define xrintg   c_xrintg  
#define xrrdb    c_xrrdb   
#define xrreal   c_xrreal  
#define xrstrp   c_xrstrp  

#endif 		/* VAX_VMS */

#ifdef UNIX

/*
 *	UNIX pre-processing:
 *
 *	The following changes the names of the FORTRAN-callable
 *	subroutines to have a trailing underscore for UNIX
 *	compatibility.
 */

#define xmput 	 xmput_
#define xpinit 	 xpinit_ 
#define xpatr 	 xpatr_
#define xpfile 	 xpfile_ 
#define xpfilp 	 xpfilp_
#define xpintg 	 xpintg_
#define xpreal 	 xpreal_
#define xpstr 	 xpstr_
#define xpstrp 	 xpstrp_ 
#define xqdynp 	 xqdynp_
#define xqfilp	 xqfilp_ 
#define xqini	 xqini_
#define xqintg   xqintg_   
#define xqreal   xqreal_  
#define xqstrp   xqstrp_  
#define xqwrtb 	 xqwrtb_
#define xratr  	 xratr_
#define xrfilp   xrfilp_   
#define xrinim 	 xrinim_
#define xrintg   xrintg_ 
#define xrrdb    xrrdb_ 
#define xrreal   xrreal_  
#define xrstrp   xrstrp_ 

#endif 		/*	UNIX	*/
#endif
