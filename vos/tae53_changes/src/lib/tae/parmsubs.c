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



/* TPAM CHECKOUT FILE_TIME=15-JUL-1987 18:48 DUA1:[TAEV2.OLB.GENLIB]PARMSUBS.C;3 */
/* TPL CHECKOUT FILE_TIME=27-MAY-1987 11:45 DUA1:[TAEV2.OLB.GENLIB]PARMSUBS.C;2 */
/* TDM CHECKOUT FILE_TIME= 9-SEP-1986 14:15 DUA1:[TAEV1.OLB]PARMSUBS.C;1 */
/* TPEB CHECKOUT FILE_TIME= 9-NOV-1984 14:28 DUA0:[TAEV1.OLB]PARMSUBS.C;11 */
/*
 *	PARMSUBS.  Functions for outboard parameter 
 *	manipulation
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	09-nov-84	New pk_size and pm_size for PR 826...palm
 *	27-nov-84	Add pm_nopar,pk_nopar,pm_oldp,pk_oldp...peb
 *	14-sep-86	Make x_error call p_mput/m_msg for FORTRAN 
 *			vs. C application...dm
 *      06-jul-87       Added version 2.2 mode check...tpl
 *	05-aug-87	Fixed mode handling (&& -> &) in x_error...palm
 *	07-aug-87	Move x_error to separate module so that you
 *			can override it and use TAE subroutine 
 *			without being under TAE...palm
 *      28-dec-88       Added P_BADSIZE message...tpl
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"subdef.inc"	/* prototype constants in C form	*/
#include "syninc.inc"
#include "taeintproto.h"



/*
 *	Standard error messages and keys.
 */

GLOBAL TEXT pm_name[]="Variable '%s' not found in V-block.";
GLOBAL TEXT pk_name[]="TAE-VBADNAME";

GLOBAL TEXT pm_type[]="V-block type discrepancy for variable '%s'.";
GLOBAL TEXT pk_type[]="TAE-VBADTYPE";

GLOBAL TEXT pm_count[]="V-block count discrepancy for variable '%s'.";
GLOBAL TEXT pk_count[]="TAE-VBADCOUNT";

GLOBAL TEXT pm_dup[]  ="Attempt to add duplicate variable name '%s' to V-block.";
GLOBAL TEXT pk_dup[]  ="TAE-VDUPNAME";

GLOBAL TEXT pm_nopar[]  ="Attempt to add qualifier to non-existent parameter: '%s'.";
GLOBAL TEXT pk_nopar[]  ="TAE-VNOPARM";

GLOBAL TEXT pm_oldp[]  ="Parameter from old TAE allows no qualifiers: '%s'.";
GLOBAL TEXT pk_oldp[]  ="TAE-VOLDPARM";

GLOBAL TEXT pm_room[] ="V-block overflow on variable '%s'.";
GLOBAL TEXT pk_room[] ="TAE-VNOROOM";

GLOBAL TEXT pm_killed[]="Process terminated by user.";
GLOBAL TEXT pk_killed[]="TAE-KILLED";

GLOBAL TEXT pm_dim[]   ="Caller's dimension too small for '%s'.";
GLOBAL TEXT pk_dim[]   ="TAE-VDIMEN";

GLOBAL TEXT pm_trans[] ="Error translating TAE file specification '%s'.";
GLOBAL TEXT pk_trans[] ="TAE-VTRANS";

GLOBAL TEXT pm_size[] = "String '%s' too long.";
GLOBAL TEXT pk_size[] = "TAE-BADSIZE";

GLOBAL TEXT pm_invsz[] = "Invalid string size specified for variable '%s'.";
GLOBAL TEXT pk_invsz[] = "TAE-INVSIZE";

/* type of application program	*/
GLOBAL  CODE	applic_type = UNKNOWN_TYPE;  	/* initialize to unknown */



/*
 *	p_code.  Code translation. 
 *      Translate table[i][0] into table[i][1].
 *	A value in table[i][0] of -1 terminates the table and the
 *	default output value is table[i][1].
 */

FUNCTION CODE p_code
(
 CODE	table[][2],	/* input: translation table	*/
 CODE	code		/* input: value to translate	*/
 )
    {
    COUNT	i;
    
    for (i=0; table[i][0] != -1; i++)
        if (table[i][0] == code) break;
    return (table[i][1]);
    }

/*
 *	p_code caller can pass any conversion table.  The following useful
 *	table converts "C" error codes into prototype error codes.
 */

    GLOBAL CODE pc_returns[][2]  =  {	   /* return codes	*/
      {P_BADNAME,	XBADNAME},
      {P_BADTYPE, 	XBADTYPE},
      {P_BADCOUNT,	XBADCOUNT},
      {P_DUPNAME, 	XBADNAME},
      {P_NOROOM,	XNOROOM},
      {P_KILLED,	XKILLED},
      {SUCCESS,	XSUCCESS},
      {-1,		XERROR}    };

/*
 * 	p_s2p.  Convert C string to packed/eos returning the number of
 *	TAEINT elements used for the C string.
 */

FUNCTION COUNT p_s2p
(
 TEXT	c_string[],		/* in: C string			*/
 TAEINT	packed[]		/* out: FORTRAN packed/eos 	*/
 )
	{
	COUNT	elements;		/* number of FORTRAN TAEINTs	*/
	COUNT	i;
	TEXT	*p;			/* pointer to the FORTAN string	*/
	

	p = (TEXT *) packed; 		/* convert pointer for...	*/
	for(i=0; *c_string != EOS; i++, c_string++, p++)
	    *p = *c_string;		
	*p = EOS;
	elements = i/sizeof(TAEINT) + 1;  /* includes space for EOS	*/
	return(elements);
	}

/*
 *	p_s2ca1.   Convert C string to Counted A1 FORTRAN format.  The function value
 *	returned value is the COUNT of the number of FORTRAN TAEINT elements
 *	required to store the string, including the count element.
 */

FUNCTION COUNT p_s2ca1
(
 TEXT	c_string[],		/* in: C string			*/
 TAEINT	ca1_string[]		/* out: FORTRAN CA1 string	*/
 )
	{
	COUNT	i;
	static union 			/* portable A1 construction	*/
		{
		TAEINT	a1;
		TEXT	a1_chars[sizeof(TAEINT)];
		} overlay;		/* overlays n chars on TAEINT	*/


	for (i=1; i < sizeof(TAEINT); i++)	/* blank fill A1 skeleton...	*/
	    overlay.a1_chars[i] = ' ';		/* leaving [0] empty		*/

	for(i=0; c_string[i] != EOS; i++)
	    {
	    overlay. a1_chars[0] = c_string[i];	/* place into blank filled element	*/
	    ca1_string[i+1] = overlay.a1;	/* move A1 format element		*/
	    }

	ca1_string[0] = i;			/* count element		*/
	return(i+1);				/* count plus data chars	*/    
	}	    

/*
 * 	parm_err. Report error from  xr/xq packages   
 */
 
FUNCTION VOID  parm_err
(
 TEXT 	control[],		/* in: control string		*/
 TEXT	key[],			/* in: message key		*/
 uintptr_t a1,			/* in: integers or string ptrs	*/
 uintptr_t a2,
 uintptr_t a3
 )
    {
    TEXT	message[2*STRINGSIZ+1];	/* formatted message string	*/


    sprintf(message, control, a1, a2, a3);	
    if (s_length(message) > STRINGSIZ)		     /* allow upto STRINGSIZ  */
	s_copy("...", &message[STRINGSIZ-3]);		/* truncate with ...  */

    if (applic_type == C_TYPE)				/* c application pgm  */
	m_msg(message, key);				/* 'c' msg out call   */
    else						/* assume FORTAN      */
    	p_mput(message, key);				/* output the message */
    return;
    }


