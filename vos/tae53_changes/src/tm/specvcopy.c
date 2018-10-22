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



/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:49 DUA1:[TAEV2.TM]SPECVCOPY.C;2 */
/* TPEB CHECKOUT FILE_TIME=15-SEP-1984 17:55 DUA0:[TAEV1.TM]SPECVCOPY.C;2 */
/*
 *	Routines for copying VARIABLE structures.  Like vcopy, only
 *	the special parts of the variable structure are copied also.
 *
 *	CHANGE LOG:
 *	
 *	07-nov-84	TCL 67: copy parameter qualifiers...peb
 *	13-nov-84	TCL 117: don't copy v_cvp if v_deref...peb
 *	20-nov-84	TCL 67: Use v_pv12...peb
 *	22-dec-84	PR 863: set v_pv12 for all variable class...lia
 *	13-jul-87	Locals with qualifiers.  Here the change is to
 *			copy the qualifier symbol table (v_qualst)
 *			unconditionally, not just for V_PARMs...palm
 *	
 */

/*
 *	Standard TAE include files.
 */

#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"tminc.inc"	/* TM include file			*/
#include	"symtab.inc"	/* TM symbol table			*/
#include "taeintproto.h"



/*
 *	specvcopy.   Copy VARIABLE structure to new allocated storage and
 *	return its pointer.  Copy  includes copying the special parts.
 *
 */

FUNCTION CODE  specvcopy
(
    FAST struct VARIABLE *vin,	/* input: VARIABLE struct to move	*/
    FAST struct VARIABLE *vout	/* input: VARIABLE struct to move to	*/

 )
    {
    struct VARIABLE *up;	/* unresolved version of p		*/
    struct R_VALID *vdin;	/* pointer to 'valid'			*/
    FAST CODE	code;

    if ((*vin).v_class == V_PARM && (*vin).v_iparm)
	return(FAIL);		/* must not copy a RESIDVAR parameter	*/
    up = vin;			/* unresolved version of P		*/
    vin = RESOLVE(up);		/* resolve indirection			*/
    MOVE_STRUCT(*vin, *vout);	/* move structure 			*/
    s_copy ((*up).v_name, (*vout).v_name);	/* use unresolved name	*/
    (*vout).v_link = NULL;	/* end of chain for now			*/
    (*vout).v_default = (*up).v_default;	/* in case NAME parm	*/

/*   null out pointers to dynamic memory to avoid double de-allocation	*/

    (*vout).v_pv12 = TRUE;		/* flag new TAE version		*/
    (*vout).v_qualst.link = NULL;	/* qualifier symbol table	*/
    if ((*vout).v_class == V_PARM)
        (*vout).v_tp = NULL;
    if ((*vout).v_class == V_GLOBAL)
        (*vout).v_pdf = NULL;
    (*vout).v_cvp = NULL;
    (*vout).v_dvp = NULL;
    (*vout).v_valid = NULL;

/* allocate and move current values	 */

    if (!((*vout).v_class == V_PARM  && (*vin).v_pv12  &&  (*vout).v_deref))
	{
	if ( ((*vout).v_cvp = (GENPTR) allval(vout)) ==NULL)
	    {
	    overr();
	    return(FAIL);		/* allocation failure */
	    }
	code  = cpy_val((*vin).v_type, (*vin).v_cvp, 
			(*vout).v_cvp, (*vin).v_count);
	if (code != SUCCESS)
	    {
	    overr();
	    return(FAIL);		/* allocation failure */
	    }
	}

/* and now the default values	 */

    if ( (*vin).v_dvp != NULL)
	{
	if ( ((*vout).v_dvp = (GENPTR) allval(vout)) ==NULL)
	    {
	    overr();
	    return(FAIL);		/* allocation failure */
	    }
	code  = cpy_val((*vin).v_type, (*vin).v_dvp, (*vout).v_dvp, (*vin).v_dcount);
	if (code != SUCCESS)
	    {
	    overr();
	    return(FAIL);		/* allocation failure */
	    }
	}

/* now do the qualifier symbol table (if input var from TAE version	*/
/* which had parameter qualifiers)					*/

    if ((*vin).v_pv12)
	{
	code = movest(&(*vin).v_qualst, &(*vout).v_qualst); /* move qual sym tab*/
	if (code != SUCCESS)
	    {
	    overr();
	    return(FAIL);			/* allocation failure */
	    }
	}

/* now do valids			*/

    vdin = (struct R_VALID *) (*vin).v_valid;
    if (vdin != NULL)
    	{
	if ( ((*vout).v_valid = allleg(vout,(*vdin).count)) ==NULL)	/* set defaults		*/
	    {
	    overr();
	    return(FAIL);		/* allocation failure */
	    }
	cpy_vld((*vin).v_type, (*vin).v_valid, (*vout).v_valid);
    	}

/* and finally the DEFPDF for globals	*/

    if ((*vout).v_class == V_GLOBAL  &&  (*vin).v_pdf != NULL)
	{
	(*vout).v_pdf = (struct DEFPDF *) tae_alloc(1, sizeof(struct DEFPDF));
	if ((*vout).v_pdf == NULL)
	    {
	    overr();
	    return(FAIL);		/* allocation failure */
	    }
	MOVE_STRUCT(*(*vin).v_pdf, *(*vout).v_pdf);
	}

    return (SUCCESS);
    }
