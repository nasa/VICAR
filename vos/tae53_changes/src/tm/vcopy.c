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
 *	Routines for copying VARIABLE structures.
 *
 *	CHANGE LOG:
 *	
 *	
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	04-aug-83	Checked for RESIDVAR parameter...dm
 *	04-aug-83	Check for V_PARM before look at v_iparm...palm
 *	21-aug-83	Nullables...palm
 *	13-sep-83	Fix vdin defined as GENPTR but really R_VALID...palm
 *	16-sep-83	Delete movement of defaults...palm
 *			(Tutor resets defaults upon entry)
 *	22-sep-83	Fix portability problem with valid structure...palm
 *	10-oct-83	Fix unix compilation errors...palm
 *	07-nov-84	TCL 67: vcopy to copy parameter qualifiers...peb
 *	20-nov-84	TCL 67: use v_pv12...peb
 *	22-dec-84	PR 863: set v_pv12 for all variable class...lia
 *	13-jul-87	Implement qualifiers for locals; the change here is
 *			to move the v_qualst unconditionally, not just
 *			for V_PARM...palm
 *	02-feb-89	New POINTER_VALIDS logic...palm
 *	09-feb-89	VAX C requires faithful declaration of s_save()...ljn
 *
 */

/*
 *	Standard TAE include files.
 */

#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
	
#include	"symtab.inc"	/* TM symbol table			*/
#include "taeintproto.h"


/*
 *	vcopy.   Copy VARIABLE structure to new allocated storage and 
 *	return its pointer.
 *
 */

FUNCTION CODE  vcopy
(
    FAST struct VARIABLE *vin,	/* in: VARIABLE struct to move		*/
    FAST struct VARIABLE *vout	/* in: VARIABLE struct to move to	*/

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

    (*vout).v_pv12 = TRUE;		/* flag var built by post V 1.2 TAE	*/
    (*vout).v_qualst.link = NULL;	/* qualif symbol table	*/
    if ((*vout).v_class == V_PARM) 
	(*vout).v_tp = NULL;
    if ((*vout).v_class == V_GLOBAL)
        (*vout).v_pdf = NULL;
    (*vout).v_cvp = NULL;
    (*vout).v_dvp = NULL;
    (*vout).v_valid = NULL;

/* allocate and move values	 */

    if ( ((*vout).v_cvp = (GENPTR) allval(vout)) ==NULL)
	{
	overr();
	return(FAIL);		/* allocation failure */
	}
    code  = cpy_val((*vin).v_type, (*vin).v_cvp, (*vout).v_cvp, (*vin).v_count);
    if (code != SUCCESS)
	{
	overr();
	return(FAIL);		/* allocation failure */
	}

/* now do the qualifier symbol table (if input variable created by TAE	*/
/* version which used parameter qualifiers)				*/

    if ((*vin).v_pv12)
	{
	code = movest(&(*vin).v_qualst, &(*vout).v_qualst);/* move qual sym tab	*/
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
    return (SUCCESS);
    }

/*
 *	cpy_val. Copy a value vector.
 *
 *	return FAIL if out of dynamic memory
 */
FUNCTION CODE cpy_val
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in value(s)			*/
    GENPTR		out,		/* out value(s)			*/
    FUNINT		count		/* in: number of in values	*/

 )
    {
    TEXT	*sptr[MAXVAL];		/* string pointer vector	*/
    FAST COUNT	i,j;		
    FAST TEXT	**svalue;		/* casted value vector		*/

    if (count <= 0)
	return (SUCCESS);		/* nothing to copy		*/
    if (type == V_REAL)
	bytmov(in, out, count * sizeof(TAEFLOAT));
    else if (type == V_INTEGER)
	bytmov(in, out, count * sizeof(TAEINT));
    else
	{
	/* strategy for strings: build a local value vector (sptr).  Once
	   it is successfully built, then move it in.  This way if we run
	   out of memory, we haven't destroyed v's current value.
        */

	svalue = (TEXT **) in;	
	for (i=0; i < count; i++)
	    {
	    sptr[i] = s_save(svalue[i]);
	    if (sptr[i] == NULL)	
		{			/* not enough memory to store	*/
		for (j=0; j < i; j++)	/* free everything so far	*/
		    s_free(sptr[j]);
		return (FAIL);		
		}
	    }
	bytmov((GENPTR) sptr, out, count * sizeof(TEXT *));
    	}
    return (SUCCESS);
    } 

/*
 *	cpy_vld.  Copy a variable's valid structure
 *
 *	return FAIL if out of dynamic memory
 *
 */
FUNCTION VOID cpy_vld
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in: valid structure		*/
    GENPTR		out		/* out: valid structure 	*/

 )
    {
    FAST struct R_VALID *vin, *vout;	/* use any type to get count	*/

    
    vin = (struct R_VALID *)in;
    vout = (struct R_VALID *)out;
    bytmov (in, out, valid_size (type, (*vin).count));
    (*vout).count = (*vin).count;
    if (type == V_STRING)
#ifdef POINTER_VALIDS
	{
	COUNT i;
	struct S_VALID *svalidIn = (struct S_VALID *)  in;
	struct S_VALID *svalidOut = (struct S_VALID *) out;
	for (i=0; i < (*svalidIn).count; i++)
	    (*svalidOut).slist[i].string = s_save ((*svalidIn).slist[i].string);
	}
#endif
    return;
    }
