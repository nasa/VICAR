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
 *      (An outboard version of $taetm:vcopy.c)  
 *	CHANGE LOG:
 *                10-Apr-87 Created...tpl	
 *                10-Sep-87 Copy qualifiers for all variables, not just 
 *                          parmS...tpl
 *
 *		  26-jan-89 New POINTER_VALIDS logic...palm
 *		  02-feb-89 New Vm_CopyValid argument...palm	
 *			and fix call to tae_alloc...palm
 * 25-apr-91	New VMS TAE logicals...ljn
 * 20-aug-91	Minor Efficiency in Vm_MoveSymtab...krw
 * 22-jul-92	PR1519: Label all functions UNSUPPORTED...kbs
 *	
 */

/*
 *	Standard TAE include files.
 */

#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
	
#include	"symtab.inc"	/* TM symbol table			*/
#include "syninc.inc"
#include "taeintproto.h"

UNSUPPORTED CODE Vm_CopyValue
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in value(s)			*/
    GENPTR		out,		/* out value(s)			*/
    FUNINT		count		/* in: number of in values	*/

 );


/*
 *	Vm_CopyVar.   Copy VARIABLE structure to new allocated storage and 
 *	return its pointer.
 *
 */

UNSUPPORTED CODE  Vm_CopyVar
(
    FAST struct VARIABLE *vin,	/* in: VARIABLE struct to move		*/
    FAST struct VARIABLE *vout	/* in: VARIABLE struct to move to	*/

 )
    {
    struct VARIABLE *up;	/* unresolved version of p		*/
    struct R_VALID *vdin;	/* pointer to 'valid'			*/
    FAST CODE	code;

    up = vin;			/* unresolved version of P		*/
    vin = RESOLVE(up);		/* resolve indirection			*/
    MOVE_STRUCT(*vin, *vout);	/* move structure 			*/
    s_copy ((*up).v_name, (*vout).v_name);	/* use unresolved name	*/
    (*vout).v_link = NULL;	/* end of chain for now			*/
    (*vout).v_default = (*up).v_default;	/* in case NAME parm	*/

/*   null out pointers to dynamic memory to avoid double de-allocation	*/

    (*vout).v_pv12 = TRUE;		/* flag var built by post V 1.2 TAE	*/
    (*vout).v_qualst.link = NULL;	/* parm qualif symbol table	*/
    if ((*vout).v_class == V_PARM)
	{
        (*vout).v_tp = NULL;
	}
    if ((*vout).v_class == V_GLOBAL)
        (*vout).v_pdf = NULL;
    (*vout).v_cvp = NULL;
    (*vout).v_dvp = NULL;
    (*vout).v_valid = NULL;

/* allocate and move values	 */

    (*vout).v_cvp = (GENPTR) Vm_AllocValue(vout);
    code  = Vm_CopyValue((*vin).v_type, (*vin).v_cvp, (*vout).v_cvp, (*vin).v_count);
    if (code != SUCCESS)
	{
	return(FAIL);		/* allocation failure */
	}

/* now do the qualifier symbol table (if input variable created by TAE	*/
/* version which used parameter qualifiers)				*/

    if ( (*vin).v_pv12 )
	{
	code = Vm_MoveSymtab(&(*vin).v_qualst, &(*vout).v_qualst);/* move qual sym tab	*/
	if (code != SUCCESS)
	    {
	    return(FAIL);			/* allocation failure */
	    }
	}

/* now do valids			*/

    vdin = (struct R_VALID *) (*vin).v_valid;
    if (vdin != NULL)
    	{
	if ( ((*vout).v_valid = Vm_AllocValid(vout,(*vdin).count)) ==NULL)	/* set defaults		*/
	    {
	    return(FAIL);		/* allocation failure */
	    }
	Vm_CopyValid((*vin).v_type, (*vin).v_valid, (*vout).v_valid, NULL);
    	}
    return (SUCCESS);
    }

/*
 *	Vm_CopyValue. Copy a value vector.
 *
 *	return FAIL if out of dynamic memory
 */
UNSUPPORTED CODE Vm_CopyValue
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
 *	Vm_CopyValid.  Copy a variable's valid structure
 *
 *	return FAIL if out of pooled memory.  If you
 *	pass NULL for pool, then it's ok to ignore 
 *	the return code (because tae_alloc always
 *	returns with memory or aborts).
 *
 */
UNSUPPORTED CODE Vm_CopyValid
(
    FUNINT		type,		/* in: variable type		*/
    GENPTR		in,		/* in: valid structure		*/
    GENPTR		out,		/* out: valid structure 	*/
    GENPTR		pool		/* in: NULL for tae_allocation  */
					/* or pool for r_allocation     */

 )
    {
    FAST struct R_VALID *vin, *vout;	/* use any type to get count	*/
    struct S_VALID *svalidIn;
    struct S_VALID *svalidOut;
    COUNT	i, length;
    TEXT	*s;			/* valid string pointer */

    vin = (struct R_VALID *)in;
    vout = (struct R_VALID *)out;
    bytmov (in, out, Vm_ValidSize (type, (*vin).count));
    (*vout).count = (*vin).count;
#ifdef POINTER_VALIDS
    if (type == V_STRING)
	{
	svalidIn = (struct S_VALID *) vin;
	svalidOut = (struct S_VALID *) vout;
	for (i=0; i < (*svalidIn).count; i++)
	    {
	    length = s_length ((*svalidIn).slist[i].string);
	    if (pool)
		{
		  s = (TEXT *) r_alloc ((ALIGN *) pool, length+1);  /* pooled allocation */
		if (s == NULL)
		    return (FAIL);
		}
	    else
		s = (TEXT *) tae_alloc (1, length+1);	/* free allocation */
	    s_copy ((*svalidIn).slist[i].string, s);
	    (*svalidOut).slist[i].string = s; 
	    }
	}
#endif
    return(SUCCESS);
    }

/*
 *	Vm_SpCopyVar.   Copy VARIABLE structure to new allocated storage and
 *	return its pointer.  Copy  includes copying the special parts.
 *
 */

UNSUPPORTED CODE  Vm_SpCopyVar
(
    FAST struct VARIABLE *vin,	/* input: VARIABLE struct to move	*/
    FAST struct VARIABLE *vout	/* input: VARIABLE struct to move to	*/

 )
    {
    struct VARIABLE *up;	/* unresolved version of p		*/
    struct R_VALID *vdin;	/* pointer to 'valid'			*/
    FAST CODE	code;

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
	{
        (*vout).v_tp = NULL;
	}
    if ((*vout).v_class == V_GLOBAL)
                   (*vout).v_pdf = NULL;
    (*vout).v_cvp = NULL;
    (*vout).v_dvp = NULL;
    (*vout).v_valid = NULL;

/* allocate and move current values	 */
    if (!((*vout).v_class == V_PARM  && (*vin).v_pv12  &&  (*vout).v_deref))
	{
	(*vout).v_cvp = (GENPTR) Vm_AllocValue(vout);
	code  = Vm_CopyValue((*vin).v_type, (*vin).v_cvp, 
			(*vout).v_cvp, (*vin).v_count);
	if (code != SUCCESS)
	    {
	    return(FAIL);		/* allocation failure */
	    }
	}

/* and now the default values	 */

    if ( (*vin).v_dvp != NULL)
	{
	(*vout).v_dvp = (GENPTR) Vm_AllocValue(vout);
	code  = Vm_CopyValue((*vin).v_type, (*vin).v_dvp, (*vout).v_dvp, (*vin).v_dcount);
	if (code != SUCCESS)
	    {
	    return(FAIL);		/* allocation failure */
	    }
	}

/* now do the qualifier symbol table (if input var from TAE version	*/
/* which had parameter qualifiers)					*/

    if ( (*vin).v_pv12 )
	{
	code = Vm_MoveSymtab(&(*vin).v_qualst, &(*vout).v_qualst); /* move qual sym tab*/
	if (code != SUCCESS)
	    {
	    return(FAIL);			/* allocation failure */
	    }
	}

/* now do valids			*/

    vdin = (struct R_VALID *) (*vin).v_valid;
    if (vdin != NULL)
    	{
	if ( ((*vout).v_valid = Vm_AllocValid(vout,(*vdin).count)) ==NULL)	/* set defaults		*/
	    {
	    return(FAIL);		/* allocation failure */
	    }
	Vm_CopyValid((*vin).v_type, (*vin).v_valid, (*vout).v_valid, NULL);
    	}

/* and finally the DEFPDF for globals	*/

    if ((*vout).v_class == V_GLOBAL  &&  (*vin).v_pdf != NULL)
	{
	(*vout).v_pdf = (struct DEFPDF *) tae_alloc(1, sizeof(struct DEFPDF));
	if ((*vout).v_pdf == NULL)
	    {
	    return(FAIL);		/* allocation failure */
	    }
	MOVE_STRUCT(*(*vin).v_pdf, *(*vout).v_pdf);
	}

    return (SUCCESS);
    }

/*	Vm_MoveSymtab.  Move a symbol table.
 *	Assumes that the caller has allocated both of the actual symbol table
 *	structures.  Deletes the destination table (if it's not initially
 *	empty) before doing the move.
 */

UNSUPPORTED CODE Vm_MoveSymtab 
(
    struct SYMTAB	*st1,		/* in:  symbol table to move from	*/
    struct SYMTAB	*st2		/* out: symbol table to move to		*/

 )
    {
    struct VARIABLE	*v1;
    struct VARIABLE	*v2;
    struct VARIABLE	*v3;
    CODE		code;

    Vm_FreeTable(st2);			/* clear the destination symbol table	*/
    for (v3=(struct VARIABLE *) st2; (*v3).v_link != NULL; v3=(*v3).v_link)
	;				/* find end of chain ...*/
    for (v1 = (*st1).link; v1 != NULL; v1 = (*v1).v_link)
	{
	v2 = Vm_AllocVar(st2);
	code = Vm_CopyVar(v1, v2);
	if (code != SUCCESS)
	    {
	    Vm_FreeTable(st2);
	    return(FAIL);
	    }
        v3 = (*v3).v_link = v2;			/* link in new struc	*/
	}
    return(SUCCESS);
    }
