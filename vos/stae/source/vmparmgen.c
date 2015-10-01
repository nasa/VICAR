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
 *	Vm_ functions to build and manipulate a no-pool V-block.
 *
 * CHANGE LOG:
 * 23-APR-87 	Created...tpl
 * 10-SEP-87 	Qualifier for all variables not just parms...tpl
 * 31-MAR-88 	Added SetParmPage...tpl
 * 10-jul-88	Remove unnecessary clear (and potentially)
 *				a portability error) from Vm_Alloc...palm
 * 02-dec-88	Vm_SetOneString...tni
 * 26-jan-89	new POINTER_VALIDS logic...palm
 * 09-feb-90	Vm_NewCA(), Vm_FreeCA()  added for C++ 2.0 compatibility...ljn
 * 19-feb-91	Removed most calls to zero_block because tae_alloc uses
 *		calloc (or should) which clears the block...krw
 * 08-apr-92	Removed some unused or duplicate code from Vm_Alloc...krw
 * 22-jul-92	PR1519: Label some functions as UNSUPPORTED or CLASSIC...kbs
 * 22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3 does not
 *		like it...rt
 * 09-mar-93	PR1855: Added Vm_FreeVarCA to support C++ Symbol deletion..krw
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include "taeintproto.h"



/* 
 *	Vm_Alloc.  Allocate VARIABLE structure in dynamic storage.
 */

    UNSUPPORTED struct VARIABLE *Vm_Alloc(
 
    struct SYMTAB	*symtab,	/* in: symtab to link to	*/
    TEXT		name[],		/* in: name of variable		*/
    FUNINT		type,		/* in: variable type		*/
    FUNINT		count,		/* in: number of values		*/
    FUNINT              strsiz          /* in: string max size          */
    )
    {
    struct VARIABLE	*v;
    struct VARIABLE	*pc;

    v = (struct VARIABLE *) Vm_AllocVar( symtab );
    s_copy(name, (*v).v_name);
    (*v).v_minc = (*v).v_maxc = (*v).v_count = count;
    (*v).v_type = type;
    (*v).v_class = V_PARM;

    if ( strsiz > STRINGSIZ || strsiz < 1 )
        (*v).v_size = STRINGSIZ;
    else
        (*v).v_size = strsiz;

    if (count >= 1)
	(*v).v_cvp = Vm_AllocValue( v );
    else
	(*v).v_cvp = NULL;		/* no current values if < 1	*/
    
    for(pc=(struct VARIABLE *) symtab; (*pc).v_link != NULL; 
						  pc=(*pc).v_link)
    	;				/* find end of chain		*/
    (*pc).v_link = v;			/* link new entry to chain	*/
    return(v);
    }

/*
 *	Vm_Save.   Copy string to dynamic memory
 */

    UNSUPPORTED TEXT *Vm_Save (

    TEXT	string[]		/* string to copy		*/
    )
 
    {
    TEXT	*s;

    s = tae_alloc(1, s_length(string) + 1);
    if (s == NULL) return (NULL);
    s_copy(string, s);
    return (s);
    }

/*
 *    Vm_AllocValue.  Allocate space for values.  The maxcount and variable type
 *    are obtained from the VARIABLE structure passed as argument.   Usage:
 *
 *    (*v).v_dvp = Vm_AllocValue(v);	-- allocate defaults
 *    (*v).v_cvp = Vm_AllocValue(v);	-- allocate current
 *
 * Note that values pointers in the VARIABLE structure must be cast into
 * proper format before accessing values.
 *
 */

    UNSUPPORTED char *Vm_AllocValue(

    FAST struct VARIABLE *v	/* in/out: variable to have values allocated  */
    )

    {
    FAST COUNT 		vsize;
    FAST GENPTR 	val;

/************************************************************************/    
/*  to do:  check type and count                                        */
/************************************************************************/    

    vsize = Vm_ValueSize((*v).v_type);	/* size of one value		*/
    val = tae_alloc((*v).v_maxc, vsize);
    return (val);
    }

/*
 *    Vm_AllocVar.   Build VARIABLE structure.
 *
 *    Allocates structure and places it at end of symbol table chain. 
 *
 */

    UNSUPPORTED struct VARIABLE *Vm_AllocVar(

    struct SYMTAB *head	/* in/out: pointer to symbol table header 	*/
    )

    {
    FAST struct VARIABLE *p;	/* pointer to allocated structure*/


    p = (struct VARIABLE *) tae_alloc(1, sizeof(struct VARIABLE));
    (*p).v_pv12 = TRUE;			/* flag TAE version post 1.2 */
    (*p).v_minc = 1;			/* default mincount	*/
    return(p);
    }

/*
 *	 Vm_FreeValue.  Delete values.
 */

    UNSUPPORTED VOID Vm_FreeValue(

    TEXT	*pv,				/*in: pointer to values	*/
    FUNINT	type,				/*in: type of variable	*/
    FUNINT	count				/*in: current value cnt	*/
    )

    {
    FAST TEXT  **q;			/* ptr to vector of ptrs	*/
    FAST COUNT i;

    if (pv == NULL)
        return;				/* no values allocated 		*/
    if (type == V_STRING)
	{
	q = (TEXT **) pv;		/* cast to q format (from char*)*/
	for (i=0; i < count; i++)
	    if (q[i] != NULL)
		tae_free(q[i]);		/* delete string		*/
	}
    tae_free(pv);			/* delete value vector		*/
    return;
    }

/*
 *	Vm_ValueSize.  Determine bytes for one value.
 *	Note that NAME parameters have no storage allocation requirment.
 *	(The referenced variable has the storage.)
 */

    UNSUPPORTED COUNT Vm_ValueSize(

    FUNINT	type		/* V_STRING, V_REAL, V_INTEGER		*/
    )

    {
    if (type == V_INTEGER) return(sizeof(TAEINT));
    if (type == V_REAL) return (sizeof(TAEFLOAT));
    if (type == V_STRING) return (sizeof(TEXT*));
    if (type == V_NAME) return (0);
    return (0);
    }

