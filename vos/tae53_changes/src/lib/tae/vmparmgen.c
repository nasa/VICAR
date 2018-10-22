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
#include        "vminc.inc"         /* Dynamic allocation definitions      */
#include "syninc.inc"
#include "taeintproto.h"



static FUNCTION VOID init_new
(
 struct VM_STRUCT *h,           /* parblk handle                */
 FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
 );
UNSUPPORTED  VOID Vm_FreeVar
(
    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/
 );



/*
 *	Vm_New.  Create a no-pool parblk and return handle
 */

FUNCTION struct VM_STRUCT * Vm_New
(
 FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
)
    {
    struct VM_STRUCT *h;           /* parblk handle                */
    COUNT	l,c;
    CODE	code;

    /* TBD: remove t_init call in Vm_init.  Needed now to make m_put work. */
    t_init(&l, &c, &code);

    /*
        allocate memory space for the header 
    */
    h = (struct VM_STRUCT *)tae_alloc ( 1, sizeof(struct VM_STRUCT) ); 
    init_new (h, mode);
    return( h );
    }



/*
 *	Vm_NewCA. Initialize a caller-allocated parblk.
 *
 *	This is used by C++ constructors where C++ has already allocated
 *	"this." This is a kludge so that Co_Readfile can create SymbolTable's
 *	that can then be used in C++.
 */

UNSUPPORTED VOID Vm_NewCA 
(
    struct VM_STRUCT * vm,		/* parblk to initialize		*/	
    FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/

 )
    {
      zero_block ((GENPTR) vm, sizeof (struct VM_STRUCT) );       
    init_new (vm, mode);
    return;
    }



/*
 *	init_new.  Initialize no-pool parblk and return handle
 */

static FUNCTION VOID init_new
(
 struct VM_STRUCT *h,           /* parblk handle                */
 FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
 )
    {
    struct NP_PARBLK *np;
    np = &( (*h).npblk );
    (*np).last   = 1;
    (*np).symtab.link = NULL;

    if ( mode == P_ABORT )
        (*np).mode = P_MODE_ABORT | P_MODE_TAE_ALLOC;
    else if ( mode == P_CONT )
        (*np).mode = P_MODE_CONT | P_MODE_TAE_ALLOC;
    else
        {
        mode = mode & (P_MODE_ABORT | P_MODE_CONT);
        (*np).mode = mode | P_MODE_TAE_ALLOC;
        }
    return;
    }


/*
 *	Vm_SetIntg.	Place integer values into object context
 */ 

FUNCTION CODE Vm_SetIntg
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count,		/* in: count of variable	*/
 TAEINT		intg[],		/* in: value vector		*/
 FUNINT		mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)& (*vm).npblk;        
    return(q_intg( p, name, count, intg, mode) );
    }

/*
 *	Vm_SetReal.	Place real (TAEFLOAT) values into context.
 */

FUNCTION CODE Vm_SetReal
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count,		/* in: count of variable	*/
 TAEFLOAT	real[],		/* in: reals			*/
 FUNINT		mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;        
    return( q_real(p, name, count, real, mode) );
    }

/* 
 *	Vm_SetString.  Set string values into context.
 */

FUNCTION CODE Vm_SetString
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count,		/* in: count of vector		*/
					/* (0 means set count = 0)	*/
 TEXT		*vector[],	/* in: vector of string ptrs	*/
 FUNINT 	mode		/* in: P_UPDATE or P_ADD	*/
)
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;        
    return( q_string( p, name, count, vector, mode) );
    }		

/* 
 *	Vm_SetOneString.  Set one string value into context, rather
 *		than a vector as per Vm_SetString.
 */

ADA_FUNCTION CODE Vm_SetOneString
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	idx,		/* in: which string value	*/
				/* (0 means set count = 0)	*/
 TEXT		onestr[],	/* in: one string ptr		*/
 FUNINT 	mode		/* in: P_UPDATE or P_ADD	*/
 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;        
    return( q_one_string( p, name, idx, onestr, mode) );
    }		


/* 
 *	Vm_SetShortString.  Set string values into context.
 */

UNSUPPORTED CODE Vm_SetShortString
(
    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of vector		*/
					/* (0 means set count = 0)	*/
    FUNINT              strsiz,         
    TEXT		*vector[],	/* in: vector of string ptrs	*/
    FUNINT 		mode		/* in: P_UPDATE or P_ADD	*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;        
    return( q_shortstring( p, name, count, strsiz, vector, mode) );
    }		

/* 
 *	Vm_Alloc.  Allocate VARIABLE structure in dynamic storage.
 */

UNSUPPORTED struct VARIABLE *Vm_Alloc
( 
    struct SYMTAB	*symtab,	/* in: symtab to link to	*/
    TEXT		name[],		/* in: name of variable		*/
    FUNINT		type,		/* in: variable type		*/
    FUNINT		count,		/* in: number of values		*/
    FUNINT              strsiz         /* in: string max size          */
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

UNSUPPORTED TEXT *Vm_Save 
(
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

UNSUPPORTED char *Vm_AllocValue
(
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

UNSUPPORTED struct VARIABLE *Vm_AllocVar
(
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
 *	Vm_FreeTable.  Delete symbol table.
 */

UNSUPPORTED VOID  Vm_FreeTable
(
    FAST struct SYMTAB *head		/* in/out: symbol table header		*/

 )
    {

    while ((*head).link != NULL)	/* delete first till list empty	*/
	Vm_FreeVar(head, (*head).link);		
    (*head).link = NULL;		/* reinitialize symbol table	*/
    return;
    }


/*
 *	Vm_FreeVarCA.  Delete entry (i.e., a variable) from symbol table.
 *		       The actual variable must be freed by the caller.
 */

UNSUPPORTED  VOID Vm_FreeVarCA
(
    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/
 )
    {
    FAST struct VARIABLE *pc, *pn;	/* current, next ptrs	*/
    struct S_VALID *svalid;
    COUNT	i;

    for (pc=(struct VARIABLE *) head, pn=(*pc).v_link; 
			pn != NULL; pc=pn, pn=(*pc).v_link)
	if (v == pn) break;		/* find entry in table	*/
    if (pn == NULL)
        /*?????*/;                          /* bad variable pointer */
    (*pc).v_link = (*v).v_link;		/* unlink from table 	*/
    if ((*v).v_class == V_PARM && (*v).v_type == V_NAME)  /* no values for NAME	*/
	{
	if ((*v).v_nref != NULL)
	    tae_free((*v).v_nref);
	}
    else
	{
	Vm_FreeValue((*v).v_dvp, (*v).v_type, (*v).v_dcount);  
	if ((*v).v_class == V_PARM  &&  (*v).v_deref)
	    tae_free((*v).v_cvp);
	else
	    Vm_FreeValue((*v).v_cvp, (*v).v_type, (*v).v_count);   
    	if ((*v).v_valid != NULL)
	    {
#ifdef POINTER_VALIDS
	    if ((*v).v_type == V_STRING)
		{
	        svalid = (struct S_VALID *) (*v).v_valid;
	        for (i=0; i < (*svalid).count; i++)
		    tae_free ((*svalid).slist[i].string);
		}
#endif
	    tae_free((*v).v_valid);		/* delete VALID		*/
	    }
        Vm_FreeTable(&(*v).v_qualst);		/* delete qualif symb tab*/
	}
    }


/*
 *	Vm_FreeVar.  Delete entry (i.e., a variable) from symbol table.
 */

UNSUPPORTED  VOID Vm_FreeVar
(
    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/
 )
    {
    Vm_FreeVarCA(head, v);
    tae_free((GENPTR) v);		/* delete VARIABLE	*/
    }



/*
 *	Vm_FindStVar.  Find name in a symbol table using exact match.
 *
 *	CAUTION: the efficiency of this function is critical
 *	to TAE performance.
 */

UNSUPPORTED struct VARIABLE *Vm_FindStVar
(
    struct SYMTAB *head,		/* in: symbol tab to search	*/
    TEXT name[]			/* in: name to find		*/

 )
    {
    IMPORT TEXT s_table[];		/* lower to upper case mapping  */
    FAST struct VARIABLE *p;		/* current place in table	*/
    FAST TEXT *s, *t;			/* string pointers		*/
    TEXT tmp_name[STRINGSIZ+1];
    COUNT period_index;

/*   Note that we avoid an (expensive) s_equal call here with in-line code */

    for (p=(*head).link; p != NULL; p=(*p).v_link)
	{
        s = name;			/* start of name		*/
        t = (*p).v_name;		/* start of v_name		*/
	while (*s && *t)		/* while both have a next char	*/     
	    {
	      if (s_table[(int) *s++] != s_table[(int) *t++])
    		goto big_break;
	    }
	if (!*s && !*t)
	    return(p);			/* match if both at EOS	        */
        big_break:;			/* for optimization		*/
	}

/*	This is the "not found" case: here it's okay to relax the 
	optimization a little--this is not the high probability case.
        Here, we start again and consider qualification!
*/

    period_index = s_index (name, '.');	/* look for qualifier		*/
    if (period_index < 0)		/* if no qualifier present	*/
        return(NULL);			/* not found 			*/	
    s_copy (name, tmp_name); 		/* make a temp copy		*/
    tmp_name [period_index] = EOS;	/* clip at first period		*/
    p = Vm_FindStVar (head, tmp_name);	/* look up first level name	*/
    if (p == NULL)
        return (NULL);
    if (tmp_name[period_index+1] == EOS)	/* remainder is null       */
        return (p);
    p = Vm_FindStVar (&(*p).v_qualst, &tmp_name[period_index+1]);  /* find quals */
    return (p);
    }

/*
 *	Vm_ValueSize.  Determine bytes for one value.
 *	Note that NAME parameters have no storage allocation requirment.
 *	(The referenced variable has the storage.)
 */

UNSUPPORTED COUNT Vm_ValueSize
(
    FUNINT	type		/* V_STRING, V_REAL, V_INTEGER		*/

 )
    {
    if (type == V_INTEGER) return(sizeof(TAEINT));
    if (type == V_REAL) return (sizeof(TAEFLOAT));
    if (type == V_STRING) return (sizeof(TEXT*));
    if (type == V_NAME) return (0);
    return (0);
    }

/*
 *	Vm_AddVar - Add a variable (from a PARBLK) to a symtab.
 *
 */

UNSUPPORTED CODE Vm_AddVar 
(
    struct SYMTAB	*stb,		/* in/out: symtab to add to	*/
    struct VARIABLE	*var		/* in:  variable to add		*/

 )
    {
    struct VARIABLE	*targvar;
    struct VARIABLE	*save_link;
    CODE		type, class;
    CODE		code;

    type  = (*var).v_type;
    class = (*var).v_class;
    targvar = Vm_AllocVar(stb);        	/* alloc new var in symtab	*/
    save_link = (*targvar).v_link;
    code = Vm_SpCopyVar(var, targvar);	/* copy var to symb tab var*/
    if (code != SUCCESS)
        return(FAIL);
    (*targvar).v_link = save_link;	/* Vm_SpCopyVar nulled the link	*/
    return(SUCCESS);
    }

/*
 *        Vm_Free - free the no-pool parblk structure
 *
 *
 */
FUNCTION VOID Vm_Free
(
 struct VM_STRUCT* vm              /* IN: handle of no-pool parblk to be free */
)
    {
    Vm_FreeTable ( &(*vm).npblk.symtab );      /* free the symtab first */
    tae_free((GENPTR) vm);                     /* free everything else */
    return;
    }


/*
 *        Vm_FreeCA - free the Vm object except for the VM_STRUCT
 *	  itself (which was allocated by the caller).
 *
 *	  For more discussion, see Vm_NewCA()  in this file.
 *
 */
UNSUPPORTED VOID Vm_FreeCA 
(
    GENPTR      h              /* IN: handle of no-pool parblk to be free */

 )
    {
    struct VM_STRUCT *vm;

    vm = (struct VM_STRUCT *)h;
    Vm_FreeTable ( &(*vm).npblk.symtab );      /* free the symtab first */
    return;
    }


/*
 *	Vm_SetMin.	Place minimum variable value into object context
 */

FUNCTION CODE Vm_SetMin
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count		/* in: count of variable	*/
)
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)& (*vm).npblk;        
    return(q_min( p, name, count) );
    }

/*
 *	Vm_SetMax.	Place maximum variable value into object context
 */

FUNCTION CODE Vm_SetMax
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	count		/* in: count of variable	*/
)
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)& (*vm).npblk;        
    return(q_max( p, name, count) );
    }


/*
 *	Vm_SetStringLength.	Set string variable length
 */

FUNCTION CODE Vm_SetStringLength
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 FUNINT 	length		/* in: string length    	*/
)
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)& (*vm).npblk;        
    return(q_stringlength( p, name, length) );
    }


/*
 *	Vm_SetParmPage.	Place parm page into a variable
 */

CLASSIC_FUNCTION CODE Vm_SetParmPage
(
 GENPTR	        h,		/* in: handle                   */
 TEXT		name[],		/* in: variable name		*/
 BOOL           flag
 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)& (*vm).npblk;        
    return(q_parmpage( p, name, flag) );
    }
