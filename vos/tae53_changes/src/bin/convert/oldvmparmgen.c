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



/* TPL CHECKOUT FILE_TIME=15-JUL-1987 19:17 DUA1:[TAEV2.OLB.GENLIB]VMPARMGEN.C;1 */
/*
 *	Vm_ functions to build and manipulate a no-pool V-block.
 *
 *	CHANGE LOG:
 *      23-APR-87 	Created...tpl
 *      10-SEP-87 	Qualifier for all variables not just parms...tpl
 *      31-MAR-88 	Added SetParmPage...tpl
 *	10-jul-88	Remove unnecessary clear (and potentially)
 *			a portability error) from Vm_Alloc...palm
 *	02-dec-88	Vm_SetOneString...tni
 *	22-oct-92	Prototyping tae_alloc is unecessary and Ultrix 4.3
 *			does not like it...rt
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include        "vminc.inc"         /* Dynamic allocation definitions      */

#include "taeintproto.h"


    COUNT	Vm_ValueSize(FUNINT);
    GENPTR      Vm_AllocValue(FAST struct VARIABLE *);
    struct VARIABLE *Vm_FindStVar(struct SYMTAB *, TEXT[]);
    struct VARIABLE *Vm_AllocVar(struct SYMTAB *);

    FUNCTION struct VARIABLE *Vm_Alloc(
        struct SYMTAB	*symtab,	/* in: symtab to link to	*/
        TEXT		name[],		/* in: name of variable		*/
        FUNINT		type,		/* in: variable type		*/
        FUNINT		count,		/* in: number of values		*/
        FUNINT              strsiz         /* in: string max size          */
    );

/*
 *	Vm_New.  Create a no-pool parblk and return handle
 */

    FUNCTION GENPTR Vm_New(

    FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
    )

    {
    struct VM_STRUCT *h;           /* parblk handle                */
    COUNT	l,c;
    CODE	code;
    struct NP_PARBLK *np;

    /* TBD: remove t_init call in Vm_init.  Needed now to make m_put work. */
    t_init(&l, &c, &code);

    /*
        allocate memory space for the header 
    */
    h = (struct VM_STRUCT *)tae_alloc ( 1, sizeof(struct VM_STRUCT) ); 
    zero_block ( (GENPTR)h, sizeof (struct VM_STRUCT) );       
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
    return( (GENPTR)h );
    }

/*
 *	Vm_SetIntg.	Place integer values into object context
 */

    FUNCTION CODE Vm_SetIntg(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of variable	*/
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

    FUNCTION CODE Vm_SetReal(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of variable	*/
    TAEFLOAT		real[],		/* in: reals			*/
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

    FUNCTION CODE Vm_SetString(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count,		/* in: count of vector		*/
					/* (0 means set count = 0)	*/
    TEXT		*vector[],	/* in: vector of string ptrs	*/
    FUNINT 		mode		/* in: P_UPDATE or P_ADD	*/
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

    FUNCTION CODE Vm_SetOneString(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		idx,		/* in: which string value	*/
					/* (0 means set count = 0)	*/
    TEXT		onestr[],	/* in: one string ptr		*/
    FUNINT 		mode		/* in: P_UPDATE or P_ADD	*/
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

    FUNCTION CODE Vm_SetShortString(

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

    FUNCTION struct VARIABLE *Vm_Alloc(
 
    struct SYMTAB	*symtab,	/* in: symtab to link to	*/
    TEXT		name[],		/* in: name of variable		*/
    FUNINT		type,		/* in: variable type		*/
    FUNINT		count,		/* in: number of values		*/
    FUNINT              strsiz          /* in: string max size          */
    )
    {
    struct VARIABLE	*v;
    struct VARIABLE	*pc;
    COUNT		vsize;

    v = (struct VARIABLE *) Vm_AllocVar( symtab );
    zero_block((GENPTR)v, sizeof (struct VARIABLE));
    s_copy(name, (*v).v_name);
    (*v).v_minc = (*v).v_maxc = (*v).v_count = count;
    (*v).v_type = type;
    (*v).v_class = V_PARM;

    if ( strsiz > STRINGSIZ || strsiz < 1 )
        (*v).v_size = STRINGSIZ;
    else
        (*v).v_size = strsiz;

    (*v).v_pv12 = TRUE;			/* parm created by post V1.2 TAE  */

    if      (type == V_INTEGER) vsize = sizeof(TAEINT);
    else if (type == V_REAL)    vsize = sizeof(TAEFLOAT);
    else if (type == V_STRING)  vsize = sizeof(TEXT *);

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

    FUNCTION TEXT *Vm_Save (

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

    FUNCTION GENPTR Vm_AllocValue(

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
    zero_block ( val, (*v).v_maxc * vsize );
    return (val);
    }

/*
 *    Vm_AllocVar.   Build VARIABLE structure.
 *
 *    Allocates structure and places it at end of symbol table chain. 
 *
 */

    FUNCTION struct VARIABLE *Vm_AllocVar(

    struct SYMTAB *head	/* in/out: pointer to symbol table header 	*/
    )

    {
    FAST struct VARIABLE *p;	/* pointer to allocated structure*/


    p = (struct VARIABLE *) tae_alloc(1, sizeof(struct VARIABLE));
    zero_block((GENPTR) p, sizeof (struct VARIABLE));
    (*p).v_pv12 = TRUE;			/* flag TAE version post 1.2 */
    (*p).v_minc = 1;			/* default mincount	*/
    return(p);
    }

/*
 *	Vm_FreeTable.  Delete symbol table.
 */

FUNCTION  VOID Vm_FreeVar(

    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/
);

    FUNCTION VOID  Vm_FreeTable(

    FAST struct SYMTAB *head		/* in/out: symbol table header		*/
    )

    {
    while ((*head).link != NULL)	/* delete first till list empty	*/
	Vm_FreeVar(head, (*head).link);		
    (*head).link = NULL;		/* reinitialize symbol table	*/
    return;
    }

/*
 *	 Vm_FreeValue.  Delete values.
 */

    FUNCTION VOID Vm_FreeValue(

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
 *	Vm_FreeVar.  Delete entry (i.e., a variable) from symbol table.
 */

    FUNCTION  VOID Vm_FreeVar(

    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/
    )

    {
    FAST struct VARIABLE *pc, *pn;	/* current, next ptrs	*/


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
	tae_free((GENPTR) v);
        return;
	}
    else
	{
	Vm_FreeValue((*v).v_dvp, (*v).v_type, (*v).v_dcount);  
	if ((*v).v_class == V_PARM  &&  (*v).v_deref)
	    tae_free((*v).v_cvp);
	else
	    Vm_FreeValue((*v).v_cvp, (*v).v_type, (*v).v_count);   
    	if ((*v).v_valid != NULL)
	    tae_free((*v).v_valid);		/* delete VALID		*/
        Vm_FreeTable(&(*v).v_qualst);		/* delete qualif symb tab*/
	tae_free((GENPTR) v);			/* delete VARIABLE	*/
        return;
	}
    }

/*
 *	Vm_FindStVar.  Find name in a symbol table using exact match.
 *
 *	CAUTION: the efficiency of this function is critical
 *	to TAE performance.
 */

    FUNCTION struct VARIABLE *Vm_FindStVar(

    struct SYMTAB *head,		/* in: symbol tab to search	*/
    TEXT name[]				/* in: name to find		*/
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
	    if (s_table[(int)*s++] != s_table[(int)*t++])
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

    FUNCTION COUNT Vm_ValueSize(

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

    FUNCTION CODE Vm_AddVar (

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
    FUNCTION VOID Vm_Free (

    GENPTR      h              /* IN: handle of no-pool parblk to be free */
    )

    {
    struct VM_STRUCT *vm;

    vm = (struct VM_STRUCT *)h;
    Vm_FreeTable ( &(*vm).npblk.symtab );      /* free the symtab first */
    tae_free ( h );                            /* free everything else  */
    return;
    }

/*
 *	Vm_SetMin.	Place minimum variable value into object context
 */

    FUNCTION CODE Vm_SetMin(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count		/* in: count of variable	*/
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

    FUNCTION CODE Vm_SetMax(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		count		/* in: count of variable	*/
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

    FUNCTION CODE Vm_SetStringLength(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		length		/* in: string length    	*/
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

    FUNCTION CODE Vm_SetParmPage(

    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    BOOL                flag
    )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)& (*vm).npblk;        
    return(q_parmpage( p, name, flag) );
    }
