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



/* Functions to manipulate the symbol tables.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	12-jul-83	Fixed delvar() to free VARIABLE for V_NAME type...dm
 *	22-jul-83	Dealloc panel posit arrays in deltut...peb
 *	29-jul-83	Process intrinsic (RESIDVAR) parameters differently..dm
 *	04-aug-83	Call zero_block() to zero out a block...dm
 *	04-aug-83	Check for V_PARM to examine v_iparm...palm
 *	04-aug-83	Declare create_ivp() in get_ivp() for portability...dm
 *			Change v_type to v_class in delvar()...dm
 *	05-aug-83	Alloc intrinsics for NAME parameters...dm
 *	11-aug-83	Change crash() calls  to tmierr() calls...dm
 *	13-sep-83	Add functions to order symbol tables alpabetically...dm
 *	10-oct-83	Fix unix lint/compilation errors...palm
 *	23-nov-83	Test tae_alloc output in allval (PR 597)...palm
 *	01-mar-84	Deallocate defaults for intrinsics...dm
 *	21-aug-84	Delete hardcoding of align in create_ivp()...dm
 *	07-nov-84	Alphabetize, TCL 67: delvar to delete parm quals...peb
 *	13-nov-84	TCL 117: delvar to delete v_nref...peb
 *	20-nov-84	TCL 67: allvar to set v_pv12...peb
 *	27-jul-85	Optimize lookab/lookex...palm
 *	01-apr-87	Fix lookab to handle qualified names so that
 *			TCL lines can have things like x.q.z=value...palm
 *	03-apr-87	Let "X." be same as "X" in lookex/lookab so 
 *			substitution in all of TCLTEST works ok...palm
 *	13-jul-87	Locals with qualifiers.  Here the change is to
 *			unconditionally delete the v_qualst table in 
 *			delvar...palm
 *	13-jul-87	Lookex to fix bug where "n.q" lookup would fail
 *			if n were a NAME variable...palm
 *	21-jun-88	remove ASCII dependency...tp
 *			  use s_table instead of UPPER macro 
 *			  change type of allval() to TEXT from char
 *	26-jan-89	INT_MAXVAL is now the max vector for intrinsics...palm
 *			new delete_ivp_pool...palm
 *	31-jul-91	In delvar() don't free unless non-NULL...ljn
 *	22-oct-92	Prototype of tae_alloc unnecessary and Ultrix 4.3 does
 *			not like it...rt
 *
 *****************************************************************************
 */


/******  NOTE: This module uses #601, 602, 603 as tmierr() numbers    ********/

#include "taeconf.inp"
#include "symtab.inc"
#include "tminc.inc"
#include "taeintproto.h"

FUNCTION  VOID rels_ivp
(
    struct  VARIABLE	*p		/* in: ptr to block to be released */
  
 );
FUNCTION  BOOL  low_order
(   
    FAST	TEXT	*str1,		/* in: first string	*/
    FAST	TEXT  	*str2		/* in: second string	*/

    );
    IMPORT TEXT s_table[];		/* lower to upper case mapping  */

    static  struct  VARIABLE	*ipp_head = NULL;
    static  COUNT		ivp_free = 0;
   


/*   allleg.  Allocate valid structure.
 *   Caller must cast the returned pointer into pointer to proper VALID.

 *	For POINTER_VALIDS, this does NOT allocate the strings,
 *	just the pointers to the strings.
 */

FUNCTION GENPTR allleg
(
    FAST struct VARIABLE *v,	/* in/out: ptr to var being validized	*/
    FUNINT lcount		/* in:  number of valid ranges	     */

 )
    {
    COUNT	bytes;
    FAST TEXT *p;
    FAST struct I_VALID *q;			

    bytes = valid_size ((*v).v_type, lcount);
    (*v).v_valid = p = tae_alloc(1, bytes);	
    if (p == NULL) return(p);
    q = (struct I_VALID *) p;			/* any cast to x_VALID ok */
    (*q).count = lcount;		
    return(p);
    }

/*
 *    allval.  Allocate space for values.  The maxcount and variable type
 *    are obtained from the VARIABLE structure passed as argument.   Usage:
 *
 *    (*v).v_dvp = allval(v);	-- allocate defaults
 *    (*v).v_cvp = allval(v);	-- allocate current
 *
 * Note that values pointers in the VARIABLE structure must be cast into
 * proper format before accessing values.
 *
 */

FUNCTION TEXT *allval
(
    FAST struct VARIABLE *v	/* in/out: variable to have values allocated  */

 )
    {
    FAST COUNT 		vsize;
    FAST GENPTR 	val;
    FAST COUNT		i;
    FAST TEXT		**vs;

    
    vsize = valsize((*v).v_type);	/* size of one value		*/
    val = tae_alloc((*v).v_maxc, vsize);
    if (val == NULL) 
	return (NULL);    
    if ((*v).v_type == V_STRING)
	{				/* for strings, NULL the vector	*/
	vs = (TEXT **) val;
	for (i=0; i < (*v).v_maxc; i++)
	    vs[i] = NULL;
	}
    return (val);
    }

/*
 *    allvar.   Build VARIABLE structure.
 *
 *    Allocates structure and places it at end of symbol table chain. 
 *
 */

FUNCTION struct VARIABLE *allvar
(
    struct SYMTAB *head	/* in/out: pointer to symbol table header 	*/

 )
    {
    FAST struct VARIABLE *p;	/* pointer to allocated structure*/
    FAST struct VARIABLE *pc;	/* current pointer		 */


    p = (struct VARIABLE *) tae_alloc(1, sizeof(struct VARIABLE));
    if (p==NULL)
        return(NULL);
    zero_block((GENPTR) p, sizeof (struct VARIABLE));
    (*p).v_pv12 = TRUE;			/* flag TAE version post 1.2 */
    (*p).v_minc = 1;			/* default mincount	*/
    for (pc=(struct VARIABLE *) head; (*pc).v_link != NULL; pc=(*pc).v_link)
	;				/* find end of chain ...*/
    (*pc).v_link = p;			/* link in new struc	*/
    return(p);
    }

/*
 * alpha_var . Add a variable in alphbetic order to symbol table.
 */

FUNCTION  struct VARIABLE * alpha_var
(
    TEXT	  name[],	/* in: name of variable to be added	  */
    struct SYMTAB *head	/* in/out: pointer to symbol table header */

 )
    {
    FAST struct VARIABLE *p;			/* ptr to allocated struct*/
    FAST struct VARIABLE *lv;			/* variable to left of slot */

    p = (struct VARIABLE *) tae_alloc(1, sizeof(struct VARIABLE));
    if (p==NULL)
        return(NULL);
    zero_block((GENPTR) p, sizeof (struct VARIABLE));
    (*p).v_pv12 = TRUE;			/* flag TAE version post 1.2 */
    (*p).v_minc = 1;			/* default mincount	 */

    lv = find_slot(name, head);		/* get proper slot	 */
    if (lv == NULL)			/* if none lower	 */
	{
	(*p).v_link = (*head).link;	/* link at start of tbl  */
	(*head).link = p;
	}
    else
	{
	(*p).v_link = (*lv).v_link;
	(*lv).v_link = p;		/* link to table 	*/
        }
    return(p);
    }

/*
 * create_ivp - create a new VARIABLE for intrinsic parameter pool.
 * NOTE: This function allocates the VARIABLE struct and the current value
 * vector for the intrinsic parameter. The value vector is adjacent to
 * the VARIABLE, aligned on a worstcase boundary.
 */

    FUNCTION  static struct VARIABLE * create_ivp(void)

    {
    struct  VARIABLE	*p;
    COUNT		ext_size;	/* size of extension to VARIABLE */

#define  ALIGN_SIZ  sizeof(ALIGN)	/* worstcase alignment boundary  */

    /*	The following restricts the IVP pool entries to (a) a max vector size of
	INT_MAXVAL and (b) integers and strings.   If you have other
	requirements, then use normal "allvar".
    */
    ext_size = INT_MAXVAL * MAX(sizeof(TAEINT),sizeof(TEXT *)) + 
		ALIGN_SIZ;		/* max val vector + worst case align */


    p = (struct VARIABLE *) tae_alloc(1, sizeof(struct VARIABLE)+ext_size);
    if (p == NULL)
        return(NULL);
    zero_block((GENPTR) p, sizeof (struct VARIABLE));
    (*p).v_pv12 = TRUE;			/* flag TAE version post 1.2 */
    (*p).v_cvp = (GENPTR) p + 
	(sizeof(struct VARIABLE)/ALIGN_SIZ+1)*ALIGN_SIZ;
    (*p).v_class = V_PARM;
    (*p).v_iparm = TRUE;
    ivp_free++;				/* increment # of blocks free	*/
    return(p);
    }


/*	
	The following is called to free the pool so we
	can monitor allocated memory more conveniently.
	The pool gets automatically re-created when needed.
	This is called by the HI002 intrinsic.
*/

FUNCTION VOID delete_ivp_pool (void)
    {
    struct VARIABLE *v;
    for (v=ipp_head; v != NULL; v=(*v).v_link)
	tae_free(v);
    ipp_head = NULL;
    ivp_free = 0;
    }


/*
 *	deltab.  Delete symbol table.
 */

FUNCTION VOID  deltab
(
    FAST struct SYMTAB *head		/* in/out: symbol table header		*/

 )
    {
    while ((*head).link != NULL)	/* delete first till list empty	*/
	delvar(head, (*head).link);		
    (*head).link = NULL;		/* reinitialize symbol table	*/
    return;
    }

/*	deltut - deallocate tutor extensions from a variable in a symbol table.
 *	Deallocates any stored help level 1 text.
 *	Deallocates any .compnlin arrays.
 *	Only deallocates from variables whose class allows a tutor extension
 *	(for now only V_PARM).
 *	On exit from this function the v_tp pointer is NULL.
 */

FUNCTION VOID deltut 
(
    struct VARIABLE	*v		/* in/out: variable to deallocate from	*/

 )
    {
    struct TUTEXT	*t;

    if ((*v).v_class != V_PARM)
	return;
    if ((t = (*v).v_tp) == NULL)
	return;
    if ((*t).compnlin != NULL)
	tae_free((GENPTR)(*t).compnlin);
    fretxt(&(*t).l1help);		/* deallocate the lev 1 help text if any*/
    tae_free((GENPTR)t);		/* deallocate the extension		*/
    (*v).v_tp = NULL;
    return;
    }

/*
 *	 delval.  Delete values.
 */

FUNCTION VOID delval
(
    TEXT	*pv,				/*in: pointer to values	*/
    FUNINT	type,				/*in: type of variable	*/
    FUNINT	count				/*in: current value cnt	*/

 )
    {
    FAST TEXT  **q;			/* ptr to vector of ptrs	*/
    FAST COUNT i;

    if ( count == 0xFFFFFFFF )
      count = -1;

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
 *	delvar.  Delete entry (i.e., a variable) from symbol table.
 */

FUNCTION  VOID delvar
(
    struct SYMTAB *head,		/* in/out: symbol table		*/
    FAST struct VARIABLE *v		/* in:  entry to delete		*/

 )
    {
    FAST struct VARIABLE *pc, *pn;	/* current, next ptrs	*/
    FAST struct DEFPDF	 *defpdf;	/* defining pdf block	*/

    for (pc=(struct VARIABLE *) head, pn=(*pc).v_link; 
			pn != NULL; pc=pn, pn=(*pc).v_link)
	if (v == pn) break;		/* find entry in table	*/
    if (pn == NULL)
	tmierr (602);
    (*pc).v_link = (*v).v_link;		/* unlink from table 	*/
    deltut(v);				/* delete tutor extension if any */
    if ((*v).v_class == V_PARM && (*v).v_iparm)	
	{
	rels_ivp(v);			/* release to ipp 	*/
  	return;
	}
    else if ((*v).v_class == V_PARM && (*v).v_type == V_NAME)  /* no values for NAME	*/
	{
	if ((*v).v_nref != NULL)
	    tae_free((*v).v_nref);
	tae_free((GENPTR) v);
        return;
	}
    else
	{
	delval((*v).v_dvp, (*v).v_type, (*v).v_dcount);  
	if ((*v).v_class == V_PARM  &&  (*v).v_deref)
	    tae_free((*v).v_cvp);
	else
	    delval((*v).v_cvp, (*v).v_type, (*v).v_count);   
#ifdef POINTER_VALIDS
	if ((*v).v_type == V_STRING && (*v).v_valid)
	    {
	    struct S_VALID *svalid = (struct S_VALID *) (*v).v_valid;
	    COUNT	i;
	    for (i=0; i < (*svalid).count; i++)		/* free each string */
		tae_free ((*svalid).slist[i].string);
	    }
#endif
	if ((*v).v_valid)
	    tae_free((*v).v_valid);		/* delete structure */
    	if ((*v).v_class == V_GLOBAL  &&  (*v).v_pdf != NULL)
	    {
	    defpdf = (*v).v_pdf;
	    (*defpdf).refcount--;		/* one less "user" of the block		*/
	    if ((*defpdf).refcount == 0)
	        tae_free (defpdf);		/* free it when no one is referencing	*/
	    }	
	deltab(&(*v).v_qualst);			/* delete parm qualif symb tab*/
	tae_free((GENPTR) v);			/* delete VARIABLE	*/
        return;
	}
    }

/*
 *  find_slot.  Find the logical slot for a variable in the symbol table.
 *	   	The symbol table is assumed to be sorted in ascending
 *		alphabetic order w.r.t. variable name.
 *
 *  Function return:
 *		Pointer to the variable on the left side of the slot.
 */

FUNCTION  struct  VARIABLE  *find_slot
(
    TEXT		name[],		/* in: variable name to match	 */
    struct  SYMTAB	*symtab	/* in: symbol table to look thru */

 )
    {
    struct  VARIABLE    *v;
    struct  VARIABLE	*back_link;	/* ptr to previous variable	 */

    back_link = NULL;
    for (v=(*symtab).link; v != NULL; back_link=v, v=(*v).v_link)
	{
	if (low_order(name, (*v).v_name))	/* lower than name in tbl */
	    break;
	}
    return (back_link);
    }

/*
 * get_ivp. Get a VARIABLE block for an intrinsic (RESIDVAR) parameter and 
 * link it to the symbol table.
 * NOTE: get_ivp tries to get the next VARIABLE block (pointed to by 
 * ipp_head) from the intrinsic parameter pool(ipp). If the pool is empty 
 * a new VARIABLE block is dynamically allocated and added to the pool, 
 * and then used. This way we do not run out of blocks in case of nested 
 * intrinsic processing, but do not allocate more than worst case 
 * requirements.
 * The VRIABLE blocks are released to the intrinsic parameter pool by
 * calling rels_ivp.
 */

FUNCTION  struct VARIABLE * get_ivp
(
    struct  SYMTAB  *head		/* in/out: symbol table header	*/

 )
    {
    struct  VARIABLE  * pc;
    struct  VARIABLE  *p;

    if (ipp_head == NULL)		/* no more free blocks		*/
	{
	ipp_head = create_ivp();	/* create a new one		*/
	if (ipp_head == NULL)		/* no more dynamic memory	*/
            return(NULL);
	}
    p = ipp_head;			/* pointer to next free block	*/
    ipp_head = (*ipp_head).v_link;	/* unlink block from pool	*/
    ivp_free--;				/* decrement # of free blocks	*/
    for (pc=(struct VARIABLE *) head; 
		(*pc).v_link != NULL; pc=(*pc).v_link)
	;				/* find end of chain ...	*/
    (*pc).v_link = p;			/* link in new struc		*/
    (*p).v_link = NULL;			/* mark as end of chain		*/
    return(p);
    }

/*  initab.  Initialize a symbol table.
 */

FUNCTION VOID initab
(
    struct SYMTAB *head		/* header to init		*/

 )
    {
    (*head).link = NULL;		
    return;
    }

/*	lookab.  Lookup name in table allowing abbreviations.
 *	The name may be qualified (e.g., X.Y.Z).
 *
 *	Function returns:
 *
 *	SUCCESS	if name found successfully (1 instance)
 *	FAIL	if name cannot be found in the symbol table
 *	AMBIG	if name is an ambiguous abbreviation within symbol table
 */

FUNCTION CODE lookab 
(
    struct SYMTAB	*head,		/* in:  header to symbol table		*/
    TEXT		name[],		/* in:  name of variable (parm) to find	*/
    FAST struct VARIABLE **v		/* out: variable found (NULL unless SUCCESS)*/

 )
    {
    FAST struct VARIABLE *p, *psave = 0;	/* pointers to variables in symbol tab	*/
    COUNT		nmatch;		/* number of matches found	*/
    COUNT	period_index ;		/* index of first period */
    TEXT	tmp_name[STRINGSIZ+1];	/* temporary copy 	 */
    CODE	code;	

    nmatch = 0;
    period_index = s_index (name, '.');
    if (period_index > 0)			/* qualified name ?     */
	{
	s_copy (name, tmp_name); 
	tmp_name[period_index] = EOS;		/* clip to simple name  */
	name = tmp_name;			/* point to temp copy	*/
	if (tmp_name[period_index+1] == EOS)	/* is period last char? */
	    period_index = -1;			/* then NOT qualified   */
	}
    for (p = (*head).link; p != NULL; p = (*p).v_link)
	{
	if (s_lseq(name, (*p).v_name))		/* if substring match:	*/
	    {
    	    if (s_equal(name, (*p).v_name))	/* try for exact match	*/
    	 	{
		if (period_index > 0) 
		    goto handle_qualifier;
    		*v = p;			/* set output 			 */
    		return (SUCCESS);	/* exact match -- don't continue */
    		}
	    psave = p;
	    nmatch++;
	    }
	}
    if (nmatch == 1)
	{
	p = psave;			/* THE match that was found	*/
        if (period_index > 0)
            goto handle_qualifier;
	*v = psave;
	return(SUCCESS);
	}
    else if (nmatch == 0)		/* variable not in table	*/
	{
	*v = NULL;
	return(FAIL);
	}
    else 				/* ambiguous abbreviation	*/
	{
	*v = NULL;
	return(AMBIG);
	}


handle_qualifier:

	p = RESOLVE (p);		/* follow NAME reference	*/
	if (p == NULL)
	    {
	    *v = NULL;
	    return (FAIL);
	    }
	code = lookab (&(*p).v_qualst, &name[period_index+1], v);
	return (code);

    }

/*
 *	lookex.  Find name in a symbol table using exact match.
 *
 *	CAUTION: the efficiency of this function is critical
 *	to TAE performance.
 */

FUNCTION struct VARIABLE *lookex
(
    struct SYMTAB *head,		/* in: symbol tab to search	*/
    TEXT name[]			/* in: name to find		*/

 )
    {
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
    p = lookex (head, tmp_name);	/* look up first level name	*/
    if (p == NULL)
        return (NULL);
    p = RESOLVE (p);			/* follow NAME reference	*/
    if (p == NULL)
	return (NULL);
    if (tmp_name[period_index+1] == EOS)	/* remainder is null       */
        return (p);
    p = lookex (&(*p).v_qualst, &tmp_name[period_index+1]);  /* find quals */
    return (p);
    }

/*
 * 	low_order. Check if 1st string is alphabetically lower order than 2nd.
 *
 * 	Return codes:	TRUE, if first string is lower in order
 *			FALSE, otherwise.
 *		      
 */
  
FUNCTION  BOOL  low_order
(   
    FAST	TEXT	*str1,		/* in: first string	*/
    FAST	TEXT  	*str2		/* in: second string	*/

    )
    {
    FAST	TEXT	s1, s2;

    for ( ; *str1 != EOS && *str2 != EOS;
		str1++,str2++)		/* if both have a next char     */
	{
	  s1 = s_table[(int) *str1];	
	  s2 = s_table[(int) *str2];
	if (s1 != s2) 
	    return(s1 < s2);			/* TRUE if first one smaller */
	}
    return (*str1 == EOS);			/* TRUE if first one shorter */
    }

/*
 * rels_ivp. Release an intrinsic parameter VARIABLE struct to the pool.
 */	

FUNCTION  VOID rels_ivp
(
    struct  VARIABLE	*p		/* in: ptr to block to be released */
  
 )
    {
    FAST  COUNT		i;
    TEXT		**q;

/*	first delete the current value strings if type = string		*/
    if ((*p).v_type == V_STRING)
	{
 	q = (TEXT **) (*p).v_cvp;	/* current value pointer	*/
	for (i=0; i < (*p).v_count; i++)
	    if (q[i] != NULL) tae_free(q[i]);		/* free string 	*/
	}

    /*
    	The following check for V_NAME is required because repval has
	the logic to (sometimes) change an intrinsic parm to V_NAME
    */

    if ((*p).v_type != V_NAME)
        delval((*p).v_dvp, (*p).v_type, (*p).v_dcount);   /* delete defaults */
    (*p).v_link = ipp_head;		/* now put at top of free pool	*/
    ipp_head  = p;			/* new pool header		*/
    ivp_free++;				/* free blocks in pool 		*/
    return;
    }	

/*	valid_size.   Return size of valid structure.
 */

FUNCTION COUNT valid_size 
(    
    FAST  FUNINT type,		/* in: V_INTEGER, V_REAL, V_STRING	*/
    FAST  FUNINT count		/* in: number of ranges			*/

     )
    {
    FAST COUNT	basic = 0;		/* size of valid COUNT plus one range	*/
    FAST COUNT  increment = 0;	/* size of additional ranges		*/

    if (type == V_INTEGER)
	{
	basic = sizeof(struct I_VALID); 
        increment =  sizeof(struct I_RANGE) ;
	}
    else if (type == V_REAL)
	{
	basic= sizeof(struct R_VALID) ;
	increment = sizeof(struct R_RANGE) ;
	}
    else if (type == V_STRING)
	{
	basic = sizeof(struct S_VALID);
	increment = sizeof(struct S_RANGE) ;
	}
    else tmierr (601);
    if (count <= 0)
        return (basic);
    return (basic + (count-1)*increment);
    }

/*
 *	valsize.  Determine bytes for one value.
 *	Note that NAME parameters have no storage allocation requirment.
 *	(The referenced variable has the storage.)
 */

FUNCTION COUNT valsize
(
    FUNINT	type		/* V_STRING, V_REAL, V_INTEGER		*/

 )
    {
    if (type == V_INTEGER) return(sizeof(TAEINT));
    if (type == V_REAL) return (sizeof(TAEFLOAT));
    if (type == V_STRING) return (sizeof(TEXT*));
    if (type == V_NAME) return (0);
    tmierr (603);
    return (0);
    }

