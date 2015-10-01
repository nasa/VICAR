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
 *	SYMTAB.  Symbol table structures.
 *
 *	27-JAN-83	Re-arrange VARIABLE.  Define V_BADCOUNT, etc...palm
 *	27-feb-83	Add RESOLVE macro....palm
 *	09-mar-83	Change type of v_pdf...palm
 *	08-apr-83	Add v_intrinsic...palm
 *	29-apr-83	Move v_default to vs_parm. See PR 248/242...nhe
 *	04-aug-83	New v_nullable; v_size made UTINY; new v_iparm...palm
 *	23-aug-83	Comments added on v_count...palm
 *	30-aug-83	v_kiv changed to v_keyword...palm
 *	08-sep-83	New v_file bit...palm
 *			Changed V_NOTFILE to V_NOCHECK
 *	19-sep-83	Members fixed for UNIX compiler where intermediate
 *			structures must be mentioned...palm
 *	11-oct-83	VALIDSIZ for NAMESIZ for valid structure...palm
 *	19-jan-84	New v_page flag to support PARM-PAGE...palm
 *	04-may-84	VALUE_x to xVAL and DVALUE_x to DxVAL ... ces
 *	17-may-84	Put VALUE_x BACK in for compatibility of user's C
 *			programs ... ces
 *	22-may-84	Delete VALUE_x macros...palm
 *	01-aug-84	New VALIDSIZ value...palm
 *	17-sep-84	Add V_BADAMBIG value...lia
 *	17-oct-84	Add V_implicit to VARIABLE for globals...peb
 *	03-nov-84	Add qual symbol table to VAR struct for parms...peb
 *	08-nov-84	Adjusted VALIDSIZ so not to overflow on tutor...lia
 *	09-nov-84	New wasname bit; new deref bit...palm
 *	09-nov-84	TCL 117: add v_nref for compiled PDF NAME parms...peb
 *	20-nov-84	TCL 67: add v_pv12: TRUE if parm created post 1.2...peb
 *	22-dec-84	PR 863: add v_pv12 for global & local variables...lia
 *	15-aug-85	Make negative constants of form (-1) rather than
 *			-1 to help compiler catch "{X Y}"...palm
 *	11-sep-86	PR 1048: Put VALUE_x, DVALUE_x back in for 
 *			compatibility of user's C programs ...peb
 *	02-aug-87	Add new v_event bit for TaePlus...palm
 *	03-aug-87	Add new ES_VALID struct for "commented" valids...palm
 *	26-jan-89	New BEGIN_VALID/END_VALID macros; remove ES_VALID..palm
 *	01-feb-89	Major re-work for MAXVAL > 127, invalidating
 *			all existing PAR and RES files...palm
 *	02-aug-89	PS/2 port...ljn
 *      11-sep-92 PR875	set valid size as STRINGSIZ...tpl
 */

#include "fileinc.inp"

#ifndef	I_SYMTAB		/* prevent double include	*/
#define I_SYMTAB	0

/*
 *	VARIABLE structure.  One structure per TCL variable.
 *
 *	Rules for member referencing:  
 *	
 *	The VARIABLE structure is organized so that the "intermeditate"
 *	structures and unions (vs_* and us_*) do not have to be 
 *	referenced, e.g., "v.vu_class.vs_parm.V_tp" may be referenced
 *	with "v.v_tp".  
 *
 *	The "v_" prefixes exist because, without them,
 *	all of the intermediate structures and unions would have to be
 *	referenced, e.g., v.tp would not work if "tp" were
 *	a member of some other structure.  We do not want to "reserve"
 *	popular members like "type", "name", etc.
 *	
	The picture of what overlays what is:

link, name, type, class
-----------------------
			parameter info		indirect value info
			------------------- -----------------------
			global info		direct value info	
			------------------- -----------------------
			local info			^		
			-------------------		|
				^			|
				|			|
				|			|
				|			|
			vu_class union		vu_value union

 *
 * cvp, (current value pointer) is a pointer to a dynamically allocated
 * array as follows:

 *    INTEGER: *VV_INTEGER	-- pointer to integer vector.
 *    REAL:    *VV_FLOAT	-- pointer to float vector.
 *    STRING:  *VV_STRING	-- pointer to vector of pointers to strings.

 * For simplicity, in TM the allocation is always for maxcount, even though
 * the current count may be less.  In the PARBLK structure, which is built for
 * sending the symbol tables to subprocesses, the allocation is for the current
 * count.  The pointer to a value vector is a "char pointer" 
 * and must be cast into the proper pointer before use.
 * 
 * Default values have similar rules.  It is not required that a default value 
 * vector be allocated; if one does not exist, dvp is NULL.
 * For consistency, the default arrays are always allocated using maxc, 
 * even if the current default is smaller.
 * 
 * To avoid confusing the de-allocation routines, do not simply move the 
 * default value pointer to the value pointer in order to set current value
 * to the default value.  The values must be copied.
 * 
 * NAME variables are special: the cvp field contains a pointer to the 
 * VARIABLE structure of the referenced variable.  dvp points to the
 * default VARIABLE structure.
 */

/*
 *	Typedefs for values of variables.  A value is always considered
 *	a vector, even if the vector count is 1.
 */
    
    typedef TAEINT	VV_INTEGER[];	/* value vector for integer		*/
    typedef TAEFLOAT	VV_FLOAT[];	/* value vector for floating		*/
    typedef TEXT	*VV_STRING[];   /* value vector of ptrs to strings	*/

    struct SYMTAB 				/* symbol table header	*/
	{
	struct VARIABLE	*link;			/* link to first VARIABLE */
	} ;
 

/*	The V_pv12 flag is needed for all variable class.  It belongs outside
 *	the unions but to allow upward compatible we could not change the offsets
 *	of existing members; thus, the somewhat awkward implementation was selected 
 */
struct VARIABLE
    {
    struct VARIABLE	*v_link;	/* link to next in table	*/
    TEXT		v_name[NAMESIZ+1]; /* variable name		*/
    TINY		v_type;		/* V_INTEGER, V_REAL, V_STRING	*/
				    	/* or V_NAME			*/
    TINY		v_class;	/* V_GLOBAL, V_LOCAL, V_PARM	*/

    union 		/* select by class...				*/
	{
	struct				/* for parameter class		*/
	    {		
	    BIT_FIELD_ALIGN
	    unsigned	    V_keyword	: 1;	/* TRUE if this is keyword */
	    unsigned	    V_iparm     : 1;	/* TRUE if ipp alloc       */ 
	    unsigned 	    V_default 	: 1;	/* TRUE if defaulted	   */
	    unsigned	    V_page      : 1;	/* TRUE if "page start"    */
	    unsigned        V_wasname   : 1;	/* TRUE if "was V_NAME"    */
	    unsigned 	    V_deref     : 1;	/* for compilation:  TRUE if */
						/* v_cvp points to a name  */
						/* of a de-ref'd global    */
	    unsigned        V_pv12	: 1;	/* TRUE if parm var        */
	    					/* post TAE V 1.2 format   */
	    BIT_FIELD_ALIGN
	    struct TUTEXT   *V_tp;		/* tutor extension struct  */
	    } vs_parm;

	struct				/* for global class		*/
	    {
	    BIT_FIELD_ALIGN
	    unsigned	V_protect   : 1;	/* protected from deletion */
	    unsigned	V_intrinsic : 1;	/* internally defined	   */
	    unsigned	V_implicit  : 1;	/* implicitly referenced   */
	    					/* WARNING: the following  */
	    					/*  flags must stay bits   */
	    					/*  for portability	   */
	    unsigned	V_dummy1    : 1;	/* fillers to get V_pv12   */
	    unsigned	V_dummy2    : 1;	/*  to coincide with flag  */
	    unsigned	V_dummy3    : 1;	/*  in other unions        */
	    unsigned    V_pv12	    : 1;	/* TRUE if global created  */
	    					/* post TAE V 1.2	   */
	    BIT_FIELD_ALIGN
	    struct DEFPDF *V_pdf;		/* name of defining PDF    */
	    TINY	V_refcnt;		/* reference count         */
	    } vs_globl;

	struct				/* for local class		   */
	    {  					/* WARNING: the filler	   */
	    					/*  flags must be single bits*/
	    					/*  to insure portability*/
	    BIT_FIELD_ALIGN
	    unsigned	V_dummy1    : 1;	/* fillers to get V_pv12   */
	    unsigned	V_dummy2    : 1;	/*  to coincide with others */
	    unsigned	V_dummy3    : 1;	/*  in other union	   */
	    unsigned	V_dummy4    : 1;
	    unsigned	V_dummy5    : 1;
	    unsigned	V_dummy6    : 1;
	    unsigned    V_pv12	    : 1;	/* TRUE if local var 	   */
	    					/* post TAE V 1.2          */
	    BIT_FIELD_ALIGN
	    COUNT	V_dummy;		/* currently: nothing      */
	    } vs_local;

	} vu_class;

    union 		/* select by direct vs indirect value...	*/
	{
	struct 				/* for 'indirectly' valued 	*/
					/* (type == V_NAME)		*/
	    {
	    struct VARIABLE *V_ref;	/* ultimate reference		*/
				    	/* NULL = no current value	*/
	    struct VARIABLE *V_dref;	/* default ultimate reference	*/
				    	/* NULL = no default		*/
	    TEXT	*V_nref;	/* for compiled procs -- name of ref*/
	    } vs_named;
	    
	struct				/* for 'directly' valued types	*/
	    {
	    COMPACT_COUNT V_count;	/* current vector count:	*/
					/* -1=none; 0=null		*/
	    COMPACT_COUNT V_minc;	/* minimum vector count		*/
	    COMPACT_COUNT V_maxc;	/* maximum vector count		*/
	    UTINY 	V_size;		/* max string size		*/
	    BIT_FIELD_ALIGN
	    unsigned	V_filemode: 2;	/* V_IN, OUT, INOUT, NOCHECK	*/
	    unsigned	V_nullable: 1;	/* TRUE if nullable		*/
	    unsigned    V_file : 1;	/* TRUE if TYPE=FILE		*/
	    unsigned    V_event : 1;	/* TRUE if ACCESS = EVENT	*/
	    BIT_FIELD_ALIGN
	    GENPTR	V_valid;	/* ptr to VALID structure	*/
	    GENPTR	V_cvp;		/* ptr to current value vector	*/
	    COMPACT_COUNT V_dcount;	/* vector count of default	*/
	    GENPTR	V_dvp;		/* ptr to default value vector	*/
				    	/* NULL = no default value	*/
	    struct SYMTAB V_qualst;	/* qualifiers for this parm     */
	    } vs_direct;
	    
	} vu_value;

    };


#define V_INTEGER 	1			/* integer type variable*/
#define V_REAL		2			/* real type variable	*/
#define	V_STRING	3			/* string type variable	*/
#define	V_NAME		4			/* named variable (indirect)*/

#define	V_LOCAL		1			/* local class variable	*/
#define	V_GLOBAL	2			/* global class variable*/
#define	V_PARM		3			/* parameter class	*/

#define	V_NOCHECK 	0			/* do no check on file	*/
#define	V_IN		1			/* input file		*/
#define	V_OUT		2			/* output file		*/
#define	V_INOUT		3			/* open for input /output*/

/* error codes for chk_vector function:			*/

#define SYMTAB_BASE	(-100)
#define V_BADTYPE	(-1)	
#define	V_BADCOUNT	(-2)
#define	V_BADSIZE	(-3)
#define	V_BADVALID	(-4)
#define V_BADFILE	(-5)
#define V_BADAMBIG	(-6)

/*	member re-definitions;  these macros avoid having to explicitly
 *	mention all of the intermediate structures when referencing
 *	a VARIABLE member.
 *	Note: The V_pv12 flags are aligned to coincide so only one
 *	macro, v_pv12, will be used to reference the bit flag in any
 *	variable class.
 */

#define v_keyword	vu_class.vs_parm.V_keyword
#define v_iparm		vu_class.vs_parm.V_iparm
#define v_default	vu_class.vs_parm.V_default
#define v_page		vu_class.vs_parm.V_page
#define v_wasname	vu_class.vs_parm.V_wasname
#define v_deref		vu_class.vs_parm.V_deref
#define v_pv12		vu_class.vs_parm.V_pv12
#define v_tp		vu_class.vs_parm.V_tp

#define v_protect	vu_class.vs_globl.V_protect
#define v_intrinsic	vu_class.vs_globl.V_intrinsic
#define v_implicit	vu_class.vs_globl.V_implicit
#define v_pdf		vu_class.vs_globl.V_pdf
#define v_refcnt	vu_class.vs_globl.V_refcnt

#define v_ref		vu_value.vs_named.V_ref
#define v_dref		vu_value.vs_named.V_dref
#define	v_nref		vu_value.vs_named.V_nref

#define v_count		vu_value.vs_direct.V_count
#define v_minc		vu_value.vs_direct.V_minc
#define v_maxc		vu_value.vs_direct.V_maxc
#define v_size		vu_value.vs_direct.V_size
#define v_filemode	vu_value.vs_direct.V_filemode
#define v_nullable	vu_value.vs_direct.V_nullable
#define v_file		vu_value.vs_direct.V_file
#define v_event		vu_value.vs_direct.V_event
#define v_valid		vu_value.vs_direct.V_valid
#define v_cvp		vu_value.vs_direct.V_cvp
#define v_dcount	vu_value.vs_direct.V_dcount
#define	v_dvp		vu_value.vs_direct.V_dvp
#define v_qualst	vu_value.vs_direct.V_qualst 



/* 	MACROS for manipulating VARIABLEs.
 *
 *	RESOLVE.   Transforms a pointer to a VARIABLE into a pointer
 *	to its ultimate reference if a NAME variable.   Typical use:
 *
 *		v = RESOLVE(v);
 */

#define	RESOLVE(v)	((*v).v_type==V_NAME) ? (*v).v_ref : v

/*
 *	VALUE macros.  The macros here facilitate reference to a value
 *	element of a VARIABLE. "v" is a VARIABLE structure and may be
 *	"indirect", i.e. IVAL(*v, i) is quite common.  "i" is the
 *	index of the element desired.
 *
 *	Before using the VALUE macros, the programmer must have 
 *	determined the type of the VARIABLE.
 *
 */

/*	Current values					*/

#define IVAL(v, i)   (((TAEINT *) (v).v_cvp)[i])
#define RVAL(v, i)   (((TAEFLOAT *) (v).v_cvp)[i])
#define SVAL(v, i)   (((TEXT **) (v).v_cvp)[i])

/* 	Default values					*/

#define DIVAL(v, i)   (((TAEINT *) (v).v_dvp)[i])
#define DRVAL(v, i)   (((TAEFLOAT *) (v).v_dvp)[i])
#define DSVAL(v, i)   (((TEXT **) (v).v_dvp)[i])

/*
 *  For compatibility with old application source code, the
 *  following are still available.
 */

/*	Current values					*/

#define VALUE_I(v, i)   (((TAEINT *) (v).v_cvp)[i])
#define VALUE_R(v, i)   (((TAEFLOAT *) (v).v_cvp)[i])
#define VALUE_S(v, i)   (((TEXT **) (v).v_cvp)[i])

/* 	Default values					*/

#define DVALUE_I(v, i)   (((TAEINT *) (v).v_dvp)[i])
#define DVALUE_R(v, i)   (((TAEFLOAT *) (v).v_dvp)[i])
#define DVALUE_S(v, i)   (((TEXT **) (v).v_dvp)[i])

/********************************************************/

/*
 *	The following block exists in dynamic memory for variables
 *	of class V_GLOBAL.   One block exists for all of the globals
 *	defined in one PDF, so there is a reference count to aid
 *	in "when to de-allocate" the block
 */
    struct DEFPDF
        {
        COUNT	refcount;			/* number of users	*/
        struct FSBLOCK pdf;			/* defining PDF's name	*/
        };


/* range checking structures  */

    struct I_VALID
	{
	COUNT		count;			/* number of ranges	*/
	struct I_RANGE 
	    {
	    TAEINT	low;			/* low value for this range */
	    TAEINT	high;			/* high value for this range*/
	    } range[1];				/* this gets allocated for 
						   count ranges	    */
	};

    struct R_VALID
	{
	COUNT		count;			/* number of ranges	*/
	struct R_RANGE
	    {
	    TAEFLOAT	low;			/* low value for this range */
	    TAEFLOAT	high;			/* high value for this range*/
	    } range[1];				/* this gets allocated for 
	    					   count ranges	*/
	};
    
#define VALIDSIZ	STRINGSIZ		/* max chars in valid string */
#define POINTER_VALIDS				/* select new S_VALID code   */

#ifdef POINTER_VALIDS				/* new way: use pointers */

    struct S_VALID
	{
	COUNT		count;			/* number of "string"	*/
	struct S_RANGE
	    {
	    TEXT	*string;		/* valid string		*/
	    } slist[1];				/* this gets allocated for
	    					   count strings	*/
	};

#else						/* old way:		*/

    struct S_VALID
	{
	COUNT		count;			/* number of "string"	*/
	struct S_RANGE
	    {
	    TEXT	string[VALIDSIZ+1];	/* valid string		*/
	    } slist[1];				/* this gets allocated for
	    					   count strings	*/
	};
#endif



/**********

	The following macros are used to generate
	valid lists for INTRINSICS.   Typical use:

		BEGIN_VALIDS(dumpOptions, 2)
			"dump", "nodump"
		END_VALIDS

	where "dumpOptions" is placed in the valid field
	of a RESIDVAR structure.  The two forms of the 
	macros correspond to the old S_VALID format and 
	the new ("pointer valids") format.

**********/


#ifdef POINTER_VALIDS		/* new valid format */
#define BEGIN_VALIDS(name, n)			\
    static struct 				\
	{					\
        COUNT count;				\
	struct {				\
	       TEXT *string;			\
	       } slist[n];			\
	} name = { n, 				

#define END_VALIDS    };

#else				/* traditional TAE valid format */

#define BEGIN_VALIDS(name, n)			\
    static struct 				\
	{					\
        COUNT count;				\
	struct {				\
	       TEXT string[VALIDSIZ+1];		\
	       } slist[n];			\
	} name = { n,

#define END_VALIDS  };
#endif


#endif
