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


#ifndef EXPINC_INC
#define EXPINC_INC
/* TJH CHECKOUT FILE_TIME=17-DEC-1984 11:38 DUA1:[TAEV1.INC]EXPINC.INC;2 */
/* TJM CHECKOUT FILE_TIME=18-OCT-1983 19:24 DUA0:[TAEV1.INC]EXPINC.INC;17 */
/*TJM         CHECKOUT FILE_TIME=25-MAR-1983 09:17 DMA1:[TAEV1.INC]EXPINC.INC;10 */
/*TJM         CHECKOUT FILE_TIME=28-FEB-1983 09:29 DMA1:[TAEV1.INC]EXPINC.INC;6 */
/*TNHE        CHECKOUT FILE_TIME=20-JAN-1983 15:16 DISK$USERDISK1:[TAEV1.INC]EXPINC.INC;1 */
/*
 * INCLUDE file for expression evaluation package
 *
 * Version 15
 *
 * Change log:
 *
 *	25-Feb-83	Add 'origin' to VALUE structure
 *	27-Feb-83	Make 'realval' TAEFLOAT
 *	21-mar-83	Added constants for logical expressions and increased 
 *			number of lookahead characters allowed in REDENT struct
 *			to 15 from 8...jtm
 *	24-mar-83	Add TCLfunction support...nhe
 *	25-MAR-83	Modified number of legal lookaheads...jtm
 *	11-apr-83	Added new symbol PA_NOT and fixed duplicate
 *			assignment for PA_BASE +21 (PA_CONCAT and EV_CONT)...jtm
 *	12-APR-83	INcreased dimension of REDENT lookahead characters...jtm
 *	22-aug-83	Added new symbol, PA_NULL for nullables...jtm
 *	18-oct-83	New typedef, PSTATE_PTR, so that pointers to PSTATE(i)
 *			have a common structure type name to point to for
 *			unix...palm
 *	14-feb-84	Use COMPACT_COUNT in tables...palm
 *	28-feb-84	Increased dimension of REDENT lookahead char...jtm
 *	16-jun-84	Define constants PA_SUCC, PA_FAIL, PA_EOS...nhe
 *	17-dec-84	TCL 67: NAMESIZ --> F_Q_NAMESIZ in one dimensions...peb
 *       8-apr-85       Add PA_ERRFLOW for overflows / underflows...joh
 *	05-aug-85	Re-structure for optimization...palm
 *	16-aug-85	PA_HIGHCODE should have parens so that the calc
 *			for reduce_map dimension is correct; parens
 *			added to other definitions...palm
 */
#include "dirinc.inc"
    struct VALUE		/* value structure internal to package	*/
    	{
    	CODE	type;		/* V_REAL or V_INTEGER or V_STRING		*/
	TINY	null;		/* If TRUE, then this value is the null value */
    	TINY	marked;		/* if TRUE, then this value is p/o multivalue 	*/
    	TINY 	closed;		/* if TRUE, then this value was in parens	*/
    	TINY	origin;		/* variable  (O_VAR) or constant (O_CON)	*/
    	union
    	    {
    	    TAEINT intval;	/* the value of the constant if integer */
            TAEFLOAT realval;	/* the value of the constant if real	*/
    	    TEXT   *strpt; 	/* the pointer to the value if a name or a string	*/
    	    } uval;
    	};

#define O_VAR 0		/* describes origin of the value: variable or	*/
#define O_CON 1		/* constant				*/
#define O_COMP 3	/* computed				*/
    struct ERRMSG		/* package error messages		*/
    	{
    	TEXT	*msgtext;	/* text of the error message		*/
    	TEXT	*msgkey;	/* message key				*/
    	TEXT	variable[F_Q_NAMESIZ+1];
    	};


/*	Codes for use in STATE tables.
 *
 *	The following PA_BASE should be just above the ASCII
 *	character set so that the reduce_map bit map
 *	is efficiently sized.
 */

#define	PA_BASE	128		/* start right after ASCII set for bit map */

/* outputs from GETPRIM (in addition, we get ASCII characters)		*/

#define PA_NAME	PA_BASE		/* the primitive is a name		*/
#define	PA_NUM	(PA_BASE+1)	/* the primitive is a number		*/
#define	PA_QUOTED (PA_BASE+2)	/* the primitive is a quoted string	*/
#define	PA_ERROR  (PA_BASE+3)	/* error: can't figure it out		*/
#define PA_GE (PA_BASE+13)	/* The TAE '>=' relational operator */
#define PA_LE (PA_BASE+14)	/* The TAE '<=' relational  operator */
#define PA_NE (PA_BASE+15)	/* The TAE '<>' relational  operator */
#define PA_AND (PA_BASE+16)	/* The TAE 'AND' logical operator */
#define PA_OR (PA_BASE+17)	/* The TAE 'OR' logical operator */
#define PA_CONCAT (PA_BASE+21)	/* The TAE '//' operator */
#define PA_NOT (PA_BASE+22)	/* The TAE 'NOT' logical operator */
#define PA_NULL (PA_BASE+24)	/* The TAE '--' nullable flag */
#define PA_ERRFLOW (PA_BASE+25)   /* The TAE underflow / overflow flag */

/*  symbols produced by parser (used in interface with PARSE)	*/
#define	PA_ACCEPT (PA_BASE+4)	/* 'final' parse state			*/

/* outputs from parser()					*/
#define PA_SUCC 1		/* general success		*/
#define PA_FAIL 0		/* general fail			*/
#define PA_EOS	2		/* failed on unexpected EOS	*/

/*  symbols produced via expression reductions				*/
#define	EV_EXP	(PA_BASE+5)	/* Arithmetic expression */
#define	EV_TRM	(PA_BASE+6)	/* Term */
#define EV_FCT	(PA_BASE+7)	/* Factor */
#define EV_FAK	(PA_BASE+8)	/* ? */
#define EV_COM	(PA_BASE+10)	/* Comma expression */
#define EV_REXP (PA_BASE+18)	/* Relational expression */
#define EV_AEXP (PA_BASE+19)	/* And expression */
#define EV_LEXP (PA_BASE+20)	/* Logical expression */
#define EV_CONT (PA_BASE+23)	/* General 'continue */

/* definitions required for the STATE table				*/
#define	NULLST	(-1)		/* implies no state in this position	*/
#define	PA_NOSYM (PA_BASE+9)	/* no symbol in this position		*/

/******** CAUTION: PA_HIGHCODE must be larger than any other PA_ code (because
 ******** it's used to size the reduce_map in the STATE struct)
 ********/
#define PA_HIGHCODE (PA_BASE+31)  /* must be highest possible PA_ code	*/

#define	PA_TABTERM	(-1)	/* Table terminator code:		*/
				/* MUST BE NEGATIVE (for speed)		*/

/* Internal errors generated by 'parser':				*/

#define TMI_BASE	51
#define TMI_XSTATE	TMI_BASE	/* next state undeterminable	*/
#define TMI_OVERPOP	(TMI_BASE+1)	/* state stack underflow	*/
#define TMI_OVERPUSH	(TMI_BASE+2)	/* stack overflow		*/
#define TMI_BADCODE	(TMI_BASE+3)	/* bad PA_ code upon init	*/

/* Internal errors generated by 'getprim':				*/

#define TMI_NO_SPACE	(TMI_BASE+4)	/* no dynamic space		*/

/* Internal errors generated by 'evalexp':				 */

#define TMI_VAL_BASE	3500
#define	TMI_VAL_OVERPOP0	(TMI_VAL_BASE+0)
#define	TMI_VAL_OVERPOP1	(TMI_VAL_BASE+1)
#define	TMI_VAL_OVERPOP2	(TMI_VAL_BASE+2)
#define	TMI_VAL_OVERPOP3	(TMI_VAL_BASE+3)
#define	TMI_VAL_OVERPOP4	(TMI_VAL_BASE+4)
#define	TMI_VAL_OVERPOP5	(TMI_VAL_BASE+5)
#define	TMI_VAL_OVERPOP6	(TMI_VAL_BASE+6)
#define	TMI_VAL_OVERPOP7	(TMI_VAL_BASE+7)
#define	TMI_VAL_OVERPOP8	(TMI_VAL_BASE+8)
#define	TMI_VAL_OVERPOP9	(TMI_VAL_BASE+9)
#define	TMI_VAL_OVERPOP10	(TMI_VAL_BASE+10)
#define	TMI_VAL_OVERPOP11	(TMI_VAL_BASE+11)
#define	TMI_VAL_OVERPOP12	(TMI_VAL_BASE+12)
#define	TMI_VAL_OVERPOP13	(TMI_VAL_BASE+13)
#define	TMI_VAL_OVERPOP14	(TMI_VAL_BASE+14)
#define	TMI_VAL_OVERPOP15	(TMI_VAL_BASE+15)
#define	TMI_VAL_OVERPOP16	(TMI_VAL_BASE+16)
#define	TMI_VAL_OVERPOP17	(TMI_VAL_BASE+17)
#define	TMI_VAL_OVERPOP18	(TMI_VAL_BASE+18)
#define	TMI_VAL_OVERPOP19	(TMI_VAL_BASE+19)
#define TMI_VAL_OVERPUSH	(TMI_BASE+5)
#define TMI_VAL_NO_SPACE	(TMI_BASE+6)
#define TMI_NULL		(TMI_BASE+7)
/*
 * function returns from parser() are SUCCESS, FAIL, and EOSFAIL.  See
 * We assume here that SUCCESS and FAIL are not -1:
 */
#define EOSFAIL 	(-1)

/* 
 * state structures for parsing machine
 */

typedef COMPACT_COUNT PA_CODE;	/* the idea is to get a short for these */
				/* codes to save space			*/

    struct PSTATE		/* state definition			*/
    	{			/* variable length			*/
	TINY	numpop;		/* number of pops on reduce		*/
	PA_CODE	newsymb;	/* new symbol resulting from reduce	*/
	CODE	(*action)(struct ERRMSG**);	/* reduction action			*/
	PA_CODE *redvec;	/* pointer to vector of reduce codes...	*/
				/* terminated by PA_TABTERM		*/
	struct SHFENT *shfent;	/* pointer to shift code structs	*/
	int	reduce_map[PA_HIGHCODE/(sizeof(int)*8) + 1];
				/* bit map of allowable reduce codes	*/
    	};

    struct	SHFENT
    	{
    	PA_CODE	currsymb;	/* required lang symbol of last primitive...*/
    				/*...read in order to shift		*/
				/* a code of PA_TABTERM terminates this	*/
    	COMPACT_COUNT nextstate;/* index to next sate in state pointer table */
    	};


/*  Define the TCL function descriptor		*/

    struct TCLFUNC
	{
	TEXT name[NAMESIZ+1];	/* fn name		*/
	CODE (*fn)(struct VALUE *, struct CONTXT *, struct VALUE *,
	           struct ERRMSG **);	/* fn address		*/
    	COMPACT_COUNT numargs;	/* number of arguments	*/
    	TINY arg_type[4];	/* argument types	*/
    	};
#endif
