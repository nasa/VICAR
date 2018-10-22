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



/* TPAM CHECKOUT FILE_TIME= 9-AUG-1985 11:36 DUA1:[TAEV1.TM]EVALEXP.C;3 */
/* TJH CHECKOUT FILE_TIME= 9-AUG-1984 16:59 DUA1:[TAEV1.TM]EVALEXP.C;1 */
/* TPEB CHECKOUT FILE_TIME= 3-JUL-1984 13:03 DUA0:[TAEV1.TM]EVALEXP.C;70 */
/* TNHE CHECKOUT FILE_TIME=14-MAY-1984 21:01 DUA0:[TAEV1.TM]EVALEXP.C;69 */
/* TNHE CHECKOUT FILE_TIME= 7-MAY-1984 16:00 DUA0:[TAEV1.TM]EVALEXP.C;66 */
/* TJM CHECKOUT FILE_TIME= 9-MAR-1984 17:45 DUA0:[TAEV1.TM]EVALEXP.C;64 */
/* TJM CHECKOUT FILE_TIME= 2-MAR-1984 10:58 DUA0:[TAEV1.TM]EVALEXP.C;62 */
/* TJM CHECKOUT FILE_TIME=25-NOV-1983 14:16 DUA0:[TAEV1.TM]EVALEXP.C;61 */
/* TNHE CHECKOUT FILE_TIME=28-OCT-1983 08:49 DUA0:[TAEV1.TM]EVALEXP.C;58 */
/* TJM CHECKOUT FILE_TIME=11-OCT-1983 15:03 DUA0:[TAEV1.TM]EVALEXP.C;57 */
/*TJM         CHECKOUT FILE_TIME=13-JUL-1983 10:06 DUA0:[TAEV1.TM]EVALEXP.C;52 */
/*TDM         CHECKOUT FILE_TIME=11-JUL-1983 16:35 DMA1:[TAEV1.TM]EVALEXP.C;51 */
/*	TCL Expression Evaluator
 *
 *	CHANGE LOG:
 *
 *
 *	12-jul-83	Free strings on valstack in equ_act(), 
 *			nequ_act()...dm
 *
 *	24-aug-83	Added new logic and states for nullables...jtm
 *	08-sep-83	Fixed a dimension at STRINGSIZ (+1)...palm
 *	11-oct-83	Fixed UNIX compilation errors...palm
 *	28-oct-83	Fixed error on assignment of variable
 *			with no value...jtm
 *	07-nov-83	Fixed indexact and dealstr for deall problems...nhe
 *	23-nov-83	PR 589...nhe
 *	25-nov-83	More PR 589 fixing...nhe
 *	28-feb-84	Allow nulls in expressions..jtm
 *	07-mar-84	Set type of variable in tmpval if null 
 *			value (PR#680)...tjm
 *	08-mar-84	NOT operator not legal for reals (more PR589)...jtm
 *	27-mar-84	Fixed LET R = "hello" problem (no error)...jtm	
 *	14-may-84	Catch null values in functions...nhe
 *	16-jun-84	Make better error messages...nhe
 *	13-jul-84	More specific EMs for action routines (PR396)...peb
 *	15-feb-85	Fix for real = logical expressions (PR940)...joh
 *       8-apr-85       Add underflow and overflow (PR954)...joh
 *	09-aug-85	Avoid stuffing a static string in badop_em...palm
 *	09-aug-85	Optimization of reduce and new table structures ... palm
 *	06-mar-86	Removed erroneous assignment in a if statement in 
 *			getvarval() - problem found at JPL...lia
 *	30-may-86	Fix bug whereby nested functions don't work because
 *			VALUE.null was uninitialized and happed to be TRUE
 *			so $FIX($COUNT(X)) gave TAE-NULLARG...palm
 *	27-sep-87	Negact no longer returns mixvar on real (let packout
 *			do it...tnhe
 *	12-feb-88	PR1490: Change message for CONCNULL...ljn
 *	03-feb-89	Fix TINY that holds vector count (mulcount)...palm
 *	11-sep-89	fix negact so that let i = -2147483648 works,
 *			i.e, assign to int after TAEFLOAT is negated...palm 
 *      27-oct-89	fix let r = -2.0 ...palm
 *	17-jan-90	check for integer expression divide by zero was
 *			checking the divisor prior to converting from real..krw
 */
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"syninc.inc"	/* Syntax package			*/
#include	"expinc.inc"	/* For expression evaluation		*/
#include	"tminc.inc"	/* For TM				*/
#include "taeintproto.h"


FUNCTION VOID  badop_em 
(
    struct ERRMSG	*(*errmsg),	/* out: error message constructed	*/
    CODE		type,		/* in:  data type (from VARIABLE)	*/
    TEXT		operation[]	/* in:  describes attempted operation	*/

 );
static FUNCTION VOID dealstrval
(
    struct VALUE	*value		/* in: value with possible string pointer */

 );
static FUNCTION CODE packout
(
    GENPTR		*value,		/* out: the value to pack	*/
    struct ERRMSG	*(*errmsg),	/* out: error message		*/
    COUNT		*count		/* out: number of values	*/

/* Get the output values from the stack. Allocate space accordingly	*/
 );


#ifndef testmain
    GLOBAL	COUNT	v129eval = 0;	/* Source version number	*/

#define TYPE_DEPTH 10
    TINY		type_stack[TYPE_DEPTH+STACK_OVRHD];
    						/* type stack, for arith ops */
    struct CONTXT	*expcontxt;		/* proc context block	*/
    BOOL		commaterm;		/* true if we end in a comma */
    BOOL		mulvalflag;		/* true if multi-valued	*/
    COUNT		mulcount;		/* number of values	*/

/*	Error Messages	*/

    static struct ERRMSG	er_badar = 
      {"Arithmetic operation yields overflow or underflow.",
       "TAE-ARIERR", {EOS}};
    static struct ERRMSG	er_toob = 
    		{"Subscript too large for variable '%s'.",
		 "TAE-BIGSUBS", {EOS}};
    static struct ERRMSG	er_mulvar = 
    		{"Numeric or string operation using multiple values.",
		 "TAE-MULVAR", {EOS}};
    static struct ERRMSG	er_badsub = 
    		{"The subscript for '%s' is not a positive integer.",
		 "TAE-INVSUBS", {EOS}};
    static struct ERRMSG	er_nonex = 
    		{"Reference to undefined variable '%s'.",
		 "TAE-UNDEFVAR", {EOS}};
    static struct ERRMSG	er_mixvar = 
    		{"Mixed data types.",
		 "TAE-MIXVAR", {EOS}};
    static struct ERRMSG	er_divzero = 
    		{"Divide by zero attempted.",
		 "TAE-DIVZERO", {EOS}};
    static struct ERRMSG	er_nestmul = 
    		{"Multivalued expression within another multivalued expression.",
		 "TAE-NESTMUL", {EOS}};
    static struct ERRMSG	er_novalue =
    		 {"Variable '%s' has no current value.",
		  "TAE-NOVAL", {EOS}};
    static struct ERRMSG	er_num_null =
    		 {"Null values not permitted in numeric expressions.",
		  "TAE-NUMNULL", {EOS}};
    static struct ERRMSG	er_log_null =
    		 {"Null values not permitted in logical expressions.",
		  "TAE-LOGNULL", {EOS}};
    static struct ERRMSG	er_rel_null =
    {"Null values not permitted in relational expressions, except '=' and '<>'.",
     "TAE-RELNULL", {EOS}};

    static struct ERRMSG	er_conc_null =
    {"Concatenation of null values not permitted.",
     "TAE-CONCNULL", {EOS}};

    static struct ERRMSG	er_nullelement =
    		 {"Null value not permitted as an element in a list.",
    					"TAE-NULLELEM", {EOS}};

    static struct ERRMSG	er_nullf =
    		 {"Null value not permitted as an argument to %s.",
    					"TAE-NULLARG", {EOS}};


/*	State tables for the parse					*/

/* NOTE:  to get a trace of state transitions, use the debug command
 *	  SET BREAK %LINE GOTOSTATE\nnn DO (EX NEXT; GO)
 *
 *	  where 'nnn' is the line number after the line in PARSER's GOTOSTATE
 *	  in which NEXT is set.
 */

/* Definitions for state pointer indexes		*/

#define s0pt	0
#define s1pt	1
#define s2pt	2
#define s3pt	3
#define	s4pt	4
#define	s5pt	5
#define	s6pt	6
#define	s7pt	7
#define	s8pt	8
#define	s9pt	9
#define	s10pt	10
#define	s11pt	11
#define	s12pt	12
#define	s13pt	13
#define s14pt	14
#define s15pt	15
#define s16pt	16
#define s17pt	17
#define s18pt	18
#define s19pt	19
#define s20pt	20
#define s21pt	21
#define s22pt	22
#define s23pt	23
#define s24pt	24
#define	s25pt	25
#define	s26pt	26
#define s27pt   27
#define	s28pt	28
#define	s29pt	29
#define	s30pt	30
#define s31pt	31
#define s32pt	32
#define s33pt	33
#define s34pt	34
#define s35pt	35
#define s36pt	36
#define s37pt	37
#define s38pt	38
#define s39pt	39
#define s40pt	40
#define s41pt	41
#define s42pt	42
#define s43pt	43
#define s44pt	44
#define s45pt	45
#define s46pt	46
#define s47pt	47
#define s48pt	48
#define s49pt	49
#define s50pt	50
#define s51pt	51
#define s52pt	52
#define s53pt	53
#define s23apt	54			/* special; to force 'goto' action	*/

/*  	Declarations for action routines					*/
    CODE	comact(struct ERRMSG **), conc_act(struct ERRMSG **), divact(struct ERRMSG **), dunact(struct ERRMSG **), indexact(struct ERRMSG **), lpact(struct ERRMSG **), 
    		mulact(struct ERRMSG **), negact(struct ERRMSG **), paract(struct ERRMSG **), subact(struct ERRMSG **), sumact(struct ERRMSG **), termact(struct ERRMSG **), 
    		varact(struct ERRMSG **), or_act(struct ERRMSG **), and_act(struct ERRMSG **), ge_act(struct ERRMSG **), nequ_act(struct ERRMSG **), equ_act(struct ERRMSG **),
		less_act(struct ERRMSG **), gtr_act(struct ERRMSG **), lequ_act(struct ERRMSG **), notact(struct ERRMSG **);

/*	First time flag to cause tables to be initialized	*/

    static BOOL	first_time = TRUE;

/* state 0 	V('')	*/

    static struct SHFENT shift0[] = {
      {EV_LEXP, s1pt},
      {EV_AEXP, s34pt},
      {EV_REXP, s35pt},
      {EV_EXP,  s36pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      { '(',    s37pt},
      {PA_NULL, s4pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT,	 s20pt},
      {PA_TABTERM, 0}};

    static struct PSTATE s0 = {0, 0, NULL, NULL, shift0};


/* state 1 	V(EV_LEXP)	*/
    static PA_CODE red1[] = {EOS, PA_TABTERM};  /* reduction for 'accept' */
    static struct SHFENT shift1[] = {
      {',', s2pt},
      {PA_OR, s38pt},
      {PA_TABTERM, 0}};

    static struct PSTATE s1 = {1, PA_ACCEPT, dunact, red1, shift1};

/* state 2 */		/* Final state if terminated by a comma EOS */
    static PA_CODE red2[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', '(', ')', EOS,
					PA_NUM,	PA_QUOTED, PA_NAME,
					PA_NULL, PA_TABTERM
    			    };

    static struct PSTATE s2 = {1, PA_ACCEPT, termact, red2, NULL};

/* state 3 */
    static PA_CODE red3[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL, 
    					PA_TABTERM
    			    };

    static struct PSTATE s3 = {1, EV_TRM, NULL, red3, NULL};

/* state 4 V(PA_NULL)	*/

    static PA_CODE red4[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL, 
    					PA_TABTERM
    			    };

    static struct PSTATE s4 = {1, EV_FCT, NULL, red4, NULL};

/* state 5 */
    static PA_CODE red5[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			    };

    static struct PSTATE s5 = {1, EV_FCT, NULL, red5, NULL};

/* state 6 */			/* Here on addition	*/
    static struct SHFENT shift6[] = {
      {EV_TRM, s10pt},
      {     EV_FCT, s3pt},
      {    PA_NUM, s5pt},
      {     PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {     '-',    s16pt},
      {   '+',    s14pt},	/* to s14 for unary +	*/
      {'(',    s37pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM, 0}
    			     };

    static struct PSTATE s6 = {0, 0, NULL, NULL, shift6};

/* state 7 */			/* REMOVED */

/* state 8 */
    static struct SHFENT shift8[] = {
      {EV_FCT, s13pt},
      {'(',    s37pt},
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM, 0}
    			     };

    static struct PSTATE s8 = {0, 0, NULL, NULL, shift8};

/* state 9 */
    static PA_CODE red9[] = {
					'+', '-', 
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			     };


    static struct SHFENT shift9[] = {
      {'*',   s8pt},
      {'/',   s21pt},
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s9 = {1, EV_EXP, NULL, red9, shift9};


/* state 10 */
    static PA_CODE red10[] = {
					'+', '-', 
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			     };


    static struct SHFENT shift10[] = {
      {'*', s8pt},
      {	'/', s21pt},
      {PA_TABTERM, 0}
    				    };

    static struct PSTATE s10 = {3, EV_EXP, sumact, red10, shift10};


/* state 11 */
    static PA_CODE red11[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s11 = {3, EV_FCT, paract, red11, NULL};

/* state 12 	V('NOT' FCT)	*/
    static PA_CODE red12[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s12 = {2, EV_FCT, notact, red12, NULL};

/* state 13 */
    static PA_CODE red13[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s13 = {3, EV_TRM, mulact, red13, NULL};

/* state 14 */
    static struct SHFENT shift14[] = {
      {EV_FCT, s15pt},
      {'(',    s37pt},
      {PA_NUM, s15pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s14 = {0, 0, NULL, NULL, shift14};

/* state 15 */				/* For unary plus */
    static PA_CODE red15[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s15 = {2, EV_FCT, NULL, red15, NULL};

/* state 16 */
    static struct SHFENT shift16[] = {
      {EV_FCT, s17pt},
      {'(',    s37pt},
      {PA_NUM, s17pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s16 = {0, 0, NULL, NULL, shift16};

/* state 17 */				/* For unary minus */
    static PA_CODE red17[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s17 = {2, EV_FCT, negact, red17, NULL};

/* state 18 */			/* Here on subtraction	*/
    static struct SHFENT shift18[] = {
      {EV_TRM, s19pt},
      {EV_FCT, s3pt},
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},	/* to s14 for unary +	*/
      {'(',    s37pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s18 = {0, 0, NULL, NULL, shift18};

/* state 19 */
    static PA_CODE red19[] = {
					'+', '-', 
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};


    static struct SHFENT shift19[] = {
      {'*', s8pt},
      {'/', s21pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s19 = {3, EV_EXP, subact, red19, shift19};

/* state 20 	V( 'NOT')	*/
    static struct SHFENT shift20[] = {
      {EV_FCT, s12pt},
      {'(',    s37pt},
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s20 = {0, 0, NULL, NULL, shift20};

/* state 21 */					/* Here on divide	*/
    static struct SHFENT shift21[] = {
      {EV_FCT, s22pt},
      {'(',    s37pt},
      {'/',    s32pt},	/* actually a concat	*/
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s21 = {0, 0, NULL, NULL, shift21};

/* state 22 */
    static PA_CODE red22[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s22 = {3, EV_TRM, divact, red22, NULL};

/* state 23 */			/* Here on a name */
    static PA_CODE red23[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift23[] = {
      {'(', s23apt},
      {EV_CONT, s24pt},	/* subscript or function */
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s23 = {1, EV_FCT, varact, red23, shift23};

/* state 23a */			/* Here to force an action routine only */
    static PA_CODE red23a[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS,
					PA_NUM, PA_NAME, PA_QUOTED, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s23a = {1, EV_CONT, lpact, red23a, NULL};

/* state 24 */
    static struct SHFENT shift24[] = {
      {EV_EXP, s25pt},
      {EV_TRM, s9pt},
      {EV_FCT, s3pt},
      {'(',    s37pt},
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},
      {PA_TABTERM, 0}
    				};

    static struct PSTATE s24 = {0, 0, NULL, NULL, shift24};

/* state 25 */
    static struct SHFENT shift25[] = {
      {')', s26pt},
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s25 = {0, 0, NULL, NULL, shift25};

/* state 26 */			/* Here with evaluated subscript	*/
    static PA_CODE red26[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			     };

    static struct PSTATE s26 = {4, EV_FCT, indexact, red26, NULL};

/* state 27 	V( '(' EV_COM)	*/
    static struct SHFENT shift27[] = {
      {',', s28pt},
      {')', s11pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s27 = {0, 0, NULL, NULL, shift27};

/* state 28 */
    static struct SHFENT shift28[] = {
      {EV_LEXP, s30pt},
      {EV_AEXP, s34pt},
      {EV_REXP, s35pt},
      {EV_EXP, s36pt},
      {EV_TRM, s9pt},
      {EV_FCT, s3pt},
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},	/* to s14 for unary +	*/
      {'(',    s37pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s28 = {0, 0, NULL, NULL, shift28};    

/* state 29 */
    static PA_CODE red29[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			     };

    static struct SHFENT shift29[] = {
      {PA_OR,  s38pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s29 = {1, EV_COM, NULL, red29, shift29};

/* state 30 */
    static PA_CODE red30[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_NOT,
					',', ')', EOS, PA_NULL,
					PA_TABTERM
				};

    static struct SHFENT shift30[] = {
      {PA_OR,  s38pt},
      {PA_TABTERM,0}
				};

    static struct PSTATE s30 ={3, EV_COM, comact, red30, shift30};

/* state 31 */		/* reduce on anything so action routines catch errors */
    static PA_CODE red31[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s31 = {1, EV_FCT, NULL, red31, NULL};

/* state 32 */
    static struct SHFENT shift32[] = {
      {EV_FCT, s33pt},
      {PA_NUM, s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME,s23pt},
      {'-',    s16pt},
      {'+',    s14pt},	/* to s14 for unary +	*/
      {'(',    s37pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s32 = {0, 0, NULL, NULL, shift32};

/* state 33 */
    static PA_CODE red33[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct PSTATE s33 = {4, EV_TRM, conc_act, red33, NULL};

/* state 34 	V(EV_AEXP) */
    static PA_CODE red34[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift34[] = {
      {PA_AND,  s39pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s34 = {1, EV_LEXP, NULL, red34, shift34};

/* state 35	V(EV_REXP)	 */
    static PA_CODE red35[] = {
					'+', '-', '*', '/',
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift35[] = {
      {PA_LE,  s40pt},
      {PA_GE,  s41pt},
      {PA_NE,  s42pt},
      {'=',	s43pt},
      {'<',	s44pt},
      {'>',	s45pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s35 = {1, EV_AEXP, NULL, red35, shift35};

/* state 36	V(EV_EXP)	*/
    static PA_CODE red36[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift36[] = {
      {'+',	s6pt},
      {'-',	s18pt},
      {PA_TABTERM,0}
    				};

    static struct PSTATE s36 = {1, EV_REXP, NULL, red36, shift36};

/* state 37	V('(')	*/
    static struct SHFENT shift37[] = {
      {EV_COM, s27pt},
      {EV_LEXP, s29pt},
      {EV_AEXP, s34pt},
      {EV_REXP, s35pt},
      {EV_EXP, s36pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE  s37 = {0, 0, NULL, NULL, shift37};

/* state 38	V(EV_LEXP 'OR')	*/
    static struct SHFENT shift38[] = {
      {EV_AEXP, s46pt},
      {EV_REXP, s35pt},
      {EV_EXP,  s36pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT,	 s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s38 = {0, 0, NULL, NULL, shift38};

/* state 39	V(EV_AEXP 'AND')	*/
    static struct SHFENT shift39[] = {
      {EV_REXP, s47pt},
      {EV_EXP,  s36pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s39 = {0, 0, NULL, NULL, shift39};

/* state 40	V(EV_REXP '<=')	*/
    static struct SHFENT shift40[] = {
      {EV_EXP, s48pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s40 = {0, 0, NULL, NULL, shift40};			

/* state 41	V(EV_REXP '>=')	*/
    static struct SHFENT shift41[] = {
      {EV_EXP, s49pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s41 = {0, 0, NULL, NULL, shift41};

/* state 42	V(EV_REXP '<>')	*/
    static struct SHFENT shift42[] = {
      {EV_EXP, s50pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};
    static struct PSTATE s42 = {0, 0, NULL, NULL, shift42};

/* state 43	V(EV_REXP '=')	*/
    static struct SHFENT shift43[] = {
      {EV_EXP, s51pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s43 = {0, 0, NULL, NULL, shift43};

/* state 44	V(EV_REXP '<')	*/
    static struct SHFENT shift44[] = {
      {EV_EXP, s52pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM}
    			};

    static struct PSTATE s44 = {0, 0, NULL, NULL, shift44};

/* state 45	V(EV_REXP '>')	*/
    static struct SHFENT shift45[] = {
      {EV_EXP, s53pt},
      {'(',	 s37pt},
      {EV_TRM,  s9pt},
      {EV_FCT,  s3pt},
      {PA_NUM,	 s5pt},
      {PA_QUOTED, s31pt},
      {PA_NAME, s23pt},
      {'-',     s16pt},
      {'+',     s14pt},
      {PA_NOT, s20pt},
      {PA_NULL, s4pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s45 = {0, 0, NULL, NULL, shift45};

/* state 46	V(EV_LEXP 'OR' EV_AEXP)	*/
    static PA_CODE red46[] = {
					'+', '-', '*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    					};

    static struct SHFENT shift46[] = {
      {PA_AND, s39pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s46 = {3, EV_LEXP, or_act, red46, shift46};

/* state 47	V(EV_AEXP 'AND' EV_REXP)	*/
    static PA_CODE red47[] = {
					'+', '-', '*', '/',
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			};

    static struct SHFENT shift47[] = {
      {PA_LE, s40pt},
      {PA_GE, s41pt},
      {PA_NE, s42pt},
      {'=',   s43pt},
      {'<',   s44pt},
      {'>',   s45pt},
      {PA_TABTERM,0}
    			}; 

    static struct PSTATE s47 = {3, EV_AEXP, and_act, red47, shift47};

/* state 48	V(EV_REXP '<=' EV_EXP)	*/
    static PA_CODE red48[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift48 []= {
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s48 = {3, EV_REXP, lequ_act, red48, shift48};

/* state 49	V(EV_REXP '>=' EV_EXP)	*/
    static PA_CODE red49[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift49[] = {
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s49 = {3, EV_REXP, ge_act, red49, shift49};

/* state 50	V(EV_REXP '<>' EV_EXP)	*/
    static PA_CODE red50[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift50[] = {
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s50 = {3, EV_REXP, nequ_act, red50, shift50};

/* state 51	V(EV_REXP '=' EV_EXP)	*/
    static PA_CODE red51[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift51[] = {
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s51 = {3, EV_REXP, equ_act, red51, shift51};

/* state 52	V(EV_REXP '<' EV_EXP)	*/
    static PA_CODE red52[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    			};

    static struct SHFENT shift52[] = {
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    			};

    static struct PSTATE s52 = {3, EV_REXP, less_act, red52, shift52};

/* state 53	V(EV_REXP '>' EV_EXP)	*/
    static PA_CODE red53[] = {
					'*', '/',
					'=', '>', '<', 
					PA_LE, PA_GE, PA_NE, 
					PA_AND, PA_OR, PA_NOT,
					',', ')', EOS, PA_NULL,
    					PA_TABTERM
    				};

    static struct SHFENT shift53[] = {
      {'+', s6pt},
      {'-', s18pt},
      {PA_TABTERM,0}
    			};


    static struct PSTATE s53 = {3, EV_REXP, gtr_act, red53, shift53};

/* 	List of state pointers						*/
    static struct PSTATE *statelist[] = {
	        &s0, &s1,   &s2, 
		&s3, &s4,   &s5, 
		&s6,         NULL,  &s8,
		&s9,  &s10, &s11, 
		&s12, &s13, &s14, &s15,
		&s16, &s17, &s18, &s19, 
		&s20, &s21, &s22,
		&s23, &s24, &s25, &s26, 
		&s27, &s28, &s29, 
		&s30, &s31, &s32, &s33, 
		&s34, &s35, &s36,
		&s37, &s38, &s39, &s40, 
		&s41, &s42, &s43,
		&s44, &s45, &s46, &s47, 
		&s48, &s49, &s50,
		&s51, &s52, &s53, &s23a};

/* Note: NULLs correspond to deleted states awaiting general cleanup	*/


/*
 *	Evaluate a TCL expression.
 *
 * NOTE on integer arithmetic: these routines convert the new 
 * value to an integer BEFORE doing the arithmetic function.
 *
 * NOTE: this module uses the tm utility tmierr to generate error messages.
 *
 * See the end of this source for TBD's.
 */

FUNCTION CODE evalexp
(
    struct SYNBLK	*synblk,	/* in: syntax block		*/
    struct CONTXT	*contxt,	/* in: proc context block	*/
    CODE		type,		/* in: integer, string, or real */
    GENPTR		*value,		/* out: pointer to the result, NULL if fail 	*/    
    COUNT		*count,		/* out: number of outputs	*/
    CODE		*term		/* out: terminator: EOS or S_COMSEP */

 )
    {

/* Let the parser and the action routines do all the work		*/

    CODE		code;
    struct VALUE	tmpval;
    struct ERRMSG	*errmsg;
    

    if (first_time) 
        {
        first_time = FALSE;
    	parser_init (statelist, sizeof(statelist)/sizeof(struct PSTATE *));
        }
    commaterm = FALSE;			/* will be set true if comma terminator */
    mulvalflag = FALSE;
    mulcount = 0;			/* number of values		*/
    stk_in(type_stack, TYPE_DEPTH, type);	/* init type stack	*/
    expcontxt = contxt;			/* save the context block pointer	*/
    code = parser(synblk, statelist, s0pt, &errmsg);
    if (mulvalflag == FALSE) mulcount = 1;
    *value = NULL;			/* set neutral result		*/
    if (code == PA_SUCC)
    	code = packout(value, &errmsg, count);		/* pack the output value from the stack */
    if (code != PA_SUCC)
    	{
	if (code==PA_EOS)
	  tmmsg(PROCFAIL, "Unexpected end of line.", "TAE-UNEXEOL",0,0,0,0,0);
	else if (code==PA_ERRFLOW)
	  tmmsg(PROCFAIL, er_badar.msgtext, er_badar.msgkey,0,0,0,0,0);
	else if (errmsg != NULL)
	    if ((*errmsg).variable == EOS)
	      tmmsg(PROCFAIL, (*errmsg).msgtext, (*errmsg).msgkey,0,0,0,0,0);
    	    else
    		tmmsg(PROCFAIL, (*errmsg).msgtext, (*errmsg).msgkey, 
		      (uintptr_t) (*errmsg).variable, 0, 0, 0, 0);
        else
    	    {
    	    (*synblk).errchr = (*synblk).curchr - 1;	/* set err location	*/
    	    synerr(synblk, "Unexpected characters in expression");	/* form gen'l err msg	*/
    	    tmmsg(PROCFAIL, (*synblk).errmsg, "TAE-EXPR",0,0,0,0,0);
    	    }
    	}
    while (popvstack(&tmpval) != FAIL)	/* make sure we deallocate any remaining strings */
    	if (tmpval.type == V_STRING)
    	    s_free(tmpval.uval.strpt);
    if (commaterm==TRUE)		/* set terminator indicator	*/
    	*term = S_COMSEP;
    else
    	*term = EOS;
    return(code);
    }

/*
 * and_act - action routine to compute the logical AND
 *	     of the two values on the top of the value stack.
 */
FUNCTION CODE and_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Perform the logical AND of the two values and put the 		*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP0);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_log_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    badop_em(errmsg, V_STRING, "Logical \'AND\'");
	    code = FAIL;
	    goto exit;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval && tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval && tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval && tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval && tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	if (expr_type == V_REAL)           /* If the expression is a real */
	    {				   /* make the value a real. */
	    if (tmpval1.uval.intval == 1) 
		tmpval1.uval.realval = 1.;
	    else
		tmpval1.uval.realval = 0.;
	    tmpval1.type = V_REAL;
	    }
	else	
    	    tmpval1.type = V_INTEGER;
	}    	
    else
	{
	badop_em(errmsg, expr_type, "Logical \'AND\'");
	code = FAIL;
	goto exit;
	}
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }



/*
 * badop_em - build error message for "bad operation for this data type".
 *
 */
FUNCTION VOID  badop_em 
(
    struct ERRMSG	*(*errmsg),	/* out: error message constructed	*/
    CODE		type,		/* in:  data type (from VARIABLE)	*/
    TEXT		operation[]	/* in:  describes attempted operation	*/

 )
    {
    static TEXT	  string[STRINGSIZ+1];
    static struct ERRMSG	er_badop = 
      {string, "TAE-BADOP", {EOS}};

/* First build the EM text in local buffer	*/

    s_copy(operation, er_badop.msgtext);
    s_append(" not appropriate for ", er_badop.msgtext);
    if (type == V_INTEGER)
	s_append("integer", er_badop.msgtext);
    else if (type == V_REAL)
	s_append("real", er_badop.msgtext);
    else if (type == V_STRING)
	s_append("string", er_badop.msgtext);
    else if (type == V_NAME)
	s_append("name", er_badop.msgtext);
    s_append(" variable type.", er_badop.msgtext);

/* Return as output argument	*/

    *errmsg = &er_badop;
    return;
    }


/*
 * comact - action routine upon seeing a value after a comma
 *
 */
FUNCTION CODE comact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct VALUE	tmpval1, tmpval2;
    FAST	CODE 	code;	

    popvstack(&tmpval1);		/* pop the top value and mark it	*/
    if (tmpval1.marked == TRUE)		/* new values pushed on stack but... */
    	{				/* not marked until this routine     */
    	*errmsg = &er_nestmul;		/* indicates we're here as a result of nesting */
    	dealstrval(&tmpval1);
    	return(FAIL);
    	}    
    popvstack(&tmpval2);
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_nullelement;
	code = FAIL;
	return (FAIL);
	}
   if (tmpval2.marked==TRUE && tmpval2.closed==TRUE) 
    	{
    	*errmsg = &er_nestmul;
    	dealstrval(&tmpval2);
    	return(FAIL);
    	}    
    pushvstack(&tmpval2);
    tmpval1.marked = TRUE;
    pushvstack(&tmpval1);
    if (mulvalflag == FALSE)
    	mulcount = 2;		/* account for first and second after paren */
    else
        mulcount++;			/* increment the value count		*/
    mulvalflag = TRUE;
    return(SUCCESS);

    }

/*
 * conc_act - action routine to concatenate two strings
 *
 */

FUNCTION CODE conc_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct VALUE	tmpval1, tmpval2;
    CODE		code;
    TEXT		*txtptr;

    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP1);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_conc_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (tmpval1.type!=V_STRING || tmpval2.type!=V_STRING)
    	{
    	*errmsg = &er_mixvar;
    	code = FAIL;
    	goto exit;
    	}
    txtptr = tae_alloc(1, s_length(tmpval1.uval.strpt)+s_length(tmpval2.uval.strpt)+1);
    if (txtptr == NULL)
	{
	tmierr(TMI_VAL_NO_SPACE);
	return(FAIL);
	}
    s_copy(tmpval2.uval.strpt, txtptr);		/* do the concatenation	*/
    s_append(tmpval1.uval.strpt, txtptr);
    s_free(tmpval1.uval.strpt);			/* deallocate		*/
    s_free(tmpval2.uval.strpt);
    tmpval1.uval.strpt = txtptr;	/* tmpval1 now points to concatenation */
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);	/* put the answer back		*/
#if 0	/* Can't free a local var!  Maybe meant dealstrval() but who knows */
        /* (rgd 2010/02/05) */
    if (code == FAIL)			/* NOTE: if push fails, we assume user tried ...*/
    	tae_free(&tmpval1);		/* ...too many nesting levels.		*/
#endif
    return(code);		
    				

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);
    }

/*
 * dealstrval - deallocate a value if it's a string
 *
 */
static FUNCTION VOID dealstrval
(
    struct VALUE	*value		/* in: value with possible string pointer */

 )
    {
    if ((*value).type == V_STRING || (*value).type == V_NAME)
    	s_free((*value).uval.strpt);
    return;
    }

/*
 * divact - action routine to divide two values
 *
 */
FUNCTION CODE divact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/


/* Divide the value at the top of the value stack into the value next 	*/
/* down on the stack, placing the result back on the stack.		*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/
    CODE		typeflag;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP2);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_num_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (tmpval1.type == V_STRING || tmpval2.type == V_STRING)
	{
	*errmsg = &er_mixvar;
	code = FAIL;
	goto exit;
	}
    if (expr_type==V_REAL || tmpval1.type == V_REAL || tmpval2.type == V_REAL)
	{				/* if either a real then use real arith	*/
	typeflag = V_REAL;
	if (tmpval1.type == V_INTEGER)
	    if (tmpval1.origin != O_CON)
		tmpval1.uval.realval = tmpval1.uval.intval;	/* convert	*/
	if (tmpval2.type == V_INTEGER)
	    if (tmpval2.origin != O_CON)
		tmpval2.uval.realval = tmpval2.uval.intval;	/* convert	*/
    	if ((tmpval1.type == V_INTEGER && tmpval1.origin==O_VAR)
    	   ||(tmpval2.type == V_INTEGER && tmpval2.origin==O_VAR))
    	    {				/* can mix real and integer constants...*/
    	    *errmsg = &er_mixvar;	/* but not real and int variables */
	    code = FAIL;
	    goto exit;
    	    }
    	if (tmpval1.uval.realval == 0)
    	    {
	    code = FAIL;
	    goto exit;
    	    }
    	code = fl_div(tmpval2.uval.realval,  tmpval1.uval.realval, &tmpval1.uval.realval);
    	if (code != SUCCESS)
    	    {
    	    *errmsg = &er_badar;	/* overflow or underflow	*/
    	    goto exit;
    	    }
    	}
    else if (expr_type==V_INTEGER)
	{
	typeflag = V_INTEGER;
	if (tmpval1.origin==O_CON)		/* if constant, then was real	*/
	    {
	    code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
    	if (tmpval1.uval.intval == 0)
    	    {
    	    *errmsg = &er_divzero;
    	    code = FAIL;
    	    goto exit;
    	    }
	if (tmpval2.origin==O_CON)
	    {
	    code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
	code = int_div(tmpval2.uval.intval,  tmpval1.uval.intval, &tmpval1.uval.intval);
	if (code != SUCCESS)
	    {
	    *errmsg = &er_badar;	/* overflow or underflow	*/
	    goto exit;
	    }
	}
    else
    	{
	badop_em(errmsg, expr_type, "Division");
    	code = FAIL;
    	goto exit;
    	}
    tmpval1.type = typeflag;
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * dunact - completion processing
 */
FUNCTION CODE dunact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* for now, do nothing							*/
 )
    {
    return(SUCCESS);
    }

/*
 * equ_act - action routine to determine if the values on the value stack
 *	     are equal.
 */
FUNCTION CODE equ_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compare the two values at the top of the value stack, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/
    BOOL		strequ;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP3);
	return(FAIL);
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE) ||
	(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
	if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* At least one is null */
	    {
	    if (tmpval1.origin == O_VAR && tmpval2.origin == O_VAR)
		{
		if (tmpval1.type != tmpval2.type)	/* if not constants, */
		    {					/* type must match */
		    *errmsg = &er_mixvar;
		    code = FAIL;
		    goto exit;
		    }
		}
	    dealstrval (&tmpval1);			/* Get rid of strings */
	    dealstrval (&tmpval2);
	    if (tmpval1.null==TRUE && tmpval2.null==TRUE)
		tmpval1.uval.intval = 1;		/* Both null, hence equal */
	    else
		tmpval1.uval.intval = 0;			
	    tmpval1.null = FALSE;			  /* No longer null */
	    goto done;
	    }
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    strequ = s_equal (tmpval1.uval.strpt, tmpval2.uval.strpt);
    	    dealstrval(&tmpval1);		/* deallocate strings	*/
    	    dealstrval(&tmpval2);
	    if (strequ)				/* strings equal	*/
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval == tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval == tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval == tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval == tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	}
    else
	{
	badop_em(errmsg, expr_type, "Comparison");
	code = FAIL;
	goto exit;
	}
done:
    if (expr_type == V_REAL)		/* If the expression is a real */
	{				/* make the value a real. */
	if (tmpval1.uval.intval == 1) 
	    tmpval1.uval.realval = 1.;
	else
	    tmpval1.uval.realval = 0.;
	tmpval1.type = V_REAL;
	}
    else	
    	tmpval1.type = V_INTEGER;
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    return(code);
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* error exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * ge_act - action routine to determine if a value is greater than or
 *	    equal to another value.
 */
FUNCTION CODE ge_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compare the two values at the top of the value stack to the new value, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP4);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_rel_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    badop_em(errmsg, V_STRING, "Comparison");
	    code = FAIL;
	    goto exit;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval >= tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval >= tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval >= tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval >= tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
    	if (expr_type == V_REAL)           /* If the expression is a real */
	    {				   /* make the value a real. */
	    if (tmpval1.uval.intval == 1) 
		tmpval1.uval.realval = 1.;
	    else
		tmpval1.uval.realval = 0.;
	    tmpval1.type = V_REAL;
	    }
	else	
    	    tmpval1.type = V_INTEGER;
	}
    else
	{
	badop_em(errmsg, expr_type, "Comparison");
	code = FAIL;
	goto exit;
	}
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * getvarval - get the value of a variable.
 *
 */
static FUNCTION CODE getvarval
(
    struct VARIABLE 	*varptr,	/* in: ptr to variable 		*/
    FUNINT		index,		/* in: TCL subscript		*/
    struct VALUE	*value,		/* out: value returned		*/
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    TAEINT		*intvalptr;	/* pointer to value of variable	*/
    TAEFLOAT		*realvalptr;
    TEXT		**strvalptr;

    if ((*varptr).v_count < index)
        {
    	*errmsg = &er_toob;
    	s_copy((*varptr).v_name, er_toob.variable);
    	return(FAIL);
    	}
    (*value).type = (*varptr).v_type;		/* set type		*/
    if ((*value).type == V_REAL)
    	{
 	(*value).type = V_REAL;
    	realvalptr = (TAEFLOAT *) (*varptr).v_cvp;	/* cast pointer to value */
    	(*value).uval.realval = realvalptr[index-1];  /* use -1 because TCL indexes... */
       	}					  /* start at 1			*/
    else if ((*value).type == V_INTEGER)
    	{
    	(*value).type = V_INTEGER;
    	intvalptr = (TAEINT *) (*varptr).v_cvp;  /* cast pointer to value */
    	(*value).uval.intval = intvalptr[index-1];
    	}
    else if ((*value).type == V_STRING)
    	{
    	(*value).type = V_STRING;
    	strvalptr = (TEXT **) (*varptr).v_cvp;  		/* cast pointer to value */
    	(*value).uval.strpt = s_save(strvalptr[index-1]);	/* copy to dynamic space */
    	if ((*value).uval.strpt==NULL)
    	    {
	    tmierr(TMI_VAL_NO_SPACE);
	    return(FAIL);
	    }
    	}
    return(SUCCESS);
    }

/*
 * gtr_act - action routine to determine if a value is greater than another
 *	     value.
 */
FUNCTION CODE gtr_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compare the two values at the top of the value stack to the new value, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP5);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_rel_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    badop_em(errmsg, V_STRING, "Comparison");
	    code = FAIL;
	    goto exit;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval > tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON) 
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval > tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval > tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval > tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	if (expr_type == V_REAL)           /* If the expression is a real */
	    {				   /* make the value a real. */
	    if (tmpval1.uval.intval == 1) 
		tmpval1.uval.realval = 1.;
	    else
		tmpval1.uval.realval = 0.;
	    tmpval1.type = V_REAL;
	    }
	else	
    	    tmpval1.type = V_INTEGER;
	}    	
    else
	{
	badop_em(errmsg, expr_type, "Comparison");
	code = FAIL;
	goto exit;
	}
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * indexact - pop the name and index or TCL function name off the 
 *	      stack and replace them with a value.
 *	     
 * TBD: handling for functions with more than one argument (will require
 *      a stack since we can have $fn1(1, $fn2(1)) or $fn(1,$global)).
 */
FUNCTION CODE indexact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct	VALUE	tmpval1, tmpval2, tmpval3;
    CODE		code;
    TEXT		*name;		/* name of variable		*/
    COUNT		index;		/* subscript determined		*/
    struct VARIABLE	*varptr;
    struct TCLFUNC	*funcptr;

    if (poptin(type_stack)==STACK_FAULT)	/* pop the type of index or arg */
	{
	tmierr(TMI_OVERPOP);
	return(FAIL);
	}
    if (popvstack(&tmpval1) == FAIL)	/* Get the index or arg		*/
	{
	tmierr(TMI_VAL_OVERPOP6);
	return(FAIL);			
	}
    if (tmpval1.marked)
	{
	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (popvstack(&tmpval2) == FAIL)	/* look at the name		*/
	{
 	tmierr(TMI_VAL_OVERPOP7);
	code = FAIL;
	goto exit;
	}
    name = tmpval2.uval.strpt;		/* name of variable or function	*/
    varptr = search(name, expcontxt);	/* use global context		*/
    if (varptr==NULL)
	{
	funcptr = fn_search(name);	/* not a variable; function?	*/
	if (funcptr == NULL)
	    {
	    *errmsg = &er_nonex;
	    s_copy(name, er_nonex.variable);	/* say 'non-existent' variable */
	    code = FAIL;
	    goto exit;
	    }
	else				/* we have a TCL function	*/
	    {
    	    if (tmpval1.null)		/* is the value the null value?	*/
    		{
    		*errmsg = &er_nullf;	/* yes				*/
	    	s_copy(name, er_nullf.variable);   /* null not allowed  */
    		code = FAIL;
    		goto exit;
    		}
	    tmpval3.null = FALSE;		  /* assume result not null */
	    code = (*(*funcptr).fn)(&tmpval1, expcontxt, &tmpval3, errmsg);  /* do it */
	    if (code != SUCCESS)
		goto exit;
	    dealstrval(&tmpval1);    	/* deallocate func val if string */
	    MOVE_STRUCT(tmpval3, tmpval1);
	    }    	            	        	    
	}
    else		/* a variable	*/
	{
	if (tmpval1.closed == TRUE && tmpval1.marked == TRUE)
	    {				/* multivalued subscript	*/
	    *errmsg = &er_nestmul;
	    code = FAIL;
	    goto exit;
	    }
	if (tmpval1.type == V_INTEGER)
	    if (tmpval1.origin==O_CON)
		index = tmpval1.uval.realval;	/* GETPRIM returns reals	*/
    	    else				/* origin was variable or computed	*/
		index = tmpval1.uval.intval;
	else
	    {
	    *errmsg = &er_badsub;
    	    s_copy(name, er_badsub.variable);
	    code = FAIL;
	    goto exit;
	    }
	if (index <= 0)
	    {
	    *errmsg = &er_badsub;
    	    s_copy(name, er_badsub.variable);
	    code = FAIL;		/* don't return yet...must deall. name */
	    goto exit;
	    }
	if ((*varptr).v_count == 0)		/* variable has no value	*/
	    {
	    *errmsg = &er_novalue;
	    s_copy(name, er_novalue.variable);
	    code = FAIL;
	    goto exit;
	    }
	code = getvarval(varptr, index, &tmpval1, errmsg);
    	tmpval1.origin = O_VAR;		/* new origin is a variable		*/
	}
    s_free(name);			/* release dynamic storage for name 	*/
    if (code == SUCCESS) code = pushvstack(&tmpval1);
    return(code);

exit:				/* common exit				*/
    if (code == FAIL)
	{
	dealstrval(&tmpval1);		/* deallocate if strings	*/
	dealstrval(&tmpval2);
	}
    return(code);
    }

/*
 * lequ_act - action routine to determine if a value is less than or
 * equal to another value.
 */
FUNCTION CODE lequ_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compare the two values at the top of the value stack, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP8);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_rel_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
    	{
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    badop_em(errmsg, V_STRING, "Comparison");
	    code = FAIL;
	    goto exit;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval <= tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval <= tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval <= tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval <= tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	if (expr_type == V_REAL)           /* If the expression is a real */
	    {				   /* make the value a real. */
	    if (tmpval1.uval.intval == 1) 
		tmpval1.uval.realval = 1.;
	    else
		tmpval1.uval.realval = 0.;
	    tmpval1.type = V_REAL;
	    }
	else	
    	    tmpval1.type = V_INTEGER;
	}
    else
	{
	badop_em(errmsg, expr_type, "Comparison");
	code = FAIL;
	goto exit;
	}

    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * less_act - action routine to determine if a value is less than another
 *	     value.
 */
FUNCTION CODE less_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compare the two values at the top of the value stack to the new value, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP9);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_rel_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    badop_em(errmsg, V_STRING, "Comparison");
	    code = FAIL;
	    goto exit;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval < tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval < tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval < tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval < tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	if (expr_type == V_REAL)           /* If the expression is a real */
	    {				   /* make the value a real. */
	    if (tmpval1.uval.intval == 1) 
		tmpval1.uval.realval = 1.;
	    else
		tmpval1.uval.realval = 0.;
	    tmpval1.type = V_REAL;
	    }
	else	
    	    tmpval1.type = V_INTEGER;
	}    	
    else
	{
	badop_em(errmsg, expr_type, "Comparison");
	code = FAIL;
	goto exit;
	}
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * lpact - action routine upon seeing a name followed by a left paren
 *
 * The purpose of this routine is to set the expected type for operations
 */
FUNCTION CODE lpact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct VALUE	tmpval;
    struct VARIABLE	*var_ptr;
    struct TCLFUNC	*fn_ptr;
    TEXT		*name;
    CODE		code;

    if (popvstack(&tmpval)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP10);
	return(FAIL);
	}
    pushvstack(&tmpval);		/* no change...just looking	*/
    name = tmpval.uval.strpt;		/* get name of variable or fn	*/    
    if ((var_ptr = search(name, expcontxt)) != NULL)	/* found variable */
    	{
    	code = pushti(type_stack, V_INTEGER);	/* integer operations on subscripts 	*/
    	if (code == STACK_FAULT)
    	    {
    	    tmierr(TMI_OVERPUSH);
    	    goto exit;
    	    }
    	}
    else if ((fn_ptr = fn_search(name)) != NULL)	/* TCL function?	*/
    	{
    	code = pushti(type_stack, (*fn_ptr).arg_type[0]);	/* use type of fn arg	*/
    	if (code == STACK_FAULT)
    	    {
    	    tmierr(TMI_OVERPUSH);
    	    goto exit;
    	    }
    	}
    else
	{
	*errmsg = &er_nonex;			/* undefined variable   */
	s_copy (name, er_nonex.variable);
	code = FAIL;
	}
exit:				/* common exit				*/
    return(code);
    }

/*
 * mulact - action routine to multiply two values 
 */
FUNCTION CODE mulact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Multiply the two values at the top of the value stack to the new value, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;
    CODE		typeflag;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP11);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_num_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (tmpval1.type == V_STRING || tmpval2.type == V_STRING)
	{
	*errmsg = &er_mixvar;
	code = FAIL;
	goto exit;
	}
    if (expr_type==V_REAL || tmpval1.type == V_REAL || tmpval2.type == V_REAL)
	{				/* if either a real then use real arith	*/
	typeflag = V_REAL;
	if (tmpval1.type == V_INTEGER)
	    if (tmpval1.origin != O_CON)
		tmpval1.uval.realval = tmpval1.uval.intval;	/* convert	*/
	if (tmpval2.type == V_INTEGER)
	    if (tmpval2.origin != O_CON)
		tmpval2.uval.realval = tmpval2.uval.intval;	/* convert	*/
    	if ((tmpval1.type == V_INTEGER && tmpval1.origin==O_VAR)
    	   ||(tmpval2.type == V_INTEGER && tmpval2.origin==O_VAR))
    	    {				/* can mix real and integer constants...*/
    	    *errmsg = &er_mixvar;	/* but not real and int variables */
	    code = FAIL;
	    goto exit;
    	    }
    	code = fl_mult(tmpval1.uval.realval,  tmpval2.uval.realval, &tmpval1.uval.realval);
    	if (code != SUCCESS)
    	    {
    	    *errmsg = &er_badar;	/* overflow or underflow	*/
    	    goto exit;
    	    }
    	}
    else if (expr_type==V_INTEGER)
	{
	typeflag = V_INTEGER;
	if (tmpval1.origin==O_CON)		/* if constant, then was real	*/
	    {
	    code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
	if (tmpval2.origin==O_CON)
	    {
	    code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
    	code = int_mult(tmpval1.uval.intval,  tmpval2.uval.intval, &tmpval1.uval.intval);
	if (code != SUCCESS)
	    {
	    *errmsg = &er_badar;	/* overflow or underflow	*/
	    goto exit;
	    }
	}
    else
    	{
	badop_em(errmsg, expr_type, "Multiplication");
    	code = FAIL;
    	goto exit;
    	}
    tmpval1.type = typeflag;
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * negact - negate the value at the top of the stack
 *
 */
FUNCTION CODE negact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct	VALUE	tmpval;
    CODE		code;
    TINY		expr_type;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval) == FAIL)
	{
	tmierr(TMI_VAL_OVERPOP12);
	return(FAIL);
	}
    if (tmpval.null==TRUE)			/* Null values not permitted */
	{
	*errmsg = &er_num_null;
	code = FAIL;
	goto exit;
	}
    if (tmpval.marked==TRUE && tmpval.closed==TRUE)
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_STRING || tmpval.type == V_STRING)
    	{
	badop_em(errmsg, expr_type, "Negation");
    	code = FAIL;
    	goto exit;
    	}
    else if (tmpval.type == V_REAL)
    	tmpval.uval.realval = -tmpval.uval.realval; 	/* negate	*/
    else if (tmpval.type == V_INTEGER)
    	{
    	if (expr_type==V_INTEGER && tmpval.origin==O_CON)
	/* cvt to int since all con'ts carried as real up to this point */
	    {
	    code = int_fl2i(-tmpval.uval.realval, &tmpval.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
	else if (expr_type==V_REAL && tmpval.origin==O_CON)
	    {
	    tmpval.uval.realval = - tmpval.uval.realval;   /* let x = -2.0 */
	    tmpval.type = V_REAL;			   /* get right type */ 
	    }
    	else if (expr_type==V_REAL && tmpval.origin==O_VAR)
    	/* we have a real on left but an integer variable */
    	    {
    	    *errmsg = &er_mixvar;
    	    code = FAIL;
    	    goto exit;
    	    }
	else
	    tmpval.uval.intval = - tmpval.uval.intval;
    	}
    else
    	{
	badop_em(errmsg, expr_type, "Negation");
    	code = FAIL;
    	goto exit;
    	}
    tmpval.origin = O_COMP;
    code = pushvstack(&tmpval);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	dealstrval(&tmpval);		/* deallocate if strings	*/
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * nequ_act - action routine to determine if two values on the value
 *	      stack are not equal.
 */
FUNCTION CODE nequ_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compare the two values at the top of the value stack, placing the 	*/
/* result back on the stack.						*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/
    BOOL		strequ;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP13);
	return(FAIL);
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE) ||
	(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
	if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* At least one is null */
	    {
	    if (tmpval1.origin == O_VAR && tmpval2.origin == O_VAR)
		{
		if (tmpval1.type != tmpval2.type)	/* if not constants, */
		    {					/* type must match */
		    *errmsg = &er_mixvar;
		    code = FAIL;
		    goto exit;
		    }
		}

	    dealstrval (&tmpval1);			/* Get rid of strings */
	    dealstrval (&tmpval2);
	    if (tmpval1.null==TRUE && tmpval2.null==TRUE)
		tmpval1.uval.intval = 0;		/* Both null, hence equal */
	    else
		tmpval1.uval.intval = 1;			
	    tmpval1.null = FALSE;			  /* No longer null */
	    goto done;
	    }
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    strequ = s_equal (tmpval1.uval.strpt, tmpval2.uval.strpt);
	    dealstrval(&tmpval1);		/* deallocate strings	*/
    	    dealstrval(&tmpval2);
	    if (strequ)
		tmpval1.uval.intval = 0;
	    else
		tmpval1.uval.intval = 1;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval != tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval != tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval != tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval != tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	}
    else
	{
	badop_em(errmsg, expr_type, "Comparison");
	code = FAIL;
	goto exit;
	}
done:
    if (expr_type == V_REAL)		/* If the expression is a real */
	{				/* make the value a real. */
	if (tmpval1.uval.intval == 1) 
	    tmpval1.uval.realval = 1.;
	else
	    tmpval1.uval.realval = 0.;
	tmpval1.type = V_REAL;
	}
    else	
    	tmpval1.type = V_INTEGER;
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    return(code);				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* error exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * notact - Perform the logical NOT operation on the value at the top 
 *	    of the stack
 *
 */
FUNCTION CODE notact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct	VALUE	tmpval;
    CODE		code;
    TINY		expr_type;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval) == FAIL)
	{
	tmierr(TMI_VAL_OVERPOP14);
	return(FAIL);
	}
    if (tmpval.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_log_null;
	code = FAIL;
	goto exit;
	}
    if (tmpval.marked==TRUE && tmpval.closed==TRUE)
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
    	if (tmpval.type == V_STRING)
    	    {
    	    *errmsg = &er_mixvar;
    	    code = FAIL;
    	    goto exit;
    	    }
	if (tmpval.type == V_INTEGER)
	    {
	    if (tmpval.origin==O_CON)		/* Does type lie? */
		{
		code = int_fl2i(tmpval.uval.realval, &tmpval.uval.intval);
		if (code != SUCCESS) goto badarith;
		}
	    tmpval.uval.intval = !tmpval.uval.intval;
	    }
	else
	    tmpval.uval.intval = !tmpval.uval.realval;
	if (expr_type == V_REAL)	/* if the expression is a real */
	    {				/* make the value a real. */
	    if (tmpval.uval.intval == 1)
		tmpval.uval.realval = 1.;
	    else
		tmpval.uval.realval = 0.;
	    }
    	}
    else
    	{
	badop_em(errmsg, expr_type, "\'NOT\' operation");
    	code = FAIL;
    	goto exit;
    	}
    tmpval.type = expr_type;
    tmpval.origin = O_COMP;
    code = pushvstack(&tmpval);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	dealstrval(&tmpval);		/* deallocate if strings	*/
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * or_act - action routine to compute the logical OR of the two values
 *	    at the top of the value stack.
 */
FUNCTION CODE or_act
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Compute the logical OR of the two values at the top of the stack 
   and push the result back on the stack.			*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;		/* type to use		*/

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP15);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_log_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (expr_type == V_REAL || expr_type == V_INTEGER) 	/* Real and Integer are 
 *			 the only valid types for relation expressions */
       	{
    	if (tmpval1.type == V_STRING && tmpval2.type == V_STRING)
    	    {
	    badop_em(errmsg, V_STRING, "Logical \'OR\'");
	    code = FAIL;
	    goto exit;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_INTEGER) 
	    {
	    if (tmpval1.origin == O_CON)		    /* Does type lie? */
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval || tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_INTEGER && tmpval2.type == V_REAL)
	    {
	    if (tmpval1.origin == O_CON)
    		{
		code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.realval || tmpval1.uval.intval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_INTEGER)
	    {
	    if (tmpval2.origin == O_CON)
    		{
		code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
    		if (code != SUCCESS) goto badarith;
    		}
	    if (tmpval2.uval.intval || tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else if (tmpval1.type == V_REAL && tmpval2.type == V_REAL)
	    {
	    if (tmpval2.uval.realval || tmpval1.uval.realval)
		tmpval1.uval.intval = 1;
	    else
		tmpval1.uval.intval = 0;
	    }
	else
	    {
	    *errmsg = &er_mixvar;
	    code = FAIL;
	    goto exit;
	    }
	if (expr_type == V_REAL)           /* If the expression is a real */
	    {				   /* make the value a real. */
	    if (tmpval1.uval.intval == 1) 
		tmpval1.uval.realval = 1.;
	    else
		tmpval1.uval.realval = 0.;
	    tmpval1.type = V_REAL;
	    }
	else	
    	    tmpval1.type = V_INTEGER;
	}    	
    else
	{
	badop_em(errmsg, expr_type, "Logical \'OR\'");
	code = FAIL;
	goto exit;
	}
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * packout - pack the output value
 *
 */

static FUNCTION CODE packout
(
    GENPTR		*value,		/* out: the value to pack	*/
    struct ERRMSG	*(*errmsg),	/* out: error message		*/
    COUNT		*count		/* out: number of values	*/

/* Get the output values from the stack. Allocate space accordingly	*/
 )
    {
    struct VALUE	tmpval;		/* for popped values		*/
    COUNT		i;
    TAEINT		*intptr=0;
    TAEFLOAT		*realptr=0;
    TEXT		**stringptr=0;
    TINY		expr_type;
    CODE		code;


    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (expr_type == V_INTEGER)			/* allocate space for values	*/
    	{
    	*value = tae_alloc(mulcount, sizeof(TAEINT));
    	intptr = (TAEINT *)*value;
    	if (intptr == NULL)
    	    {
    	    tmierr(TMI_VAL_NO_SPACE);
    	    return(FAIL);
    	    }
    	}
    else if (expr_type == V_REAL)
    	{
    	*value = tae_alloc(mulcount, sizeof(TAEFLOAT));
    	realptr = (TAEFLOAT *)*value;
    	if (realptr == NULL)
    	    {
    	    tmierr(TMI_VAL_NO_SPACE);
    	    return(FAIL);
    	    }
    	}
    else if (expr_type == V_STRING)
    	{
    	*value = tae_alloc(mulcount, sizeof(TEXT *));
    	stringptr = (TEXT **)*value;
    	if (stringptr == NULL)    
    	    {
    	    tmierr(TMI_VAL_NO_SPACE);
    	    return(FAIL);
    	    }
    	}
    for (i=mulcount-1; i >= 0; i--)		/* fill the value array */
    	{
    	if (popvstack(&tmpval)==FAIL)
	    {
	    tmierr(TMI_VAL_OVERPOP16);
	    return(FAIL);
	    }
	if (tmpval.null == TRUE)		/* if a null value	*/
	    {
	    if (mulcount != 1) 
		{
		tmierr (TMI_NULL);		/* can't have multiple values */
		return (FAIL);		
		}
	    *count = 0;				/* no values		*/
	    return (PA_SUCC);
	    }
	else
	    *count = mulcount;			
    	if (expr_type == V_INTEGER)
    	    {
    	    if (tmpval.type != V_INTEGER) 
    		{
    		tae_free(intptr);
    		goto badtype;
    		}
	    if (tmpval.origin==O_CON)		/* if constant, then was real	*/
    		{
		code = int_fl2i(tmpval.uval.realval, &tmpval.uval.intval);
    		if (code != PA_SUCC)
    		    {
    		    tae_free(intptr);
    		    goto badarith;
    		    }
    		}
    	    intptr[i] = tmpval.uval.intval;    	
    	    }
    	else if (expr_type == V_REAL)
    	    {
	    if (((tmpval.type == V_INTEGER) && (tmpval.origin == O_CON)) ||
		tmpval.type == V_REAL)
		{
		realptr[i] = tmpval.uval.realval;	/* intg constant ok	*/
		}
	    else
		{					
		tae_free(realptr);
		goto badtype;
		}
    	    }
    	else if (expr_type == V_STRING)	
    							/* this one's a character pointer */
    	    {
    	    if (tmpval.type != V_STRING) 
    		{
    		for (i++; i<mulcount; i++)		/* deallocate strings in output */
    		    s_free(stringptr[i]);
    		tae_free(stringptr);
    		goto badtype;
    		}
    	    stringptr[i] = tmpval.uval.strpt;
    	    }
    	}
    return(PA_SUCC);

badtype:			/* here on unexpected type		*/
    dealstrval(&tmpval);	/* deallocate current if a string	*/
    *errmsg = &er_mixvar;	/* 'mixed variables' error message	*/
    return(PA_FAIL);

badarith:
    dealstrval(&tmpval);
    *errmsg = &er_badar;	/* integer overflow			*/
    return(PA_FAIL);
    }

/*
 * paract - action on closing a paren
 *
 */

FUNCTION CODE paract
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct VALUE	tmpval;
    
    popvstack(&tmpval);
    tmpval.closed = TRUE;
    pushvstack(&tmpval);
    return(SUCCESS);
    }

/*
 * subact - action routine to subtract two values 
 *
 */
FUNCTION CODE subact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/


/* Subtract the value at the top of the value stack from the value next */
/* down on the stack, placing the result back on the stack.		*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code;
    TINY		expr_type;
    CODE		typeflag;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP17);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_num_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (tmpval1.type == V_STRING || tmpval2.type == V_STRING)
	{
	*errmsg = &er_mixvar;
	code = FAIL;
	goto exit;
	}
    if (expr_type==V_REAL || tmpval1.type == V_REAL || tmpval2.type == V_REAL)
	{				/* if either a real then use real arith	*/
	typeflag = V_REAL;
	if (tmpval1.type == V_INTEGER)
	    if (tmpval1.origin != O_CON)
		tmpval1.uval.realval = tmpval1.uval.intval;	/* convert	*/
	if (tmpval2.type == V_INTEGER)
	    if (tmpval2.origin != O_CON)
		tmpval2.uval.realval = tmpval2.uval.intval;	/* convert	*/
    	if ((tmpval1.type == V_INTEGER && tmpval1.origin==O_VAR)
    	   ||(tmpval2.type == V_INTEGER && tmpval2.origin==O_VAR))
    	    {				/* can mix real and integer constants...*/
    	    *errmsg = &er_mixvar;	/* but not real and int variables */
	    code = FAIL;
	    goto exit;
    	    }
    	code = fl_sub(tmpval2.uval.realval,  tmpval1.uval.realval, &tmpval1.uval.realval);
    	if (code != SUCCESS)
    	    {
    	    *errmsg = &er_badar;	/* overflow or underflow	*/
    	    goto exit;
    	    }
    	}
    else if (expr_type==V_INTEGER)
	{
	typeflag = V_INTEGER;
	if (tmpval1.origin==O_CON)		/* if constant, then was real	*/
	    {
	    code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
	if (tmpval2.origin==O_CON)
	    {
	    code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
    	code = int_sub(tmpval2.uval.intval,  tmpval1.uval.intval, &tmpval1.uval.intval);
	if (code != SUCCESS)
	    {
	    *errmsg = &er_badar;	/* overflow or underflow	*/
	    goto exit;
	    }
	}
    else
    	{
	badop_em(errmsg, expr_type, "Subtraction");
    	code = FAIL;
    	goto exit;
    	}
    tmpval1.type = typeflag;
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }


/*
 * sumact - action routine to add two values 
 *
 */
FUNCTION CODE sumact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* Add the value at the two values at the top of the value stack, 	*/
/* placing the result back on the stack.				*/
 )
    {
    struct	VALUE	tmpval1, tmpval2;
    CODE		code, typeflag;
    TINY		expr_type;

    expr_type = toptin(type_stack);		/* get type of expression	*/
    if (popvstack(&tmpval1)==FAIL || popvstack(&tmpval2)==FAIL)
	{
	tmierr(TMI_VAL_OVERPOP18);
	return(FAIL);
	}
    if (tmpval1.null==TRUE || tmpval2.null==TRUE) /* Null values not permitted */
	{
	*errmsg = &er_num_null;
	code = FAIL;
	goto exit;
	}
    if ((tmpval1.marked==TRUE && tmpval1.closed==TRUE)||(tmpval2.marked==TRUE && tmpval2.closed==TRUE))
    	{			/* if both true then we had a completed multivalue */
    	*errmsg = &er_mulvar;
    	code = FAIL;
    	goto exit;
    	}
    if (tmpval1.type == V_STRING || tmpval2.type == V_STRING)
	{
	*errmsg = &er_mixvar;
	code = FAIL;
	goto exit;
	}
    if (expr_type==V_REAL || tmpval1.type == V_REAL || tmpval2.type == V_REAL)
	{				/* if either a real then use real arith	*/
	typeflag = V_REAL;
	if (tmpval1.type == V_INTEGER)
	    if (tmpval1.origin != O_CON)
		tmpval1.uval.realval = tmpval1.uval.intval;	/* convert	*/
	if (tmpval2.type == V_INTEGER)
	    if (tmpval2.origin != O_CON)
		tmpval2.uval.realval = tmpval2.uval.intval;	/* convert	*/
    	if ((tmpval1.type == V_INTEGER && tmpval1.origin==O_VAR)
    	   ||(tmpval2.type == V_INTEGER && tmpval2.origin==O_VAR))
    	    {				/* can mix real and integer constants...*/
    	    *errmsg = &er_mixvar;	/* but not real and int variables */
	    code = FAIL;
	    goto exit;
    	    }
    	code = fl_add(tmpval2.uval.realval,  tmpval1.uval.realval, &tmpval1.uval.realval);
    	if (code != SUCCESS)
    	    {
    	    *errmsg = &er_badar;	/* overflow or underflow	*/
    	    goto exit;
    	    }
    	}
    else if (expr_type==V_INTEGER)
	{
	typeflag = V_INTEGER;
	if (tmpval1.origin==O_CON)		/* if constant, then was real	*/
	    {
	    code = int_fl2i(tmpval1.uval.realval, &tmpval1.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
	if (tmpval2.origin==O_CON)
	    {
	    code = int_fl2i(tmpval2.uval.realval, &tmpval2.uval.intval);
	    if (code != SUCCESS) goto badarith;
	    }
	code = int_add(tmpval2.uval.intval,  tmpval1.uval.intval, &tmpval1.uval.intval);
	if (code != SUCCESS)
	    {
	    *errmsg = &er_badar;	/* overflow or underflow	*/
	    goto exit;
	    }
	}
    else
    	{
	badop_em(errmsg, expr_type, "Addition");
    	code = FAIL;
    	goto exit;
    	}
    tmpval1.type = typeflag;
    tmpval1.origin = O_COMP;
    code = pushvstack(&tmpval1);		/* put the answer back		*/
    				/* NOTE: if push fails, we assume user tried ...*/
    				/* ...too many nesting levels.		*/

exit:				/* common exit				*/
    if (code == FAIL)
    	{
    	dealstrval(&tmpval1);		/* deallocate if strings	*/
    	dealstrval(&tmpval2);
    	}
    return(code);

badarith:
    code = FAIL;
    *errmsg = &er_badar;
    goto exit;
    }

/*
 * termact - set the 'commaterm' flag
 *
 */
   
FUNCTION CODE termact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

/* The only way to get here is on a comma terminator.  Set the flag. 	*/
 )
    {
    commaterm = TRUE;
    return(SUCCESS);
    }

/*
 * varact - replace the name at the top of the stack with the corresponding
 *	     variable value.
 */
FUNCTION CODE varact
(
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct	VALUE	tmpval;
    CODE		code=0;
    TINY		expr_type;
    TEXT		*name;		/* name of variable		*/
    struct VARIABLE	*varptr;
    COUNT		i;

    if (popvstack(&tmpval) == FAIL)	/* look at the name		*/
	{
	tmierr(TMI_VAL_OVERPOP19);
	return(FAIL);
    	}
    expr_type = toptin(type_stack);
    if (expr_type == V_NAME)	/* if name expression, use it as a literal	*/
    	{
    	tmpval.type = V_NAME;
	code = pushvstack(&tmpval);	/* push it				*/
    	return(code);
    	}
    name = tmpval.uval.strpt;
    varptr = search(name, expcontxt);
    if (varptr==NULL)
        {
	*errmsg = &er_nonex;
	s_copy(name, er_nonex.variable);
    	code = FAIL;
    	}
    else if ((*varptr).v_count == 0)		/* variable has null value	*/
	{
	tmpval.null = TRUE;			/* Set null flag */
	tmpval.origin = O_VAR;			/* origin was a variable */
	tmpval.type = (*varptr).v_type;		
	if ((tmpval.type == V_STRING) || (tmpval.type == V_NAME))
	    tmpval.uval.strpt = NULL;		/* Protect against dealloc error */
	code = pushvstack (&tmpval);		/* Return value to stack */
	}
    else if ((*varptr).v_count == -1)		/* variable has no value */
	{
	*errmsg = &er_novalue;
	s_copy (name, er_novalue.variable);
	code = FAIL;
	}
    else
    	{
    	if ((*varptr).v_count > 1)
    	    {
    	    mulvalflag = TRUE;
    	    mulcount = (*varptr).v_count;
    	    }
    	for (i=1; i <= (*varptr).v_count; i++)
    	    {
    	    code = getvarval(varptr, i, &tmpval, errmsg);	/* put the value in tmpval */
    	    if (code==FAIL) break;
    	    if ((*varptr).v_count > 1) 
    		{
    		tmpval.marked = TRUE;	/* indicate p/o multi-valued		*/
       	    	tmpval.closed = TRUE;	/* also completed multi-value		*/
    		}
    	    tmpval.origin = O_VAR;	/* got the value from a variable	*/
    	    code = pushvstack(&tmpval);	/* push it				*/
    	    if (code==FAIL) break;
    	    }
    	}
    s_free(name);			/* release dynamic storage for name	*/
    return(code);
    }

#else
/*
 *	NOTE:  THIS IS PROBABLY OBSOLETE...NHE 5/7/84
 *
 * Test for EVALEXP
 *
 */
    struct	VARIABLE vari1, vari2, varr1, vars1;  /* fake TCL variables 	*/
    TAEINT	int1val = 100, int2val[4] = {0,1,200,300};
    TAEFLOAT	real1val = 55.5;
    TEXT	string1val[] = {"we got it"};
    TEXT	string2val[] = {" and we need it"};
    TEXT	*s1ptr[] = {&string1val, &string2val};

    FUNCTION main (void)

    {
    IMPORT TEXT 	vrsion[];
    TEXT		string[STRINGSIZ+1];
    CODE		evalexp();
    struct SYNBLK 	sb;
    struct CONTXT 	contxt;
    CODE		code;
    COUNT		dummy;
    COUNT		i;
    TAEINT		*intptr;
    TAEFLOAT		*realptr;
    TEXT		**stringptr;

    CODE		term;		/* the terminator		*/
    GENPTR		value;		/* the answer			*/
    COUNT		count;

    printf(" Expression evaluation test, version %s\n", vrsion);
    printf(" Enter a string  ");
    t_init(&dummy, &dummy, &dummy);
    inicontxt(&contxt);		/* initialize the fake context block	*/
    t_read(string, &dummy);
    for (; string[0]!='@';)	/* exit on '@'				*/
    	{
    	initok(&sb, string);
    	code = evalexp(&sb, &contxt, V_REAL, &value, &count, &term);	/* call the parser	*/
    	printf("Real evaluation complete. Success/fail: %d, terminator: %d, values: \n", code, term);
    	if (code == SUCCESS)
    	    {
    	    realptr = (TAEFLOAT *)value;
    	    for (i=0; i < count; i++)
    		printf("\t%lf\n", realptr[i]);
    	    tae_free(realptr);
    	    }
    	else
    	    {
    	    sb.errchr = sb.curchr -1;
    	    synerr(&sb, "syntax error");
    	    printf("%s\n\n", sb.errmsg);
    	    }

	initok(&sb, string);
	code = evalexp(&sb, &contxt, V_INTEGER, &value, &count, &term);
    	printf("Integer evaluation complete. Success/fail: %d, terminator: %d, values: \n", code, term);
    	if (code == SUCCESS)
    	    {
    	    intptr = (TAEINT *)value;
    	    for (i=0; i < count; i++)
    		printf("\t%d\n", intptr[i]);
    	    tae_free(intptr);
    	    }

    	initok(&sb, string);
    	code = evalexp(&sb, &contxt, V_STRING, &value, &count, &term);
    	printf("String evaluation complete. Success/fail: %d, terminator: %d, values: \n", code, term);
    	if (code == SUCCESS)
    	    {
    	    stringptr = (TEXT **)value;
    	    for (i=0; i < count; i++)
    		{
    		printf("\t%s\n", stringptr[i]);
    	        tae_free(stringptr[i]);
    		}
    	    tae_free(stringptr);
    	    }
        printf("\nEnter a string  ");
        t_read(string, &dummy);
    	}
    }


/*
 * inicontxt - initialize the context block for the expeval test
 */
static FUNCTION VOID inicontxt
(
    struct	CONTXT	*c

 )
    {
    COUNT	i;

    (*c).backlink = NULL;		/* pointer to previous CONTXT	*/
    (*c).toprefs = -1;		/* top defd gbl - negative means none defined*/
    for (i=0; i < MAXREF; i++);
	(*c).refs[i] = NULL;		/* ptrs to refs (gbls) - NULL indicates deleted*/
    (*c).locst.link = &vari1;
    (*c).parmst.link = NULL;		/* no parameters to search	*/

/* init the variables	*/
    vari1.v_link = &vari2;
    s_copy("i1",vari1.v_name);
    vari1.v_type = V_INTEGER;
    vari1.v_count = 1;
    vari1.v_minc = vari1.v_maxc = 1;
    vari1.v_cvp = &int1val;

    vari2.v_link = &varr1;
    s_copy("i2",vari2.v_name);
    vari2.v_type = V_INTEGER;
    vari2.v_count = 4;
    vari2.v_minc = vari2.v_maxc = 4;
    vari2.v_cvp = &int2val;

    varr1.v_link = &vars1;
    s_copy("r1",varr1.v_name);
    varr1.v_type = V_REAL;
    varr1.v_count = 1;
    varr1.v_minc = varr1.v_maxc = 1;
    varr1.v_cvp = &real1val;

    vars1.v_link = NULL;
    s_copy("s1", vars1.v_name);
    vars1.v_type = V_STRING;
    vars1.v_count = 2;
    vars1.v_minc = vars1.v_maxc = 2;
    vars1.v_cvp = &s1ptr;
    return;
    }

#endif

/*
 * STUDY AREAS
 *
 *	--Need to study ways of optimizing the state transitions and
 *	  state tables.  For example, we might have a way of indicating
 *	  in a look-ahead list, any symbol is ok.
 *
 *	--Could use a macro that eliminates the redundancy in the PSTATE 
 *	  entries, i.e, the number of current symbol/next state pairs is
 *	  now indicated in three distinct places:  the PSTATE macro input,
 *	  the constant in the data list that indicates the number of entries
 *	  following, and the actual number of entries.
 *
 *	--Should reorganize the states in the state table to be more readable
 *	  e.g, subtract should follow add.
 *
 *	--Review the REDENT and PSTATE definitions to minimize space taken,
 *	  e.g., the CODE's could probably be TINY's if we work with -1 through
 *	  -127.  (Note need to investigate if negative numbers in bytes is 
 *	  portable.
 *
 *	--Consider changing the output value to come through the function
 *	  return rather than an argument; GENPTR EVALEXP(...).  NULL would
 *	  indicate failure.
 *
 */
