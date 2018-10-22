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



/* TDB CHECKOUT FILE_TIME= 9-SEP-1986 14:15 DUA1:[TAEV1.OLB]GETPRIM.C;3 */
/* TJH CHECKOUT FILE_TIME=16-DEC-1984 18:42 DUA1:[TAEV1.OLB]GETPRIM.C;1 */
/* TJM CHECKOUT FILE_TIME= 3-JUL-1984 13:04 DUA0:[TAEV1.OLB]GETPRIM.C;18 */
/* TNHE CHECKOUT FILE_TIME=28-MAR-1984 14:19 DUA0:[TAEV1.OLB]GETPRIM.C;17 */
/* TJM CHECKOUT FILE_TIME= 2-MAR-1984 10:58 DUA0:[TAEV1.OLB]GETPRIM.C;15 */
/* TJM CHECKOUT FILE_TIME=11-OCT-1983 17:30 DUA0:[TAEV1.OLB]GETPRIM.C;14 */
/*TJM         CHECKOUT FILE_TIME=13-JUL-1983 15:39 DUA0:[TAEV1.OLB]GETPRIM.C;12 */
/*  
 *  GETPRIM PACKAGE - Get a primitive language symbol for parsing in expressions
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *
 *	22-aug-83	Add return of PA_NULL from getprim...jtm
 *	11-oct-83	Fix unix compilation errors...palm
 *	29-FEB-84	Make null values be of type integer...jtm
 *	08-mar-84	Fix floating overflow crash (PR 589)...jtm
 *	28-mar-84	Fix initialization of value structure...palm
 *	26-jun-84	UNCHECKED-OUT: Fix to allow "-.i"...nhe
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *      08-apr-85       Add error messages for underflow or overflow...joh
 *	26-jun-85	Fix bug in function fract...lia
 *	08-aug-85	Add a STATE structure named errflow...lia
 *	10-sep-86	PR 1111: Prevent overflow into integer sign bit via call
 *			to 's_s2r1' (rather than 's_s2i1'). New alogorithm to
 *			compute .00000...3 (38 zeros) in 'fract'...dab
 *	15-sep-86	Fix state 9 to cover "-"...nhe
 *      18-apr-94	Concurrent port: defined PA_NOT...dgf
 *
 *********************************************************************
 * Retrofit of changes to UNIX v1.3:
 *
 *	13-nov-86	Fixed on UNIX 15-aug-85: Change error[] to errst[]
 *			to avoid conflict w/ UNIX catalog manager...peb for dm
 *
 *********************************************************************
 *
 *	15-jun-88	Remove ascii dependency;
 *			use isalpha, isdigit and new isextletter macros
 *			rather than range check...tp
 */
 

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"syninc.inc"	/* syntax package			*/
#include	"tminc.inc"	/* TM definitions			*/
#include	"expinc.inc"	/* definitions for expressions		*/
#include	"chartype.inc"
#include "taeintproto.h"

#ifdef masscomp
#define PA_NOT (PA_BASE+22)
#endif

    GLOBAL COUNT v60pr = 0;

#ifndef testmain
#ifdef NAME
#undef NAME
#endif
/*									*/
/*  Define token classes not already defined by gettok			*/
/*  These tokens are only of concern to functions in this source but	*/
/*  should be integrated into the SYNTAX-defined classes.		*/

#define	NAME	700		/* pick a number unlikely to conflict	*/
#define INTG	NAME+1
#define EOT	NAME+2
#define E	NAME+3
#define EINT	NAME+4
#define INTE	NAME+5
#define INTEINT NAME+6
#define DEFAULT NAME+8
#define LO_NOTYPE NAME+9	/* for a variable name in locval structure */

    	
    struct	LOCVAL			/* structure containing the parsed */
    	{				/*   components of the value as	*/
    					/*   they are gathered.		*/
    	CODE	class;			/* V_INTEGER, or V_REAL, or	*/
    					/* V_STRING, or PA_NAME, or LO_NOTYPE */
    	CODE	sign;			/* POSITIVE or NEGATIVE		*/
    	DOUBLE	realval;		/* the value so far		*/
    	CODE	expsign;		/* sign of the exponent		*/
    	TEXT	lostring[STRINGSIZ+1];	/* value if a string or name	*/
    	};
#define NEGATIVE 0		/* for signs				*/
#define POSITIVE 1

/*  global for saving gettok position					*/
    TEXT	*gt_sav;
    BOOL	putbakfl;	/* true if a "put-back" is necessary	*/

/* declaration of action routines (internal to this source)		*/
CODE asciiact(TEXT*, struct LOCVAL *);	/* on single ascii values */
CODE expact(TEXT*, struct LOCVAL *);	/* on isolated exponent */
CODE fract(TEXT*, struct LOCVAL *);	/* on fractional parts */
CODE intact(TEXT*, struct LOCVAL *);	/* on integer values */
CODE nameact(TEXT*, struct LOCVAL *);	/* on variable names */
CODE negexact(TEXT*, struct LOCVAL *);	/* on negative exponent */
CODE putbact(TEXT*, struct LOCVAL *);	/* to put back a final token */
CODE quact(TEXT*, struct LOCVAL *);	/* on quoted string */
CODE gequact(TEXT*, struct LOCVAL *);	/* on >= */
CODE lequact(TEXT*, struct LOCVAL *);	/* on <= */
CODE nequact(TEXT*, struct LOCVAL *);	/* on <> */
CODE concact(TEXT*, struct LOCVAL *);	/* on // */
CODE lessact(TEXT*, struct LOCVAL *);	/* on < */
CODE gtract(TEXT*, struct LOCVAL *);	/* on > */
CODE nullact(TEXT*, struct LOCVAL *);	/* on -- */


/*	STATE TABLES							*/

    struct 	STATE			/* state structure		*/
    	{				/* several of these per state	*/
    	CODE 	tkclass;
    	COUNT	statendx;		/* index to pointer in statelist */
    	CODE	(*action)(TEXT*, struct LOCVAL *); 
				        /* pointer to action routine	*/
    	};				/* pointers in table below	*/

/* define the indexes to the state pointers in the statelist array	*/
/* NOTE: this list must be changed if any states are added		*/
#define	DONEX	0
#define	ERRORX	1
#define ERRFLOX 2
#define	S0X	3
#define	S1X	4
#define	S2X	5
#define	S3X	6
#define	S4X	7
#define	S5X	8	
#define	S6X	9
#define	S7X	10
#define S8X	11
#define S9X	12
#define S10X	13
#define S11X	14

/*  start  */
    struct STATE	s0[]=
    	{
    	 {'+',	DONEX, asciiact},	
    	 {'-',	S11X,  asciiact},
    	 {'=',	DONEX, asciiact},
    	 {'>', 	S8X,   gtract},
    	 {'<', 	S9X,   lessact},
    	 {'/',	DONEX, asciiact},		/* TBD: Change to S10X, NULL 
 *						to get concatenation */
    	 {'*',	DONEX, asciiact},
    	 {',',	DONEX, asciiact},
    	 {'(',	DONEX, asciiact},
    	 {')',	DONEX, asciiact},
    	 {EOS,	DONEX, asciiact},
    	 {S_WHITE, S0X, NULL},		/* loop on white	*/
    	 {'.',	S1X, NULL},
    	 {INTG,	S2X, intact},			/* integer		*/
    	 {INTE, S6X, intact},			/* integerE		*/
    	 {INTEINT, S3X, intact},		/* integerEint		*/
    	 {S_QUOTED,S3X, quact},		/* quoted string 	*/
    	 {NAME,	DONEX, nameact},		/* name			*/
    	 {EINT, DONEX, nameact},		/* name			*/
    	 {E,	DONEX, nameact},		/* name			*/
    	 {EOT,  0, NULL}
    	};
/*  .	*/
    struct STATE	s1[]=
    	{
    	 {INTG,	S4X, fract},			/* fractional component	*/
    	 {E,	S6X, NULL},
    	 {INTE,	S6X, fract},
    	 {INTEINT, S3X, fract},		/*...possibly with exp	*/
    	 {EOT,	0, NULL}
    	};
/*  integer */
    struct STATE	s2[]=
    	{
    	 {EOS,	DONEX, NULL},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'=',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {'.',	S5X, NULL},
    	 {E,	S6X, NULL},
    	 {EINT, S3X, intact},		/* action on integer exponent	*/
    	 {EOT,  0, NULL}
    	};
/*  almost done */
    struct STATE	s3[]=
    	{
    	 {EOS,	DONEX, NULL},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'=',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {EOT,  0, NULL}
    	};
/* .integer */
    struct STATE	s4[]=
    	{
    	 {EOS,	DONEX, putbact},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'=',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {E,	S6X, NULL},
    	 {EINT, S3X, intact},		/* integer exponent	*/
    	 {EOT,  0, NULL}
    	};
/*  integer. */
    struct STATE	s5[]=
    	{
    	 {EOS,	DONEX, putbact},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'=',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {E,	S6X, NULL},
    	 {EINT, S3X, fract},
    	 {INTG,	S4X, fract},
    	 {INTE,	S6X, fract},
    	 {INTEINT, S3X, fract},
    	 {EOT,  0, NULL}
    	};
/*  .integerE or integer.E or integer.integerE */
    struct STATE	s6[]=
    	{
    	 {'+',	S7X, NULL},
    	 {'-',	S7X, negexact},		/* negative exponent	*/
    	 {INTG,	S3X, intact},		/* integer exponent	*/
    	 {EOT,  0, NULL}
    	};
/*  xE+-	*/
    struct STATE	s7[]=
    	{
    	 {INTG,	S3X, expact},
    	 {EOT,  0, NULL}
    	};
/*  >		 */
    struct STATE	s8[]=
	{
	 {'=',	DONEX,	gequact},
    	 {EOS,	DONEX, putbact},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {'(',	DONEX, putbact},
    	 {E,	DONEX, putbact},
	 {NAME,	DONEX, putbact},
	 {S_QUOTED, DONEX, putbact},
    	 {EINT, DONEX, putbact},
    	 {INTG,	DONEX, putbact},
    	 {INTE,	DONEX, putbact},
    	 {INTEINT, DONEX, putbact},
    	 {EOT,  0, NULL}
	};

/*  <		 */
    struct STATE	s9[]=
	{
	 {'=',	DONEX,	lequact},
    	 {EOS,	DONEX, putbact},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'>', 	DONEX, nequact},
    	 {'<', 	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {'(',	DONEX, putbact},
    	 {E,	DONEX, putbact},
	 {NAME,	DONEX, putbact},
	 {S_QUOTED, DONEX, putbact},
    	 {EINT, DONEX, putbact},
    	 {INTG,	DONEX, putbact},
    	 {INTE,	DONEX, putbact},
    	 {INTEINT, DONEX, putbact},
    	 {EOT,  0, NULL}
	};

/*  /		 */
    struct STATE	s10[]=
	{
	 {'/',	DONEX,	concact},
    	 {EOS,	DONEX, putbact},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, putbact},
    	 {'=',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {'(',	DONEX, putbact},
    	 {E,	DONEX, putbact},
	 {NAME,	DONEX, putbact},
	 {S_QUOTED, DONEX, putbact},
    	 {EINT, DONEX, putbact},
    	 {INTG,	DONEX, putbact},
    	 {INTE,	DONEX, putbact},
    	 {INTEINT, DONEX, putbact},
    	 {EOT,  0, NULL}
	};

/*  -		 */
    struct STATE	s11[]=
	{
    	 {EOS,	DONEX, putbact},
    	 {S_WHITE, DONEX, NULL},
    	 {'+',	DONEX, putbact},
    	 {'-',	DONEX, nullact},
    	 {'=',	DONEX, putbact},
    	 {'>', 	DONEX, putbact},
    	 {'<', 	DONEX, putbact},
    	 {'*',	DONEX, putbact},
    	 {'/',	DONEX, putbact},
    	 {',',	DONEX, putbact},
    	 {')',	DONEX, putbact},
    	 {'(',	DONEX, putbact},
    	 {E,	DONEX, putbact},
	 {NAME,	DONEX, putbact},
	 {S_QUOTED, DONEX, putbact},
    	 {'.',	DONEX, putbact},
    	 {EINT, DONEX, putbact},
    	 {INTG,	DONEX, putbact},
    	 {INTE,	DONEX, putbact},
    	 {INTEINT, DONEX, putbact},
    	 {EOT,  0, NULL}
	};

/*  done	 */
    struct STATE	done[]=
    	{
    	 {DEFAULT, DONEX, NULL},
    	 {EOT,  0, NULL}
    	};
/*  error	*/
    struct STATE	errst[]=
    	{
    	 {DEFAULT, DONEX, NULL},
    	 {EOT,	0, NULL}
    	};

/*  overflow or underflow error	*/
    struct STATE	errflow[]=
    	{
    	 {DEFAULT, DONEX, NULL},
    	 {EOT,	0, NULL}
    	};

/* List of pointers to states						*/
    struct STATE	*statelist[]={done, errst, errflow, s0, s1, s2, 
    					 s3, s4, s5, s6, s7, s8, s9,
					 s10, s11};


FUNCTION static VOID inival
(
 struct LOCVAL 	*locval,	/* input: pointer to local value  */
 struct VALUE	*value
 );


FUNCTION DOUBLE power
(
 DOUBLE	num,		/* input: the number to raise		*/
 FUNINT	exp		/* input: the exponent			*/
 )
    {
    DOUBLE	temp;
    TAEINT	i;


    temp = 1;

    exp = (int) exp; /* 64-bit fix */

    if (exp>=0)
    	{
        for(i=1; i<=exp; i++)
       	    if (fl_mult(temp, num, &temp) != SUCCESS)   /* floating mult routine */
    		return(0);		/* failure			*/
        return(temp);
        }
    else
    	{
    	for (i=1; i <= -exp; i++)
    	    if (fl_div(temp, num, &temp) != SUCCESS)
    		return(0);
    	return(temp);
    	}
    }
/*
 * Get a primitive language 'symbol' for parsing, and, if appropriate, a value
 *
 * Returned CODEs (i.e., the "symbols") are:
 *
 *	PA_NAME - The string is a unquoted and is in the value structure
 *	PA_NUM 	- The string is a numeric constant and its value is in 'value'
 *	PA_QUOTED - The string is a string constant and its value is in 'value'
 *	PA_AND 	- The TAE AND logical operator
 *	PA_OR 	- The TAE OR logical operator
 *	PA_GE 	- The TAE greater than or equal to relational operator
 *	PA_LE 	- The TAE less than or equal to relational operator
 *	PA_NE 	- The TAE Not equal to realtioanl operator
 *	PA_CONCAT - The TAE concatenation operator
 *	PA_NULL	- The TAE null value indicator (--)
 *	EOS - End of line
 *	An ASCII code for one of the following:
 *		<, >, +, -, *, /, =, ), (
 *	PA_ERROR - Error detected
 *      PA_ERRFLOW - Underflow or overflow error detected.
 *
 *	NOTE:  If the value is a numeric, the value returned is always real.
 *	       This is so that the integer can be larger than a TAEINT.
 */
FUNCTION static CODE getctok
(

/*  Get a token, special numerical classification version.  This version
 *  is required because "E" must be a separate token for real numbers
 */
 struct	SYNBLK	*sb,		/* pointer to syntax block	*/
 char	*token			/* the token read		*/
 );
FUNCTION static CODE packval
(
 struct LOCVAL	*locval,	/* input			*/
 struct VALUE	*value		/* output			*/
 );
FUNCTION static struct STATE *getstate
(
 struct STATE	*curstate,	/* input: pointer to current state	*/
 CODE		class,		/* input: token class			*/
 TEXT		token[],	/* input: token				*/
 struct LOCVAL	*locval	/* input: pointer to local value	*/
 );

FUNCTION CODE getprim
(
 struct SYNBLK	*sb,		/* input:  syntax block		*/
 struct VALUE	*value		/* output:  value structure	*/
 )    
    {
    struct	STATE	*curstate;	/* pointer to current state	*/
    TEXT	token[TOKESIZ+1];
    CODE	class = 0;
    struct LOCVAL locval;


    inival(&locval, value);	/* initialize the local value structures */
    putbakfl = FALSE;		/* initialize "put-back" flag		*/
    curstate = s0;		/* initialize to start state	*/

    while (curstate!=done)
	{

    	class = getctok(sb, token); 

	curstate = getstate(curstate, class, token, &locval);	/* get new state, do action	*/

	if (curstate==errst || curstate==errflow)
	    break;
	}
    if (putbakfl==TRUE) (*sb).curchr = gt_sav;	/* restore saved position */
    if (curstate==errst || class==PA_ERROR)
	return(PA_ERROR);
    else if (curstate==errflow)
	return(PA_ERRFLOW);
    else
    	{
        class = packval(&locval, value);	/* pack the local value into the output */
	return(class);
    	}
    }

FUNCTION static CODE classify
(
 CODE	type,		/* token type, from GETTOK	*/
 char	*string	/* the token			*/
 )
    {
    CODE	class;
    COUNT	i;
    BOOL	eseen;		/* indicator of E in string	*/


    if (type == '>') return('>');	/* binary operators except +- (which */
    else if (type == '<') return('<');	/*  	are also unary		*/
    else if (type == '=') return('=');
    else if (type == '*') return('*');
    else if (type == '/') return('/');
    else if (type == S_COMSEP) return(',');
    else if (isextletter(string[0]))		/* NAME criteria */
    	type = NAME;		/* it's a name unless it's an integer with an E	*/
    class = NAME;		/* assume a name	*/
    if ((string[0]=='e'||string[0]=='E')&& s_length(string)==1)
    	return(E);		/* special class:  E and only E		*/
    eseen = FALSE;
    for (i=0; string[i]!=EOS; i++)
    	if (!isdigit(string[i]))		/* not an integer */
    	    if (string[i]=='e' || string[i]=='E')
    	    	eseen = TRUE;	/* an integer with an E somewhere in it	*/
    	    else
		{
		class = type;	/* not an integer	*/
		break;
		}
    	else
    	    class = INTG;
    if (string[0]==EOS) return(EOS);
    if (class==INTG && eseen==TRUE) {
      if (i==1) 
	class = E;	/* E alone 		*/
      else {
	if (string[0]=='e' || string[0]=='E') 
	  class = EINT;
    	else { 
	  if (string[i-1]=='e' || string[i-1]=='E')
	    class = INTE;
	  else {
	    if (string[0]!='e' && string[0]!='E')
	      class = INTEINT;
	  }
	}
      }
    }
    return(class);
    }
/* deal_pval - deallocate a string in a primitive value
 *
 * Deallocates a string pointed to by the value structure, if necessary
 *
 */
FUNCTION VOID deal_pval
(
 struct VALUE	*value,		/* input: the value 		*/
 CODE		class		/* input: value class		*/
 )
    {
    if (class == PA_QUOTED || class == PA_NAME)
	if ((*value).type == V_STRING)
	    tae_free((*value).uval.strpt);
    return;
    }

FUNCTION static CODE getctok
(

/*  Get a token, special numerical classification version.  This version
 *  is required because "E" must be a separate token for real numbers
 */
 struct	SYNBLK	*sb,		/* pointer to syntax block	*/
 char	*token			/* the token read		*/
 )
    {
    CODE	type;			/* the type of token read	*/
    CODE	class;			/* further classification 	*/

    gt_sav = ((*sb).curchr);		/* save token position for putback */
    type = gettok(sb, token);		/* get the token		*/    
    class = classify(type,token);	/* classify			*/
    return(class);
    }

/* 
 * Return the next state for the state table.  If the token is illegal at
 * this time, return a pointer to the ERROR state.  If there is an 'action'
 * routine, execute it.
 */
FUNCTION static struct STATE *getstate
(
 struct STATE	*curstate,	/* input: pointer to current state	*/
 CODE		class,		/* input: token class			*/
 TEXT		token[],	/* input: token				*/
 struct LOCVAL	*locval	/* input: pointer to local value	*/
 )
    {
    CODE		check;
    struct STATE	*out;

    out = errst;	/*assume not found, put to error state		*/
    for (; (*curstate).tkclass!=EOT; curstate++)  /* search state for this class */
    	{

    	if((*curstate).tkclass==class)
    	    {
    	    if ((*curstate).action!=NULL)  /* if there's an action routine...*/
    		{

    		check = (*(*curstate).action)(token, locval); 	/* update the value structure */

    	    	if (check == FAIL)			/*  failure in action routine	*/
    		    return (errst);
		else if (check == PA_ERRFLOW)		/*  underflow or overflow  */
		    return (errflow);
    		}
            return(statelist[(*curstate).statendx]);	/* found the correct class */
    	    }
    	if((*curstate).tkclass==DEFAULT)
    	    out = statelist[(*curstate).statendx];	/* if there's a default, use it*/
    	}
    return(out);
    }

/*  
 *  Initializes the locval and value structures
 */
FUNCTION static VOID inival
(
 struct LOCVAL 	*locval,	/* input: pointer to local value  */
 struct VALUE	*value
 )
    {
    (*locval).class = LO_NOTYPE;
    (*locval).sign = POSITIVE;
    (*locval).realval = 0;
    (*locval).expsign = POSITIVE;
    (*locval).lostring[0] = EOS;

    (*value).type = V_INTEGER;
    (*value).null = TRUE;
    (*value).marked = FALSE;
    (*value).closed = FALSE;
    (*value).origin = O_CON;
    (*value).uval.realval = 0;		/* worst case clear for uval */

    return;
    }

/*
 * Pack the output value structure with the data from the local value
 * Return the PA code for the class
 */
FUNCTION static CODE packval
(
 struct LOCVAL	*locval,	/* input			*/
 struct VALUE	*value		/* output			*/
 )
    {
    CODE		class;
    TEXT		*strpt;

    if ((*locval).class == PA_NAME || (*locval).class == PA_QUOTED)
    	{
	(*value).null = FALSE;
    	class = (*locval).class;	/* NAME or quoted string	*/
    	(*value).type = V_STRING;
    	strpt = s_save((*locval).lostring); /* allocate and place string */
    	if (strpt == NULL)
    	    class = PA_ERROR;
    	(*value).uval.strpt = strpt;	/* set the pointer to the string */
    	}
    else if ((*locval).class == V_REAL || (*locval).class == V_INTEGER)
    	{
    	class = PA_NUM;			/* it's a number		*/
	(*value).null = FALSE;	
    	(*value).type = (*locval).class;
    	(*value).uval.realval = (*locval).realval;	/* always real	*/
    	}
    else if ((*locval).class == PA_NULL)
	{
	class = PA_NULL;
	(*value).type = V_INTEGER;
	(*value).null = TRUE;
	}
    else
    	class = (*locval).class;
    (*value).marked = FALSE;		/* intialize housekeeping	*/
    (*value).closed = FALSE;
    (*value).origin = O_CON;
    return(class);
    }


/*		ACTION ROUTINES		(SORTED ALPHABETICALLY)		*/
/*
 *  Action routine for ASCII values
 */
FUNCTION CODE asciiact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = *token;
    return(SUCCESS);
    }

/* 
 *  Action routine for concatenation operator
 */
FUNCTION CODE concact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = PA_CONCAT;
    return(SUCCESS);
    }

/*
 *  Action routine for isolated exponent values
 *
 */
FUNCTION CODE expact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    TAEINT	exp;
    CODE	check, code;
    DOUBLE	temp;

    check = s_s2i1(token, &exp);		/* convert exponent			*/
    if (check!=SUCCESS) return(check);
    if ((*locval).expsign == NEGATIVE) exp = -exp;
    if (exp==0) (*locval).class = V_INTEGER;	/* if zero exponent: integer 	*/
    if ( (temp = power(10.0, exp))==0) return(PA_ERRFLOW);
    code = fl_mult ((*locval).realval, temp, &(*locval).realval);
    if (code==FAIL) code=PA_ERRFLOW;
    return(code);
    }

/* 
 *  Action routine for a token that is of the form int or intE or intEint
 *  or Eint when seen after a decimal point.
 *
 */
FUNCTION CODE fract
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    TAEINT	i;
    TEXT	tint[2];
    TAEINT	num;			/* count of leading zeroes	*/
    TAEINT	exp;			/* exponent			*/
    TAEFLOAT	temp;
    DOUBLE	realval;		/* holder for real values	*/
    CODE	check, code;		/* return from s_si2s		*/

    realval = (*locval).realval;
    (*locval).class = V_REAL;
    tint[1] = EOS;
    i=0;
    for (tint[0]=token[0]; token[i]!=EOS; i++)
    	{
    	if (token[i]=='e' || token[i]=='E') break;	/* exponent	*/
    	tint[0] = token[i];

    	s_s2i1(tint, &num);		/* convert to integer		*/

    	if ((temp = power(10.0, -i)) == 0) return(FAIL);	/* conversion failure		*/

	code = fl_mult(temp, (TAEFLOAT)(.1*num), &temp);

	if (code==FAIL) return(PA_ERRFLOW);

	code = fl_add(realval, temp, &realval);

	if (code==FAIL) return(PA_ERRFLOW);

    	}
    if ((*locval).realval == realval) 
    	(*locval).class = V_INTEGER;			/* no fraction 	*/
    (*locval).realval = realval;

    if (token[i]==EOS || token[i+1]==EOS) return(SUCCESS); /* return if no E value */

    if ((check =  s_s2i1(&token[(int) i+1], &exp)) != SUCCESS) return(check); /* convert into exp */

    if ((*locval).expsign == NEGATIVE) exp = -exp;

    if ((temp = power(10.0, exp)) == 0) return(PA_ERRFLOW);

    code = fl_mult((*locval).realval, temp,&(*locval).realval);

    if (code==FAIL) code = PA_ERRFLOW;
    return(code);
    }    

/* 
 *  Action routine for greater than or equal to relational operator
 */
FUNCTION CODE gequact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = PA_GE;
    return(SUCCESS);
    }

/* 
 *  Action routine for greater than operator
 */
FUNCTION CODE gtract
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = '>';
    return(SUCCESS);
    }

/* 
 *  Action routine for a token that is of the form int or intE or intEint
 *  or Eint when seen before a decimal point.
 *
 */
FUNCTION CODE intact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    TINY	i;
    TEXT	tint[TOKESIZ+1];
    TAEINT	exp;			/* value of the exponent	*/
    CODE	check, code;
    DOUBLE	temp;

    (*locval).class = V_INTEGER;
    i=0;
    for (tint[0]=token[0]; token[(int)i]!=EOS; i++)
    	{
	  if (token[(int)i]=='e' || token[(int)i]=='E') break;	/* exponent	*/
	  tint[(int)i] = token[(int)i];
    	}
    tint[(int)i] = EOS;			/* terminate the string			*/
    check = s_s2r1(tint, &(*locval).realval);	/* convert and put in local value 	*/
    if (check != SUCCESS) return(check);
    if (token[(int)i] == EOS || token[(int)i+1] == EOS) return(check);	/* return if no E		*/
    if ( s_s2i1(&token[i+1], &exp) != SUCCESS) return(check);
    if ((temp = power(10.0, exp)) == 0) return(PA_ERRFLOW);
    code = fl_mult((*locval).realval, temp, &(*locval).realval);
    if (code==FAIL) code = PA_ERRFLOW;    
    return(code);
    }    

/* 
 *  Action routine for less than operator
 */
FUNCTION CODE lessact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = '<';
    return(SUCCESS);
    }

/*
 *  Action routine upon detecting a variable name
 */
FUNCTION CODE nameact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    if (s_equal (token, "AND"))
	(*locval).class = PA_AND;
    else if (s_equal (token, "OR"))
	(*locval).class = PA_OR;
    else if (s_equal (token, "NOT"))
	(*locval).class = PA_NOT;
    else
	(*locval).class = PA_NAME;		/* set to temporary type, NAME	*/
    if (s_length(token) > F_Q_NAMESIZ) return(FAIL);	/* don't copy if too big	*/
    s_copy(token, (*locval).lostring);	/* just save it 		*/
    return(SUCCESS);
    }

/* 
 *  Action routine for negative exponent
 */
FUNCTION CODE negexact
(    
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
)
    {
    (*locval).class = V_REAL;
    (*locval).expsign = NEGATIVE;
    return(SUCCESS);
    }

/* 
 *  Action routine for less than or eqaul to relational operator
 */
FUNCTION CODE lequact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = PA_LE;
    return(SUCCESS);
    }

/* 
 *  Action routine for not-equal-to relational operator
 */
FUNCTION CODE nequact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = PA_NE;
    return(SUCCESS);
    }

/* 
 *  Action routine for null indicator
 */
FUNCTION CODE nullact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = PA_NULL;
    return(SUCCESS);
    }

/*
 *  Action routine for putting back the last token
 */
FUNCTION CODE putbact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    putbakfl = TRUE;
    return(SUCCESS);
    }

/*
 *  Action routine upon detecting a quoted string
 */
FUNCTION CODE quact
(
 TEXT	token[],		/* input: the token		*/
 struct LOCVAL *locval		/* input: local value structure	*/
 )
    {
    (*locval).class = PA_QUOTED;
    if (s_length(token) > STRINGSIZ) return(FAIL);
    strpqu(token); 			/* strip quotes and ...		*/
    s_copy(token, (*locval).lostring);	/* save it 			*/
    return(SUCCESS);
    }

#else


/* test driver for syntax package.
 */

    FUNCTION main (void)

    {
    IMPORT	TEXT vrsion[];
    TEXT	string[STRINGSIZ+1];
    CODE	getprim();
    CODE	x;
    struct	SYNBLK sb;
    TEXT	token[TOKESIZ+1];
    struct VALUE value;

    printf(" Classify test, version %s\n", vrsion);
    printf(" Enter a string  ");
    t_init(&x, &x, &x);
    t_read(string, &x);
    for (; string[0]!='@';)	/* exit on '@'				*/
    	{
    	initok(&sb, string);
    	while ((x = getprim(&sb, &value))!=EOS)
    	    {
    	    printf("symbol: %d", x);
    	    printf("  type: %d", value.type);
    	    if (value.type == V_REAL || value.type == V_INTEGER)
    	        printf("  realval: %20.12lf \n", value.uval.realval);
    	    else if (value.type == V_STRING)
    	        printf("  strval: %s\n", value.uval.strpt);
    	    }    	    
        printf(" Enter a string  ");
        t_read(string, &x);
    	}
    }
#endif


