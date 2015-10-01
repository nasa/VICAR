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



/* TDB CHECKOUT FILE_TIME=30-JUL-1985 15:22 DUA1:[TAEV1.OLB]SYNTAX.C;6 */
/* TPAM CHECKOUT FILE_TIME=16-DEC-1984 19:24 DUA1:[TAEV1.OLB]SYNTAX.C;1 */
/* TJM CHECKOUT FILE_TIME=29-NOV-1984 17:06 DUA0:[TAEV1.OLB]SYNTAX.C;82 */
/* TLA CHECKOUT FILE_TIME=28-NOV-1984 13:38 DUA0:[TAEV1.OLB]SYNTAX.C;81 */
/* TPEB CHECKOUT FILE_TIME=26-NOV-1984 18:07 DUA0:[TAEV1.OLB]SYNTAX.C;80 */
/* TPEB CHECKOUT FILE_TIME= 2-AUG-1984 16:47 DUA0:[TAEV1.OLB]SYNTAX.C;79 */
/*TPAM        CHECKOUT FILE_TIME=29-AUG-1983 16:51 DUA0:[TAEV1.OLB]SYNTAX.C;73 */
/*TPAM        CHECKOUT FILE_TIME=23-AUG-1983 09:56 DUA0:[TAEV1.OLB]SYNTAX.C;72 */
/*TPAM        CHECKOUT FILE_TIME=13-JUL-1983 15:47 DUA0:[TAEV1.OLB]SYNTAX.C;71 */
/* Syntax subroutines for TAE monitor.
 * This package is used to parse a TCL command string.
 * Caller must include SYNBLK to define the syntax block structure (SYNBLK).
 * The caller initializes the block by passing it and the command string
 * to "initok".
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	20-aug-83	Implement null value syntax ("--") in getval...palm
 *	29-aug-83	Keyword type parameters in getkey...palm
 *	26-sep-83	De-reference syntax in getval...palm
 *	11-oct-83	Unix compilation errors...palm
 *	12-oct-83	Fix minor recompilation error...dm
 *	24-feb-84	New freval name and calling sequence...palm
 *	02-aug-84	"Terminal Monitor" --> "TAE Monitor" (PR396)...peb
 *	26-nov-84	TCL 67: "|" becomes a field terminator...peb
 *	28-nov-84	TCL 67: de_ref() to handle parm.qual...peb
 *	29-nov-84	TUTOR 15: subchar() to detect presence of dash...lia
 *	06-dec-84	TCL 67: S_QALPHA return from gettok...jtm
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *	27-jul-85	Optimize gettok and fndsep...palm
 *	08-nov-85	PR 929/1087: Validates 'param|qual|' syntax is OK (i.e.,
 *			NOT 'param,|qual|'). Moved QUAL_SYM to SYNINC.INC...dab
 *	03-apr-87	Fix getkey to accept qualified names (X.Y.Z) so that
 *			TCL commands can have things like X.Y="hello"...palm
 *	08-jun-88	Remove ascii dependency; add type_table and white_table
 *			for ebcdic...tp
 *	13-feb-89	Fix missing entry for ~ in white_table...palm 
 */

#include	"taeconf.inp"	/* TAE configuration definitions		*/
#include 	"syninc.inc"	/* Syntax control block				*/
#include	"tmhost.inp"	/* Host-specific				*/
#include	"tminc.inc"	/* TM-only host-independent definitions		*/
#include	"chartype.inc"	/* character classification definitions		*/
#include "taeintproto.h"

FUNCTION VOID igfdp
(
 struct SYNBLK	*sb
);
FUNCTION VOID synerr
(
 FAST struct SYNBLK	*sb,		/* in/out: syntax block			*/
 FAST TEXT		*es		/* in:  error message string		*/
 );

/* MACROS:	*/

#define	GETC		( !(*(*sb).curchr) ? 0 : *(*sb).curchr++)
#define	SAVPOS		((*sb).curchr)		/* save command stream posit	*/
#define	SETPOS(P)	((*sb).curchr = (P))	/* restore cmd stream posit	*/


#ifndef testmain

/*	Type table.  type_table[c] provides a fast classification 
 *	of a character.
 */

#define  DG   S_DIGIT		/* to make table construction easy */
#define  LT   S_LETTER

#ifdef ASCII
static COMPACT_COUNT type_table[128] =
{
0,     1,    2,     3,    4,    5,    6,    7,    8,    9,
10,   11,   12,    13,   14,   15,   16,   17,   18,   19,
20,   21,   22,    23,   24,   25,   26,   27,   28,   29,
30,   31,   32,    33,   34,   35,   LT,   37,   38,   39,  /* $=36 */
40,   41,   42,    43,   44,   45,   46,   47,   DG,   DG,
DG,   DG,   DG,    DG,   DG,   DG,   DG,   DG,   58,   59, 
60,   61,   62,    63,   64,   LT,   LT,   LT,   LT,   LT,
LT,   LT,   LT,    LT,   LT,   LT,   LT,   LT,   LT,   LT,
LT,   LT,   LT,    LT,   LT,   LT,   LT,   LT,   LT,   LT,
LT,   91,   92,    93,   94,   LT,   96,   LT,   LT,   LT,  /* _=95 */
LT,   LT,   LT,    LT,   LT,   LT,   LT,   LT,   LT,   LT,
LT,   LT,   LT,    LT,   LT,   LT,   LT,   LT,   LT,   LT,
LT,   LT,   LT,   123,   124,  125,  126,  127
};
#else
#ifdef EBCDIC
static COMPACT_COUNT type_table[256] =
{
  0,    1,    2,    3,    4,    5,    6,    7,    8,    9,
 10,   11,   12,   13,   14,   15,   16,   17,   18,   19,
 20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
 30,   31,   32,   33,   34,   35,   36,   37,   38,   39,
 40,   41,   42,   43,   44,   45,   46,   47,   48,   49,
 50,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 60,   61,   62,   63,   64,   65,   66,   67,   68,   69,
 70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 90,   LT,   92,   93,   94,   95,   96,   97,   98,   99,  /* $=91 */
100,  101,  102,  103,  104,  105,  106,  107,  108,   LT,  /* _=109 */
110,  111,  112,  113,  114,  115,  116,  117,  118,  119,
120,  121,  122,  123,  124,  125,  126,  127,  128,   LT,
 LT,   LT,   LT,   LT,   LT,   LT,   LT,   LT,  138,  139,
140,  141,  142,  143,  144,   LT,   LT,   LT,   LT,   LT,
 LT,   LT,   LT,   LT,  154,  155,  156,  157,  158,  159,
160,  161,   LT,   LT,   LT,   LT,   LT,   LT,   LT,   LT,
170,  171,  172,  173,  174,  175,  176,  177,  178,  179,
180,  181,  182,  183,  184,  185,  186,  187,  188,  189,
190,  191,  192,   LT,   LT,   LT,   LT,   LT,   LT,   LT,
 LT,   LT,  202,  203,  204,  205,  206,  207,  208,   LT,
 LT,   LT,   LT,   LT,   LT,   LT,   LT,   LT,  218,  219,
220,  221,  222,  223,  224,  225,   LT,   LT,   LT,   LT,
 LT,   LT,   LT,   LT,  234,  235,  236,  237,  238,  239,
 DG,   DG,   DG,   DG,   DG,   DG,   DG,   DG,   DG,   DG,
250,  251,  252,  253,  254,  255
};
#else
	compilation error: only ASCII and EBCDIC tables are coded.
#endif
#endif


/*	white_table.  Provides a fast BOOL telling whether 
 *	a character is white space and not EOS.
 */
#ifdef ASCII
static TEXT white_table[128] =	
    {
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 00-09 */
    TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 10-19 */
    TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 20-29 */
    TRUE,  TRUE, TRUE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 30-39 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 40-49 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 50-59 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 60-69 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 70-79 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 80-89 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/* 90-99 */
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/*100-109*/
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,/*110-119*/
    FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE  		/*120-127*/
    };

#else
#ifdef EBCDIC
static TEXT white_table[256] =	
    {
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 00-09 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 10-19 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 20-29 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 30-39 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 40-49 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 50-59 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 60-69 */
     TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, /* 70-79 */
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /* 80-89 */
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE, TRUE, /* 90-99 */
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE, /*100-109*/
    FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /*110-119*/
     TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE,FALSE, /*120-129*/
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE, TRUE, /*130-139*/
     TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE, /*140-149*/
    FALSE,FALSE,FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /*150-159*/
     TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, /*160-169*/
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /*170-179*/
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /*180-189*/
     TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, /*190-199*/
    FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE, /*200-209*/
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE, TRUE, /*210-219*/
     TRUE, TRUE, TRUE, TRUE,FALSE, TRUE,FALSE,FALSE,FALSE,FALSE, /*220-229*/
    FALSE,FALSE,FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, /*230-239*/
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, /*240-249*/
     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE				 /*250-255*/
    };
#else
	compilation error: only ASCII and EBCDIC tables are coded.
#endif
#endif



/* addqu - put quotes around a string.
 * NOTE:  String array must be large enough to hold the
 *	  string with quotes added (i.e., to hold 2 more characters).
 */

FUNCTION VOID addqu
(
   FAST TEXT	s[]		/* in/out: string to quote */
 )
    {
    COUNT	len;		/* string length				*/
    FAST COUNT	i;

    for (i = len = s_length(s); i >= 1; i--)
	s[i] = s[i-1];

    s[len+1] = '"';
    s[0]     = '"';
    s[len+2] = EOS;
    return;
    }

/* chkend - check that there are no more non-white tokens,
 * i.e., check that this is the end of the command.
 * Function return codes:
 *
 *	SUCCESS
 *	S_SYNERR
 */

FUNCTION CODE chkend
(
 FAST struct SYNBLK	*sb	/*  in/out: syntax block		*/
 )
    {
    FAST CODE	code;			/* return code				*/
    CODE	toktyp;			/* token type				*/
    TEXT	dummy[TOKESIZ+1];

    if ((toktyp = gettok(sb, dummy)) == S_WHITE)	/* get token...		*/
	toktyp = gettok(sb, dummy);	/* if white, get another		*/
    if (toktyp == EOS)
	code = SUCCESS;
    else
	{
	code = S_SYNERR;
	(*sb).errchr = (*sb).curchr - 1;
	synerr(sb, "Unexpected trailing characters");
	}
    return(code);
    }

/*	dash_dash.   Returns TRUE if we are now positioned at "--"
 *	followed by white, comsep, or EOS.
 *	If dash_dash returns FALSE, then position is unchanged.
 *	If dash_dash returns TRUE, the -- plus separator has been gobbled.
 */
 
FUNCTION BOOL dash_dash
(
    FAST struct SYNBLK	*sb
 )
    {
    TEXT    token[TOKESIZ+1];
    CODE    toke_type;
    TEXT    *init_pos;
    
    init_pos = SAVPOS;
    toke_type = gettok(sb, token);
    if (toke_type == S_WHITE)
        toke_type = gettok(sb, token);
    if (toke_type == '-')
        {
	toke_type = gettok(sb, token);		/* look for 2nd dash 	*/
	if (toke_type == '-')
	    {
	    toke_type = gettok(sb, token);	/* get separator	*/
	    if (toke_type == S_WHITE || toke_type == S_COMSEP ||
	        toke_type == EOS)
		{
		(*sb).lstcg = toke_type;
	        return (TRUE);
		}
	    }
	}
    SETPOS(init_pos);				/* return to init pos	*/
    return (FALSE);
    }	    

/*	
 *	de_ref.    Look for de-referenced variable, i.e., @name.
 *	NOTE: variable may be a full name field, e.g. parm.qual.
 *
 *	Returns:
 *		SUCCESS -- valid de-reference found
 *		FAIL    -- not a de-reference; sb re-positioned
 *		S_SYNERR-- syntax error in de-reference
 *
 */
 
FUNCTION static CODE de_ref
(
 FAST struct SYNBLK *sb,		/* in/out: syntax blk -- cmd stream*/
 FAST TEXT	    name[F_Q_NAMESIZ+1]	/* out: deref'd var name string	*/
 )
    {
    TEXT	token[TOKESIZ+1];
    FAST CODE	toke_type;
    TEXT	*init_pos, *fld_pos;
    CODE	code;
    
    name[0] = EOS;
    init_pos = SAVPOS;
    toke_type = gettok(sb, token);
    if (toke_type == S_WHITE)
        toke_type = gettok(sb, token);
    if (toke_type == '@')
        {
	fld_pos = SAVPOS;			/* posit of begin of name field	*/
	toke_type = gettok(sb, token);		/* look for symbol	*/
	if (toke_type == S_ALPHA  ||  toke_type == S_QALPHA)
	    {
	    SETPOS(fld_pos);			/* back up to get entire name fld*/
	    code = getfld(sb, token);
	    if (code == SUCCESS  ||
		code == S_WHITE  ||  code == S_COMSEP  ||  code == EOS)
    	        {
		s_bcopy(token, name, F_Q_NAMESIZ);
		(*sb).lstcg = code;
	        return (SUCCESS);
		}
	    }
	(*sb).errchr = (*sb).curchr - 1;
	synerr(sb, "A variable name must follow '@'.");
	return (S_SYNERR);
	}
    SETPOS(init_pos);				/* return to init pos	*/
    return (FAIL);
    }	    

/* errstr - get error string which surrounds the position of the error
 * detection in the command string.
 */

FUNCTION VOID errstr
(
 FAST struct SYNBLK *sb,	/* in/out: syntax block			*/
 FUNINT		nchar,		/* in:  number of characters to return	*/
 FAST TEXT	*txtstr	        /* out: surrounding chars string	*/
 )
    {
    COUNT	i, k;
    FAST TEXT	*ierr;			/* pntr to error chars in syntax block	*/

    i    = (*sb).errchr - (*sb).inichr - (nchar/2) + 1;
    if(i < 1)
      i = 1;
    ierr = (*sb).inichr + i - 1;	/* point to 1st char of text		*/
    for (k = 1; k <= nchar; k++, txtstr++, ierr++)
	{
	if (*ierr == EOS)
	    break;
	*txtstr = *ierr;		/* move chars to output string		*/
	}
    *txtstr = EOS;			/* put on string terminator		*/
    return;
    }

/* fldtrm - returns TRUE if token is a field terminator.
 */

FUNCTION BOOL fldtrm
(
  FAST FUNINT	toktyp		/*in:  token type */
)
    {
    if (toktyp == S_WHITE   ||
	toktyp == S_COMSEP  ||
	toktyp == ')'       ||
	toktyp == '='       ||
	toktyp == '<'       ||
	toktyp == '>'       ||
	toktyp == QUAL_SYM  ||
	toktyp == EOS)

	return(TRUE);
    else
	return(FALSE);
    }

/* fndsep - report separator token if next in command stream
 * and position past it.
 * No repositioning if separator isn't next.
 * Function return values:
 *
 *	S_WHITE  for white space (blanks & tabs)
 *	S_COMSEP for a comma (possibly surrounded by white space)
 *	anything else if separator is not next in command stream
 * 
 *	CAUTION: the efficiency of this function is critical
 *	to TAE performance.
 */

FUNCTION CODE fndsep
(
  FAST struct SYNBLK	*sb		/* in/out: syntax block		*/
)
    {
    FAST CODE	code;			/* return code			*/
    FAST COUNT	c;

    code = c = GETC;			/* get next char		*/
    if (white_table[c])			/* if white char and not EOS	*/
	{
	for (c = GETC; white_table[c]; c = GETC)
	    ;				/* eat white until non-white	*/
	if (c == ',')			/* if white terminated with comma*/
	    {
	    for (c = GETC; white_table[c];  c = GETC)
		;			/* eat trailing white		*/
    	    code = S_COMSEP;
	    }
        else
    	    code = S_WHITE;
	}
    else if (c == ',')			/* if comma			*/
	{
        code = S_COMSEP;
	for (c = GETC; white_table[c];  c = GETC)
	    ;				/* eat trailing white		*/
	}
    if (c != EOS)			/* put back non-separator char	*/
	(*sb).curchr--;
    return(code);
    }

/*	free_val - free command line values from dynamic memory.
 */

FUNCTION VOID free_val
(
 FAST TEXT	*value[],	/* in/out: array of pointers to strings	*/
 FUNINT 	count		/* in/out: actual number of values	*/
 )
    {
    FAST COUNT	i;

    for (i = 0; i < count; i++)    
	{
	s_free(value[i]);
	value[i] = NULL;
	}
    return;
    }

/* getchr- get next character.
 * Should only be used by callers outside this source file.
 * Callers inside this source file should use the GETC macro.
 * Returns EOS if end of string.
 */

FUNCTION TEXT getchr
(
  struct SYNBLK	*sb		/* in/out: syntax block			*/
)
    {
    return(GETC);
    }

/* getfld - get a value field, i.e., a string of tokens terminated by
 * white space, a comma, a right paren, an equal sign, a less than
 * or greater than sign, a qualifier separator, or end of string.
 * Function return codes:
 *
 *	SUCCESS   - success
 *	S_SYNERR  - syntax error
 *	S_QUOTED  - quoted string (quotes removed)
 *	S_RPAREN  - right parenthesis
 *	EOS	  - end of command stream
 *	another field terminator
 */

FUNCTION CODE getfld
(
 FAST struct SYNBLK	*sb,	/* in/out: syntax block			*/
 FAST TEXT		*field	/* out: field string of length TOKESIZ+1*/
)
    {
    CODE	code;
    FAST CODE	toktyp;			/* token type				*/
    TEXT	token[TOKESIZ+1];

    *field = EOS;			/* initialize field to null string	*/
    code   = SUCCESS;
    if ((toktyp = gettok(sb,token)) == S_WHITE)	/* get token			*/
	toktyp = gettok(sb,token);	/* if white, get another		*/
    if (toktyp == S_SYNERR)		/* if syntax error			*/
	code = S_SYNERR;
    else if (toktyp == S_COMSEP)
	if ((*sb).inpar)
	    (*sb).lstcp = S_COMSEP;
	else
	    (*sb).lstcg = S_COMSEP;
    else if (toktyp == EOS)
	if ((*sb).lstcg == S_COMSEP || (*sb).lstcg == S_START)
	    (*sb).lstcg = EOS;
	else
	    code = EOS;
    else if (toktyp == ')' )		/* if right paren			*/
	if ((*sb).lstcp  == S_COMSEP || (*sb).lstcp == '(' )
	    {
	    (*sb).lstcp = ')';
	    (*sb).curchr--;		/* so we get right paren on next call	*/
	    }
	else
	    {
	    code  = S_RPAREN;
	    (*sb).inpar = FALSE;	/* no longer w/in parentheses		*/
	    fndsep(sb);			/* posit past trailing sep if present	*/
	    }
    else if (fldtrm(toktyp))		/* if token was a field terminator	*/
	{
	code   = toktyp;
	s_copy(token, field);
	}
    else
	{
	if (toktyp==S_QUOTED) code = S_QUOTED;	/* call it S_QUOTED if we lead with a quote*/
	while (!fldtrm(toktyp))		/* loop until a field terminator	*/
	    {
	    if (toktyp == S_QUOTED)	/* if token is a quoted string		*/
		strpqu(token);		/* strip quotes				*/
	    if (s_length(token) + s_length(field) > TOKESIZ)
		return(S_SYNERR);
	    s_append(token, field);	/* add token to field			*/
	    if ((toktyp = gettok(sb, token)) == S_SYNERR) /* get another token	*/
		return(S_SYNERR);
	    }
	if (toktyp != S_COMSEP && toktyp != S_WHITE)	/* put back terminator...*/
	    if (toktyp != EOS)		/* if not also separator		*/
		(*sb).curchr--;
	if ((*sb).inpar)
	    (*sb).lstcp = toktyp;
	else
	    (*sb).lstcg = toktyp;
	}
    return(code);
    }

/* getkey - get a keyword from the command stream.
 * Skip leading separators & eat trailing equals symbol.
 * Function return codes:
 *
 *	SUCCESS  - success
 *	S_SYNERR - syntax error
 *	S_NONE	 - no keyword in this parm position
 *	S_KEYWORD- flagged keyword found (e.g., 'LIST)
 *	EOS	 - end of command stream
 */

FUNCTION CODE getkey
(
 FAST struct SYNBLK	*sb,		/* in/out: symbol table		*/
 TEXT		key[TOKESIZ+1]	/* out: parameter name		*/
 )
    {
    FAST CODE	code;
    FAST CODE	toktyp;			/* token type				*/
    TEXT	dummy[TOKESIZ+1];

    key[0] = EOS;
    code = SUCCESS;
    if ((toktyp = gettok(sb, key)) == S_WHITE)	/* get token into key		*/
	toktyp = gettok(sb, key);	/* if white, get another		*/
    if (toktyp == S_COMSEP)		/* if comma separator			*/
	code = S_NONE;
    else if (toktyp == EOS)
	if ((*sb).lstcg == S_COMSEP)	/* if following comma			*/
	    {
	    code = S_NONE;		/* null value				*/
	    (*sb).lstcg = EOS;
	    }
	else				/* otherwise				*/
	    code = EOS;			/* end of command stream		*/
    else if (toktyp == FLAG_CHAR)	/* start of flagged keyword?	*/
	code = S_KEYWORD;
    else if (toktyp != S_ALPHA && toktyp != S_QALPHA)
	{
	(*sb).errchr = (*sb).curchr - 1;/* point to where error detected	*/
	synerr(sb, "Invalid parameter name");	/* put EM in syntax block	*/
	code = S_SYNERR;
	}
    else				/* got a keyword (or positional value)	*/
	{
	if ((toktyp = gettok(sb, dummy)) == S_WHITE)	/* eat the "="		*/
	    toktyp = gettok(sb, dummy);
	if (toktyp ==  '=')		/* if trailing "=" was present		*/
	    {
	    if (s_length(key) > F_Q_NAMESIZ)
		{
		(*sb).errchr = (*sb).curchr - 1;
		synerr(sb, "Parameter name too long");
		code = S_SYNERR;
		}
	    }
	else				/* else no trailing blank, so...	*/
	    code = S_NONE;		/* value without keyword		*/
	}
    return(code);
    }

/* getqlf - get qualifier string.
 * Gets the qualifier string from the command stream.
 * If no qualifiers are found, the command stream is not repositioned.
 * If qualifiers are found, the command stream is positioned to after
 * the closing slash, and one string containing all qualifiers (with the
 * slashes stripped) is returned.
 * Return codes:
 *
 *	SUCCESS
 *	S_NONE
 *	S_SYNERR
 */

FUNCTION CODE getqlf
(
 struct SYNBLK	*sb,	/* in/out: syntax block			*/
 TEXT		qualstr[] /* out:  qualifier string		*/
 )

    {
    GENPTR		pos;
    CODE		toktyp;
    TEXT		token[TOKESIZ+1];
 

    pos = SAVPOS;			/* save position in case no quals found	*/
    if ((toktyp = gettok(sb, token)) == S_WHITE)	/* get token...		*/
	toktyp  = gettok(sb, token);	/* if white, get another		*/
    if (toktyp == S_SYNERR) goto syn_err;
    if (toktyp != QUAL_SYM)		/* if no qualifiers			*/
	{
	SETPOS(pos);			/* restore position			*/
	return(S_NONE);
	}

    /* The following checks are made to prevent improper syntax of 'proc a,|b|'
       or 'proc (a,b),|c|' from being processed identically to 'proc a|b|' or
       'proc (a,b)|c|', respectively.					*/

    pos = SAVPOS;			/* save pos as syntax check will change it	*/
    if ((*sb).lstcg == S_COMSEP)	/* if terminator char of last field was a comma */
        goto syn_err;
    else if ((*sb).lstcg == S_RPAREN)	/* since a comma separator isn't recorded... */
	{				/* check for one explicitly			*/
	SETPOS(pos-2);			/* back over current and '|' chars		*/
	while (*(*sb).curchr != ')')
	    {
	    if (*(*sb).curchr == ',')
		goto syn_err;
	    else
		(*sb).curchr--;
	    }
	SETPOS(pos);			/* restore position	*/
	}

    qualstr[0] = EOS;
    while (FOREVER)
	{
	if ((toktyp = gettok(sb, token)) == S_SYNERR) goto close_err;	/* get next token*/
	if (toktyp == QUAL_SYM)
	    break;			/* break if qualifiers done		*/
	if (toktyp == EOS) goto close_err;	/* syntax error if no closing "/"*/
	if (s_length(token) + s_length(qualstr) > CMDLINSIZ) goto close_err;	/* if no closing "/"*/
	s_append(token, qualstr);
	}
    return(SUCCESS);

syn_err:
    (*sb).curchr = (*sb).curchr - 1;	/* general syntax error			*/
    synerr(sb, "Incorrect format for qualifier");
    return(S_SYNERR);

close_err:
    (*sb).errchr = (*sb).curchr - 1;	/* point to error detection		*/
    synerr(sb, "No closing '|' in qualifier list");
    return(S_SYNERR);
    }

/* gettok - get next token from command stream in syntax block.
 * Function return codes:
 *
 *	S_ALPHA	 - a/n token (not quoted)
 *	S_QALPHA - a/n token, with an embedded period ("."), indicating
 *		   a variable name with qualifier(s)
 *	S_WHITE	 - white space (blanks and/or tabs)
 *		   note: this return indicates that all consecutive white
 *			has been passed
 *	S_QUOTED - a quoted string
 *	S_COMSEP - comma separator (possibly surrounded by white space)
 *	EOS	 - if end of command stream
 *	S_SYNERR - if a GETTOK error
 *	a single character if other non a/n
 */

FUNCTION CODE gettok
(
 FAST struct SYNBLK	*sb,		/* in/out: syntax block			*/
 FAST TEXT		*token		/* out: token (if the function value
					 *	is S_WHITE, token is one blank,
					 *	if the function value is S_COMSEP,
    					 *	token is ","			*/
)
    {
    FAST CODE	code;	
    CODE	first_type, c;
    BOOL	qualifier;
    FAST COUNT	i;
    TEXT	junk;

/*
 *	CAUTION:  the efficiency of this function is critical 
 *	to TAE performance.  The loop that collects an alphnumeric 
 *	string is especially critical.
 *
 *
 *	First, this is a copy of the fndsep ("find separator") logic
 *	copied in-line here to avoid the function call overhead.
 */
    code = c = GETC;			/* get next char		*/
    if (white_table[c])			/* if white char and not EOS	*/
	{
	for (c = GETC; white_table[c]; c = GETC)
	    ;				/* eat white until non-white	*/
	if (c == ',')			/* if white terminated with comma*/
	    {
	    for (c = GETC; white_table[c];  c = GETC)
		;			/* eat trailing white		*/
    	    code = S_COMSEP;
	    }
        else
    	    code = S_WHITE;
	}
    else if (c == ',')			/* if comma			*/
	{
        code = S_COMSEP;
	for (c = GETC; white_table[c];  c = GETC)
	    ;				/* eat trailing white		*/
	}
    if (c != EOS)			/* put back non-separator char	*/
	(*sb).curchr--;
    if (code == S_WHITE)
	{
	token[0] = ' ';
	token[1] = EOS;
	return (code);
	}
    else if (code == S_COMSEP)
	{
	token[0] = ',';
        token[1] = EOS;
	return (code);
	}

/*	Here, the token is either Alpha/numeric string (S_ALPHA),
 *	a qualified A/N (S_QALPHA), a quoted string
 *	(S_QUOTED), or punctuation (the punctuation character).
 */

    *token = GETC;			/* get first char of token	*/
    code = type_table[(int) *token++];	/* classify first character	*/
    if (code == S_LETTER || code == S_DIGIT)	/* alph/numeric string  */
	{
	first_type = code;		/* type of first character	*/
	qualifier = FALSE;		/* assume not qualified name	*/
	for (i = 2; i <= TOKESIZ; i++, token++)
	    {
	    *token = GETC;		/* get next character		*/
	    code = type_table[(int) *token];  /* get character type		*/
	    if (code == '.'   &&   first_type == S_LETTER)
		qualifier = TRUE;	/* name with a qualifier */
	    else if (code != S_LETTER && code != S_DIGIT)
		break;			/* end of A/N string	*/
	    }
	if (i > TOKESIZ)
	    return (S_SYNERR);
	else
	    {
	    if (code != EOS)			/* put last char back	*/
		(*sb).curchr--;
	    *token = EOS;			/* token terminator	*/
	    if (qualifier)
		return (S_QALPHA);		/* qualified A/N	*/
	    else
		return (S_ALPHA);
	    }
	}
    else if (code == '"')			/* a quoted string	*/
	{
	i = 1;					/* character counter	*/
	token--;				/* go back to start	*/
	do					/* outer loop is to handle interior quotes	*/
	    {
	    for (i++, token++; (*token = GETC) != '"'; i++, token++)
		if (*token == EOS || i >= TOKESIZ)
    		    return (S_SYNERR);
	    }
	while ((junk = GETC) == '"');		/* end outer loop	*/
	if (junk != EOS) (*sb).curchr--;	/* make sure we re-read the last char	*/
	*(token+1) = EOS;			/* string terminator	*/
        return (S_QUOTED);
	}
    else					/* 1 char, non-alpha	*/
        {
	*token	= EOS;				/* string terminator	*/
        return (code);
        }
    }

/* getval - get value string(s) from the command stream.
 * Returns pointers to value strings in dynamic storage, 
 * and the number of values.
 * If parens, all values between parens are used, else the next value is used.
 * Function return codes (status):
 *
 *	SUCCESS	 - success
 *	S_SYNERR - syntax error
 *	S_DEREF  - de-reference syntax
 *	EOS	 - end of command stream
 *
 *	Note that the "--" and '@' syntax is only detected outside 
 *	of parentheses;
 *	these notations cannot be inside parentheses because 
 *	they represent a total value, not a component of a value.
 */

FUNCTION CODE getval
(
 FAST struct SYNBLK *sb,	/* in/out: syntax block		*/
 FAST TEXT	*value[],	/* out: array of pointers to strings	*/
 FUNINT		maxval,		/* in:  dimension of value	*/
 COUNT		*count		/* out: actual number of values:*/
				/* -1 = no value; 0 = --;	*/
 )
    {
    CODE	toktyp;			/* token type			*/
    FAST CODE	i;
    TEXT	field[TOKESIZ+1];	/* single value field		*/
    CODE	code, status;		/* return code			*/
    GENPTR	pos;
    TEXT	token[TOKESIZ+1];


    *count = 0;		
    code   = SUCCESS;
    pos    = SAVPOS;			/* save position for look-ahead	*/
    if (dash_dash(sb))
	{
	*count = 0;
	return (SUCCESS);		/* note *count is zero for --	*/
	}
    status = de_ref (sb, field);	/* look for de-reference syntax	*/
    if (status == SUCCESS)
        {
	value[0] = s_save (field);
	if (value[0] == NULL)
	    goto gv_merr;
	*count = 1;
	return (S_DEREF);
	}
    else if (status == S_SYNERR)
        return (status);

    if ((toktyp = gettok(sb, token)) == S_WHITE)	/* get token...		*/
	toktyp = gettok(sb, token);	/* if white, get another		*/
    if (toktyp == S_COMSEP)		/* if no value				*/
	*count = -1;
    else if (toktyp == S_SYNERR) goto gv_serr;	/* if syntax error		*/
    else if (toktyp == '(' )		/* left paren seen - assume multi-valued*/
	{
	igfdp(sb);			/* init block for getfld (w/in parens)	*/
	for (i = getfld(sb, field); i != S_RPAREN; i = getfld(sb, field))
	    {
	    if (i == S_SYNERR || i == EOS) goto gv_bval;
	    if (*count >= maxval) goto gv_mval;	/* error if max values reached	*/
	    value[*count] = s_save(field);	/*set value ptr to alloc values	*/
    	    if (value[*count] == NULL)
    		goto gv_merr;			/* no memory left		*/
	    (*count)++;
	    }
        if (*count == 0)
	    *count = -1;		/* flag 'no value'			*/
	(*sb).lstcg = S_RPAREN;	/* last terminator is right paren	*/
	}
    else				/* not multi-valued			*/
	{
	SETPOS(pos);			/* restore cmd stream position		*/
	if ((i = getfld(sb, field)) == S_SYNERR || i == S_RPAREN || i == '=')
	    goto gv_serr;
	else if (i == EOS)
	    code = EOS;
	else if (!NULLSTR(field) || i == S_QUOTED)
            {
	    value[0] = s_save(field);
	    if (value[0] == NULL) goto gv_merr;
	    *count = 1;
      	    }
        else if (NULLSTR(field))
	    *count = -1;		/* flag for 'no value'		*/
	}
    return(code);

gv_serr:
    (*sb).errchr = (*sb).curchr - 1;	/* point to error detection		*/
    synerr(sb, "Invalid value format");	/* put EM in block			*/
    code = S_SYNERR;
    return(code);
gv_bval:
    code = S_SYNERR;
    (*sb).errchr = (*sb).curchr - 1;	/* point to error detection		*/
    synerr(sb, "Missing value or invalid value format");
    free_val (value, *count);		/* free values allocated before error	*/
    *count = 0;
    return(code);
gv_mval:
    (*sb).errchr = (*sb).curchr - 1;	/* point to error detection		*/
    s_copy("Maximum number of values exceeded", (*sb).errmsg);
    code = S_SYNERR;
    free_val(value, *count);		/* free values allocated before error	*/
    *count = 0;
    return(code);
gv_merr:
    code = S_SYNERR;
    (*sb).errchr = (*sb).curchr - 1;	/* point to error detection		*/
    synerr(sb, "TAE Monitor internal memory overflow");
    free_val (value, *count);		/* free values allocated before error	*/
    *count = 0;
    return(code);
    }

/* 	getvrb - get verb from a command stream.  The verb may be a quoted
 *	host file spec.   
 *
 *	getvrb is not valid for command mode.  See cmdfield.c for TAE
 *	command line processing.
 */

FUNCTION CODE getvrb
(
 FAST struct SYNBLK *sb,    /* in/out: syntax block			*/
 TEXT		*verb	    /*out: verb				*/
)
    {
    FAST CODE	code;			/* return code				*/
    FAST CODE	toktyp;			/* token type				*/

    code = SUCCESS;
    if ((toktyp = gettok(sb, verb)) == S_WHITE)	/* get a token...		*/
	toktyp = gettok(sb, verb);	/* if white, get another		*/
    if (toktyp != S_ALPHA && toktyp != S_QUOTED)  
	{
	code = S_SYNERR;
	(*sb).errchr = (*sb).curchr - 1;
	synerr(sb, "Incorrectly formatted command name");
	}
    strpqu(verb);			/* remove quotes if present		*/
    return(code);
    }

/* igfdp - initialize getfld function for multi-valued parm w/in parentheses.
 */

FUNCTION VOID igfdp
(
 struct SYNBLK	*sb
)
    {
    (*sb).inpar = TRUE;
    (*sb).lstcp = '(' ;
    return;
    }

/* initok - initialize syntax block for syntax package.
 */

FUNCTION VOID initok
(
 FAST struct SYNBLK	*sb,		/* out: syntax block to initialize	*/
  TEXT		*usrstr	/* in:  command stream			*/
 )
    {
    (*sb).inichr  = (*sb).curchr = usrstr;	/* point to beginning of string	*/
    (*sb).inpar = FALSE;		/* not within parens			*/
    (*sb).lstcg = S_START;
    (*sb).errchr  = NULL;		/* no error message now			*/
    *(*sb).errmsg = EOS;		/* init message string to null string	*/
    return;
    }

/*
 *  name_check - Check if a string is a valid TAE standard name
 *
 *  returns SUCCESS or FAIL
 */
FUNCTION CODE name_check
(
 TEXT		name[]		/* in: candidate name		*/
 )
    {
    TEXT		*npt;

    for (npt = name; (*npt)!=EOS; npt++)
    	if (	(*npt) != '_'		/* valid chars: $, _, any a/n	*/
    	      &&(*npt) != '$'
    	      &&(!isalpha(*npt))
    	      &&(!isdigit(*npt))	)
    	    return(FAIL);
    return(SUCCESS);
    }

/* sindex - return index to position in string of the first occurence of
 * supplied character.  Return value is character position (1st char = 0),
 * -1 if char not found.
 */

FUNCTION COUNT sindex
(
 FAST TEXT	*s,			/* in:  the string			*/
 FUNINT	c			/* in: the character			*/
)
    {
    FAST COUNT	index;

    for (index = 0; *s != EOS; index++, s++)
	if (*s == c)
	    return(index);
    return(-1);
    }

/* spchar - TRUE if string contains a token delimiter.
 */

FUNCTION BOOL spchar
(
 FAST TEXT	*s			/* in:  string			*/
 )
    {
    FAST CODE	chrtyp;			/* character type		*/

    for ( ; *s != EOS; s++)
	{
	  chrtyp = type_table[(int) *s];	/* get type of character	*/
	if (chrtyp != S_LETTER && chrtyp != S_DIGIT)
	    return(TRUE);
	}
    return(FALSE);
    }

/* strpqu - strip quote symbols from a quoted string.
 * If not quoted, do nothing.
 * NOTE:  Do not pass constant string to this routine!
 */

FUNCTION VOID strpqu
(
    FAST TEXT	s[]			/* in/out: string to strip		*/
 )
    {
    FAST COUNT	i;

    if (*s == '"')			/* if string starts with quote		*/
	{
	i = s_shift(s, 1);		/* eliminate left quote			*/
	s[i-1] = EOS;			/* eliminate right quote		*/
	}
    return;
    }

/* subchar - TRUE if string contains a dash for subcommand.
 */

FUNCTION BOOL subchar
(
    FAST TEXT	*s			/* in:  string				*/
)
    {
    for ( ; *s != EOS; s++)
	{
	if (*s == '-')			/* if string contain a dash		*/
	    return(TRUE);
	}
    return(FALSE);
    }

/* synerr - put syntax error message in syntax block.
 * Included in the error message are NERRCH characters surrounding
 * the point of detection of the syntax error.
 *
 * NOTE: if the caller's error message is too long a default error message,
 * it's truncated.
 */

FUNCTION VOID synerr
(
 FAST struct SYNBLK	*sb,		/* in/out: syntax block			*/
 FAST TEXT		*es		/* in:  error message string		*/
 )
#define		NERRCH	10		/* dump 10 chars of surrounding text	*/

    {
    FAST COUNT	i;
    TEXT	txt[NERRCH+1];		/* buffer to hold surrounding text	*/

    
    for (i=0; *es != 0; es++, i++)	/* copy caller's string			*/
    	{
	if (i == EMSIZ - (NERRCH+sizeof(" at or near  ")+2) )
    	    break;					/* don't wipe out 	*/
    	(*sb).errmsg[i] = *es;		/* move caller's string to block	*/
    	}
    s_copy(" at or near  ", &(*sb).errmsg[i]);
    errstr(sb, NERRCH, txt);		/* get text surrounding error		*/
    s_append("'", (*sb).errmsg);	/* enclose in tick marks		*/
    s_append(txt, (*sb).errmsg);	/* append text to EM			*/
    s_append("'", (*sb).errmsg);
    return;
    }

#else


/* test driver for syntax package.
 */

FUNCTION main (void)

    {
    struct SYNBLK	sb;		/* syntax block				*/
    TEXT		s[STRINGSIZ+1];	/* string buffer			*/
    TEXT		verb[TOKESIZ+1];/* verb					*/
    TEXT		key[TOKESIZ+1];	/* keyword				*/
    TEXT		*value[MAXVAL];	/* value pointers			*/
    CODE		code;
    COUNT		count;
    COUNT		tstnum;
    COUNT		i;
    CODE		dum;

    dumrout(&code, &tstnum);		/* dummy - get addrs w/ debugger	*/

    tstnum = 0;

/* TEST 1 */
    printf("\n TEST 1 \n");
    s_copy("menu file=alpha a", s);
    initok(&sb, s);
    tstnum++;

    code = getvrb(&sb, verb);
    printf(" code: %d, verb: %s\n", code, verb);
    tstnum++;

    code = getkey(&sb, key);
    printf(" code: %d, key: %s\n", code, key);
    tstnum++;

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);
    tstnum++;

    code = chkend(&sb);
    tstnum++;

/* TEST 2 */
    printf("\n TEST 2 \n");
    s_copy("menu (A, B, CDE) file = alpha", s);
    initok(&sb, s);

    code = getvrb(&sb, verb);
    printf(" code: %d, verb: %s\n", code, verb);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = getkey(&sb, key);
    printf(" code: %d, key: %s\n", code, key);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = chkend(&sb);

/* TEST 3 */
    printf("\n TEST 3 \n");
    s_copy("vrb ,(A,,C,) f,", s);
    initok(&sb, s);

    code = getvrb(&sb, verb);
    printf(" code: %d, verb: %s\n", code, verb);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

/* TEST 4 */
    printf("\n TEST 4 \n");
    s_copy("(\"aaa\", \"bbb\")", s);
    initok(&sb, s);


    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);


/* TEST 5 */
    printf("\n TEST 5 \n");	/* white space test		*/
    s_copy("menu (A, B, CDE) file = alpha", s);
    initok(&sb, s);

    code = getvrb(&sb, verb);
    printf(" code: %d, verb: %s\n", code, verb);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = getkey(&sb, key);
    printf(" code: %d, key: %s\n", code, key);

    code = getval(&sb, value, MAXVAL, &count);
    printf(" code: %d, count: %d, values:  \n", code, count);
    for (i=0; i<count; i++)
	printf("    %s\n", value[i]);

    code = chkend(&sb);


/* USER TEST */
    while (FOREVER)
	{

	printf("\n Enter a string of values:\n");
	t_init(&dum, &dum, &dum);
	t_read(s, &dum);
	initok(&sb, s);


	code = getval(&sb, value, MAXVAL, &count);
	while (code !=EOS && code!=S_SYNERR)
	    {
	    printf(" code: %d, count: %d, values:  \n", code, count);
	    for (i=0; i<count; i++)
		printf("    %s\n", value[i]);
	    code = getval(&sb, value, MAXVAL, &count);
	    }	
	}

    sb.errchr = sb.curchr - 1;
    synerr(&sb, "Test complete");

    exit(TRUE);
    }

    VOID dumrout (code, tstnum)
    CODE	*code;
    COUNT	*tstnum;
    {
    }
#endif
