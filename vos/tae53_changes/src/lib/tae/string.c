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



#include <ctype.h>
#include	"taeconf.inp"
#undef isalpha
#undef isalnum
#undef isdigit
#undef islower
#undef isupper
#undef tolower
#undef toupper
#include	"chartype.inc"
#include "syninc.inc"
#include "taeintproto.h"    
#ifndef testmain
/*
 *  CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	11-oct-83	Fixed unix compilation...palm
 *	25-oct-83	New s_lower...palm
 *	21-jun-85	Optimized s_equal; new s_upper...palm
 *	22-jul-85	Change name of upper_case table and make
 *			it global...palm
 *	25-jul-85	Optimize s_index...palm
 *	07-aug-87	PR1081: Optimize s_lseq...palm  
 *	08-jun-88	Remove ascii dependency; add s_table for ebcdic...tp
 *	25-sep-90	Moved su_substring() from wbsubs.cc...ljn
 *
 */

/*      TAE string package for C callers.
 *	Note that string conversion functions are in stringcnv.c.
 *	Note that string allocation functions are in stralloc.c.
 *	Note that performance critical string functions are in stringmar.c.
 *	The above source splits are to prevent inclusion
 *	of unnecessary modules in application programs and/or
 *	for performance reasons.
 */

/*	Mapping table to produce upper case ASCII. This allows
 *	for fast conversion to upper case.
 */

#ifdef ASCII
GLOBAL TEXT s_table [128] = 
{
0,     1,    2,     3,    4,    5,    6,    7,    8,    9,
10,   11,   12,    13,   14,   15,   16,   17,   18,   19,
20,   21,   22,    23,   24,   25,   26,   27,   28,   29,
30,   31,   32,    33,   34,   35,   36,   37,   38,   39,
40,   41,   42,    43,   44,   45,   46,   47,   48,   49,
50,   51,   52,    53,   54,   55,   56,   57,   58,   59,
60,   61,   62,    63,   64,   65,   66,   67,   68,   69,
70,   71,   72,    73,   74,   75,   76,   77,   78,   79,
80,   81,   82,    83,   84,   85,   86,   87,   88,   89,
90,   91,   92,    93,   94,   95,   96,   97-32, 98-32, 99-32,
100-32,  101-32, 102-32,   103-32,   104-32,  105-32,  106-32,  107-32,  108-32,  109-32,
110-32,  111-32, 112-32,   113-32,   114-32,  115-32,  116-32,  117-32,  118-32,  119-32,
120-32,  121-32, 122-32,   123,      124,        125,     126,     127
};
#else
#ifdef EBCDIC
GLOBAL TEXT s_table [256] = 
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
 90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
100,  101,  102,  103,  104,  105,  106,  107,  108,  109,
110,  111,  112,  113,  114,  115,  116,  117,  118,  119,
120,  121,  122,  123,  124,  125,  126,  127,  128,  129+64,
130+64, 131+64, 132+64, 133+64, 134+64, 135+64, 136+64, 137+64,    138,    139,
   140,    141,    142,    143,    144, 145+64, 146+64, 147+64, 148+64, 149+64,
150+64, 151+64, 152+64, 153+64,    154,    155,    156,    157,    158,    159,
   160,    161, 162+64, 163+64, 164+64, 165+64, 166+64, 167+64, 168+64, 169+64,
170,  171,  172,  173,  174,  175,  176,  177,  178,  179,
180,  181,  182,  183,  184,  185,  186,  187,  188,  189,
190,  191,  192,  193,  194,  195,  196,  197,  198,  199,
200,  201,  202,  203,  204,  205,  206,  207,  208,  209,
210,  211,  212,  213,  214,  215,  216,  217,  218,  219,
220,  221,  222,  223,  224,  225,  226,  227,  228,  229,
230,  231,  232,  233,  234,  235,  236,  237,  238,  239,
240,  241,  242,  243,  244,  245,  246,  247,  248,  249,
250,  251,  252,  253,  254,  255
};
#else
	compilation error: only ASCII and EBCDIC tables are coded.
#endif
#endif


/*
 *    s_blank.  Produce a string with n blanks.
 */

FUNCTION VOID s_blank
(
 FAST TEXT *s,
 FUNINT n
)
    {
    FAST COUNT i;

    for (i=0; i<n; i++, s++)
	*s = ' ';
    *s = 0;
    return;
    }

/*  
 *   s_equal.   Test strings for equal.  Case not significant.
 */

FUNCTION BOOL s_equal(FAST TEXT *s, FAST TEXT *t)
    {
    while (*s && *t)		/* while both have a next character	*/     
	{
	  if (s_table[(int) *s++] != s_table[(int) *t++])
	    return (FALSE);
	}
    if (*s || *t)
	return (FALSE);			/* lengths not equal		*/
    return(TRUE);			/* lengths equal	        */
    }

/*
 *   s_index.  Find character in string.
 *
 *	Returns:  index if found, negative if not found.
 */

FUNCTION COUNT s_index
(
 FAST TEXT 	s[],		/* in: string			*/
 FAST FUNINT	 c		/* in: character to find	*/
)   
    {
    FAST TEXT	*sptr;		/* working pointer		*/

    for (sptr = s; *sptr != EOS; )
	if (c == *sptr++)
	    return (sptr - s - 1);	/* note: sptr auto-bumped one too far */
    return(-1);
    }
	
/*	s_lower.   Convert string to lowercase.
 *
 */

FUNCTION COUNT s_lower
(
 FAST TEXT	*s
)
    {
    FAST TEXT	*p;			/* initial value of s	*/

    p = s;
    while (*s != EOS)
        {
        *s = tolower(*s);
	s++;
	}
    return (s-p);
    }
 
/*
 *   s_lseq.  Test for s being a left substring of t.  Case insignificant.
 *
 *	BEWARE: A null string is a left substring of every string.
 *
 */

FUNCTION BOOL s_lseq
(
 FAST TEXT *s,
 FAST TEXT *t
 )
    {

    while (*s) 		/* while s has a char (if t finished, no match)*/   
	{
	  if (s_table[(int) *s++] != s_table[(int) *t++])
	    return (FALSE);
	}
    if (!(*s)) 
	return(TRUE);		/* s is finished so it's a substring	*/
    return(FALSE);		/* s not finished, but t is finished	*/
    }

/*
 *    s_shift.  Shift string left n characters and return its new length.
 *   Shift left means the left n characters are discarded and the string
 *   becomes shorter by n characters.
 *
 *   The shift count must be less or equal to the string size.
 *   Null strings are not changed.
 *
 *   Note: right shift is not supported because it's not clear what it means
 *   (other than to move the eos down n characters, which can be done by
 *   		s[s_length(s) - n] = 0;      ).
 */

FUNCTION COUNT s_shift
(
 FAST TEXT s[],			/* input/output string		*/
 FAST FUNINT n			/* input: characters to shift	*/
)   
    {
    FAST COUNT i;

    if (s[0] == 0) return(0);		/* null strings do not get shifted */
    for (i=0; (s[i] = s[i+n]) != 0; i++)
	;
    return(i);
    }

/*
 *	s_strip.  Remove trailing spaces from C string.
 *	Function value:
 *
 *		resulting string length.
 */

FUNCTION COUNT s_strip(TEXT s[])
    	{
	COUNT	i;

	for(i=s_length(s)-1; i >= 0; i--)		/* work backwards	*/
	    if (s[i] != ' ') break;

	s[i+1] = EOS;
	return(i+1);
	}

/*	s_upper -- convert string to uppercase and return length.
 */

FUNCTION COUNT s_upper 
(
 FAST TEXT	*s
 )
    {
    FAST TEXT	*p;			/* initial value of s	*/

    p = s;
    while (*s != EOS)
        {
	  *s = s_table[(int) *s];
	s++;
	}
    return (s-p);
    }


/*
 *	s_substring.
 *
 *   Return pointer if substring is a substring of fullstring.
 */

FUNCTION TEXT *s_substring
(
 TEXT * substring,
 TEXT * fullstring
 )
    {

    for (; *fullstring; fullstring++)
        {
	  if (s_table[(int) *substring] != s_table[(int) *fullstring])    /* optimization */
            continue;
        if (s_lseq (substring, fullstring))
            return fullstring;
        }
    return NULL;
    }


/*
 *	Test driver.
 */

#else

    main()
    {
    static  COUNT i1, i2, i3, i4;
    static TEXT s[20];

    i1 = s_copy("ab", s);
    i2 = s_append("cd", s);

    if (s_equal("AbCd", s)) printf("success \n");
    else printf("failure \n");

    if (s_lseq("aBc", s)) printf("success \n" );
    else printf("failure \n");  
    
    s_copy("aabcd", s);
    i3 = s_shift(s, 1);
    if (s_equal("abcd", s)) printf("success \n");
    else printf("failure \n");
    }

#endif
