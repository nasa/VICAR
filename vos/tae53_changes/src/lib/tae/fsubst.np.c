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




/* >>UNIX<<     */
/*
 *	File name sustitution routine.
 *	The functions in this module are kept separate from the
 *	file access package as they may be needed by application
 *	processes (XI package) to perform file name substitution
 *	independent of the file package.
 *
 *	CHANGE LOG:
 *	
 *	23-jan-84	Retain $name in fspec if no substitution
 *			available...dm
 *	08-jun-88	Remove ascii dependency;  used isalpha, isdigit
 *			macros rather than range checking...tp
 *	13-feb-89	Expand ~ as unix user would expect...palm
 *
 */

#include        "taeconf.inp"
#include        "chartype.inc"
#include        "fileinc.inp"
#include "taeintproto.h"


FUNCTION  static  CODE  addsub
(
 TEXT        substr[],       /* in: string value to be substituted   */
 TEXT        *bufstart,      /* in: start of buffer                  */
 TEXT        **bufptr       /* current position in buffer           */
 );
FUNCTION  static  TEXT    *ext_symb
(
 TEXT        *sp,                    /* in: ptr to $symbol in text string */
 TEXT        symbol[]               /* output: symbol (without $ prefix) */
 );



/*
 * perform substitutions in the file spec
 * If no value found for the symbol in the environment table, the 
 * symbol is transferrred unsubstituted.
 */

FUNCTION  CODE  f_subst
(
 TEXT        *fspec,         /* in: file spec                        */
 TEXT        *subspec       /* out: file spec after substitution    */
)
    {
    FAST TEXT   *ptr;                   /* pointers to string characters   */
    TEXT        *subptr;                
    TEXT        symbol[FSPECSIZ+1];     /* symbol to be substituted        */
    TEXT        *substr;                /* ptr to substitution string      */
    CODE        code;

    subspec[0] = EOS;
    for (ptr=fspec, subptr=subspec; *ptr != EOS; )      /* scan each char  */
        {
        if (subptr - subspec >= FSPECSIZ)
            return(F_LONGSUB);
        if (*ptr != '$' && *ptr != '~')       /* if not a subst symbol    */
            *subptr++ = *ptr++;         /* move char to output buffer      */
        else                            /* requires substitution           */
            {
            ptr = ext_symb(ptr, symbol);  /* extract symbol from input spec */
            if ((substr = getenv(symbol)) == NULL) /*  ptr to subst string */ 
		{				     	 /* no value found */
		code = addsub("$", subspec, &subptr);    /* transfer the $ */
		code = addsub(symbol, subspec, &subptr); /* and the name   */
		}
	    else
		{
                code = addsub(substr, subspec, &subptr);  /* add the string */
		}
            if (code == FAIL) 
                return(F_LONGSUB);      /* value too long                  */
            }
        }                               /* ptr, subptr already incremented */
    *subptr = EOS;                      /* mark end of output string       */
    return (SUCCESS);
    } 

/*
 *      ext_symb. Extract the symbol that needs substitution.
 *
 *      Returns pointer to the input string past the $symbol.
 */

FUNCTION  static  TEXT    *ext_symb
(
 TEXT        *sp,                    /* in: ptr to $symbol in text string */
 TEXT        symbol[]               /* output: symbol (without $ prefix) */
)
    {
    FAST TEXT   *wptr;
    FAST TEXT   *ptr;
#undef S_LETTER
#define S_LETTER(c)  (isalpha(c) || (c) == '_')

    ptr = sp;
    if (*ptr == '~')					/* treat ~ as $HOME */
	{
	s_copy ("HOME", symbol);
	return (++ptr);
	}
    for (ptr++, wptr = &(symbol[0]); *ptr != EOS;)   /* check each character */
        {
        if (S_LETTER (*ptr) || isdigit(*ptr))        /* not a terminator     */
            *wptr++ = *ptr++;                        /* copy the character   */
        else                                    
            break;
        }
    *wptr = EOS;
    return (ptr);
    }

/*
 *      addsub.  Add the substitution value to the buffer.
 */

FUNCTION  static  CODE  addsub
(
 TEXT        substr[],       /* in: string value to be substituted   */
 TEXT        *bufstart,      /* in: start of buffer                  */
 TEXT        **bufptr       /* current position in buffer           */
 )
    {
    COUNT       length;         /* string length                        */
    FAST  COUNT i;
    FAST  TEXT  *j, *k;

    length = s_length(substr);
    k = *bufptr;
    if (k - bufstart + length > FSPECSIZ )
        return(FAIL);                   /* no more room in buffer       */
    for (i=0, j = &(substr[0]); i < length; i++)
        *k++ = *j++;                    /* copy string to buffer        */
    *bufptr = k;
    return (SUCCESS); 
    }

