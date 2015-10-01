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
 *	chartype.inc
 *	Character classification definitions via char_type table in chartype.c
 *
 * 	Change log:
 *
 *	17-jun-88	Initial release...tp
 *	07-feb-89	For VAX_VMS, use globalref instead of IMPORT...ljn
 *	14-sep-89	OASYS C++ on VMS doesn't like dimensionless decl's...ljn
 *      08-dec-93       Solaris port (backported dag from aug-93)...cob
 */

#ifndef I_CHARTYPE			/* prevent double include		*/
#define I_CHARTYPE	0

#ifdef VAX_VMS
#ifdef c_plusplus
globalref unsigned char char_type[1];
#else
globalref unsigned char char_type[];
#endif
#else
IMPORT unsigned char char_type[];
#endif

#define CH_DIGIT	0x1
#define CH_UPPER	0x2
#define CH_LOWER	0x4
#define CH_LETTER	0x8
#define CH_GRAPH	0x10
#define CH_WHITE	0x20
#define CH_CONTROL	0x40
#define CH_EXTLTR	0x80

/*
 *	macros
 */
#define isalpha(c)	(char_type[(int) c] & (CH_UPPER | CH_LOWER))
#define isalnum(c)	(char_type[(int) c] & (CH_UPPER | CH_LOWER | CH_DIGIT))
#define isdigit(c)	(char_type[(int) c] & CH_DIGIT)
#define islower(c)	(char_type[(int) c] & CH_LOWER)
#define isupper(c)	(char_type[(int) c] & CH_UPPER)
#define isletter(c)	(char_type[(int) c] & CH_LETTER)
#define isextletter(c)	(char_type[(int) c] & (CH_LETTER | CH_EXTLTR))
#define iswhite(c)	(char_type[(int) c] & CH_WHITE)
#define iscontrol(c)	(char_type[(int) c] & CH_CONTROL)

#ifdef ASCII
#define tolower(c)	(isupper(c) ? ((c) + ('a' - 'A')) : (c))
#define toupper(c)	(islower(c) ? ((c) - ('a' - 'A')) : (c))
#endif

#ifdef EBCDIC
#define tolower(c)	(isupper(c) ? ((c) - ('A' - 'a')) : (c))
#define toupper(c)	(islower(c) ? ((c) + ('A' - 'a')) : (c))
#endif

#endif
