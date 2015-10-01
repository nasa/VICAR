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



#include	"taeconf.inp"


/*	checkHost
 *
 *	This makes some checks that the underlying C
 *	compiler and hardware architecture are compatible
 *	with TAE.   Successful execution of this module
 *	does not necessarily mean that all is well,
 *	but we do test the most popular port problems here.
 *
 *	To build this:
 *
 *		cc -I$TINC checkhost.c
 * 
 *	and run a.out.
 */

#define PRINT_SIZE(type) printf ("type = %d bytes.\n", sizeof (type));

FUNCTION VOID error (TEXT *message);

FUNCTION int main (void)

    {
    COUNT	i;
    double 	*dp;
    float	*fp;
    int		*ip;
    short	*sp;
    long	*lp;
    TINY	tiny_type;
    ALIGN	align[50], 	*palign;
    TAEINT	taeint1, 	taeint2;
    COMPACT_COUNT  compact;
    COMPACT_UCOUNT ucompact;

    PRINT_SIZE(int);
    PRINT_SIZE(short);
    PRINT_SIZE(TAEINT);
    PRINT_SIZE(ALIGN);
    PRINT_SIZE(COUNT);
    PRINT_SIZE(UCOUNT);
    PRINT_SIZE(TINY);
    PRINT_SIZE(UTINY);
    PRINT_SIZE(TAEFLOAT);
    PRINT_SIZE(COMPACT_COUNT);
    PRINT_SIZE(COMPACT_UCOUNT);
    /*
     *	TINYs must be signed.  If char types are not signed, then
     *	TINY must be defined to be 'short'.
     */

    tiny_type = -1;
    if (tiny_type > 0) goto badHost1;
    
    /*	The RESTRICT package assumes that a COMPACT_UCOUNT can be stored
     *	within a GENPTR (for the purposes of "relative" offset).
     */

    if (sizeof (GENPTR) < sizeof (COMPACT_UCOUNT))  goto badHost2;
        
    /*	
     *	Attempt to check that the ALIGN typedef will work: 
     */

    for (i=0; i < 8; i++)
	{
        palign = &align[i];
	dp = (double *) palign;  (*dp) = 10.0 ;  if ((*dp) != 10.0  ) 
    		goto badHost4;
	fp = (float  *) palign;  (*fp) = 11.0 ;  if ((*fp) != 11.0  ) 
    		goto badHost5;
	ip = (int    *) palign;  (*ip) = 100  ;  if ((*ip) != 100   ) 
    		goto badHost6;
	lp = (long   *) palign;  (*lp) = 99  ;  if ((*lp) != 99   ) 
    		goto badHost7;
	sp = (short  *) palign;  (*sp) = 98  ;  if ((*sp) !=  98   ) 
    		goto badHost8;
	}
                
    /*	Misc checks.  
     */

    taeint1 = SMALLEST_TAEINT;  
    taeint2 = LARGEST_TAEINT;
    if (taeint1 > 0 || taeint2 < 0 || taeint1 >= taeint2) goto badHost9;
    compact = MAX_COMPACT_COUNT;
    if (compact < 0) goto badHost10;
    ucompact = MAX_COMPACT_UCOUNT;		
/*    if (ucompact < 0) goto badHost11;  unneeded due to unsigned type */
    printf ("\nSUCCESFUL EXECUTION.\n");
    exit (0);
    
#if 0
badHost11:
    error ("MAX_COMPACT_UCOUNT not right.\n");
#endif
badHost10:
    error ("MAX_COMPACT_COUNT not right.\n");
badHost9:
    error ("SMALLEST_TAEINT/LARGEST_TAEINT not right.\n");
badHost8:
badHost7:
badHost6:
badHost5:
badHost4:
    error ("ALIGN not right.\n");
badHost2:
    error ("GENPTR won't fit in a COMPACT_UCOUNT.\n");
badHost1:
    error ("TINY data type must be signed.\n");

    return(0);
    }



FUNCTION VOID error (TEXT *message)

    {
    printf ("CHECKHOST FAILURE:\n");
    printf (message);
    exit (1);
    }
