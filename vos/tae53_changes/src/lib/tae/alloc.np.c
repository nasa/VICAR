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



/*<<UNIX>>*/
/*
 *	tae_alloc, tae_free.
 *	
 *	These are VAX-dependent front-ends to the calloc and free 
 *	calls to do integrity checks on allocation/deallocation.
 *
 *	The technique here is to build "guard" bytes around the
 *	caller's block when allocation is done.  When a block
 *	is deallocated, the guard bytes are checked for validity.
 *	The caller does not know about the guard bytes.
 *
 *	TBD: make tae_alloc and tae_free portable.
 *	(They are non-portable for now because it takes some effort
 *	to make them portable.)
 *
 *	The extra memory and runtime overhead imposed here may be
 *	eliminated by:
 *		
 *		#define tae_alloc	calloc
 *		#define tae_dealloc	free
 *
 *	CHANGE LOG:
 *	
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	04-aug-83	Added global counter dm_totall...dm
 *	18-oct-83	Removed VMS dependencies for unix at edc...palm
 *	24-oct-83	Remove cfree() return value check...dm
 *	28-jan-84	Cast p to GENPTR...dm
 *	11-apr-84	Change cfree to free to avoid SUN problem...dm
 *	19-may-84	Check for null pointer in tae_free...dm
 *	22-feb-88	New "abort on no memory" philosophy...palm
 *	06-feb-91	Prepare for use of FAST_ALLOC...ljn
 */


#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include "taeintproto.h"
#ifdef FAST_ALLOC
    int dymmy;
#else



/* accounting data.  the following can be examined with the debugger	*/

    GLOBAL long	dm_bytes = 0;	/* number of bytes currently allocated	*/
    GLOBAL long	dm_max   = 0;	/* max bytes ever allocated		*/
    GLOBAL long	dm_allocs= 0;	/* number of allocations currently		*/
    GLOBAL long	dm_totall= 0;	/* total number of allocation calls	*/

#define SENTINEL  0xabcdef12

    FUNCTION GENPTR tae_alloc(n, bytes)

    FUNINT	n;		/* in: multiplier for...		*/
    FUNINT	bytes;		/* in: bytes to allocate		*/

    {
    COUNT	total;		/* total bytes to really allcoate	*/
    COUNT	utotal;		/* total bytes for caller (n*bytes)	*/
    long	*p;		
    GENPTR	calloc();

    dm_allocs++;		/* current allocation counter		*/
    dm_totall++;		/* call counter				*/
    utotal = n * bytes;
    total = utotal + 12;	/* we need two sentinels and an index	*/
    total = (1+(total-1)/4)*4;  /* get to longword boundary		*/
    p = (long *) calloc(1, total);	/* allocate memory		*/
    if (p == NULL)		/* got nothing				*/
	{
	printf ("Insufficient memory.  Aborting from tae_alloc.\n");
	abort ();
	}
    dm_bytes += total;		/* total bytes allocated so far		*/
    if (dm_bytes > dm_max)	/* track largest dm_bytes		*/
        dm_max = dm_bytes;	
    p[0] = SENTINEL;		/* stuff sentinel			*/
    p[1] = total/4 - 1;		/* index of trailer sentinel		*/
    p[p[1]] = SENTINEL;		/* sentinel in last longword		*/
    return((GENPTR) &p[2]);	/* caller starts at p[2]		*/ 
    } 

/*
 *   tae_free.
 */

    FUNCTION tae_free(ptr)

    GENPTR	ptr;		/* block to free			*/

    {
    long	*p;

    if (ptr == NULL)
	return;
    p = (long *)ptr - 2;	/* back up to supposed sentinel block	*/
    if (p[0] != SENTINEL)
	abort();
    if (p[p[1]] != SENTINEL)
	abort();
    dm_bytes -= 4*(p[1] + 1);	/* number of bytes being freed		*/
    p[0] = p[p[1]] = 0;		/* zero sentinel words			*/
    free((GENPTR) p); 
    dm_allocs--;		/* decrement  allocation counter	*/
    return;
    }
#endif
