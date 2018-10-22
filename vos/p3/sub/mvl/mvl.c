static char sccsMvl[] = "@(#) mvl.c 2.1 9/4/86 PDS Vicar2";


/*UNIX/C version of MVL (called from FORTRAN )                ...ACB...
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *	VICAR SUBROUTINE                                           MVL
 *
 *	MVL is an efficient routine for moving data between
 *       two buffers.
 * FORTRAN FORMAT OF CALL:
 *
 *	CALL MVL(L1, L2, N)
 *
 * PARAMETERS:
 *
 *	L1   ... Source array
 *	L2   ... Destination array
 *	N    ... Number of bytes to transfer
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

mvl_(l1, l2, n,  /*FORTRAN version: plus dummy f77 lengths*/ d1,d2)
{
 mvl(l1,l2,n);
}

mvl(l1, l2, n ) /* C version */
char *l1, *l2 ;
int *n ;
{
	int i ;
	
	for(i=0;i<*n;i++)
		*l2++ = *l1++ ;
}
