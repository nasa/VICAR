#include <stdlib.h>

/************************************************************************/
/* BigMalloc() and BigMallocForce() are currently stubs under Unix which
 * merely call malloc().  The routines are mainly needed for VMS.
 * A Unix implementation should be done in the future.
 */

unsigned char *BigMalloc(size)
  int size;			/* in: # of bytes to allocate		*/
{

  return malloc(size);
}


unsigned char *BigMallocForce(size)
  int size;			/* in: # of bytes to allocate		*/
{

  return malloc(size);
}


/************************************************************************/
/* BigFree() frees a section of memory allocated by BigMalloc() or
 * BigMallocForce().  It is called exactly like free().  There is no
 * return value or error indication.
 */

void BigFree(addr)
  unsigned char *addr;		/* in: address to free */
{

  free(addr);
}
