#include <rms.h>
#include <ssdef.h>
#include <secdef.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

static struct bigmem_ctrl {
  struct bigmem_ctrl *next;
  unsigned char *start;
  unsigned char *end;
  int channel;
} *bigmem_head = NULL;

unsigned char *malloc();

/************************************************************************/
/* BigMalloc() allocates a large section of memory by setting up an alternate
 * paging file.This is needed to get around the PAGFIL quota limit in VMS.
 * It is called exactly like malloc().   This alternate page file is put
 * on the disk that is pointed at by the logical name "v2$scratch".  Note
 * that v2$scratch can point to a directory or even a file; only the disk
 * part of the name is used.  If there is not enough room on the v2$scratch
 * disk, then normal malloc is tried.  If this fails, NULL is returned.
 * BigMallocForce() will always attempt to set up the paging file.  BigMalloc()
 * will only do so if the requested size is over one megabyte, otherwise
 * it will use normal malloc().  Most applications should call BigMalloc()
 * instead of BigMallocForce().  BigFree() is used to free the memory obtained
 * by these routines.
 *
 * The return value for these routines is a pointer to the requested area,
 * or NULL if the allocation failed.
 *
 * The file created on the v2$scratch disk is a temporary file with no
 * directory entry.  It will never show up in a directory, and is automatically
 * deleted when the file is closed.
 */

unsigned char *BigMalloc(size)
  int size;			/* in: # of bytes to allocate		*/
{
  unsigned char *BigMallocForce();
  unsigned char *addr;

  if (size <= 1024*1024) {	/* smaller than a megabyte, so use malloc() */
    addr = malloc(size);
    if (addr)
      return addr;
  }			/* if small malloc() failed, try BigMalloc anyway */
  return BigMallocForce(size);
}


unsigned char *BigMallocForce(size)
  int size;			/* in: # of bytes to allocate		*/
{
  int status;
  int blocks;
  unsigned char *s[2], *r[2];
  int flags;
  struct bigmem_ctrl *mem;
  struct FAB fab;

  mem = malloc(sizeof(struct bigmem_ctrl));
  if (mem == NULL)
    return malloc(size);		/* not likely */

  blocks = (size+511) / 512;

  fab = cc$rms_fab;
  fab.fab$l_fna = "v2$scratch:vidstemp.pgfl";
  fab.fab$b_fns = strlen(fab.fab$l_fna);
  fab.fab$w_mrs = 512;
  fab.fab$b_rfm = FAB$C_FIX;
  fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
  fab.fab$l_fop = FAB$M_UFO | FAB$M_MXV | FAB$M_TMD;	/* user-file-open, */
			/* maximize version #,temporary file delete-on-close */
  fab.fab$l_alq = blocks;

  status = sys$create(&fab);

  if (status != RMS$_NORMAL && status != RMS$_CREATED) {
    free(mem);
    return malloc(size);		/* try a normal alloc */
  }

  mem->channel = fab.fab$l_stv;		/* save channel */

  r[0] = r[1] = 10;	/* Having these addresses the same causes the	*/
			/* program region to be expanded (10 is just an	*/
			/* arbitrary number < p1 region			*/

  flags = SEC$M_EXPREG | SEC$M_WRT;   /* expand program region, allow writes */

  status = sys$crmpsc(r,s,0,flags,0,0,0,mem->channel,blocks,0,0,0); /* map it */

  mem->start = s[0];
  mem->end = s[1];

  if ((status != SS$_NORMAL) && (status != SS$_CREATED)) {
    status = sys$dassgn(mem->channel);
    free(mem);
    return malloc(size);		/* try a normal malloc */
  }

  mem->next = bigmem_head;
  bigmem_head = mem;

  return mem->start;

}

/************************************************************************/
/* BigFree() frees a section of memory allocated by BigMalloc() or
 * BigMallocForce().  It is called exactly like free().  There is no
 * return value or error indication.
 */

void BigFree(addr)
  unsigned char *addr;		/* in: address to free */
{
  int status;
  unsigned char *r[2];
  struct bigmem_ctrl *mem, *oldmem;

  oldmem = NULL;
  for (mem = bigmem_head; mem != NULL; mem = mem->next) {
    if (mem->start == addr)
      break;
    oldmem = mem;
  }

  if (mem == NULL) {		/* whoops, address not allocated */
    free(addr);			/* maybe it was allocated normally */
    return;			
  }

  if (oldmem == NULL)		/* remove this struct from linked list */
    bigmem_head = mem->next;
  else
    oldmem->next = mem->next;

  r[0] = mem->start;
  r[1] = mem->end;

  status = sys$deltva(r, 0, 0);		/* delete the mapped section */

  status = sys$dassgn(mem->channel);	/* close the file (which deletes it) */

  free(mem);

}
