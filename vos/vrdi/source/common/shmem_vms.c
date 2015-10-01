/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/
/*									*/
/* The VRDI version is named ".c", and the TAE version is named ".cnp",	*/
/* as per their respective naming standards.	                        */
/*									*/
/* There is no difference in the source files for the TAE and VRDI	*/
/* versions.  One of the symbols VRDI or TAE must be defined on the	*/
/* compile line to select the right version.				*/

#include "xvmaininc.h"

/* VMS System dependent shared memory control routines

   Written by: Bob Deen
   Date:       3/23/89
*/

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"
#include "xdfuncs.h"

static struct bigmem_ctrl {
  struct bigmem_ctrl *next;
  char   *start;
  char   *end;
  int    channel;
  int    type;
  int    unit;
} *bigmem_head = NULL;

/************************************************************************/
/* Alloc_Shmem:
 *	This just calls Attach_Shmem.  See that routine for details.
 */

int Alloc_Shmem(unit, shmem_size, type)
int unit;		/* in:  unit number */
int shmem_size;		/* in:  size of shared memory needed, in bytes */
int type;		/* in:  type of shared memory to be allocated,     */
			/*      e.g., DCB, device-specific, etc.           */
{
   return Attach_Shmem(unit, shmem_size, 0, type);
}

/************************************************************************/
/* Attach_Shmem:
 *	Allocates and attaches a pointer to shared memory.
 *
 *	A temporary global page file section is used for the shared memory.
 *	The name of the section is "XD", followed by the type, followed by
 *	"$", followed by the device name, underscore, and the PID of the
 *	parent process that allocated it.  For example, "XD0$EPA2_00603142".
 *
 *	The global section goes away when nobody is mapped to it.  So, when
 *	USEDISP calls ALLOC_SHMEM, the global section is created but is
 *	immediately lost when the USEDISP image terminates.  When an applica-
 *	tion calls ATTACH_SHMEM, the section is re-created and stays around
 *	as long as the application does.  If another application maps to it
 *	in the meantime, it will use the same memory as the first and the
 *	memory will hang around as long as either process needs it.  If TAE
 *	does the allocation, it stays around as long as TAE is active.  In
 *	other words, the ALLOC_SHMEM function is often a no-op, and
 *	ATTACH_SHMEM also allocates.  This does not cause a problem,
 *	as no information needs to be saved between the time the device is
 *	allocated and the time the device is used (with xddunit or equivalent).
 *
 *	The "type" parameter to this function is used to differentiate between
 *	shared memory required by specific devices as compared to that
 *	required by the VRDI or TAE.  Types 0-9 are reserved for the VRDI/TAE.
 *
 *	If the memory requested is greater than an arbitrary maximum limit,
 *	then the memory is allocated by setting up an alternate paging file.
 *	This is to get around the PAGFIL quota in VMS.
 */

int Attach_Shmem(unit, shmem_size, aptr, type)
int unit;		/* in:  unit number */
int shmem_size;		/* in:  size of shared memory needed, in bytes */
int **aptr;		/* out: address of shared memory pointer.  If NULL */
			/*      is passed in, the pointer is not returned. */
int type;		/* in:  type of shared memory to be allocated,     */
			/*      e.g., DCB, device-specific, etc.           */
{
  int status, size, oldsize, flags;
  int mypid;
  struct dsc$descriptor_s section;
  struct bigmem_ctrl	*mem=NULL;
  struct FAB	fab;
  char *r[2], *s[2];

  /*  Determine if the shared memory had been previously created by the    */
  /*  calling process--by checking the list.                               */

  for (mem = bigmem_head; mem != NULL; mem = mem->next)
  {
    if ((mem->type == type) && (mem->unit == unit))
      break;
  }

  /*  If it had been created, but the requested size does not match the    */
  /*  actual size, return an error.  Otherwise, return SUCCESS.            */

  if (mem != NULL)
  {
    size = (shmem_size + 511) / 512;
    oldsize = (mem->end - mem->start + 1) / 512;
    if (size != oldsize)
    {
      if (aptr != NULL)
        *aptr = 0;
      return(SHMEM_CANNOT_ALLOC);
    }
    else
    {
      if (aptr != NULL)				/* get pointer to memory */
        *aptr = mem->start;
      return(SHMEM_ALREADY_ACTIVE);
    }
  }

  /*  If the shared memory had not been previously created by the calling  */
  /*  process, set up a new structure to track the shared memory.          */

  mem = malloc(sizeof(struct bigmem_ctrl));
  if (mem == NULL)
    return (MEMORY_ERROR);

  /*  Determine if another process has created the shared memory needed    */
  /*  by the calling process.  If so, map to it.  If not, create it.       */

  status = GetParentPID(&mypid);		/* get current PID */
  if (status != SUCCESS)
    return (status);

  /*  Create a name for the shared memory that will be unique for different*/
  /*  processes, devices, and types of shared memory.  The process id is   */
  /*  the id of the parent of the process--so that all processes in the    */
  /*  same process tree have access to the same section of shared memory.  */

  sprintf(SectionName, "XD%d$%s_%8X", type, DIB[unit]->DeviceName, mypid);

  section.dsc$w_length = strlen(SectionName);
  section.dsc$b_class = DSC$K_CLASS_S;
  section.dsc$b_dtype = DSC$K_DTYPE_T;
  section.dsc$a_pointer = SectionName;

  r[0] = 10;				/* any arbitrary address in P0 space */
  r[1] = 10;
  size = (shmem_size + 511) / 512;	/* compute size in pages */

  flags = SEC$M_WRT | SEC$M_EXPREG | SEC$M_GBL;
  status = sys$mgblsc(r, s, 0, flags, &section, 0, 0);

  /*  If the shared memory has been created by another process in the same */
  /*  process tree, then map to it and return SUCCESS.                     */

  if (status == SS$_NORMAL)
  {
    mem->start = s[0];			/* save start and end addresses */
    mem->end = s[1];
    mem->unit = unit;
    mem->type = type;
    mem->next = bigmem_head;
    bigmem_head = mem;
    if (aptr != NULL)			/* get pointer to memory */
      *aptr = mem->start;
    return(SHMEM_ALREADY_ACTIVE);
  }

  /*  If the shared memory has not been created by this or any other pro-  */
  /*  cess in the same process tree, then create it.                       */

  if (shmem_size <= (256*1024))		/* small amount of memory requested */
  {
    flags = SEC$M_WRT | SEC$M_EXPREG | SEC$M_GBL | SEC$M_PAGFIL | SEC$M_DZRO;
    mem->channel = 0;
  }
  else					/* large amount of memory requested  */
  {
    fab = cc$rms_fab;
    fab.fab$l_fna = "v2$scratch:vrditemp.pgfl";
    fab.fab$b_fns = strlen(fab.fab$l_fna);
    fab.fab$w_mrs = 512;
    fab.fab$b_rfm = FAB$C_FIX;
    fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
    fab.fab$l_fop = FAB$M_UFO | FAB$M_MXV | FAB$M_TMD;
    fab.fab$l_alq = size;

    status = sys$create(&fab);
    if ((status != RMS$_NORMAL) && (status != RMS$_CREATED))
      return (MEMORY_ERROR);

    flags = SEC$M_WRT | SEC$M_EXPREG | SEC$M_GBL | SEC$M_DZRO;
    mem->channel = fab.fab$l_stv;
  }

  SystemError = sys$crmpsc(r,s,0,flags,&section,0,0,mem->channel,size,0,0,0);

  if ((SystemError == SS$_NORMAL) || (SystemError == SS$_CREATED))
  {
    mem->start = s[0];			/* save start and end addresses */
    mem->end = s[1];
    mem->unit = unit;
    mem->type = type;
    mem->next = bigmem_head;
    bigmem_head = mem;
  }
  else					/* an error occurred */
  {
    if (mem->channel != 0)
      status = sys$dassgn(mem->channel);
    free(mem);
    s[0] = 0;				/* zero address if error occurred */
  }

  if (aptr != NULL)			/* get pointer to memory */
    *aptr = s[0];

  if ((SystemError == SS$_NORMAL) || (SystemError == SS$_CREATED))
    status = SUCCESS;
  else if (SystemError == SS$_ACCVIO)
    status = SHMEM_ACCESS_VIOLATION;
  else if (SystemError == SS$_EXGBLPAGFIL || SystemError == SS$_EXQUOTA)
    status = SHMEM_OUT_OF_MEMORY;
  else
    status = SHMEM_CANNOT_ALLOC;

  return(status);
}

/************************************************************************/
/* Remove_Shmem:
 *	This just calls Detach_Shmem.  See that routine for details.
 */

int Remove_Shmem(unit, type)
int unit;		/* in:  unit number */
int type;		/* in:  type of shared memory to be allocated,     */
			/*      e.g., DCB, device-specific, etc.           */
{
   return Detach_Shmem(unit, 0, type);
}

/************************************************************************/
/* Detach_Shmem:
 *	Detaches a pointer from shared memory.
 *	The memory is detached with $DELTVA.  If nobody else is using the
 *	global section, it will automatically go away.
 */

int Detach_Shmem(unit, aptr, type)
int unit;		/* in: unit number */
int **aptr;		/* in: address of shared memory pointer */
int type;		/* in:  type of shared memory to be allocated,     */
			/*      e.g., DCB, device-specific, etc.           */
{
  int status;
  char	*r[2];
  struct bigmem_ctrl	*mem, *oldmem=NULL;

  /*  Determine if shared memory has been used by the calling process--by  */
  /*  checking the linked list.  If not, just return.                      */

  for (mem = bigmem_head; mem != NULL; mem = mem->next)
  {
    if ((mem->type == type) && (mem->unit == unit))
      break;
    oldmem = mem;
  }

  if (mem == NULL)
    return(SUCCESS);

  /*  If the shared memory has been used by the calling process, update    */
  /*  the linked list, detach the shared memory, and close the channel (if */
  /*  applicable).                                                         */

  if (oldmem == NULL)
    bigmem_head = mem->next;
  else
    oldmem->next = mem->next;

  r[0] = mem->start;
  r[1] = mem->end;
  SystemError = sys$deltva(r, 0, 0);

  if (mem->channel != 0)
    status = sys$dassgn(mem->channel);
  free(mem);

  if (SystemError == SS$_NORMAL)
    status = SUCCESS;
  else if (SystemError == SS$_ACCVIO)
    status = SHMEM_ACCESS_VIOLATION;
  else
    status = SHMEM_CANNOT_DETACH;

  return(status);    
}


/*	This returns the PID of the parent process.  This code is copied
 *	from xd_utilties.c:GetMasterPID.  It is duplicated here so x11_device
 *	can avoid linking to xd_utilties.
 */

static GetParentPID(MasterPID)
int	*MasterPID;
{
  int	TempPID;
  int	MPID;

/*	Build information block to receive the data from SYS$GETJPI.	*/

  SysInformation.Length   = 4;
  SysInformation.ItemCode = JPI$_MASTER_PID;
  SysInformation.Address  = &MPID;
  SysInformation.RetAddr  = &ReturnLen;
  SysInformation.End      = 0;

  TempPID = 0;

  SystemError = SYS$GETJPIW( 0, &TempPID, 0, &SysInformation, 0, 0, 0 );

/*	Check for successful completion.				*/

  if (BadError) return ( GET_PID_ERROR );

  *MasterPID = MPID;

  return SUCCESS;
}

