#include "xvmaininc.h"

/* Unix System dependent shared memory control routines
 *
 * Each user has a block of 256 shared memory id's available (starting at
 * uid*256).  In order to find the correct shared memory segment, the entire
 * block is searched.  A set of "magic numbers" are stored at the beginning
 * of the shared memory segment to identify it as a VRDI segment, what device,
 * and what type of shared memory it is.  A segment is considered found only
 * if it exists, it can be attached to, and all the magic numbers match.
 */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"
#include "xdfuncs.h"

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)	{printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

#define MAX_SHMEM_KEYS	256

static int shm_attach[MAX_SHMEM_KEYS];
static char *shm_addr[MAX_SHMEM_KEYS];
static int shm_id[MAX_SHMEM_KEYS];

static int firsttime = TRUE;

struct magic {
    char vicar_id[4];		/* VICR */
    char vrdi_id[4];		/* VRDI */
    int uid;
    int unit;
    int type;
};

int Attach_Shmem();

/************************************************************************/
/* Alloc_Shmem:
 *	This just calls Attach_Shmem.  See that routine for details.
 */

int Alloc_Shmem(unit, shmem_size, type)
int unit;		/* in:  unit number */
int shmem_size;		/* in:  size of shared memory needed, in bytes */
int type;		/* in:  type of shared memory to be allocated,	*/
			/*	e.g., DCB, device-specific, etc.	*/
{
   return Attach_Shmem(unit, shmem_size, 0, type);
}

/************************************************************************/
/* Attach_Shmem:
 *	Allocates and attaches a pointer to shared memory.
 *
 *	The "type parameter to this function is used to differentiate between
 *	shared memory required by specific devices as compared to that
 *	required by the VRDI or TAE.  Types 0-9 are reserved for the VRDI/TAE.
 */

int Attach_Shmem(unit, shmem_size, aptr, type)
int unit;		/* in:  unit number */
int shmem_size;		/* in:  size of shared memory needed, in bytes */
char **aptr;		/* out: address of shared memory pointer.  If NULL */
			/*	is passed in, the pointer is not returned. */
int type;		/* in:  type of shared memory to be allocated,     */
			/*	e.g., DCB, device-specific, etc.           */
{
  int shmid = 0;
  int match;
  int size;
  int i;
  key_t key, start_key, stop_key, free_key;
  struct magic *magic_id;
  char *test_addr;

  if (firsttime) {
    for (key=0; key<MAX_SHMEM_KEYS; key++) {
      shm_attach[key] = FALSE;
      shm_addr[key] = NULL;
      shm_id[key] = 0;
    }
    firsttime = FALSE;
  }

  size = shmem_size + sizeof(struct magic);

/* Start out by searching the possible segments to see if one already exists */

  start_key = getuid() * MAX_SHMEM_KEYS;
  stop_key = getuid() * MAX_SHMEM_KEYS + MAX_SHMEM_KEYS-1;

  free_key = -1;
  match = FALSE;

  DPR(("shmem_attach: Searching for a key: start=%d, stop=%d, unit=%d, type=%d\n", start_key, stop_key, unit, type));
  for (i=start_key; i<=stop_key && !match; i++) {
    key = i;
    if (shm_attach[key-start_key])
      test_addr = shm_addr[key-start_key];
    else {
      shmid = shmget(key, size, 0777);
      if (shmid == -1) {
        if (errno == ENOENT && free_key == -1)
          free_key = key;		/* save in case we need to create one */
        test_addr = NULL;
      }
      else {
        test_addr = (char *)shmat(shmid, 0, 0);
        if ((long int)test_addr == -1)
          test_addr = NULL;
      }
    }

    if (test_addr != NULL) {		/* Got an address, now test it */
      DPR(("shmem_attach: Testing key %d\n", key));
      magic_id = (struct magic *)test_addr;
      DPR(("VICR='%4.4s',VRDI='%4.4s',uid=%d,unit=%d,type=%d\n",magic_id->vicar_id,magic_id->vrdi_id,magic_id->uid,magic_id->unit,magic_id->type));
      if (strncmp(magic_id->vicar_id, "VICR", 4) == 0)
        if (strncmp(magic_id->vrdi_id, "VRDI", 4) == 0)
          if (magic_id->uid == getuid())
            if (magic_id->unit == unit)
              if (magic_id->type == type)
                match = TRUE;

      if (!shm_attach[key-start_key])		/* detach if we attached it */
        shmdt(test_addr);
      DPR(("shmem_attach: match=%d\n", match));
    }
  }

/* If we didn't find a match, then find an empty slot */

  if (!match) {
    DPR(("shmem_attach: no match found, using %d\n", free_key));
    if (free_key == -1)
      return SHMEM_NO_MORE_IDS;		/* Oops!  No free ids! */
    key = free_key;
    shmid = shmget(key, size, 0777 | IPC_CREAT);
    if (shmid == -1)
      return SHMEM_CANNOT_ALLOC;
  }

/* Now attach to the shared memory segment if we haven't already */

  if (!shm_attach[key-start_key]) {
    DPR(("shmem_attach: attaching the segment\n"));
    shm_addr[key-start_key] = (char *)shmat(shmid, 0, 0);
    if ((long int)shm_addr[key-start_key] == -1)
      return SHMEM_CANNOT_ALLOC;
    shm_attach[key-start_key] = TRUE;
    shm_id[key-start_key] = shmid;

/* Set up the magic numbers if we created this */

    if (!match) {
      DPR(("shmem_attach: setting the magic numbers\n"));
      magic_id = (struct magic *)shm_addr[key-start_key];
      strncpy(magic_id->vicar_id, "VICR", 4);
      strncpy(magic_id->vrdi_id, "VRDI", 4);
      magic_id->uid = getuid();
      magic_id->unit = unit;
      magic_id->type = type;
      memset(shm_addr[key-start_key]+sizeof(struct magic), 0, shmem_size);
    }
  }

  if (aptr != NULL)
    *aptr = shm_addr[key-start_key] + sizeof(struct magic);

  return SUCCESS;
}

/************************************************************************/
/* Detach_Shmem:
 *	Detaches a pointer from shared memory.
 */
int Detach_Shmem(unit, aptr, type)
int unit;		/* in:  unit number */
char **aptr;		/* in:  address of shared memory pointer */
int type;		/* in:  type of shared memory to be allocated,	*/
			/*	e.g. DCB, device-specific, etc.		*/
{
  key_t key;
  int i, status;

  if (firsttime) {
    for (key=0; key<MAX_SHMEM_KEYS; key++) {
      shm_attach[key] = FALSE;
      shm_addr[key] = NULL;
      shm_id[key] = 0;
    }
    firsttime = FALSE;
  }

  key = -1;

/* See if this segment is attached */

  for (i=0; i<MAX_SHMEM_KEYS; i++) {
    DPR(("detach_shmem: att=%d, addr=%d, ptr-magic=%d\n", shm_attach[i],shm_addr[i],*aptr - sizeof(struct magic)));
    if (shm_attach[i] && (shm_addr[i] == *aptr - sizeof(struct magic))) {
      key = getuid() * MAX_SHMEM_KEYS + i;
      DPR(("detach_shmem: key=%d\n", key));
      shm_attach[i] = FALSE;
      break;
    }
  }

  if (key == -1)			/* not attached */
    return SHMEM_NO_SUCH_ID;

/* It is attached, so free it */
  status = shmdt(*aptr - sizeof(struct magic));
  if (status == -1)
    return SHMEM_CANNOT_DETACH;

  return SUCCESS;
}

/************************************************************************/
/* Remove_Shmem:
 *	Remove a shared memory segment.
 */

int Remove_Shmem(unit, type)
int unit;		/* in:  unit number */
int type;		/* in:  type of shared memory to be allocated,	*/
			/*	e.g. DCB, device-specific, etc.		*/
{
  int shmid;
  int size, status;
  key_t key, start_key, stop_key;
  struct magic *magic_id;
  char *test_addr;

  if (firsttime) {
    for (key=0; key<MAX_SHMEM_KEYS; key++) {
      shm_attach[key] = FALSE;
      shm_addr[key] = NULL;
      shm_id[key] = 0;
    }
    firsttime = FALSE;
  }

  size = sizeof(struct magic);

/* Search the possible segments to see if it exists */

  start_key = getuid() * MAX_SHMEM_KEYS;
  stop_key = getuid() * MAX_SHMEM_KEYS + MAX_SHMEM_KEYS-1;

  DPR(("shmem_remove: Searching for a key: start=%d, stop=%d, unit=%d, type=%d\n", start_key, stop_key, unit, type));
  for (key=start_key; key<=stop_key; key++) {
    if (shm_attach[key-start_key]) {
      test_addr = shm_addr[key-start_key];
      shmid = shm_id[key-start_key];
    }
    else {
      shmid = shmget(key, size, 0777);
      if (shmid == -1)
        test_addr = NULL;
      else {
        test_addr = (char *)shmat(shmid, 0, 0);
        if ((long int)test_addr == -1)
          test_addr = NULL;
      }
    }

    if (test_addr != NULL) {		/* Got an address, now test it */
      DPR(("shmem_remove: Testing key %d\n", key));
      magic_id = (struct magic *)test_addr;
      DPR(("VICR='%4.4s',VRDI='%4.4s',uid=%d,unit=%d,type=%d\n",magic_id->vicar_id,magic_id->vrdi_id,magic_id->uid,magic_id->unit,magic_id->type));
      if (strncmp(magic_id->vicar_id, "VICR", 4) == 0) {
        if (strncmp(magic_id->vrdi_id, "VRDI", 4) == 0)
          if (magic_id->uid == getuid())
            if (magic_id->unit == unit)
              if (magic_id->type == type) {	/* Found it! */
                DPR(("shmem_remove: removing key %d\n", key));
                shmdt(test_addr);		/* don't care about status */
                shm_attach[key-start_key] = FALSE;
                status = shmctl(shmid, IPC_RMID, 0);
                if (status == -1)
                  return SHMEM_CANNOT_REMOVE;
                else
                  return SUCCESS;
              }
      }
      if (!shm_attach[key-start_key])		/* detach if we attached it */
        shmdt(test_addr);
    }
  }

/* If we get here, we didn't find it */

  return SHMEM_NO_SUCH_ID;
}

