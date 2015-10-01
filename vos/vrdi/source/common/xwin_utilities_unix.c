#include "xvmaininc.h"

/* Unix System dependent X-windows allocation routines. */

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#if FCNTL_AVAIL_OS
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#if !LOCKF_AVAIL_OS
#include <sys/file.h>
#endif

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"
#include "xdfuncs.h"

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A) printf A
#else /* DEBUG */
#define DPR(A)
#endif /* DEBUG */

/************************************************************************/
/* check_x_device(unit, device, time);
 *	Returns TRUE if the device is allocated, FALSE otherwise.
 *	The time of last access (write) is returned in time.
 */

int check_x_device(unit, device, time)
int unit;
char *device;
time_t *time;
{
  int fd, status;
  char filename[100];
  struct stat statbuf;
  char *getenv();

  *time = 0;

  /* Get the lock filename */

  if (getenv("VRDITMP") == NULL)
    sprintf(filename, "%s%s.%d", X_SCRATCH_DIR, device, getuid());
  else
    sprintf(filename, "%s%s.%d", getenv("VRDITMP"), device, getuid());

  /* Try to open it for read and test the lock */

  DPR(("check_x_device: opening '%s'\n", filename));
  fd = open(filename, O_RDONLY);
  if (fd == -1)
    return FALSE;			/* Not there if file not present */

  DPR(("check_x_device: testing lock\n"));
#if LOCKF_AVAIL_OS
  status = lockf(fd, F_TEST, 0);
#else
  status = flock(fd, LOCK_EX | LOCK_NB);
#endif
  if ((status == -1) && (errno==EACCES || errno==EAGAIN || errno==EWOULDBLOCK)) {	/* locked? */
    DPR(("check_x_device: file locked\n"));
    status = fstat(fd, &statbuf);
    if (status == -1) {			/* oops! */
      close(fd);
      return FALSE;
    }
    *time = statbuf.st_mtime;		/* get time of last modify */
    close(fd);
    DPR(("check_x_device: return TRUE, time=%d\n", *time));
    return TRUE;			/* device is allocated */
  }
#if !LOCKF_AVAIL_OS
  flock(fd, LOCK_UN);
#endif
  close(fd);		/* File exists but not locked: process died */
  DPR(("check_x_device: return FALSE\n"));
  return FALSE;

}

/************************************************************************/
/* allocate_x_device(unit, device);
 *	Start the X-windows subprocess for the given device.
 *	It is assumed that the subprocess does not exist - this must be
 *	checked (via check_x_device()) before calling this routine.
 */

allocate_x_device(unit, device)
int unit;
char *device;
{
  int status;
  char file[255];
  char nl[10], ns[10], strunit[10];
  char *ptr;
  char windowid[20];
  char *getenv();
  time_t time;

  /* Get the executable name */
  sprintf(file, "%s/%s", getenv("VRDILIB"), "x11_device");

  /* Now get the parameters */
  sprintf(nl, "%d", DIB[unit]->nLines);
  sprintf(ns, "%d", DIB[unit]->nSamps);
  ptr = getenv("WINDOWID");
  if (ptr == NULL)
    strcpy(windowid, "0");
  else
    strcpy(windowid, ptr);
  sprintf(strunit, "%d", unit);

  /* fork off a new process */

  DPR(("%s %s %s %s %s %s!!\n",file,device,nl,ns,windowid,strunit));
  if ((status = fork()) == 0) {
    /* if this process is the child process, start up the device */
    status = execl(file, file, device, nl, ns, windowid, strunit, NULL);
  }

  while (!check_x_device(unit, device, &time))
    ;	/* Wait for start (so unit may be called immediately upon return */

  return SUCCESS;
}

/************************************************************************/
/* free_x_device(unit, device)
 *	Destroys subprocess for X-windows device.
 *	This routine assumes that the subprocess exists.  This must be
 *	checked (via check_x_device()) before calling this routine.
 */
int free_x_device(unit, device)
int unit;
char *device;
{
  int fd;
  int request;
  int status;
  struct sockaddr mstr_header;
  char socket_name[20];

  sprintf(socket_name, "%s%s%04.4x", X_SOCKET_HDR, device, getuid());
  DPR(("free_x_device: socket_name='%s'\n", socket_name));
  fd = socket_open(0, socket_name, &mstr_header,  0);
  if (fd != -1) {
    request = FREE_DEVICE;
    status = socket_send(fd, &request, sizeof(int));
    socket_free(fd, NULL);
    if (status == 0)
      return CANNOT_DEALL_DEVICE;
    else
      return SUCCESS;
  }

  return SUCCESS;		/* is this really valid? */
}

/************************************************************************/
/* current_x_device(unit, device);
 *	Sets the device to be the current one by writing to the lock
 *	file (thereby setting the modification date).  Note that
 *	the file is written even if the lock is in place - this works
 *	because the lock is only advisory, and is intentional.  The lock
 *	is only used to see if the subprocess is still running, anyway.
 */

int current_x_device(unit, device)
int unit;
char *device;
{
  int fd;
  char filename[100];
  int value;
  char *getenv();

  /* Get the lock filename */

  if (getenv("VRDITMP") == NULL)
    sprintf(filename, "%s%s.%d", X_SCRATCH_DIR, device, getuid());
  else
    sprintf(filename, "%s%s.%d", getenv("VRDITMP"), device, getuid());

  /* Try to open it for write */

  fd = open(filename, O_WRONLY);
  if (fd == -1)
    return SUCCESS;			/* Don't care about errors */

  DPR(("current_x_device: writing file\n"));
  value = 0;
  write(fd, &value, sizeof(value));

  close(fd);
  DPR(("returning from current_x_device\n"));
  return SUCCESS;

}

