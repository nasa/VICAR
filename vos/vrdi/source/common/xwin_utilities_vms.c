/* This same code is used in both the VRDI and in the USEDISP/FREEDISP  */
/* commands in TAE.  The code must be repeated since TAE cannot link to */
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so   */
/* the device will be allocated in the parent process.                  */
/*                                                                      */
/* The VRDI version is named ".c", and the TAE version is named ".cnp", */
/* as per their respective naming standards.                            */
/*                                                                      */
/* The symbol VRDI must be defined for the VRDI version, and TAE must   */
/* be defined for the TAE version.                                      */

#ifndef TAE
#include "xvmaininc.h"
#endif

/* VMS System dependent X-windows allocation routines. */

#ifdef VRDI
#include <types.h>
#include <socket.h>
#endif

#include <descrip.h>

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"
#include "xdfuncs.h"
#ifdef VRDI
#include "sock_emulate.h"
#endif

#ifdef DEBUG
#define DPR(A) printf A
#else /* DEBUG */
#define DPR(A)
#endif /* DEBUG */

/************************************************************************/
/* check_x_device(unit, device);
 *	Returns TRUE if the device is allocated, FALSE otherwise.
 */

int check_x_device(unit, device)
int unit;
char *device;
{
  int pid, dummy, ret_len;
  char proc_name[100];
  struct dsc$descriptor_s prcdescr;

  /* Build process name */
  GetMasterPID(0, &pid);
  sprintf(proc_name, "%s_%x", device, pid);

  prcdescr.dsc$w_length = strlen(proc_name);
  prcdescr.dsc$a_pointer = proc_name;
  prcdescr.dsc$b_class = DSC$K_CLASS_S;
  prcdescr.dsc$b_dtype = DSC$K_DTYPE_T;

  /* Now call $getjpi to see if the process exists */

  SysInformation.Length = 4;
  SysInformation.ItemCode = JPI$_PID;	/* doesn't matter what code */
  SysInformation.Address = &dummy;
  SysInformation.RetAddr = &ret_len;
  SysInformation.End = 0;

  SystemError = SYS$GETJPIW(0, 0, &prcdescr, &SysInformation, 0, 0, 0);

  if (SystemError == SS$_NORMAL)
    return TRUE;		/* it exists */
  else
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
  int i, j, pid;
  int status;
  char file[20];
  char nl[10], ns[10], strunit[10];
  char c_unit[10];
  char *ptr;
  struct dsc$descriptor_s cmddescr, prcdescr;
  char cmd[100], proc_name[20];
  long flags=1; 		/* nowait (for lib$spawn) */

  DPR(("buw - unit=%d", unit));

  /* open the device on the screen */
  /* the slave program is called: x11_device */

  DPR(("bring_up_window x11_device\n");)
  sprintf(file, "x11_device");
  DPR(("buw: access DIB[%d]\n", unit));

  sprintf(nl, "%d", DIB[unit]->nLines);
  sprintf(ns, "%d", DIB[unit]->nSamps);
  sprintf(strunit, "%d", unit);
  
  /* fork off a new process */
  DPR(("buw: fork\n"));

  sprintf( cmd, "%s %s %s %s %s, %s", file, device, nl, ns, "0", strunit);

  cmddescr.dsc$w_length = strlen( cmd);
  cmddescr.dsc$a_pointer = cmd;
  cmddescr.dsc$b_class = DSC$K_CLASS_S;
  cmddescr.dsc$b_dtype = DSC$K_DTYPE_T;

  /* Build process name */
  GetMasterPID(0, &pid);
  sprintf(proc_name, "%s_%x", device, pid);

  prcdescr.dsc$w_length = strlen(proc_name);
  prcdescr.dsc$a_pointer = proc_name;
  prcdescr.dsc$b_class = DSC$K_CLASS_S;
  prcdescr.dsc$b_dtype = DSC$K_DTYPE_T;

  status=lib$spawn( &cmddescr, 0, 0, &flags, &prcdescr, 0, 0, 0, 0, 0, 0, 0);

  DPR(("lib$spawn(>%s<)=%xx\n", cmd, status));

  if ( status & SS$_NORMAL) {
    DPR(("  lib$spawn'ed off %s\n", file));
  }
  else {
    DPR(("  failed to lib$spawn off %s\n", file));
  }
  
  DPR(("leave bring up window\n"));

  return status;
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
  int fd, pid;
  int request;
  int status, usedel;
#ifdef VRDI
  struct sockaddr mstr_header;
  char socket_name[20];
#endif
  char proc_name[20];
  struct dsc$descriptor_s prcdescr;

  usedel = TRUE;		/* delete subprocess if socket doesn't work */

#ifdef VRDI
  sprintf(socket_name, "%s%s", X_SOCKET_HDR, device);
  fd = socket_open(0, socket_name, &mstr_header,  0);
  if (fd != -1) {
    request = FREE_DEVICE;
    status = socket_send(fd, &request, sizeof(int));
    if (status != 0)
      usedel = FALSE;	/* socket worked, don't care about deleting subproc */
    socket_free(fd, NULL);
  }
#endif

  /* Build process name */
  GetMasterPID(0, &pid);
  sprintf(proc_name, "%s_%x", device, pid);

  prcdescr.dsc$w_length = strlen(proc_name);
  prcdescr.dsc$a_pointer = proc_name;
  prcdescr.dsc$b_class = DSC$K_CLASS_S;
  prcdescr.dsc$b_dtype = DSC$K_DTYPE_T;

  status = sys$forcex(0, &prcdescr, 0);
  if (usedel) {			/* check error code */
    if (status != SS$_NORMAL)
      return CANNOT_DEALL_DEVICE;
  }
  return SUCCESS;
}

