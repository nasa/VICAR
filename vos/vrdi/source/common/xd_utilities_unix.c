#include "xvmaininc.h"

/*	XD_UTILITIES - Utility routines for XDDAllocate, XDDFree, and DISPLAY
 *	Unix version
 *
 *	Purpose:
 *
 *	Written by:	 S. Tews
 *	Date:		May 4, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = FindDefault( Device )
 *		STATUS = FindGeneric( Generic, Device )
 *		STATUS = FindOwnedGeneric( Generic, Device )
 *		STATUS = GetMasterPID( PID, MasterPID )
 *		STATUS = GetUserName( PID, UserName )
 *		STATUS = GetUserTerminal( PID, Terminal )
 *		STATUS = GetUserVAX()
 *		STATUS = GetDeviceInfo()
 *
 *	Parameter List:
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xderrors.h"
#include "xdalloc.h"
#include "xdfuncs.h"

#ifdef VRDI
#include "xdroutines.h"
#endif /* VRDI */

#include <stdio.h>

#if FCNTL_AVAIL_OS
#include <fcntl.h>
#else
#include <sys/file.h>
#endif

#include <pwd.h>
#include <sys/types.h>
#include <string.h>


/*	Searches the device list for the device that is on the current
 *	VAX and has the default terminal name equal to the users
 *	terminal name.  If it is not successful, it prints a message
 *	and exits.
 */

int FindDefault( Device )
char	*Device;
{
  int status;
  int DevNumber;

  status = NO_DEFAULT_DEVICE;
  return ( status );
}

/*	Searches the device list first for the generic device that is
 *	on the current VAX and has the same DEFAULT terminal as the
 *	user's terminal.  If it is unsuccessful, it searches again,
 *	this time for the first available, unowned device on the
 *	current VAX that matches the generic name.
 */

int FindGeneric( Generic, Device )
char	*Generic, *Device;
{					/* Find DEFAULT device (if one) */

  int status;
  int DevNumber;

  status = NO_AVAIL_GENERIC_DEV;
  return ( status );
}

/*	Searches the device list for the generic device owned by the
 *	current user.  This assumes that the user has previously
 *	allocated a device and now is trying to deallocate it using
 *	just a generic name.  If nothing is found a message is printed
 *	and the program exits.
 */

int FindOwnedGeneric( Generic, Device )
char	*Generic, *Device;
{
  int status;
  int DevNumber;

  status = GetMasterPID ( 0, &UserPID );
  if ( status == SUCCESS ) {

    status = GetDeviceInfo ();
    if ( status == SUCCESS ) {

      for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {
	if ( MATCH( Generic, DIB[DevNumber]->DeviceName, 2 ) &&
	   ((DIB[DevNumber]->SystemNumber == UserVAX) ||
            (DIB[DevNumber]->SystemNumber == -1)) &&
	   (Devices[DevNumber].UserPID == UserPID) ) break;
      }
      if ( DevNumber == TotalDevices ) {
	status = NO_OWNED_GENERIC_DEV;
      }
      else
	strcpy( Device, DIB[DevNumber]->DeviceName );
    }
  }    
  return ( status );
}

/*	This returns the PID of the parent process in the job of the
 *	specified process.  NOTE:  If PID is 0 then the MASTER PID
 *	of the current process is returned.
 */

int GetMasterPID( PID, MasterPID )
int	PID;
int	*MasterPID;
{

  /* Since Unix always calls with PID==0, we can just use getppid().
     NOTE:  MUST NOT CALL THIS ROUTINE with an arbitrary PID under Unix!!
  */
  if (PID != 0)
    return GET_PID_ERROR;

  *MasterPID = getppid();

  return ( SUCCESS );
}

#ifdef VRDI
/*	Gets the username of the job associated with the given PID.
 *	NOTE:  Since all Unix devices are currently X devices, which
 *	are tracked per user instead of systemwide, this routine just
 *	returns the userid of the current user.
 */

int GetUserName( PID, UserName )
int	PID;
char	*UserName;
{

#if CUSERID_AVAIL_OS
  char *cuserid(char *);    /* does not seem to be defined for 64-bit linux */
  strcpy(UserName, (char *)cuserid(NULL));
#else
  struct passwd *pw;
  pw = getpwuid(getuid());
  strcpy(UserName, pw->pw_name);
#endif

  return ( SUCCESS );
}

#endif /* VRDI */


/*	Gets the terminal name associated with the specified PID.
 *	NOTE:  The given PID should be the MASTER PID of the process.
 */

int GetUserTerminal( PID, Terminal )
int	PID;
char	*Terminal;
{
  int i;
  int t_fd;			/* fd of current term */
  char *tty_name;		/* pointer to terminal name */
  char *basename;		/* ptr to basename of terminal */
  char *ttyname();
   
  if ((t_fd = open("/dev/tty", O_RDONLY)) == -1) {
    strcpy(Terminal, "");
  }
  else {
    close(t_fd);
    if ((tty_name = ttyname(t_fd)) == 0) {
      strcpy(Terminal, "");
    }
    else {
#if STRCHR_AVAIL_OS
        if ((basename = strchr(tty_name,(int)'/')) == 0)  {
          strcpy(Terminal, tty_name);
      }
#else
        if ((basename = (char *) rindex(tty_name, '/')) == 0) {
          strcpy(Terminal, tty_name);
      }
#endif
      else {
	strcpy(Terminal, basename);
      }
    }
  }
  
  return ( SUCCESS );   
}


/*	Gets the "VAX number" of the VAX that User is currently
 *	running on.  This is useful for clustered VAXs where one data
 *	file can be kept containing all the display devices for all the
 *	VAXs in the cluster.  Each VAX is then given a number via a VMS
 *	symbol.  This routine gets that symbol and checks its value.
 *	The name of the symbol is in VAX_SYMBOL_STRING and maybe
 *	changed for a given installation.  If the symbol is not found
 *	the number is assumed to be 0.
 */

int GetUserVAX( UserVAX )
int *UserVAX;
{

  *UserVAX = 0;

  return ( SUCCESS );
}

/*	Determines the User PID, User Terminal and UserName of currently
 *	owned devices
 */

int GetDeviceInfo()
{
  int status;
  int OwnerPID;
  int DevNumber;
  char Temp[10];
  time_t time;
  int check_x_device();

  status = SUCCESS;
  for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {
#ifdef VRDI
    Devices[DevNumber].UserName[0] = '\000';
    Devices[DevNumber].UserTerminal[0] = '\000';
#endif /* VRDI */

    if ((DIB[DevNumber]->SystemNumber == UserVAX) ||
        (DIB[DevNumber]->SystemNumber == -1)) {

      if (!DIB[DevNumber]->Available)
        OwnerPID = 0;
      else {
        if (check_x_device(DevNumber, DIB[DevNumber]->DeviceName, &time))
          OwnerPID = -1;		/* allocated */
        else
          OwnerPID = 0;
      }
    }

    if (OwnerPID == 0) {
      Devices[DevNumber].UserPID = 0;
      strcpy(Devices[DevNumber].UserName, "none");
    }
    else {
      status = GetMasterPID(0, &Devices[DevNumber].UserPID);
      if (status == GET_PID_ERROR) {
        Devices[DevNumber].UserPID = -1;
        strcpy(Devices[DevNumber].UserTerminal, "xxxx");
        strcpy(Devices[DevNumber].UserName, "xxxxxx");
      }
      else {
        status = GetUserTerminal(Devices[DevNumber].UserPID,
				 Devices[DevNumber].UserTerminal);
        if (status != SUCCESS)
           strcpy(Devices[DevNumber].UserTerminal, "xxxx");
        GetUserName(Devices[DevNumber].UserPID,
		    Devices[DevNumber].UserName);
      }
    }
  } /* for (DevNumber... */
  return (SUCCESS);
}
