#include "xvmaininc.h"

/* Unix System dependent device allocation routines

   Written by: Bob Deen
   Date:       3/23/89
*/

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include <sys/types.h>

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

static void UpperCaseString();

/************************************************************************/
/* XD_Allocate_Device:
 *	Allocates device to user.
 */

int XD_Allocate_Device( unit, dev )
int  unit;				/* in: unit number */
char *dev;				/* in: device name */
{
int status;
char device[DEVNAME_SIZE+1];
time_t time;

  UpperCaseString(dev, device, DEVNAME_SIZE);

  if (check_x_device(unit, device, &time))	/* Already allocated? */
    status = DEVICE_ALREADY_ALLOC;
  else
    status = allocate_x_device(unit, device);

  current_x_device(unit, device);		/* Set it as current */

  return (status);
}

/************************************************************************/
/* XD_Get_Device: 
 *      Returns unit number of device currently in use (last allocated) if
 *      dev_name is NULL, or named device if it's not.
 */

int XD_Get_Device( unit, dev_name )
int  *unit;			/* out: unit number */
char *dev_name;			/* in: device name (or NULL for current) */
{
  int   status, find_name; 
  int   DevNumber;
  time_t time, latest_time;
  int latest_unit;

  GetUserVAX(&UserVAX);
  DPR(("get_device: UserVAX=%d\n", UserVAX));

  if (dev_name == NULL || strlen(dev_name) == 0)	/* No dev name given */
    find_name = TRUE;
  else {				/* device name given */
    UpperCaseString(dev_name, Actual, DEVNAME_SIZE);
    find_name = FALSE;
  }
  DPR(("get_device: find_name=%d, name='%s'\n", find_name, Actual));

  status = SUCCESS;
  if (!xd_initialized) {
    status = XD_Initialize();
  }
  if ( status == SUCCESS ) {
    latest_time = 0;
    latest_unit = -1;
    DPR(("get_device: top of loop\n"));
    for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {

      if ((DIB[DevNumber]->SystemNumber == UserVAX) ||
          (DIB[DevNumber]->SystemNumber == -1)) {

        if (find_name) {		/* Search for last allocated device */
          if (check_x_device(DevNumber, DIB[DevNumber]->DeviceName, &time)) {
            if (time > latest_time) {		/* last one allocated? */
              DPR(("get_device: found latest time=%d, unit=%d\n",time,DevNumber));
              latest_time = time;
              latest_unit = DevNumber;
            }
          }
        }
        else {
	  if (MATCH(DIB[DevNumber]->DeviceName, Actual, 4)) {
            DPR(("get_device: found name, checking alloc\n"));
            if (check_x_device(DevNumber, Actual, &time)) {
              latest_unit = DevNumber;
              DPR(("get_device: allocated, unit=%d\n", DevNumber));
              break;			/* Device is allocated */
            }
          }
	}
      }
    }

    /*  Check to see if a device was found  */

    if (latest_unit == -1)
      status = DEVICE_NOT_ALLOC;
    else
      *unit = latest_unit;
  }
  return (status);
}

/************************************************************************/
/* XD_Free_Device:
 *	Deallocates device if allocated
 */

int XD_Free_Device( unit, dev )
int  unit;				/* in: unit number */
char *dev;				/* in: device name */
{
  char	device[DEVNAME_SIZE+1];
  int	status;
  time_t time;

  UpperCaseString(dev, device, DEVNAME_SIZE);

  /*  Close device if still open.  */

  if ( DCB[DeviceNumber] != 0 ) zddclose( DeviceNumber );
  XD_Allocated[DeviceNumber] = FALSE;

  if (check_x_device(unit, device, &time))
    status = free_x_device(unit, device);
  else
    status = DEVICE_NOT_ALLOC;

  return (status);
}


/************************************************************************/
/* XD_Get_Devices_Alloc:
 *	Returns devices currently in use by the current process.  Devices
 *	are returned in a two-dimensional string array "devices".  The
 *	size of each row is given by "len".
 */
int XD_Get_Devices_Alloc(ndev, len, devices)
int *ndev;		/* out: # of devices allocated */
int  len;		/* in: max len of strings */
char devices[];		/* out: two-dimensional array holding strings */

{
  int	  maxdev;
  int     DevNumber;
  time_t time;

  GetUserVAX(&UserVAX);

  maxdev = *ndev;

  *ndev = 0;

  for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {

    if ((DIB[DevNumber]->SystemNumber == UserVAX) ||
        (DIB[DevNumber]->SystemNumber == -1)) {

      if (check_x_device(DevNumber, DIB[DevNumber]->DeviceName, &time))
        if (*ndev < maxdev) {
          strncpy(&devices[*ndev*len], DIB[DevNumber]->DeviceName, len);
          (*ndev)++;
        }
    }
  }

  return(SUCCESS);
}

/************************************************************************/
/* UpperCaseString:
 *	Converts a string to upper case with a maximum length.
 */

static void UpperCaseString(in, out, maxlen)
char *in, *out;
int maxlen;
{
  int i, Length;

  Length = MIN(strlen(in), maxlen);
  for ( i = 0; i < Length; i++ ) {
    if (islower(in[i]))
      out[i] = toupper(in[i]);
    else
      out[i] = in[i];
  }
  out[Length] = '\0';
}

