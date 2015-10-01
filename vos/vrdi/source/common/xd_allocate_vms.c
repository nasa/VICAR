/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/
/*									*/
/* The VRDI version is named ".c", and the TAE version is named ".cnp",	*/
/* as per their respective naming standards.	                        */
/*									*/
/* The symbol VRDI must be defined for the VRDI version, and TAE must   */
/* be defined for the TAE version.                                      */

#ifndef TAE
#include "xvmaininc.h"
#endif

/* VMS System dependent device allocation routines

   Written by: Bob Deen
   Date:       3/23/89
*/

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"
#include "xdfuncs.h"

/************************************************************************/
/* XD_Allocate_Device:
 *	Allocates device to user.
 */

int XD_Allocate_Device( unit, dev )
int  unit;				/* in: unit number */
char *dev;				/* in: device name */
{
int status, already;
char device[DEVNAME_SIZE+1];
char  Temp[10];

  UpperCaseString(dev, device, DEVNAME_SIZE);

  already = FALSE;

  if (DIB[DeviceNumber]->SystemAllocatable == SYSALLOC_YES) {

    /*  If the device is allocatable by the computer, then build the  */
    /*  device name string descriptor for call to allocate device.    */

    StringDescriptor.dsc$a_pointer = device;
    StringDescriptor.dsc$w_length  = strlen(device);

    /*  Build argument list for SYS$CMEXEC to call SYS$ALLOC.  */

    ArgListVrdi.Count = 5;
    ArgListVrdi.Arg1  = &StringDescriptor;
    ArgListVrdi.Arg2  = 0;
    ArgListVrdi.Arg3  = 0;
    ArgListVrdi.Arg4  = PSL$C_SUPER;
    ArgListVrdi.Arg5  = 0;

    SystemError = SYS$CMEXEC( SYS$ALLOC, &ArgListVrdi );

    /*  Check for successful allocation.  NOTE:  Device may already be  */
    /*  allocated to you which is a success.				*/

    if (SystemError == SS$_NOPRIV ) {
      SystemError = SYS$ALLOC( &StringDescriptor,0,0,0,0 );
    }
    if (SystemError == SS$_DEVALLOC)	/* another user has it */
      return CANNOT_ALLOC_DEVICE;
    if (SystemError == SS$_DEVALRALLOC)	/* we already have it */
      already = TRUE;
  }
  else if (DIB[DeviceNumber]->SystemAllocatable == SYSALLOC_X) { /* X-windows */

    if (check_x_device(unit, device))	/* Already allocated? */
      already = TRUE;
    else
      SystemError = allocate_x_device(unit, device);

  }
  else {				/*  Device is not allocatable  */
    /*  Device is not allocatable.  To determine if it has been previously  */
    /*  allocated, we check for the existence of the device-specific logi-  */
    /*  cal name.                                                           */

    if (TranslateLogicalName(DIB[DeviceNumber]->LogicalName, Temp,
                             VMS_JOB_TABLE) == SUCCESS)
      already = TRUE;
    SystemError = SS$_NORMAL;
  }

  /*  Lastly, set up the logical names that are needed.  */

  if (BadError && !(SystemError == SS$_DEVALRALLOC)) {
    status = CANNOT_ALLOC_DEVICE;
  }
  else {
    status = DefineLogicalName(device, &DIB[DeviceNumber]->LogicalName,
                               VMS_JOB_TABLE);
    if (status == SUCCESS) {
      status = DefineLogicalName(device, "XDDEVICE", VMS_JOB_TABLE);
      if ((status == SUCCESS) && (already)) {
	status = DEVICE_ALREADY_ALLOC;
      }
    }
  }
  return (status);
}

#ifdef VRDI

/************************************************************************/
/* XD_Get_Device: 
 *      Returns unit number of device currently in use (last allocated) if
 *      dev_name is NULL, or named device if it's not.
 */

int XD_Get_Device( unit, dev_name )
int  *unit;			/* out: unit number */
char *dev_name;			/* in: device name (or NULL for current) */
{
  int   i, status; 
  int   DevNumber;
  int   mypid;
  char  Temp[10];

/*	If no device name given, get it from the XDDEVICE logical name.	*/

  if (dev_name == NULL || strlen(dev_name) == 0) {
    status = TranslateLogicalName("XDDEVICE", Actual, VMS_JOB_TABLE);
  }
  else {				/* device name given */
    UpperCaseString(dev_name, Actual, DEVNAME_SIZE);
    status = SUCCESS;
  }

  if ( (status == SUCCESS) && (!xd_initialized ) ) {
    status = XD_Initialize();
  }
  if ( status == SUCCESS ) {
    status = GetUserVAX( &UserVAX );
    if ( status == SUCCESS ) {
      status = GetMasterPID(0, &mypid);		/* get current PID */
      if (status == SUCCESS) {
	status = GetDeviceInfo();	/* get owner PID for all devices */
	if (status == SUCCESS) {
          for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {

            /*  To determine if the device is being used by this process,  */
            /*  we match the device name against the actual name and the   */
            /*  device system number against the process system number.    */

	    if (MATCH(DIB[DevNumber]->DeviceName, Actual, 4) &&
               ((DIB[DevNumber]->SystemNumber == UserVAX) ||
                (DIB[DevNumber]->SystemNumber == -1))) {

              /*  If the device is allocatable, we check the id of the     */
              /*  process that owns the device against the current process */
              /*  id.  If they match, the device is owned by the process   */

              if (DIB[DevNumber]->SystemAllocatable == SYSALLOC_YES) {
                if (Devices[DevNumber].UserPID == mypid)
                  break;
              }

	      /*  If an X-windows device, see if the process is still running */
              else if (DIB[DevNumber]->SystemAllocatable == SYSALLOC_X) {
                if (check_x_device(DevNumber, Actual)) {
                  Devices[DevNumber].UserPID = mypid;
                  break;
                }
              }
 
              /*  If the device is not allocatable, then it will not have  */
              /*  a process id associated with it.  So we check the device-*/
              /*  specific logical name to determine if it is active.  If  */
              /*  so, then the device is owned by the process.             */
              else {
                if (TranslateLogicalName(DIB[DevNumber]->LogicalName, Temp,
                                         VMS_JOB_TABLE) == SUCCESS) {
                  Devices[DevNumber].UserPID = mypid;
                  break;
                }
              }
            }
	  }
	}
      }

      /*  Check to see if a device was found  */

      if (DevNumber == TotalDevices ) {
	status = DEVICE_NOT_ALLOC;
      }
      else {
	*unit = DevNumber;
      }
    }
  }
  return (status);
}

#endif /* VRDI */

/************************************************************************/
/* TranslateLogicalName:
 *	Returns the translation of a logical name, after removing any
 *	leading '_'s or trailing ':'s.
 */

TranslateLogicalName(Logical, Actual, Table)
char *Logical;				/* in: logical name to translate */
char *Actual;				/* out: translated name */
int  Table;
{
  int status;
  int i;

  if (Table == VMS_PROCESS_TABLE)
    strcpy( LogicalTable, "LNM$PROCESS");
  else
    strcpy( LogicalTable, "LNM$JOB");

  strcpy( LogicalUnit, Logical);

/*	Build the string descriptor for the logical name.		*/

  StringDescriptor.dsc$a_pointer = LogicalUnit;
  StringDescriptor.dsc$w_length  = strlen(LogicalUnit);

  LogicalNameTable.dsc$a_pointer = LogicalTable;
  LogicalNameTable.dsc$w_length  = strlen(LogicalTable);

/*	Build the system information block for SYS$TRNLNM.		*/

  Itemlist.Length   = 20;
  Itemlist.ItemCode = LNM$_STRING;
  Itemlist.Address  = Actual;
  Itemlist.RetAddr  = &ReturnLen;
  Itemlist.End      = 0;

  SystemError = SYS$TRNLNM ( 0, &LogicalNameTable, &StringDescriptor, 0,
			    &Itemlist );

/*	Check for successful completion.  				*/

  if ( SystemError == SS$_NORMAL ) {
    status = SUCCESS;
  }    
  else if ( SystemError == SS$_NOLOGNAM ) {
    status = DEVICE_NOT_ALLOC;
  }
  else {
    status = TRANS_LNM_ERROR;
  }
  if ( status == SUCCESS ) {

    Actual[ReturnLen] = '\000';
    if ( Actual[ReturnLen-1] == ':' ) { /* Remove trailing ':'	*/
      Actual[ReturnLen-1] = '\000';
    }
    while (Actual[0] == '_' ) {		/* Remove leading '_'	*/
      for ( i = 0; i < strlen(Actual); i++ ) {
        Actual[i] = Actual[i+1];
      }
    }
  }

  return status;
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
  char	name[10], Temp[10];
  int	status;
  int	ndev;

  UpperCaseString(dev, device, DEVNAME_SIZE);

  /*  Close device if still open.  */

#ifdef VRDI
  if ( DCB[DeviceNumber] != 0 ) zddclose( DeviceNumber );
  XD_Allocated[DeviceNumber] = FALSE;
#endif /* VRDI */

  if (DIB[DeviceNumber]->SystemAllocatable == SYSALLOC_YES) {
    /*  Build device name string descriptor.  */

    StringDescriptor.dsc$a_pointer = device;
    StringDescriptor.dsc$w_length  = strlen(device);

    /*  Build argument list for SYS$CMEXEC to call SYS$DALLOC.  */

    ArgListVrdi.Count = 2;
    ArgListVrdi.Arg1  = &StringDescriptor;
    ArgListVrdi.Arg2  = PSL$C_SUPER;

    SystemError = SYS$CMEXEC( SYS$DALLOC, &ArgListVrdi );
    if (SystemError == SS$_NOPRIV ) {
      SystemError = SYS$DALLOC( &StringDescriptor,0 );
    }
  }
  else if (DIB[DeviceNumber]->SystemAllocatable == SYSALLOC_X) {
    if (check_x_device(unit, device))
      SystemError = free_x_device(unit, device);
    else
      SystemError = SS$_DEVNOTALLOC;
  } 
  else {				/*  Device is not allocatable  */
    if (TranslateLogicalName(DIB[DeviceNumber]->LogicalName, Temp,
                             VMS_JOB_TABLE) == SUCCESS)
      SystemError = SS$_NORMAL;
    else
      SystemError = SS$_DEVNOTALLOC;
  }

  /*  Check for successful completion.  */

  if (BadError) {
    return (CANNOT_DEALL_DEVICE);
  }

  /*  Delete logical names that are no longer needed.  */

  status = DeleteLogicalName( DIB[DeviceNumber]->LogicalName, VMS_JOB_TABLE );
  if ( status != SUCCESS )
    return(status);

  /*  If the XDDEVICE logical name points to this device, reassign it	*/
  /*  to point to the new current device, or just delete it if there	*/
  /*  are none.							        */

  status = TranslateLogicalName("XDDEVICE", Actual, VMS_JOB_TABLE);
  if (status != SUCCESS)
    return (status);

  if (MATCH(Actual, device, 4)) {
    status = DeleteLogicalName( "XDDEVICE", VMS_JOB_TABLE );
    if (status != SUCCESS)
      return (status);

    ndev = 1;				/* get first device still allocated */
    status = XD_Get_Devices_Alloc(&ndev, 10, name);
    if (status != SUCCESS)
      return (status);

    if (ndev == 1) {
      status = DefineLogicalName(name, "XDDEVICE", VMS_JOB_TABLE);
      if (status != SUCCESS)
        return(status);
    }
  }

  return (SUCCESS);
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
  int     status;
  int     DevNumber;
  int     mypid;

  maxdev = *ndev;

  *ndev = 0;

  status = GetUserVAX(&UserVAX);
  if (status != SUCCESS)
    return status;

  status = GetDeviceInfo();		/* get info on all devices */
  if (status != SUCCESS)
    return status;

  status = GetMasterPID(0, &mypid);		/* get current PID */
  if (status != SUCCESS)
    return status;

  for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {

    if (((DIB[DevNumber]->SystemNumber == UserVAX) ||
         (DIB[DevNumber]->SystemNumber == -1)) &&
        (Devices[DevNumber].UserPID == mypid)) {	/* match */

      if (*ndev < maxdev) {
        strncpy(&devices[*ndev*len], DIB[DevNumber]->DeviceName, len);
        (*ndev)++;
      }
    }
  }

  status = SUCCESS;

  return(status);
}

/************************************************************************/
/* UpperCaseString:
 *	Converts a string to upper case with a maximum length.
 */

static UpperCaseString(in, out, maxlen)
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

