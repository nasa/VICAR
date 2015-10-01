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

/*	XD_UTILITIES - Utility routines for XDDAllocate, XDDFree, and DISPLAY
 *
 *	Purpose:
 *
 *	Written by:	 S. Tews
 *	Date:		May 4, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = DefineLogicalName( Actual, Logical, Table )
 *		STATUS = DeleteLogicalName( Logical, Table )
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


/*	Defines a logical name given the actual (equivalence) name.
 *	Logical name definition must be done in SUPERVISOR mode in
 *	order to work correctly, thus CMEXEC must be used.  The logical
 *	name table that will be used is found in the global variable
 *	LogicalNameTable.
 */

DefineLogicalName( Actual, Logical, Table )
char	*Actual, *Logical;
int	Table;
{
  if (Table == VMS_PROCESS_TABLE)
    strcpy (LogicalTable, "LNM$PROCESS");
  else
    strcpy (LogicalTable, "LNM$JOB");
   
/*	Build the string descriptor for the logical name.		*/

  StringDescriptor.dsc$a_pointer = Logical;
  StringDescriptor.dsc$w_length  = strlen(Logical);

  LogicalNameTable.dsc$a_pointer = LogicalTable;
  LogicalNameTable.dsc$w_length  = strlen(LogicalTable);

/*	Build the system information block for SYS$CRELNM.		*/

  SysInformation.Length   = strlen(Actual);
  SysInformation.ItemCode = LNM$_STRING;
  SysInformation.Address  = Actual;

/*	Build the arguement list for SYS$CMEXEC to pass to SYS$CRELNM.	*/

  AccessMode = PSL$C_SUPER;
  ArgListVrdi.Count = 5;
  ArgListVrdi.Arg1  = 0;
  ArgListVrdi.Arg2  = &LogicalNameTable;
  ArgListVrdi.Arg3  = &StringDescriptor;
  ArgListVrdi.Arg4  = &AccessMode;
  ArgListVrdi.Arg5  = &SysInformation;

  SystemError = SYS$CMEXEC( SYS$CRELNM, &ArgListVrdi );

  if (BadError && (SystemError != SS$_SUPERSEDE)) {

    SystemError = SYS$CRELNM( 0,&LogicalNameTable,
			      &StringDescriptor,0,
			      &SysInformation );
  }
/*	Check for successful completion.  If the name was just
 *	superseded, do not sweat it.					
 */

  if (BadError && (SystemError != SS$_SUPERSEDE)) return( CREATE_LNM_ERROR );

  return( SUCCESS );
}


/*	Deletes the specified logical name.  This must be done in
 *	SUPERVISOR mode since that is how the logical names were
 *	defined, thus CMEXEC is used.  The name of the logical name
 *	table that it is deleted from is found in the global string
 *	descriptor LogocalNameTable.
 */

DeleteLogicalName( Logical, Table )
char	*Logical;
int	Table;
{
  if (Table == VMS_PROCESS_TABLE)
    strcpy (LogicalTable, "LNM$PROCESS");
  else
    strcpy (LogicalTable, "LNM$JOB");

/*	Build the string descriptor for the logical name.		*/

   StringDescriptor.dsc$a_pointer = Logical;
   StringDescriptor.dsc$w_length  = strlen(Logical);

  LogicalNameTable.dsc$a_pointer = LogicalTable;
  LogicalNameTable.dsc$w_length  = strlen(LogicalTable);

/*	Build the arguement list for SYS$CMEXEC to pass to SYS$DELLNM.	*/

  AccessMode = PSL$C_SUPER;
  ArgListVrdi.Count = 3;
  ArgListVrdi.Arg1  = &LogicalNameTable;
  ArgListVrdi.Arg2  = &StringDescriptor;
  ArgListVrdi.Arg3  = &AccessMode;

  SystemError = SYS$CMEXEC( SYS$DELLNM, &ArgListVrdi );

  if (BadError && (SystemError != SS$_NOLOGNAM)) {

    SystemError = SYS$DELLNM( &LogicalNameTable, &StringDescriptor,0 );
  }

/*	Check for an error.  There had better not be one!!		*/

  if (BadError && (SystemError != SS$_NOLOGNAM)) return( DELETE_LNM_ERROR );

  return( SUCCESS );
}


/*	Searches the device list for the device that is on the current
 *	VAX and has the default terminal name equal to the users
 *	terminal name.  If it is not successful, it prints a message
 *	and exits.
 */

FindDefault( Device )
char	*Device;
{
  int status;
  int DevNumber;

  status = GetMasterPID ( 0, &UserPID );
  if ( status == SUCCESS ) {

    status = GetUserTerminal ( UserPID, &UserTerminal );
    if ( status == SUCCESS ) {

      for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {
	if ((MATCH( UserTerminal, DIB[DevNumber]->Terminal, 4 )) &&
	    ((DIB[DevNumber]->SystemNumber == UserVAX) ||
             (DIB[DevNumber]->SystemNumber == -1))) break;
      }
      if ( DevNumber == TotalDevices ) {
	status = NO_DEFAULT_DEVICE;
      }
      else {
	strcpy( Device, DIB[DevNumber]->DeviceName );
      }
    }
  }

  return ( status );
}


/*	Searches the device list first for the generic device that is
 *	on the current VAX and has the same DEFAULT terminal as the
 *	user's terminal.  If it is unsuccessful, it searches again,
 *	this time for the first available, unowned device on the
 *	current VAX that matches the generic name.
 */

FindGeneric( Generic, Device )
char	*Generic, *Device;
{					/* Find DEFAULT device (if one) */

  int status;
  int DevNumber;

  status = GetMasterPID ( 0, &UserPID );
  if ( status == SUCCESS ) {

    status = GetUserTerminal ( UserPID, &UserTerminal );
    if ( status == SUCCESS ) {

      for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {
	if ( MATCH( UserTerminal, DIB[DevNumber]->Terminal, 4 ) &&
	   MATCH( Generic, DIB[DevNumber]->DeviceName, 2 ) &&
	   DIB[DevNumber]->Available &&
	   ((DIB[DevNumber]->SystemNumber == UserVAX) ||
            (DIB[DevNumber]->SystemNumber == -1))) break;
      }

/*	If device not found or user can not have device...		*/

      status = GetDeviceInfo();
      if ( status == SUCCESS ) {

	if ((DevNumber == TotalDevices ) ||
	   ((Devices[DevNumber].UserPID != 0) && 
	      (Devices[DevNumber].UserPID != UserPID))) {
	  for ( DevNumber = 0;		/* Search for one he can	 */
	      DevNumber < TotalDevices; 
	      DevNumber++ ) {
	    if ( MATCH( Generic, DIB[DevNumber]->DeviceName, 2 ) &&
	       ((DIB[DevNumber]->SystemNumber == UserVAX) ||
                (DIB[DevNumber]->SystemNumber == -1)) &&
		    DIB[DevNumber]->Available &&
	       (Devices[DevNumber].UserPID == 0) )
	      break;
	  }
	}

/*	Check to see if a device was found.				*/

	if ( DevNumber == TotalDevices ) {
	  status = NO_AVAIL_GENERIC_DEV;
	}
	else
	  strcpy( Device, DIB[DevNumber]->DeviceName );
      }
    }
  }
  return ( status );
}


/*	Searches the device list for the generic device owned by the
 *	current user.  This assumes that the user has previously
 *	allocated a device and now is trying to deallocate it using
 *	just a generic name.  If nothing is found a message is printed
 *	and the program exits.
 */

FindOwnedGeneric( Generic, Device )
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

GetMasterPID( PID, MasterPID )
int	PID;
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

  TempPID = PID;

  SystemError = SYS$GETJPIW( 0, &TempPID, 0, &SysInformation, 0, 0, 0 );

/*	Check for successful completion.				*/

  if (BadError) return ( GET_PID_ERROR );

  *MasterPID = MPID;

  return ( SUCCESS );
}


#ifdef VRDI
/*	Gets the username of the job associated with the given PID.
 *	NOTE:  The PID should be the MASTER PID of the process.
 */

GetUserName( PID, UserName )
int	PID;
char	*UserName;
{
  int	TempPID, i;

/*	Build information block to receive data from SYS$GETJPI.	*/

  SysInformation.Length   = USER_NAME_SIZE;
  SysInformation.ItemCode = JPI$_USERNAME;
  SysInformation.Address  = UserName;
  SysInformation.RetAddr  = &ReturnLen;
  SysInformation.End      = 0;

  TempPID = PID;

  SystemError = SYS$GETJPIW( 0, &TempPID, 0, &SysInformation, 0, 0, 0 );

  UserName[ReturnLen] = '\000';

/*	Check for successful completion.				*/

  if (BadError) return ( GET_OWNER_NAME_ERROR );

/*	Remove trailing blanks.						*/

  for ( i = strlen(UserName)-1; UserName[i] == ' '; i-- ) {
    UserName[i] = '\000';
  }

  return ( SUCCESS );
}

#endif /* VRDI */


/*	Gets the terminal name associated with the specified PID.
 *	NOTE:  The given PID should be the MASTER PID of the process.
 */

GetUserTerminal( PID, Terminal )
int	PID;
char	*Terminal;
{
  int i;
  int	TempPID;

/*	Build information block to receive data from SYS$GETJPI.	*/

  SysInformation.Length   = TERMINAL_NAME_SIZE;
  SysInformation.ItemCode = JPI$_TERMINAL;
  SysInformation.Address  = Terminal;
  SysInformation.RetAddr  = &ReturnLen;
  SysInformation.End      = 0;

  TempPID = PID;

  SystemError = SYS$GETJPIW( 0, &TempPID, 0, &SysInformation, 0, 0, 0 );

/*	Check for successful completion.				*/

  if (BadError) return ( GET_OWNER_TERM_ERROR );

  Terminal[ReturnLen] = '\000';
  StringDescriptor.dsc$a_pointer = Terminal;
  StringDescriptor.dsc$w_length  = strlen(Terminal);
   
  SysInformation.Length   = TERMINAL_NAME_SIZE;
  SysInformation.ItemCode = DVI$_TT_PHYDEVNAM;
  SysInformation.Address  = Terminal;
  SysInformation.RetAddr  = &ReturnLen;
  SysInformation.End      = 0;
   
  SystemError = SYS$GETDVIW( 0, 0, &StringDescriptor,
				&SysInformation, 0, 0, 0, 0 );

/*	Check for successful completion.				*/

  if (BadError) return ( GET_OWNER_TERM_ERROR );

  if (Terminal[ReturnLen-1] == ':') {		/* Remove trailing ':'	*/
    Terminal[ReturnLen-1] = '\000';
  }

  while (Terminal[0] == '_') {			/* Remove leading '_'	*/
    for ( i = 0; i < strlen(Terminal); i++ ) {
      Terminal[i] = Terminal[i+1];
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

GetUserVAX( UserVAX )
int *UserVAX;
{
  static char	String[11];
  struct dsc$descriptor_s
    RetString = {
    10, DSC$K_DTYPE_T, DSC$K_CLASS_S, &String
  };

/*	Build symbol name string descriptor for LIB$GET_SYMBOL.		*/

  StringDescriptor.dsc$a_pointer = VAX_SYMBOL_STRING;
  StringDescriptor.dsc$w_length  = strlen( VAX_SYMBOL_STRING );

  SystemError = LIB$GET_SYMBOL( &StringDescriptor,
				       &RetString, &ReturnLen, 0 );

/*	Check for successful completion.				*/

  if (SystemError == LIB$_NOSUCHSYM) {

    *UserVAX == 0;				/* Assume value is 0	*/
  }
  else {

    if (BadError) return( GET_VAX_SYMBOL_ERROR );

    sscanf( RetString.dsc$a_pointer, "%d", UserVAX );
  }

  return ( SUCCESS );
}

/*	Determines the User PID, User Terminal and UserName of currently
 *	owned devices
 */

GetDeviceInfo()
{
  int status;
  int OwnerPID;
  int DevNumber;
  char Temp[10];

  status = SUCCESS;
  for ( DevNumber = 0; DevNumber < TotalDevices; DevNumber++ ) {
#ifdef VRDI
    Devices[DevNumber].UserName[0] = '\000';
    Devices[DevNumber].UserTerminal[0] = '\000';
#endif /* VRDI */

    if ((DIB[DevNumber]->SystemNumber == UserVAX) ||
        (DIB[DevNumber]->SystemNumber == -1)) {
	    
      /*  If the device is not available, do not check to see if it has  */
      /*  been allocated.  If it is available and system-allocatable,    */
      /*  use a system call to determine if it has been previously allo- */
      /*  cated.  If it is not allocatable, check for the existence of   */
      /*  the device-specific logical name to determine if it has been   */
      /*  previously allocated.                                          */

      if (!DIB[DevNumber]->Available) {
        SystemError = SS$_NORMAL;
        OwnerPID = 0;
      }
      else {				/*  Device is available  */
        if (DIB[DevNumber]->SystemAllocatable == SYSALLOC_YES) {
          StringDescriptor.dsc$a_pointer = DIB[DevNumber]->DeviceName;
          StringDescriptor.dsc$w_length  = strlen(DIB[DevNumber]->DeviceName);

          SysInformation.Length   = 4;
          SysInformation.ItemCode = DVI$_PID;
          SysInformation.Address  = &OwnerPID;
          SysInformation.RetAddr  = &ReturnLen;
          SysInformation.End      = 0;

          SystemError = SYS$GETDVIW( 0, 0, &StringDescriptor,
					    &SysInformation, 0, 0, 0, 0 );
        }
        else if (DIB[DevNumber]->SystemAllocatable == SYSALLOC_X) {
          if (check_x_device(DevNumber, DIB[DevNumber]->DeviceName))
	    OwnerPID = -1;		/* allocated */
          else
            OwnerPID = 0;
          SystemError = SS$_NORMAL;
        }
        else {			/*  Device is not allocatable  */

          /*  An owner PID of 0 means that the device is not currently  */
          /*  allocated.  Therefore, if the device is allocated to the  */
          /*  calling process, we set the owner PID to -1.              */

          if (TranslateLogicalName(DIB[DevNumber]->LogicalName, Temp,
                                   VMS_JOB_TABLE) == SUCCESS)
            OwnerPID = -1;
          else
            OwnerPID = 0;
          SystemError = SS$_NORMAL;
        }
      }
      if ( BadError ) {
	status = GET_OWNER_PID_ERROR;
	break;
      }
#ifdef TAE
      else {
	Devices[DevNumber].UserPID = OwnerPID;
      }
#endif /* TAE */
#ifdef VRDI
      else if (OwnerPID == 0) {
	Devices[DevNumber].UserPID = 0;
 	strcpy( Devices[DevNumber].UserName, "none" );
      }
      else {

        /*  If the owner PID is -1, this user owns a non-allocateable  */
        /*  device.  Set the owner PID back to 0 to tell the           */
        /*  GetMasterPID function to find the ID of the current        */
        /*  process.                                                   */
        if (OwnerPID == -1)
          OwnerPID = 0;

	status = GetMasterPID( OwnerPID, &Devices[DevNumber].UserPID );
	if (status == GET_PID_ERROR ) {
	  Devices[DevNumber].UserPID = -1;
	  sprintf( Devices[DevNumber].UserTerminal, "xxxx" );
	  sprintf( Devices[DevNumber].UserName, "xxxxxx" );
	  status = SUCCESS;
	}
	else {
	  if (status == SUCCESS ) {
	    status = GetUserTerminal( Devices[DevNumber].UserPID, 
		     &Devices[DevNumber].UserTerminal );
            if (status != SUCCESS)	/* ignore errors here */
              strcpy(Devices[DevNumber].UserTerminal, "xxxx");
	    status = GetUserName( Devices[DevNumber].UserPID, 
			 &Devices[DevNumber].UserName );
	  }
	  else break;
	}
      }
#endif /* VRDI */
    }
  } /* for (DevNumber... */
  return ( status );
}
