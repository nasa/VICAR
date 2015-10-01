$!****************************************************************************
$!
$! Build proc for MIPL module data_transfer
$! VPACK Version 1.9, Monday, December 07, 2009, 16:10:07
$!
$! Execute by entering:		$ @data_transfer
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module data_transfer ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to data_transfer.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("data_transfer.imake") .nes. ""
$   then
$      vimake data_transfer
$      purge data_transfer.bld
$   else
$      if F$SEARCH("data_transfer.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake data_transfer
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @data_transfer.bld "STD"
$   else
$      @data_transfer.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create data_transfer.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack data_transfer.com -mixed -
	-s SocketBase.cc DataSource.cc DataSourceDisk.cc DataSourceSocket.cc -
	   DataSink.cc DataSinkDisk.cc DataSinkSocket.cc Stopwatch.cc -
	-i data_transfer.imake -
	-t tst_data_disk.cc tst_data_disk.imake tst_data_socket.cc -
	   tst_data_socket.imake README_tst
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SocketBase.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SocketBase.cc
//
//
//////////////////////////////////////////////////////////////////////////////

#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <unistd.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <netdb.h>
#include <iostream>

#include <string.h>
#include <errno.h>

#include "xvmaininc.h"

#include "SocketBase.h"
#include "Stopwatch.h"
#include "return_status.h"

#define  DEV_DEBUG      0
#define  MAX_SLEEP	60
#define  MAX_RECV_ERRORS	5

//////////////////////////////////////////////////////////////////////////////
//
//				attach
//
//	Attaches a connection to a socket that is listening.  This object
//  only supports one connection at a time, although it is probably possible
//  to accept multiple connections to the same port. --  FUTURE IMPLEMENTATION
//
//////////////////////////////////////////////////////////////////////////////

int	SocketBase::attach( void )
{ socklen_t   addrlen,
	status;
  struct sockaddr       address;

  if (_socketNumber != -1)
  { // cerr << "error condiction ... Socket already open\n";
    strcpy(_errorMessage,"Socket already open");
    return RTN_EXISTS_ERROR;
  }

  //  The accept function establishes the TCP/IP connection between 
  //  the Chan enviroment and the Unix enviroment.
  addrlen = 16;
  addrlen = sizeof(address);
#if X86_LINUX_ARCH
  _socketNumber = accept(_definedSocket, &address, (unsigned int *)(&addrlen));
#else
  _socketNumber = accept(_definedSocket, &address, &addrlen);
#endif

  if (_socketNumber < 0)
  { // cerr << "error condition ... Establishing TCP connection failed\n";
    strcpy(_errorMessage,"ACCEPTing TCP connection failed");
    _sysReason = errno;
    close_it();
    return RTN_ATTACH_ERROR;
  }

  status = fcntl(_socketNumber,F_SETFL,O_NONBLOCK);
  if ((int) status == -1)
  { strcpy(_errorMessage,"Failed to set NOBLOCK mode");
    _sysReason = errno;
#if DEV_DEBUG
cerr << _errorMessage << "; " << strerror(_sysReason) << endl;
#endif
  }

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				close_it
//
//	Closes a socket created by this object.  This could be a partial
//  shutdown of this communication object.  Depending upon how it is used.
//
//  NOTE: This routine should be considered broken now that assigning sockets
//  is allowed (such that they share port info instead of creating a new one.
//  This functionallity would be better served as a sub-function include in
//  this class.
//
//////////////////////////////////////////////////////////////////////////////

void	SocketBase::_close_it( 
  int	*SocketIdentifier )
{

  if (*SocketIdentifier != -1) close( *SocketIdentifier );
  *SocketIdentifier = -1;

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				close_it
//
//	Closes the sockets created for data x-fer and for connections.  This
//  is a complete shutdown of this communication object.
//
//////////////////////////////////////////////////////////////////////////////

void	SocketBase::close_it( void )
{
  if (!_useCount || --(*_useCount))
  { _definedSocket = _socketNumber = -1;
    _useCount = new int(1);				// Object now has it's
							// own counter again
    return;
  }

  if (_definedSocket != -1) close( _definedSocket );
  if (_socketNumber != -1) close( _socketNumber );
  _definedSocket = _socketNumber = -1;
  *_useCount = 1;

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				create
//
//	This function creates a socket and then listens on the socket for
//  connection requests.  This object could be considered a socket or port
//  server.  The attach function must be called before any data x-fer can
//  occur.
//
//////////////////////////////////////////////////////////////////////////////

int	SocketBase::create(
  int	PortNumber)
{ int	status;
//  struct servent	*serptr;
  struct sockaddr_in	sockstr;

  if (_socketNumber != -1 || _definedSocket != -1)
  { // cerr << "error condiction ... Socket already open\n";
    strcpy(_errorMessage,"Socket already open");
    return RTN_EXISTS_ERROR;
  }

  //  The socket function establishes an end point for communication 
  //  and returns a channel that describes the end point.
  _definedSocket = socket(AF_INET, SOCK_STREAM, 0);
  if (_definedSocket < 0)
  { // cerr << "error condition ... Could not create a socket\n";
    strcpy(_errorMessage,"Could not create a socket");
    _sysReason = errno;
    _definedSocket = -1;
    return RTN_CREATE_ERROR;
  }

  //  Define port information as defined by function parameter
  sockstr.sin_family = AF_INET;
  sockstr.sin_port = htons(PortNumber);
  sockstr.sin_addr.s_addr = INADDR_ANY;

  //  The bind function assigns an address to an unnamed socket.
  status = bind(_definedSocket, (struct sockaddr*)&sockstr, sizeof(sockstr));
  if (status < 0)
  { // cerr << "error condition  ... Could not bind socket to address\n";
    strcpy(_errorMessage,"Could not BIND port");
    _sysReason = errno;
    close_it();
    return RTN_ATTACH_ERROR;
  }

/***
cerr << "sockstr.sin_family\t" << sockstr.sin_family << endl;
cerr << "sockstr.sin_port\t" << sockstr.sin_port << endl;
cerr << "sockstr.sin_addr\t" << sockstr.sin_addr.s_addr << endl;
***/

  //  The listen function informs the kernel that the socket
  //  is waiting for connections. 
  status = listen(_definedSocket, SOMAXCONN);
  if (status < 0)
  { // cerr << "error condition ... Could not establish the listen function\n";
    strcpy(_errorMessage,"Could not LISTEN on port");
    _sysReason = errno;
    close_it();
    return RTN_INIT_ERROR;
  }

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				open
//
//	This function opens and attaches to an existing socket/port.  It can
//  be considered a socket client.  This function will allow connections to
//  other hosts.
//
//////////////////////////////////////////////////////////////////////////////

int	SocketBase::open(
  char	*HostName,
  int	PortNumber)
{ int	status;
  char	LocalHostName[256];
  struct hostent	*ServerData;
  struct sockaddr_in	ServerAddr;

  if (_socketNumber != -1)
  { // cerr << "error condiction ... Socket already open\n";
    strcpy(_errorMessage,"Socket already open");
    return RTN_EXISTS_ERROR;
  }

  //  Verify sockect host
  if (HostName && strlen(HostName))	// If hostname defined as parameter
     strcpy(LocalHostName,HostName);
  else					// Else use the current host
  { status = gethostname(LocalHostName,sizeof(LocalHostName));
    if (status < 0)
    { _sysReason = errno;
      strcpy(_errorMessage,"Could not get local host name");
      return RTN_ALLOCATE_ERROR;
    }
  }

  if (!(ServerData = gethostbyname(LocalHostName)))
  { // cerr << "error condition ... Can't identify Hostname\n";
    _sysReason = errno;
    strcpy(_errorMessage,"Can't identify Hostname");
    return RTN_ATTACH_ERROR;
  }

  //  Create a socket to an exisiting port
  memset((void *)&ServerAddr, 0, sizeof(ServerAddr));
  ServerAddr.sin_family = (short)AF_INET;
  memcpy((char *)&ServerAddr.sin_addr,
           ServerData->h_addr,
           ServerData->h_length);
  if (PortNumber == 0)
  { // cerr << "error condition ... No port number specified\n";
    _sysReason = errno;
    strcpy(_errorMessage,"No port number specified");
    return RTN_MISSING_VALUE;
  }
  ServerAddr.sin_port = htons(PortNumber);

  if ((_socketNumber = socket(PF_INET, SOCK_STREAM, 0)) < 0)
  { // cerr << "error condition ... Could not create socket\n";
    _sysReason = errno;
    _socketNumber = -1;
    strcpy(_errorMessage,"Could not create socket");
    return RTN_CREATE_ERROR;
  }

/***
cerr << "ServerAddr.sin_family\t" << ServerAddr.sin_family << endl;
cerr << "ServerAddr.sin_port\t" << ServerAddr.sin_port << endl;
cerr << "ServerAddr.sin_addr\t" << ServerAddr.sin_addr.s_addr << endl;
***/

  //  Connect to socket
  if (connect(_socketNumber,(sockaddr *)&ServerAddr,
              sizeof(struct sockaddr_in)) == -1)
  { // cerr << "error condition ... Can not connect to socket server\n";
    _sysReason = errno;
    strcpy(_errorMessage,"Could not connect to socket server");
    close_it();
    return RTN_ATTACH_ERROR;
  }

  status = fcntl(_socketNumber,F_SETFL,O_NONBLOCK);
  if (status == -1)
  { _sysReason = errno;
#if DEV_DEBUG
cerr << _errorMessage << "; " << strerror(_sysReason) << endl;
#endif
  }

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				read
//
//	This function will read a desired number of bytes off of a connected
//  socket.  This function can return an error while it is still returning
//  data.
//
//////////////////////////////////////////////////////////////////////////////

int	SocketBase::read(
  void	*Buffer,
  int	*BufLength,
  int	TimeOut)
{ int	status,
	Remaining,
	ErrorCycles = 0,
	WaitCycles = 1,
	SleepPeriod = 1,
	RequestedLength = *BufLength;
  char	*BufferPtr = (char *)Buffer;
  Stopwatch	Timer;

  *BufLength = 0;			// Haven't read anything yet
  if (_socketNumber == -1)
  { // cerr << "error condiction ... Socket not connected\n";
    strcpy(_errorMessage,"Socket not connected");
    return RTN_ATTACH_ERROR;
  }

  if (RequestedLength == 0) return RTN_NORMAL;

  do					// REVC loop to get all requested data
  { /***
     ***  Set-up timer stuff (if needed)
     **/
    if (TimeOut > 0)
       if (RTN_FAILURE(Timer.start()))
    { // cerr << "error condition ... " << Timer.errorMessage() << endl;
      strcpy(_errorMessage,Timer.errorMessage());
      _sysReason = Timer.errorReason();
      TimeOut = 0;
    }

    status = recv(_socketNumber,&BufferPtr[*BufLength],
                  (RequestedLength-*BufLength), 0);
    if (status == RequestedLength)	// Got all requested data
    { *BufLength = RequestedLength;
    } else				// Problem of some sort in RECV
    { _sysReason = errno;
      strcpy(_errorMessage,"Socket buffer RECV error");
      if (status > 0)			// Got some data though
      { strcpy(_errorMessage,"Socket RECV data transfer incomplete");
        *BufLength += status;
        WaitCycles = SleepPeriod = 1;	// Reset when data received

        /***
         ***  Determine time-out for obtaining remaining data
         **/
        if (TimeOut == 0)
        { return RTN_TIME_OUT;
        } else if (TimeOut > 0)
        { if ((Remaining = Timer.remaining(TimeOut)) < 0)
          { _sysReason = Timer.errorReason();
            strcpy(_errorMessage,"Timer check error");
            TimeOut = 0;
#if DEV_DEBUG
cerr << _errorMessage << "; (" << _sysReason << ") " <<
        Timer.errorMessage() << endl;
#endif
          } else TimeOut = Remaining;
        }

      } else if (status == 0)		// No error/data (unlikely event)
      { return RTN_EOD;
      } else				// Error in recv - no data returned
      { strcpy(_errorMessage,"Socket system error on RECV");
#if DEV_DEBUG
cerr << _errorMessage << "; (" << _sysReason << ") " <<
        strerror(_sysReason) << endl;
#endif
        if (_sysReason == EWOULDBLOCK)		// EWOULDBLOCK can be ignored
           ErrorCycles = 0;
        else if (ErrorCycles++ > MAX_RECV_ERRORS) return RTN_IO_ERROR;

        if (TimeOut == 0) return RTN_TIME_OUT;
        // cerr << "sleeping for " << SleepPeriod << " seconds\n";
        sleep( SleepPeriod );
        TimeOut -= SleepPeriod;

        /***
         ***  Increase sleep time every 10 contiguous delay cycles
         **/
        if (!(WaitCycles % 10)) SleepPeriod++;
        if ( SleepPeriod > MAX_SLEEP ) SleepPeriod = MAX_SLEEP;
        if (TimeOut > 0 && SleepPeriod > TimeOut) SleepPeriod = TimeOut;
        else if (TimeOut < 0) TimeOut = -1;
      }
    }
    WaitCycles++;
  } while (RequestedLength > *BufLength);

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				write
//
//	This function will write a desired number of bytes to a connected
//  socket.
//
//////////////////////////////////////////////////////////////////////////////

int	SocketBase::write(
  void	*Buffer,
  int	*BufLength,
  int	TimeOut)
{ int	status,
	RtnStatus;

  if (_socketNumber == -1)
  { // cerr << "error condiction ... Socket not connected\n";
    strcpy(_errorMessage,"Socket not connected");
    return RTN_ATTACH_ERROR;
  }

  //  Still need to address TimeOut capability

  status = send(_socketNumber,(char*)Buffer,*BufLength, 0);
  if (status != *BufLength)
  { _sysReason = errno;

    if (status < 0)
    { // cerr << "error condition ... Socket system error\n";
      strcpy(_errorMessage,"Socket buffer SEND system error");
      *BufLength = 0;
    } else
    { // cerr << "error condition ... Socket data transfer\n";
      strcpy(_errorMessage,"Socket SEND data transfer truncated");
      *BufLength = status;
    }
#if DEV_DEBUG
cerr << _errorMessage << "; " << strerror(_sysReason) << endl;
#endif
    RtnStatus = RTN_IO_ERROR;
  } else RtnStatus = RTN_NORMAL;

  return RtnStatus;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSource.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSource.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <strings.h>
#include <iostream>
#include <errno.h>

#include "DataSource.h"
#include "return_status.h"

#define  DEV_DEBUG	0

//////////////////////////////////////////////////////////////////////////////
//
//				input
//
//	Returns the specified number of bytes within the given time limit.
//  Will 'fill' more data from the source if it is not available in the
//  buffer.  The 'fill' call is specific to the derived DataSource class.
//  This routine updates the record counter and the storage size variables.
//  As an option, this routine can resize the buffer if a length larger
//  than the buffer size is requested.
//  Returns non-zero if requested legnth is greater than the buffer or the
//  return status from the _fill function.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSource::input (
  void		*Destination,
  int		*Length,
  int		TimeOut)
{ int RtnStatus = RTN_NORMAL;

  memset(Destination,0,*Length);

#if DEV_DEBUG
cerr << "DSrc: Requesting " << *Length << " bytes; " << _stored << "/" <<
     _capacity << "/" << _offset << " stored/capacity/offset\n";
#endif

  // Make sure buffer can hold all the data being requested
  if (*Length > _capacity)
  { if (*Length > MAXIMUM_DATA_SOURCE_BUFFER_SIZE)
    { sprintf(_errorMessage,"Requested larger buffer than allowed (%d vs %d)",
              *Length,MAXIMUM_DATA_SOURCE_BUFFER_SIZE);
      return RTN_INVLD_ARG;
    }

    if (*Length < DEFAULT_DATA_SOURCE_BUFFER_SIZE)
    { sprintf(_errorMessage,"Requested larger buffer size (%d)",
              DEFAULT_DATA_SOURCE_BUFFER_SIZE);
      RtnStatus = resize(DEFAULT_DATA_SOURCE_BUFFER_SIZE);
    } else
    { sprintf(_errorMessage,"Requested larger buffer size (%d)",*Length);
      RtnStatus = resize(*Length);
    }

    if (RTN_FAILURE(RtnStatus))
    { _errorReason = errno;
      return RTN_ALLOCATE_ERROR;
    }
  }

  // Check if all the requested data is in the buffer
  if (*Length > _stored)			// Need to fill buffer ?
  { _manageBuffer(*Length);
    RtnStatus = _fill( (*Length-_stored), TimeOut );

    if (RTN_FAILURE(RtnStatus) || *Length > _stored)
    { strcpy(_errorMessage,"Could not fill buffer as requested");
#if DEV_DEBUG
cerr << _errorMessage << "; " << RTN_DFLT_MSG(RtnStatus) << endl;
#endif
      if (*Length > _stored) *Length = _stored;
      if (_stored > 0)			// Whatever we got; return it
      { memmove(Destination,_buffer+_offset,_stored);
        _offset += _stored;
        _recordCount++;
        _stored = 0;
      }
      return RtnStatus;
    } else RtnStatus = RTN_NORMAL;	// As long as we got the data its ok
#if DEV_DEBUG
cerr << "DSrc: Filled buffer\n";
#endif
  }

  memmove(Destination,(_buffer+_offset),*Length);
  _offset += *Length;
  _stored -= *Length;
  _recordCount++;

#if DEV_DEBUG
cerr << "DSrc: Buffer still contains " << _stored << "/" << _capacity << "/" <<
     _offset << " stored/capacity/offset\n";
#endif

  return RtnStatus;
}

////////////////////////////////////////////////////////////////////////
//
//				_fill
//
//	Basically a stub that will fill the Data Source buffer from the
//  source.  The _fill routine supports the reload capability whenever
//  possible, this stub does not.
//
////////////////////////////////////////////////////////////////////////

int	DataSource::_fill(
  int	MinLength,
  int	TimeOut)
{
  if (TimeOut == 0) strcat((char *)_buffer,"Copied w/o timeout");
  else strcat((char *)_buffer,"Copied w/ timeout");

  _discardable = _offset = 0;
  _stored = strlen((char *)_buffer) + 1;

  return RTN_NORMAL;
}

////////////////////////////////////////////////////////////////////////
//
//				look
//
//	Copies without deleting the specified bytes from the buffer.
//  Will return a non-zero value if it could not return all the data
//  requested.  In these cases, the destination buffer is cleared
//  before the data is returned.
//
////////////////////////////////////////////////////////////////////////
int	DataSource::look(
  void	*Destination,
  int	*Length,
  int	TimeOut)
{ int	RtnStatus = RTN_NORMAL;

  memset(Destination,0,*Length);
  //  Verify that request is valid
  if (*Length > _capacity)
  { //  error condition ... requesting more than buffer size
    *Length = _capacity;	// So the caller knows what's available
    return RTN_INSUFF_MEMORY;
  }

  //  Get the requested data ... if available
  if (*Length < _stored)	// Got it all in the buffer, ship it out
     memmove(Destination,(_buffer+_offset),*Length);
  else				// Need to get some from the source
  { _manageBuffer(*Length);
    RtnStatus = _fill(((_capacity-_offset)-_stored), 0);

    if (*Length > _stored)
    { if (RTN_SUCCESS(RtnStatus))
         RtnStatus = _fill( (*Length-_stored), TimeOut );
      if (*Length > _stored) *Length = _stored;
    }
    if (*Length > 0) memmove(Destination,(_buffer+_offset),*Length);
  }

  return RtnStatus;
}

//////////////////////////////////////////////////////////////////////////////
//
//				_manageBuffer
//
//	Frees space in the source buffer as needed to meet transfer needs.
//  If possible, it will preserve data for the 'reload' option.  This routine
//  will not resize the buffer to accomidate space management nor does it
//  check.
//
//	Buffer management consists of maintaining offsets and lengths.
//  There is a "length" of data at the beginning of the buffer, _discardable,
//  that can be purged without affecting the operation of the application.
//  There is an offset, _offset, that identifies where the first unread
//  byte of data exists.  There is a final length, _stored, that defines the
//  amount of unread data in the buffer.
//
//  |------------------------- _capacity ---------------------------------|
//  |--- _discardable ---|
//  |------------ _offset ------------|
//                                    |----------- _stored ----------|
//  |-------- <retrievable data via 'reload' or 'input'> ------------|
//                       |------ <guaranteed to be retrievable> -----|
//                       |-- retain --|
//                           ... for reload whenever possible
//
//////////////////////////////////////////////////////////////////////////////

void	DataSource::_manageBuffer (
  int	Length )
{ int	retain;

  retain = _offset - _discardable;

#if DEV_DEBUG
cout << "DSrc: Need: " << Length << "; Buffer: " << _capacity << "/" <<
     _offset << "/" << _stored << "/" << _discardable << "/" << retain <<
     " (Capacity/Offset/Stored/Discard/Retain)\n";
#endif

  if (Length && Length > (_capacity-retain))	// Can not keep data for reload
  { memmove(_buffer,(_buffer+_offset),_stored);
    _discardable = _offset = 0;
  } else
  { memmove(_buffer,(_buffer+_discardable),_stored+(retain));
    _discardable = 0;
    _offset = retain;
  }

#if DEV_DEBUG
  retain = _offset - _discardable;
cout << "DSrc: Need: " << Length << "; Buffer: " << _capacity << "/" <<
     _offset << "/" << _stored << "/" << _discardable << "/" << retain <<
     " (Capacity/Offset/Stored/Discard/Retain)\n";
#endif

  return;
}

////////////////////////////////////////////////////////////////////////
//
//				reload
//
//	'Reloads' the specified number of bytes.  The reload is really
//  buffer pointer management.  Only the number of bytes transfered
//  from the buffer can be reloaded.  This is guaranteed to work for
//  the amount of data last transfered, but may fail for more.
//
////////////////////////////////////////////////////////////////////////
int	DataSource::reload(
  int	Length)
{
  if (Length > _offset) return (RTN_INSUFF_DATA);

  _offset -= Length;
  _stored += Length;
  if (_discardable > _offset) _discardable = _offset;

  return RTN_NORMAL;
}

////////////////////////////////////////////////////////////////////////
//
//				resize
//
//	Resizes the buffer and retains all of the data currently stored
//  in the buffer.  If the resize results in a buffer smaller than what
//  is being stored, the resize fails and the buffer is left untouched.
//  returns a non-zero value for any failure.
//
////////////////////////////////////////////////////////////////////////
int	DataSource::resize(
  int	NewSize)
{ unsigned char	*New_buffer;

  if (_buffer == NULL)
  { _buffer = new unsigned char [NewSize];
    if (_buffer == NULL)
    { strcpy(_errorMessage,"No buffer defined, and couldn't allocate one");
      _errorReason = errno;
      return RTN_ALLOCATE_ERROR;
    }
    memset(_buffer,0,NewSize);
    _capacity = NewSize;

    return RTN_NORMAL;
  }

  if (_stored > NewSize)
  { strcpy(_errorMessage,"Not enough space in new buffer");
    return RTN_INVLD_ARG_IGNORED;
  }

  New_buffer = new unsigned char[NewSize];
  if (New_buffer == NULL)
  { strcpy(_errorMessage,"Could not allocate new buffer");
    _errorReason = errno;
    return RTN_ALLOCATE_ERROR;
  }

  memset(New_buffer,0,NewSize);
  _manageBuffer(0);				// maintain 'reload' area
  memmove(New_buffer,_buffer,(_stored+_offset));
  delete[] _buffer;
  _buffer = New_buffer;
  _capacity = NewSize;

#if DEV_DEBUG
  cerr << "DSrc: Created buffer of " << _capacity << " bytes\n";
#endif

  return RTN_NORMAL;
}
////////////////////////////////////////////////////////////////////////
//
//				operator=
//
//	Not currently implemented ... Although some thoughts are
//  included.
//
////////////////////////////////////////////////////////////////////////

DataSource	&DataSource::operator= (
  const DataSource	&source)
{

  if (this == &source) return *this;	// in case assigning to itself
  throw "Can not assign one DataSource object to another ... yet";

  _recordCount = source._recordCount;
  _errorReason = 0;


  /***  Copy over buffer contents; enlarge buffer if needed  ***/

  return *this;
}
////////////////////////////////////////////////////////////////////////
//
//				DataSource
//
////////////////////////////////////////////////////////////////////////

DataSource::DataSource (
   const DataSource	&source)
{
  if (this == &source) return;		// in case assigning to itself
  throw "Can not assign one DataSource object to another ... yet";
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSourceDisk.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				DataSourceDisk.cc
//
//
//////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <errno.h>

#include "DataSourceDisk.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				open
//
//	Opens the disk file for the data source object.  
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceDisk::open(char *FileName)
{
  _recordCount = 0;	// In case DataSource is opened and closed

  _diskFile.open(FileName, ios::in);
  if (_diskFile.fail())
  { // error condition ... could not open diskfile
    _errorReason = errno;
    return RTN_OPEN_ERROR;
  }

  _active = 1;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				fill
//
//	Transfers the data from the buffer controlled by the base-class to 
//  the physical disk file.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceDisk::_fill(
  int	MinLength,
  int	TimeOut)
{ int	Available,
	ReadLength,
	RtnStatus = RTN_NORMAL;

  if ( _diskFile.eof() ) return RTN_EOD;

//  if (TimeOut) then RtnStatus = RTN_TIMEOUT;	// STUB

  Available = _capacity - (_offset + _stored);
  if (MinLength > Available)  // Is there room in the buffer
     return (RTN_INSUFF_MEMORY);

  ReadLength = Available;
  _diskFile.read((char *)(_buffer+_stored+_offset), ReadLength);
  ReadLength = _diskFile.gcount();
  _stored += ReadLength;

  //  Check for errors
  if (_diskFile.fail())
  { strcpy(_errorMessage,"Error on disk file read");
    _errorReason = errno;
    if (ReadLength >= MinLength) RtnStatus = RTN_NORMAL;
    else RtnStatus = RTN_IO_ERROR;
  }

  return RtnStatus;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSourceSocket.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				DataSourceSocket.cc
//
//
//////////////////////////////////////////////////////////////////////////////

#include <string.h>

#include "DataSourceSocket.h"
#include "return_status.h"

#define  DEV_DEBUG      0

//////////////////////////////////////////////////////////////////////////////
//
//				establish
//
//	Create a new socket allowing others to connect.  Does not return
//  until the other end of the socket has been connected.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceSocket::establish(
  int	PortNumber)
{ int	status;

  if (_source.connected())
  { strcpy(_errorMessage,"Connection already exists");
    return RTN_EXISTS_ERROR;
  }

  status = _source.create( PortNumber );
  if (RTN_FAILURE(status))
  { strcpy(_errorMessage,"Could not create socket");
    _errorReason = _source.errorReason();
    return status;
  }

  _active = 1;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				connect
//
//	Connect to an existing socket, created by another entity.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceSocket::connect(
  char	*HostName,
  int	PortNumber)
{ int status; 

  if (_source.connected())
  { strcpy(_errorMessage,"Connection already exists");
    return RTN_EXISTS_ERROR;
  }

  status = _source.open(HostName, PortNumber);
  if (RTN_FAILURE(status))
  { strcpy(_errorMessage,"Could not connect to socket");
    _errorReason = _source.errorReason();
    return status;
  }

  _active = 1;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				fill
//
//	Transfers the data from the buffer controlled by the base-class to
//  the socket.  Timeouts are controlled by the SocketBase object.
//
//	In order to support the 'reload' method, the _offset may be a
//  non-zero value, meaning there is some retained data at the beginning
//  of the buffer
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceSocket::_fill(
  int	MinLength,
  int	TimeOut)
{ int	Available,
	ReadLength,
	RtnStatus;

  if (!_source.connected())
  { strcpy(_errorMessage,"Not connection to socket");
    return RTN_ALLOCATE_ERROR;
  }

  Available = _capacity - (_offset + _stored);
  if (MinLength > Available)	// Is there room in the buffer
     return (RTN_INSUFF_MEMORY);

  /***
   ***  Fill as much of the buffer from the socket as possible
   **/
  ReadLength = Available;
  RtnStatus = _source.read((_buffer+_offset+_stored), &ReadLength, 0);
  _stored += ReadLength;
  if (!RTN_SUCCESS(RtnStatus))
  { _errorReason = _source.errorReason();
    strcpy(_errorMessage,"Untimed socket read not successful");
#if DEV_DEBUG
cerr << _errorMessage << "; " << strerror(_errorReason) << endl;
#endif

    if (RTN_EOD == RtnStatus || RTN_FAILURE(RtnStatus)) return RtnStatus;
  }

  /***
   *** If not enough data obtained, wait until TIME-out for what was requested
   **/
  if (ReadLength < MinLength)		// Quick-fill did not get minimum
  { MinLength -= ReadLength;		// Account for part of the minimum
    ReadLength = MinLength;
    RtnStatus = _source.read((_buffer+_stored+_offset), &ReadLength, TimeOut);
    _stored += ReadLength;
    if (!RTN_SUCCESS(RtnStatus))
    { _errorReason = _source.errorReason();
      strcpy(_errorMessage,"Timed socket read not successful");
#if DEV_DEBUG
cerr << _errorMessage << "; " << strerror(_errorReason) << endl;
#endif

      if (RTN_EOD == RtnStatus || RTN_FAILURE(RtnStatus)) return RtnStatus;
    }
  }

  if (ReadLength >= MinLength) RtnStatus = RTN_NORMAL;

  return RtnStatus;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSink.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSink.cc
//
//////////////////////////////////////////////////////////////////////////////

#include "DataSink.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				output
//
//	Transfers the data from memory to the data sink.  Memory is supplied
//  by caller.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSink::output (
  void		*Destination,
  int		*Length,
  int		TimeOut)
{ int	RtnStatus = RTN_NORMAL;

  _buffer = (unsigned char *)Destination;
  _dataLength = Length;

  RtnStatus = _drain(TimeOut);

  if (RTN_SUCCESS(RtnStatus))
  { _recordCount++;
    if (_critical) _flush();
  }

  return ( RtnStatus );
}

////////////////////////////////////////////////////////////////////////
//
//				operator=
//
//	Until it becomes a reality, this is just a stub
//
////////////////////////////////////////////////////////////////////////

DataSink	&DataSink::operator= (
  const DataSink	&sink)
{
  if (this == &sink) return *this;	// in case assigning to itself
  throw "Can not assign one DataSink object to another";

  return *this;
}

////////////////////////////////////////////////////////////////////////
//                              
//				DataSink
//
//	Until it becomes a reality, this is just a stub
//
////////////////////////////////////////////////////////////////////////

DataSink::DataSink (
  const DataSink	&sink)
{
  if (this == &sink) return;		// in case assigning to itself
  throw "Can not copy one DataSink object to another";
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSinkDisk.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSinkDisk.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <strings.h>
#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <errno.h>

#include "DataSinkDisk.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				drain
//
//	Transfers the data from the buffer controlled by the base-class to
//  the physical disk file
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkDisk::_drain (
  int		TimeOut)
{
  _diskFile.write((char *)_buffer, *_dataLength);

  if (_diskFile.fail())
  {  strcpy(_errorMessage,"I/O failure");
    _errorReason = errno;
    //  Update DataLength value

    return ( RTN_IO_ERROR );
  }

  *_dataLength = 0;

  return ( RTN_NORMAL );
}

//////////////////////////////////////////////////////////////////////////////
//
//				open
//
//	Opens the disk file for the data sink object.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkDisk::open(
  char	*FileName)
{
  _recordCount = 0;	// In case DataSource is opened and closed

  //ios::noreplace is no longer supported under gcc 3.2.3 20030502 for RedHat
  // 3.2.3-24. Alternate is to open using ios::out, get beg position then
  //  seekp to end and see if beg and end are same or not.

  _diskFile.open(FileName, ios::out | ios::app);

  if (_diskFile.fail())
  { sprintf(_errorMessage,"Could not open diskfile: %s",FileName);
    _errorReason = errno;
    return RTN_OPEN_ERROR;
  }

  ios::pos_type endMark = _diskFile.tellp();
  _diskFile.seekp(ios::beg);
  ios::pos_type begMark = _diskFile.tellp();

  if (endMark!=begMark)
  {
    sprintf(_errorMessage,"Diskfile %s exists.",FileName);
    _diskFile.close();
    return RTN_OPEN_ERROR;
  }
  return RTN_NORMAL;
}

DataSinkDisk::DataSinkDisk ( char *FileName, int CriticalFlag)
{ _initialize(); _critical = CriticalFlag;
 this->open(FileName);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSinkSocket.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				DataSinkSocket.cc
//
//
//////////////////////////////////////////////////////////////////////////////

#include <string.h>

#include "DataSinkSocket.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				establish
//
//	Create a new socket allowing others to connect.  Does not return
//  until the other end of the socket has been connected.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkSocket::establish(
  int	PortNumber)
{ int	status;

  if (_sink.connected())
  { strcpy(_errorMessage,"Connection already exists");
    return RTN_EXISTS_ERROR;
  }

  status = _sink.create( PortNumber );
  if (RTN_FAILURE(status))
  { strcpy(_errorMessage,"Could not create socket");
    _errorReason = _sink.errorReason();
    return status;
  }

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				connect
//
//	Connect to an existing socket, created by another entity.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkSocket::connect(
  char	*HostName,
  int	PortNumber)
{ int status; 

  if (_sink.connected())
  { strcpy(_errorMessage,"Connection already exists");
    return RTN_EXISTS_ERROR;
  }

  status = _sink.open(HostName, PortNumber);
  if (RTN_FAILURE(status))
  { strcpy(_errorMessage,"Could not connect to socket");
    _errorReason = _sink.errorReason();
    return status;
  }

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				drain
//
//	Pushes data down the socket.  Timeouts are enforced by the
// SocketBase object.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkSocket::_drain(
  int	TimeOut)
{ int	RtnStatus;

  if (!_sink.connected())
  { strcpy(_errorMessage,"Not connection to socket");
    return RTN_ALLOCATE_ERROR;
  }

  // Drain the buffer to the socket
  RtnStatus = _sink.write(_buffer, _dataLength, TimeOut);
  if (RTN_FAILURE(RtnStatus))
  { strcpy(_errorMessage,"Could not write from socket");
    _errorReason = _sink.errorReason();

    // "Soft" errors can/should they be recovered ?
    if (RtnStatus == RTN_TIME_OUT)
    { strcpy(_errorMessage,"Time-out on socket write");
      // Drain the buffer again
    }
  }

  return RtnStatus;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Stopwatch.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 2000, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Stopwatch
//
//
//////////////////////////////////////////////////////////////////////////////

#include <errno.h>

#include "Stopwatch.h"

#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				start
//
//	Starts the timer.  It is possible to 'start' and active timer.
//
//////////////////////////////////////////////////////////////////////////////
int	Stopwatch::start ( void )
{
   _endTime = -1;
  if (time(&_baseTime) == -1)
  { strcpy(_errorMessage,"Could not get base time");
    _errorReason = errno;
    _baseTime = -1;				// Make sure it is still -1
    return (RTN_ERROR);
  }
  return (RTN_NORMAL);
}

//////////////////////////////////////////////////////////////////////////////
//
//				stop
//
//	Stops the timer, basically gets the current time as the end time.
//  It is possible to 'stop' a timer multiple times and get increasing
//  times.  In this case the stop acts more like a 'split' time.
//
//////////////////////////////////////////////////////////////////////////////
int     Stopwatch::stop ( void )
{ if (time(&_endTime) == -1)
  { strcpy(_errorMessage,"Could not get end time");
    _errorReason = errno;
    return (RTN_ERROR);
  }
  return (RTN_NORMAL);
}

//////////////////////////////////////////////////////////////////////////////
//
//				elapsed
//
//	Seconds elapsed since start of timer and it's stop point, if set, or
//  the current time.
//
//////////////////////////////////////////////////////////////////////////////
int     Stopwatch::elapsed ( void )
{ int   Seconds;
  time_t        EndTime;

  if (_endTime != -1) EndTime = _endTime;
  else if (_baseTime == -1)
  { strcpy(_errorMessage,"Could not get offset time; base time not set");
    return (-1);
  } else if (time(&EndTime) == -1)
  { strcpy(_errorMessage,"Could not get offset time");
    _errorReason = errno;
    return (-1);
  }

  Seconds = (int)difftime(EndTime,_baseTime);

  return Seconds;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create data_transfer.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE data_transfer
/*
/*   To Create the build file give the command:
/*
/*		$ vimake data_transfer			(VMS)
/*   or
/*		% vimake data_transfer			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE data_transfer		/* Only one of these */
/*#define PROCEDURE data_transfer		/* Only one of these */
/*#define SCRIPT data_transfer		/* Only one of these */
/*#define PROGRAM data_transfer		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST SocketBase.cc \
	DataSource.cc DataSourceDisk.cc DataSourceSocket.cc \
	DataSink.cc DataSinkDisk.cc DataSinkSocket.cc Stopwatch.cc

#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MPF_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#endif
#define LIB_LOCAL

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of data_transfer imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_data_disk.cc
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <fstream.h>
#include <string.h>
#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()

#include "return_status.h"
#include "DataSourceDisk.h"
#include "DataSinkDisk.h"

main(
  int	argc,
  char	*argv[])
{ char	FileName[256],
	MyBuf[256];
  int	XferLength = 128,
	Status;
  DataSourceDisk	Test;
  DataSinkDisk		Out(1);

  if (argc < 2)
  { cout << "Must supply an input filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[1]);

  if (Test.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl;

  if (Test.resize(512))
  { cout << "Could not allocate buffer" << endl;
    exit(1);
  }

  if (argc < 3)
  { cout << "Must supply an output filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[2]);

  if (Out.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl;


  memset(MyBuf,0,256);
  cout << "Starting copy loop\n";
  while (!(Status = Test.input(MyBuf,&XferLength)) || XferLength>0)
  { Out.output(MyBuf,&XferLength);
    memset(MyBuf,0,256);
    XferLength = 128;
  }


  cout << "Finished w/ " << Test.recordCount() << " blocks" << endl;


  return(1);
}
$!-----------------------------------------------------------------------------
$ create tst_data_disk.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_data_disk
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_data_disk			(VMS)
/*   or
/*		% vimake tst_data_disk			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define PROGRAM tst_data_disk		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_data_disk.cc


#define USES_C_PLUS_PLUS

/***  Specify  Program or Subroutine specific DEFINES  ***/
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE


/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_data_disk imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_data_socket.cc
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdio.h>
#include <fstream.h>
#include <string.h>
#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()
#include <unistd.h>	// Cause of main()
#include <errno.h>

#include "SocketBase.h"
#include "return_status.h"

int	PortMaker(
  SocketBase	&OutPort,
  char	*HostName,
  int	PortNumber)
{ int	status;
  SocketBase	LocalPort("lcl");

cerr << "Trying port: " << PortNumber << endl << flush;

  if (PortNumber < 0)
  { PortNumber *= (-1);
    status = LocalPort.open(HostName,PortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Open Failed " << LocalPort.errorReason() << " " <<
              strerror(LocalPort.errorReason()) << "; " <<
              LocalPort.errorMessage() << endl;
      return (status);
    }
    // cout << "Open Succeded\n" << flush;
  } else
  { status = LocalPort.create(PortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Create Failed: " << strerror(LocalPort.errorReason()) << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    }
    cout << "Create Suceeded\n" << flush;

    status = LocalPort.attach();
    if (RTN_FAILURE(status))
    { cerr << "Attach Failed " << LocalPort.errorReason() << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    }
    // cout << "Attach Succeded\n" << flush;
  }

  OutPort = LocalPort;

  return RTN_NORMAL;
}

main(
  int	argc,
  char	*argv[])
{ char	HostName[256],
	MyBuf[256];
  int	BufLth,
	status,
	InPortNumber = 0,
	OutPortNumber = 0;
  SocketBase	Input("inp"),
		Output("out");
int temp,loopCount;
  if (argc < 2)
  { cout << "Please supply a port number\n [Positive - sink (first); " <<
            "Negative - source (second)]: ";
    cin >> InPortNumber;
  } else InPortNumber = atoi(argv[1]);
  if (InPortNumber < 0) OutPortNumber = InPortNumber * (-1);

  cout << endl << "Please note, this program generates artifical delays\n";
  cout << "as part of its normal execution\n\n";

  //
  //  Socket Source code
  //

  if (OutPortNumber)
  { if (argc < 3)
    { cout << "Please supply a destination host name: ";
      cin >> HostName;
    } else strcpy(HostName,argv[2]);
//    status = Output.open(HostName,OutPortNumber);
status = PortMaker(Output,HostName,((-1)*OutPortNumber));
    if (RTN_FAILURE(status))
    { cerr << "Open Failed " << Output.errorReason() << endl;
      return (status);
    }
    cout << "Open Suceeded\n" << flush;

    if (argc > 3) strcpy(MyBuf,argv[3]);
    else
    { cout << "Enter message for transmission .... \n" << flush;
      gets(MyBuf);
      // cin >> MyBuf;
    }
    BufLth = sizeof(MyBuf);
    cout << "Sending (" << (strlen(MyBuf)) << ") \"" << MyBuf <<
         "\"\n" << flush;
//    status = Output.write(MyBuf,&BufLth,0);
temp = 4;
for (loopCount=0;loopCount<BufLth;loopCount+=4)
{ status = Output.write(&MyBuf[loopCount],&temp,0);
cout << "Writing " << loopCount << " for " << temp << endl;
    if (RTN_FAILURE(status))
       cout << "Write Failed " << Output.errorReason() << endl;
  sleep(1);
}
/****
temp = BufLth - 20;
status = Output.write(MyBuf,&temp,0);
sleep(10);
temp = 20;
status = Output.write(MyBuf,&temp,0);
***/
    if (RTN_FAILURE(status))
       cout << "Write Failed " << Output.errorReason() << endl;
    else
    { BufLth = sizeof(MyBuf);
      memset(MyBuf,0,BufLth);
      status = Output.read(MyBuf,&BufLth,-1);
      if (RTN_SUCCESS(status) || status == RTN_MISSING_DATA)
         cout << "Received (" << strlen(MyBuf) << ") \"" << MyBuf << "\"\n";
      else cout << "Reply Failed " << Input.errorReason() << endl;
      cout << flush;
    }

    cout << "Finished OPEN" << endl;
  } else

  //
  //  Socket Sink code
  //

/******
  { status = Input.create(InPortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Create Failed: " << strerror(Input.errorReason()) << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    }
    cout << "Create Suceeded\n" << flush;

    status = Input.attach();
******/
{
status = PortMaker(Input,NULL,InPortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Attach Failed " << Input.errorReason() << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    } else
    { cout << "Attach Succeded\n" << flush;
      BufLth = sizeof(MyBuf);
status = Input.read(MyBuf,&BufLth,660);
//      status = Input.read(MyBuf,&BufLth,-1);
      if (RTN_SUCCESS(status) || status == RTN_MISSING_DATA)
      { BufLth = sizeof(MyBuf);
        cout << "Received (" << strlen(MyBuf) << ") \"" << MyBuf << "\"\n";
        strcpy(MyBuf,"Got it ... Thanks");
        cout << "Sending (" << strlen(MyBuf) << ") \"" << MyBuf << "\"\n" <<
             flush;
        status = Input.write(MyBuf,&BufLth,0);
        if (RTN_FAILURE(status))
           cout << "Reply Failed " << Input.errorReason() << endl << flush;
      } else cout << "Read Failed " << Input.errorReason() << endl << flush;

      cout << flush;
    }
    cout << "Finished CREATE" << endl;
  }

//  cout << "Finished" << endl;

  return(1);
}
$!-----------------------------------------------------------------------------
$ create tst_data_socket.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_data_socket
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_data_socket			(VMS)
/*   or
/*		% vimake tst_data_socket			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define PROGRAM tst_data_socket		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_data_socket.cc


#define USES_C_PLUS_PLUS

/***  Specify  Program or Subroutine specific DEFINES  ***/
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE


/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_data_socket imake file  **********/
$!-----------------------------------------------------------------------------
$ create README_tst
There are 2 test programs included for this module:
	tst_data_disk
    &	tst_data_socket

'tst_data_disk' is basically an disk file copy program.  It will copy any disk
file and print how many 128-byte reads it took to copy the file.  This test
verifies the Source and Sink capabilities of the base 'Data' class as well as
the 'Disk' subclasses.

'tst_data_disk' can be though of as the old string and cup telephone.  This
program must be executed two times simultaneously.  In one instance, the
program acts as a server, creating an attachable socket.  The second instance
will be the client and attaches to the server socket.  The server version
is started by passing a positive port number as the only parameter.  This
port number should be larger than 2048 so it does not conflict with system
port numbers.  It is still possible that the specified port number is being
used, so verify that the server version is ready to accept the client before
proceeding.  The client version is started by passing a negative port number,
the same value (but negative) as was specified for the server, and a host
name where the server is running.

Once the 'plumbing' is connected, the client instance will prompt for a
message to send to the server.  After it is entered, the client sends it to the
server, which ackowledges the message, afterwich both instances quit.
$ Return
$!#############################################################################
