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
