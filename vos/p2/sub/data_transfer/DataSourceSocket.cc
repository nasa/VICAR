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
