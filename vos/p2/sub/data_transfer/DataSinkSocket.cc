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
