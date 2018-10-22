#ifndef DATA_SINK_SOCKET_H
#define DATA_SINK_SOCKET_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSinkSocket.h
//
//	DataSinkSocket is a dervied from the DataSink class using a socket
//  object as the data sink.  Timeouts are supported on transfers.
//
//////////////////////////////////////////////////////////////////////////////


#include <fstream>

#include "DataSink.h"
#include "SocketBase.h"
#include "return_status.h"

class DataSinkSocket : public DataSink {

  public:
	void	close ( void ) { _sink.close_it(); return; }
		// Close or terminate the current socket connection

	int	use( SocketBase &Socket )
		// Share an existing socket connection
		{ if (_sink.connected()) return RTN_EXISTS_ERROR;
		  _sink = Socket;
		  return RTN_NORMAL;
		}

	int	connect ( char *Host = 0, int PortNumber = 0 );
		// Connect to an establised socket

	int	establish ( int PortNumber = 0 );
		// Create and allow others to connect to a new socket

	// General Constructor
	DataSinkSocket ( int CriticalFlag = 0)
	{ _initialize(); _critical = CriticalFlag;
	}

	// Destructor
	~DataSinkSocket ( void )
	{ close();
	}

  private:
	SocketBase	_sink;
	int		_drain ( int TimeOut );
	void		_flush ( void ) { return; }
};


#endif
