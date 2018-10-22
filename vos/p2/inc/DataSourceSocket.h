#ifndef DATA_SOURCE_SOCKET_H
#define DATA_SOURCE_SOCKET_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSourceSocket.h
//
//      DataSourceSocket is a dervied from the DataSource class using a socket
//  object as the data source.  Timeouts are supported on transfers.
//
//////////////////////////////////////////////////////////////////////////////


#include <fstream>

#include "DataSource.h"
#include "SocketBase.h"
#include "return_status.h"

class DataSourceSocket : public DataSource {

  public:
	void	close ( void ) { _source.close_it(); _active = 0; return; }
		// Terminates socket connection

	int	use( SocketBase &Socket )
		// Uses an existing socket as the data source.
		// This is the way to have a full-duplex socket with
		// the DataSource/Sink objects.  Define and connect a
		// socket, then have both object 'use' that one socket.
		{ if (_source.connected()) return RTN_EXISTS_ERROR;
		  _source = Socket;
		  return RTN_NORMAL;
		}

	int	connect ( char *Host = 0, int PortNumber = 0 );
		// Connect to an established data port on the specified host,
		// or current host if not specified.

	int	establish ( int PortNumber = 0 );
		// Create a data port for connection to by others

	// Default Constructor
	DataSourceSocket ( int MaxBufSize = 0 )
	{ _initialize();
	  if (MaxBufSize > 0) resize( MaxBufSize );
	}

	// Destructor
	~DataSourceSocket ( void )
	{ close();
	}


  private:
	SocketBase	_source;
	virtual	int	_fill ( int MinLength, int TimeOut );

};


#endif
