#ifndef SOCKET_BASE_H
#define SOCKET_BASE_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				SocketBase.h
//
//	SocketBase is a class of object that controls access to a socket.
//  The interface is similar to a high-level disk file interface and hides
//  the lower-level plumbing that occurs when the socket is used.
//
//	Additional capbility should include named port connections, but that
//  is left for future work (job security?)
//
//////////////////////////////////////////////////////////////////////////////

#include <string.h>

class SocketBase {

  public:
	int	attach( void );
		// Allow others to attach to this objects defined port

	int	open( char *HostName, int PortNumber );
		// Connect to an existing port on the designated host
		//  Will be over-loaded to attach to a registered service

	int	create( int PortNumber );
		// Create/define a new port
		//  Will be over-loaded to register a registered service

	void	close_it( void );
		// Closes connection with port

	int	connected( void ) { return ( _socketNumber != -1 ); }
		// Returns whether the object is connect to an active port

	int	errorReason( void ) { return _sysReason; }
		// Returns system reason for a failure

	const char	*errorMessage( void ) { return (_errorMessage); }
		// Returns a text message describing the last failure

	int	read( void *Buffer, int *BufLength, int TimeOut = 0);
		// Reads specified number of bytes to port within time-limit
		// The function can return an error while returning data

	int	write( void *Buffer, int *BufLength, int TimeOut = 0);
		// Writes specified number of bytes to port within time-limit

	int	useCount( void )
		{ if (_useCount) return (*_useCount); return (0); }
		// Returns the number of objects connected to one port

	const char	*name( void ) { return (const char *)_name; }
		// Returns the name of this object (for debugging purposes)

	// Default Constructor
	SocketBase( char *name=(char *)"dflt" )
	{ _socketNumber = _definedSocket = -1; _sysReason = 0;
	  memset(_errorMessage,0,sizeof(_errorMessage));

	  _useCount = new int;
	  if (_useCount != NULL) *_useCount = 1;

	  if (strlen(name) > sizeof(_name))
	  { memcpy(_name,name,sizeof(_name));
	    _name[sizeof(_name)-1] = 0;
	  } else strcpy(_name,name);
	}

	// Assignment Constructor
	SocketBase &operator= ( SocketBase &Socket )
	{ if (this == &Socket) return *this;
          if (connected()) close_it();		// Close an exisiting socket


	  _socketNumber = Socket._socketNumber;
	  _definedSocket = Socket._definedSocket;
	  _sysReason = Socket._sysReason;

	  if (_useCount) delete _useCount;	// Connect to the useCount and
	  _useCount = Socket._useCount;		// increment it so that the
	  if (_useCount) (*_useCount)++;	// port can not be closed while
						// an object is using it

	  return *this;
	}

	// Destructor
	virtual ~SocketBase( void )
	{ close_it();
	}

  protected:
	int		_socketNumber;	//  Socket number for data X-fer
	int		_definedSocket;	//  Socket created and registered
	int		_sysReason;	//  System reason for error condition
	char		_name[32];	//  Used to identify object (debugging)
	char		_errorMessage[256];
	int		*_useCount;	//  Counts number of objects using the
					//  same resource (used mainly by
					//  assignments and destructors)

	void	_close_it( int *SocketIdentifier );
		// Hold over from prevous supportted capability.  Check comment
		// in code for possible usage
			

};


#endif
