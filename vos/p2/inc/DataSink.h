#ifndef DATA_SINK_H
#define DATA_SINK_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSink.h
//
//	DataSink is an abstarct  base class of an object that can "store"
//  data.  It does not do any internal buffering, nor does it circumvent the
//  system buffering mechanisms.  This class does not format or recognize any
//  structure to the data being "sunk".  It treats all the data as binary
//  adding no conversion or processing what-so-ever.  This base class in and
//  of itself is useless.  It does, however, have the necessary lower-level
//  functions that are required by any derived class.
//
//	The basic API or this class besides the constructor/destructor is the
//  "data" function.  This function is the mechanism for sending data to the
//  data sink.  The parameters to this function are a pointer to the data
//  being stored, the amount of data in bytes, and an optional time limit
//  in seconds.  Specifiing a time limit of a negative value (the default)
//  instructs the the routine to wait until the data transfer is complete,
//  regardless of the time required.  A non-negative value indicates the
//  minimum amount of time to wait until terminating the transfer with an
//  error.
//
//  	The two other routines available are the "recordCount" and
//  "errorReason" routines which are basic utility routines to help track
//  transfer progress or failures.  The recordCount routine tracks the number
//  of times the data routine has successfully executed, the errorReason
//  stores the error code that caused any routine of this class or a derived
//  class to fail.  The error code can be redundant to the return code for
//  locally generated errors, such as a time out.
//
//////////////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>
#include <limits.h>

class DataSink {

  public:
	int	output ( void *Destination, int *Length, int TimeOut=-1 );
	int	data ( void *Destination, int *Length, int TimeOut=-1 )
		{ return (output(Destination,Length,TimeOut)); }
		// Transfer funtion sending data to the 'sink'

	int	recordCount ( void ) { return (_recordCount); }
		// Returns the number of records (calls) processed

	int	errorReason ( void ) { return (_errorReason); }
		// Identifies reason of last failure

	const char	*errorMessage (void ) { return (_errorMessage); }
		// Returns a text message describing the last failure

	// General Constructor
	DataSink ( int CriticalFlag = 0 )
	{ _initialize(); _critical = CriticalFlag;
	}

	// Copy Constructor - not implemented, just a stub
	DataSink (const DataSink &sink );

	// Assignment Constructor - not implemented, just a stub
	DataSink &operator= (const DataSink &sink);

	// Destructor
	virtual ~DataSink ( void ) { }

  protected:
	int	_critical;	// Critical data is not buffered
	int	_recordCount;	// Count of 'data' calls
	int	_errorReason;	// System error for failure
	int	*_dataLength;	// Pointer to transfer(ed) length
	char	_errorMessage[256];
	unsigned char	*_buffer;	// Pointer to transfer buffer
	virtual	int	_drain( int TimeOut ) = 0;
	virtual	void	_flush( void ) = 0;
	void		_initialize( void )
			{ _critical = _recordCount = _errorReason = 0;
			  memset(_errorMessage,0,sizeof(_errorMessage));
			  _buffer = NULL;
			  return;
			}

};


#endif
