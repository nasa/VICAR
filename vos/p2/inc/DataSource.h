#ifndef DATA_SOURCE_H
#define DATA_SOURCE_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSource.h
//
//	DataSource is an abstract base class of an object that can retrieve
//  data.  It performs internal buffering in addition to any system buffering
//  mechanisms.  This class does not format or recognize any structure to
//  the data being retrieved.  It treats all the data as binary adding no
//  conversion or processing what-so-ever.  This base class in and of itself
//  is useless.  It does, however, have the necessary lower-level functions
//  that are required by any derived class.
//
//	The basic API or this class besides the constructor/destructor is the
//  "data" function.  This function is the mechanism for retrieving data from
//  the data source.  The parameters to this function are a pointer to the
//  memory to place the data, the amount of data in bytes, and an optional
//  time limit in seconds.  Specifiing a time limit of a negative value (the
//  default) instructs the the routine to wait until the data transfer is
//  complete, regardless of the time required.  A non-negative value indicates
//  the minimum amount of time to wait until terminating the transfer with an
//  error.
//
//	An additional routine, "look", allows for a quick check of the data
//  in the buffer, without deleting it from the buffer.  The routine takes
//  the same parameters as "data", although the default time limit is '0',
//  which infers that only data available in the buffer will be returned.
//
//////////////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>
#include <limits.h>

#define  DEFAULT_DATA_SOURCE_BUFFER_SIZE	4096
#define  MAXIMUM_DATA_SOURCE_BUFFER_SIZE	16384

class DataSource {

  public:
	int	input ( void *Destination, int *Length, int TimeOut=-1 );
	int	data ( void *Destination, int *Length, int TimeOut=-1 )
		{ return (input(Destination,Length,TimeOut)); }
		// Transfer routine that obtains the requested number
		// of bytes within the defined timeout.

	int	look ( void *Destination, int *Length, int TimeOut=0 );
		// Transfers without removing from the source's
		// buffer, the requested number of bytes (basically
		// the 'peek' routine).

	int	reload ( int length );
		// Reloads the number of bytes specified for the next
		// transfer.  This is only buffer pointer management and
		// could fail if requesting more bytes than were returned in
		// the previous transfer.  Source objects assigned to another
		// object will not have the ability to reload until a transfer
		// is performed.

	void	discard ( void ) { _discardable = _offset; return; }
		// Identifies all data extracted from the buffer as purgable.
		// This instructs the data source that the application will
		// no longer need to 'reload' any of that data.  The data
		// source object will reclaim the buffer space as needed.
		// Use of this method allows better data management while
		// retaining the 'reload' capability.

	int	recordCount ( void ) { return (_recordCount); }
		// Returns the number of 'data' calls successfully
		// completed

	int	errorReason ( void ) { return (_errorReason); }
		// Returns the errno value for I/O errors

	int	resize ( int MaxBufSize );
		// Resizes the data transfer buffer, retaining any
		// untransfered data.  The object will resize larger
		// automatically if necessary, this call is available
		// to preset the buffer so avoid any overhead of
		// incrementally resizing the buffer.

	int	capacity ( void ) { return (_capacity); }
		// Returns the size of the current buffer

	int	active ( void ) { return (_active); }
		// Identifies the source as configured to supply data

	const char	*errorMessage (void ) { return (_errorMessage); }
		// Returns a text message describing the last failure


	// Default Constructor
        DataSource ( int MaxBufSize = 0 )
        { _initialize();
	  if (MaxBufSize > 0) resize( MaxBufSize );
        }

	// Copy Constructor
	DataSource ( const DataSource & source );

	// Assignment Constructor
	DataSource &operator=(const DataSource &source);

	// Destructor
        virtual ~DataSource ( void )
        { if (_buffer != NULL) delete[] _buffer;
          _buffer = NULL;
        }

  protected:
	int	_active;	// Source is ready (configured) to supply data
	int	_capacity;	// Capacity of buffer in bytes
	int	_offset;	// Byte offset to un'input'ed bytes in buffer
	int	_discardable;	// Bytes at start of buffer that aren't needed
	int	_stored;	// Un'input'ed bytes in the buffer
	int	_recordCount;	// Number of 'data' calls
	int	_errorReason;	// System error for failure
	char	_errorMessage[256];
	unsigned char	*_buffer; // Pointer to transfer buffer
	virtual	int	_fill ( int MinLength, int TimeOut ) = 0;
        void		_initialize ( void )
			{ _recordCount = _errorReason = _capacity = _stored = 0;
			  _discardable = _offset = _active = 0;
			  memset(_errorMessage,0,sizeof(_errorMessage));
			  _buffer = NULL;
			  return;
			}
	void		_manageBuffer ( int Length = 0 );

};

#endif
