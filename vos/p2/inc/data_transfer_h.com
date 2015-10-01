$!****************************************************************************
$!
$! Build proc for MIPL module data_transfer_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:50
$!
$! Execute by entering:		$ @data_transfer_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module data_transfer_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to data_transfer_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("data_transfer_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @data_transfer_h.bld "STD"
$   else
$      @data_transfer_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create data_transfer_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack data_transfer_h.com -mixed -
	-s SocketBase.h DataSource.h DataSourceDisk.h DataSourceSocket.h -
	   DataSink.h DataSinkDisk.h DataSinkSocket.h Stopwatch.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SocketBase.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSource.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSourceDisk.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef DATA_SOURCE_DISK_H
#define DATA_SOURCE_DISK_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSourceDisk.h
//
//	DataSourceDisk is a dervied from the DataSource class using a disk
//  file as the data source.  The "TimeOut" option is currently not applicable
//  for this class, although it may be added in at a later time.
//
//////////////////////////////////////////////////////////////////////////////

#include <fstream>
#include <errno.h>
using namespace std;
#include "DataSource.h"

class DataSourceDisk : public DataSource {

  public:
	int	open ( char *FileName );
		// Opens the disk file uses as the data source

	void	close ( void ) { _diskFile.close(); _active = 0; return; }
		// Closes the data source disk file

	// General Constructor
	DataSourceDisk ( int MaxBufSize = 0 )
	{ _initialize();
	  if (MaxBufSize > 0) resize( MaxBufSize );
	}

	// General Constructor
	DataSourceDisk ( char *FileName, int MaxBufSize = 0)
	{ _initialize();
	  _diskFile.open( FileName, ios::in); 
          if (_diskFile.fail())
             _errorReason = errno;
          else
          { if (MaxBufSize > 0) resize( MaxBufSize );
	    _active = 1;
          }
	}

	// Destructor
	~DataSourceDisk ( void )
	{ _diskFile.close();
	}

  private:
	fstream		_diskFile;
	virtual	int	_fill ( int MinLength, int TimeOut );

};


#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSourceSocket.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSink.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSinkDisk.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef DATA_SINK_DISK_H
#define DATA_SINK_DISK_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSinkDisk.h
//
//	DataSinkDisk is a dervied from the DataSink class using a disk file
//  as the data store.  The "TimeOut" option is currently not applicable for
//  this class, although it may be added in at a later time.
//////////////////////////////////////////////////////////////////////////////

#include <fstream>
#include <stddef.h>
#include <limits.h>
using namespace std;
#include "DataSink.h"

class DataSinkDisk : public DataSink {

  public:
	int	open ( char *FileName );
		// Open a disk file to be the data sink

	void	close ( void ) { _diskFile.close(); }
		// Close the disk file data sink

	// General Constructor
	DataSinkDisk ( int CriticalFlag = 0 )
	{ _initialize(); _critical = CriticalFlag; }

	// General Constructor
	DataSinkDisk ( char *FileName, int CriticalFlag = 0 );
	// Destructor
	~DataSinkDisk ( void )
	{ _diskFile.close();
	}

  private:
	int	_drain( int TimeOut );
	void	_flush ( void ) { _diskFile.flush(); }
	fstream	_diskFile;	// Disk file of sink

};


#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataSinkSocket.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Stopwatch.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef STOP_WATCH_H
#define STOP_WATCH_H

//	Copyright (c) 2000, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				Stopwatch.h
//
//
//////////////////////////////////////////////////////////////////////////////

#include <time.h>
#include <string.h>

class Stopwatch {
  public:
	int	start ( void );
		// Starts the timer.  It is possible to 'start' and active
		// timer.

	int	stop ( void );
		// Stops the timer.  It is possible to 'stop' the timer
		// multiple times, each instance will increase the elapsed
		// time (this is more of a 'split-time' feature)

	int	elapsed ( void );
		// Seconds elapsed since start of timer

	int	remaining ( int limit )
		// Seconds remaning until the limit (in seconds) is met
		// A returned '-1' indicates an error, '0' indicates limit
		// has been met or exceeded, otherwise it is what remains.
		{ int	Elapsed;

		  if ((Elapsed=elapsed()) < 0) return -1;
		  if (Elapsed > limit) return 0;
		  else return (limit - Elapsed);
		}
	int	errorReason ( void ) { return _errorReason; }
		// Returns the errno reason for the last failure

	const char	*errorMessage (void ) { return (_errorMessage); }
		// Returns a text message describing the last failure


	// Default Constructor
        Stopwatch ( void )
        { _initialize();
        }

	// Copy Constructor
	Stopwatch ( const Stopwatch & source )
	{ *this = source;
	  return;
	}

	// Assignment Constructor
	Stopwatch &operator=(const Stopwatch &source)
	{ if (this == &source) return *this;

	  _baseTime = source._baseTime;
	  _endTime = source._endTime;
	  return *this;
	}

	// Destructor
        virtual ~Stopwatch ( void )
        { 
        }

  protected:
	time_t	_baseTime;
	time_t	_endTime;
	int	_errorReason;	// System error for failure
	char	_errorMessage[256];
        void	_initialize( void )
		{ _baseTime = _endTime = -1;
		  _errorReason = 0;
		  memset(_errorMessage,0,sizeof(_errorMessage));
		  return;
		}

};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
