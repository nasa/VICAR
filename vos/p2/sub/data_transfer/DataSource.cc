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
