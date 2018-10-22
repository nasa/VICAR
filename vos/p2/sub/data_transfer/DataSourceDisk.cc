//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				DataSourceDisk.cc
//
//
//////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <errno.h>

#include "DataSourceDisk.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				open
//
//	Opens the disk file for the data source object.  
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceDisk::open(char *FileName)
{
  _recordCount = 0;	// In case DataSource is opened and closed

  _diskFile.open(FileName, ios::in);
  if (_diskFile.fail())
  { // error condition ... could not open diskfile
    _errorReason = errno;
    return RTN_OPEN_ERROR;
  }

  _active = 1;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				fill
//
//	Transfers the data from the buffer controlled by the base-class to 
//  the physical disk file.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSourceDisk::_fill(
  int	MinLength,
  int	TimeOut)
{ int	Available,
	ReadLength,
	RtnStatus = RTN_NORMAL;

  if ( _diskFile.eof() ) return RTN_EOD;

//  if (TimeOut) then RtnStatus = RTN_TIMEOUT;	// STUB

  Available = _capacity - (_offset + _stored);
  if (MinLength > Available)  // Is there room in the buffer
     return (RTN_INSUFF_MEMORY);

  ReadLength = Available;
  _diskFile.read((char *)(_buffer+_stored+_offset), ReadLength);
  ReadLength = _diskFile.gcount();
  _stored += ReadLength;

  //  Check for errors
  if (_diskFile.fail())
  { strcpy(_errorMessage,"Error on disk file read");
    _errorReason = errno;
    if (ReadLength >= MinLength) RtnStatus = RTN_NORMAL;
    else RtnStatus = RTN_IO_ERROR;
  }

  return RtnStatus;
}
