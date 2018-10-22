//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSinkDisk.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <strings.h>
#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <errno.h>

#include "DataSinkDisk.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				drain
//
//	Transfers the data from the buffer controlled by the base-class to
//  the physical disk file
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkDisk::_drain (
  int		TimeOut)
{
  _diskFile.write((char *)_buffer, *_dataLength);

  if (_diskFile.fail())
  {  strcpy(_errorMessage,"I/O failure");
    _errorReason = errno;
    //  Update DataLength value

    return ( RTN_IO_ERROR );
  }

  *_dataLength = 0;

  return ( RTN_NORMAL );
}

//////////////////////////////////////////////////////////////////////////////
//
//				open
//
//	Opens the disk file for the data sink object.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSinkDisk::open(
  char	*FileName)
{
  _recordCount = 0;	// In case DataSource is opened and closed

  //ios::noreplace is no longer supported under gcc 3.2.3 20030502 for RedHat
  // 3.2.3-24. Alternate is to open using ios::out, get beg position then
  //  seekp to end and see if beg and end are same or not.

  _diskFile.open(FileName, ios::out | ios::app);

  if (_diskFile.fail())
  { sprintf(_errorMessage,"Could not open diskfile: %s",FileName);
    _errorReason = errno;
    return RTN_OPEN_ERROR;
  }

  ios::pos_type endMark = _diskFile.tellp();
  _diskFile.seekp(ios::beg);
  ios::pos_type begMark = _diskFile.tellp();

  if (endMark!=begMark)
  {
    sprintf(_errorMessage,"Diskfile %s exists.",FileName);
    _diskFile.close();
    return RTN_OPEN_ERROR;
  }
  return RTN_NORMAL;
}

DataSinkDisk::DataSinkDisk ( char *FileName, int CriticalFlag)
{ _initialize(); _critical = CriticalFlag;
 this->open(FileName);
}

