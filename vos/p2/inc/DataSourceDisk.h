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
