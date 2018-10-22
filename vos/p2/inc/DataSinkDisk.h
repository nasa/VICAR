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
