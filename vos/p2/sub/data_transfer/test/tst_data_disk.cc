//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <fstream.h>
#include <string.h>
#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()

#include "return_status.h"
#include "DataSourceDisk.h"
#include "DataSinkDisk.h"

main(
  int	argc,
  char	*argv[])
{ char	FileName[256],
	MyBuf[256];
  int	XferLength = 128,
	Status;
  DataSourceDisk	Test;
  DataSinkDisk		Out(1);

  if (argc < 2)
  { cout << "Must supply an input filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[1]);

  if (Test.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl;

  if (Test.resize(512))
  { cout << "Could not allocate buffer" << endl;
    exit(1);
  }

  if (argc < 3)
  { cout << "Must supply an output filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[2]);

  if (Out.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl;


  memset(MyBuf,0,256);
  cout << "Starting copy loop\n";
  while (!(Status = Test.input(MyBuf,&XferLength)) || XferLength>0)
  { Out.output(MyBuf,&XferLength);
    memset(MyBuf,0,256);
    XferLength = 128;
  }


  cout << "Finished w/ " << Test.recordCount() << " blocks" << endl;


  return(1);
}
