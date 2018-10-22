//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()
#include <iostream.h>
#include <string.h>

#include "return_status.h"
#include "DataSourceDisk.h"
#include "SfduBase.h"
#include "SfduPkt.h"
#include "ChdoBase.h"
#include "Chdo082.h"
#include "Chdo128.h"

main(
  int	argc,
  char	*argv[])
{ int	Status,
	Idx;
  char	FileName[256];
  DataSourceDisk	Test;
  SfduPkt		Sfdu;
  ChdoBase		Chdo;
  ChdoBase		TestChdo;
  Chdo_082		Chdo082;
  Chdo_128		Chdo128;

  if (argc < 2)
  { cout << "Must supply an input filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[1]);

  if (argc > 2)
  { Status = Sfdu.captureSfdu(argv[2]);
    cout << "Catpure: " << RTN_DFLT_MSG(Status) << endl;
  }

  if (Test.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl;

  cout << "Starting Read\n\n" << flush;
  while (RTN_SUCCESS(Status = Sfdu.getNextPktSfdu(Test,-1)))
  { cout << "Sfdu " << Sfdu.sfduCounter() << " bytes: " << Sfdu.sfduSize() <<
         "  " << endl;
    for (Idx=0; RTN_SUCCESS(Sfdu.extractLabel(Idx,TestChdo)); Idx++)
    { Chdo = TestChdo;
      cout << "CHDO " << Chdo.type() << " (" << Chdo.length() << ")\n";
      if (Chdo.type() == 82)
      { if (RTN_SUCCESS(Status = Sfdu.extractChdo(82,Chdo082)))
        { cout << "  ERT: " << (Chdo082.ert()).formatted() << endl;
          cout << "  RCT: " << (Chdo082.rct()).formatted() << endl;
        } else
        { cout <<"Error extracting CHDO: " << RTN_DFLT_MSG(Status) << endl;
        }
      }
      if (Chdo.type() == 128)
      { if (RTN_SUCCESS(Status = Sfdu.extractChdo(82,Chdo128)))
        { cout << "  APID: " << Chdo128.apid() << endl;
          cout << "  Packet Sequence Number: " << Chdo128.pktSeqCnt() << endl;
          cout << "  SCLK: " << (Chdo128.sclk()).formatted() <<
                  "  " << Chdo128.sclk().seconds() << endl;
          cout << "  SCET: " << (Chdo128.scet()).formatted() << endl;
        } else
        { cout <<"Error extracting CHDO: " << RTN_DFLT_MSG(Status) << endl;
        }
      }
    }
    cout << endl;
  }

  cout << "Finished w/ " << Sfdu.sfduCounter() << " records" << endl;
  cout << "Status: " << RTN_DFLT_MSG(Status) << endl;

  return(1);
}
