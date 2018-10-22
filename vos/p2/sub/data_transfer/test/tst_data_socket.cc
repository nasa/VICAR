//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdio.h>
#include <fstream.h>
#include <string.h>
#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()
#include <unistd.h>	// Cause of main()
#include <errno.h>

#include "SocketBase.h"
#include "return_status.h"

int	PortMaker(
  SocketBase	&OutPort,
  char	*HostName,
  int	PortNumber)
{ int	status;
  SocketBase	LocalPort("lcl");

cerr << "Trying port: " << PortNumber << endl << flush;

  if (PortNumber < 0)
  { PortNumber *= (-1);
    status = LocalPort.open(HostName,PortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Open Failed " << LocalPort.errorReason() << " " <<
              strerror(LocalPort.errorReason()) << "; " <<
              LocalPort.errorMessage() << endl;
      return (status);
    }
    // cout << "Open Succeded\n" << flush;
  } else
  { status = LocalPort.create(PortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Create Failed: " << strerror(LocalPort.errorReason()) << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    }
    cout << "Create Suceeded\n" << flush;

    status = LocalPort.attach();
    if (RTN_FAILURE(status))
    { cerr << "Attach Failed " << LocalPort.errorReason() << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    }
    // cout << "Attach Succeded\n" << flush;
  }

  OutPort = LocalPort;

  return RTN_NORMAL;
}

main(
  int	argc,
  char	*argv[])
{ char	HostName[256],
	MyBuf[256];
  int	BufLth,
	status,
	InPortNumber = 0,
	OutPortNumber = 0;
  SocketBase	Input("inp"),
		Output("out");
int temp,loopCount;
  if (argc < 2)
  { cout << "Please supply a port number\n [Positive - sink (first); " <<
            "Negative - source (second)]: ";
    cin >> InPortNumber;
  } else InPortNumber = atoi(argv[1]);
  if (InPortNumber < 0) OutPortNumber = InPortNumber * (-1);

  cout << endl << "Please note, this program generates artifical delays\n";
  cout << "as part of its normal execution\n\n";

  //
  //  Socket Source code
  //

  if (OutPortNumber)
  { if (argc < 3)
    { cout << "Please supply a destination host name: ";
      cin >> HostName;
    } else strcpy(HostName,argv[2]);
//    status = Output.open(HostName,OutPortNumber);
status = PortMaker(Output,HostName,((-1)*OutPortNumber));
    if (RTN_FAILURE(status))
    { cerr << "Open Failed " << Output.errorReason() << endl;
      return (status);
    }
    cout << "Open Suceeded\n" << flush;

    if (argc > 3) strcpy(MyBuf,argv[3]);
    else
    { cout << "Enter message for transmission .... \n" << flush;
      gets(MyBuf);
      // cin >> MyBuf;
    }
    BufLth = sizeof(MyBuf);
    cout << "Sending (" << (strlen(MyBuf)) << ") \"" << MyBuf <<
         "\"\n" << flush;
//    status = Output.write(MyBuf,&BufLth,0);
temp = 4;
for (loopCount=0;loopCount<BufLth;loopCount+=4)
{ status = Output.write(&MyBuf[loopCount],&temp,0);
cout << "Writing " << loopCount << " for " << temp << endl;
    if (RTN_FAILURE(status))
       cout << "Write Failed " << Output.errorReason() << endl;
  sleep(1);
}
/****
temp = BufLth - 20;
status = Output.write(MyBuf,&temp,0);
sleep(10);
temp = 20;
status = Output.write(MyBuf,&temp,0);
***/
    if (RTN_FAILURE(status))
       cout << "Write Failed " << Output.errorReason() << endl;
    else
    { BufLth = sizeof(MyBuf);
      memset(MyBuf,0,BufLth);
      status = Output.read(MyBuf,&BufLth,-1);
      if (RTN_SUCCESS(status) || status == RTN_MISSING_DATA)
         cout << "Received (" << strlen(MyBuf) << ") \"" << MyBuf << "\"\n";
      else cout << "Reply Failed " << Input.errorReason() << endl;
      cout << flush;
    }

    cout << "Finished OPEN" << endl;
  } else

  //
  //  Socket Sink code
  //

/******
  { status = Input.create(InPortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Create Failed: " << strerror(Input.errorReason()) << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    }
    cout << "Create Suceeded\n" << flush;

    status = Input.attach();
******/
{
status = PortMaker(Input,NULL,InPortNumber);
    if (RTN_FAILURE(status))
    { cerr << "Attach Failed " << Input.errorReason() << endl;
      cout << "Failed CREATE (" << status << ")" << endl;
      return (status);
    } else
    { cout << "Attach Succeded\n" << flush;
      BufLth = sizeof(MyBuf);
status = Input.read(MyBuf,&BufLth,660);
//      status = Input.read(MyBuf,&BufLth,-1);
      if (RTN_SUCCESS(status) || status == RTN_MISSING_DATA)
      { BufLth = sizeof(MyBuf);
        cout << "Received (" << strlen(MyBuf) << ") \"" << MyBuf << "\"\n";
        strcpy(MyBuf,"Got it ... Thanks");
        cout << "Sending (" << strlen(MyBuf) << ") \"" << MyBuf << "\"\n" <<
             flush;
        status = Input.write(MyBuf,&BufLth,0);
        if (RTN_FAILURE(status))
           cout << "Reply Failed " << Input.errorReason() << endl << flush;
      } else cout << "Read Failed " << Input.errorReason() << endl << flush;

      cout << flush;
    }
    cout << "Finished CREATE" << endl;
  }

//  cout << "Finished" << endl;

  return(1);
}
