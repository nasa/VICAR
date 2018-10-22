//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				DataSink.cc
//
//////////////////////////////////////////////////////////////////////////////

#include "DataSink.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				output
//
//	Transfers the data from memory to the data sink.  Memory is supplied
//  by caller.
//
//////////////////////////////////////////////////////////////////////////////

int	DataSink::output (
  void		*Destination,
  int		*Length,
  int		TimeOut)
{ int	RtnStatus = RTN_NORMAL;

  _buffer = (unsigned char *)Destination;
  _dataLength = Length;

  RtnStatus = _drain(TimeOut);

  if (RTN_SUCCESS(RtnStatus))
  { _recordCount++;
    if (_critical) _flush();
  }

  return ( RtnStatus );
}

////////////////////////////////////////////////////////////////////////
//
//				operator=
//
//	Until it becomes a reality, this is just a stub
//
////////////////////////////////////////////////////////////////////////

DataSink	&DataSink::operator= (
  const DataSink	&sink)
{
  if (this == &sink) return *this;	// in case assigning to itself
  throw "Can not assign one DataSink object to another";

  return *this;
}

////////////////////////////////////////////////////////////////////////
//                              
//				DataSink
//
//	Until it becomes a reality, this is just a stub
//
////////////////////////////////////////////////////////////////////////

DataSink::DataSink (
  const DataSink	&sink)
{
  if (this == &sink) return;		// in case assigning to itself
  throw "Can not copy one DataSink object to another";
}

