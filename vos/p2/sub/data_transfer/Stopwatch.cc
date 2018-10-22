//	Copyright (c) 2000, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Stopwatch
//
//
//////////////////////////////////////////////////////////////////////////////

#include <errno.h>

#include "Stopwatch.h"

#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				start
//
//	Starts the timer.  It is possible to 'start' and active timer.
//
//////////////////////////////////////////////////////////////////////////////
int	Stopwatch::start ( void )
{
   _endTime = -1;
  if (time(&_baseTime) == -1)
  { strcpy(_errorMessage,"Could not get base time");
    _errorReason = errno;
    _baseTime = -1;				// Make sure it is still -1
    return (RTN_ERROR);
  }
  return (RTN_NORMAL);
}

//////////////////////////////////////////////////////////////////////////////
//
//				stop
//
//	Stops the timer, basically gets the current time as the end time.
//  It is possible to 'stop' a timer multiple times and get increasing
//  times.  In this case the stop acts more like a 'split' time.
//
//////////////////////////////////////////////////////////////////////////////
int     Stopwatch::stop ( void )
{ if (time(&_endTime) == -1)
  { strcpy(_errorMessage,"Could not get end time");
    _errorReason = errno;
    return (RTN_ERROR);
  }
  return (RTN_NORMAL);
}

//////////////////////////////////////////////////////////////////////////////
//
//				elapsed
//
//	Seconds elapsed since start of timer and it's stop point, if set, or
//  the current time.
//
//////////////////////////////////////////////////////////////////////////////
int     Stopwatch::elapsed ( void )
{ int   Seconds;
  time_t        EndTime;

  if (_endTime != -1) EndTime = _endTime;
  else if (_baseTime == -1)
  { strcpy(_errorMessage,"Could not get offset time; base time not set");
    return (-1);
  } else if (time(&EndTime) == -1)
  { strcpy(_errorMessage,"Could not get offset time");
    _errorReason = errno;
    return (-1);
  }

  Seconds = (int)difftime(EndTime,_baseTime);

  return Seconds;
}

