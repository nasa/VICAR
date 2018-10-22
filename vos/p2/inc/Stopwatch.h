#ifndef STOP_WATCH_H
#define STOP_WATCH_H

//	Copyright (c) 2000, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged	

//////////////////////////////////////////////////////////////////////////////
//
//				Stopwatch.h
//
//
//////////////////////////////////////////////////////////////////////////////

#include <time.h>
#include <string.h>

class Stopwatch {
  public:
	int	start ( void );
		// Starts the timer.  It is possible to 'start' and active
		// timer.

	int	stop ( void );
		// Stops the timer.  It is possible to 'stop' the timer
		// multiple times, each instance will increase the elapsed
		// time (this is more of a 'split-time' feature)

	int	elapsed ( void );
		// Seconds elapsed since start of timer

	int	remaining ( int limit )
		// Seconds remaning until the limit (in seconds) is met
		// A returned '-1' indicates an error, '0' indicates limit
		// has been met or exceeded, otherwise it is what remains.
		{ int	Elapsed;

		  if ((Elapsed=elapsed()) < 0) return -1;
		  if (Elapsed > limit) return 0;
		  else return (limit - Elapsed);
		}
	int	errorReason ( void ) { return _errorReason; }
		// Returns the errno reason for the last failure

	const char	*errorMessage (void ) { return (_errorMessage); }
		// Returns a text message describing the last failure


	// Default Constructor
        Stopwatch ( void )
        { _initialize();
        }

	// Copy Constructor
	Stopwatch ( const Stopwatch & source )
	{ *this = source;
	  return;
	}

	// Assignment Constructor
	Stopwatch &operator=(const Stopwatch &source)
	{ if (this == &source) return *this;

	  _baseTime = source._baseTime;
	  _endTime = source._endTime;
	  return *this;
	}

	// Destructor
        virtual ~Stopwatch ( void )
        { 
        }

  protected:
	time_t	_baseTime;
	time_t	_endTime;
	int	_errorReason;	// System error for failure
	char	_errorMessage[256];
        void	_initialize( void )
		{ _baseTime = _endTime = -1;
		  _errorReason = 0;
		  memset(_errorMessage,0,sizeof(_errorMessage));
		  return;
		}

};

#endif
