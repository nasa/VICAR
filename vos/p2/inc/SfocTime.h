#ifndef SFOC_TIMECLASS_H
#define SFOC_TIMECLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

#include <strings.h>

//////////////////////////////////////////////////////////////////////////////
//
//				SfocTime.h
//
//	This class defines the standard SFOC time object.  It defines the
//  mechanism to insert, remove and format the time, as well as perform
//  standard arithmetic and relational operations.
//
//////////////////////////////////////////////////////////////////////////////

/* SFOC Epoch: 1-1-58 @ 00:00:00.000 */

#define  INVLD_SFOC_TIME	"1900-01-01T00:00:00.000"
#define  DEFAULT_SFOC_EPOCH	1958	// Code can handle a 'variable' EPOCH
					// year.  Epoch must start on
					// yyyy-01-01T00:00:00.000 though.

class	SfocTime {

  public:
	int	epoch( void ) { return _epochYear; }
		// Returns the epoch used for the SFOC time

	int	changeEpoch( int epoch, int convert = 1 );
		// Changes the epoch and optionally converts the internal
		// components so that the output time remains constant

	long	milliSeconds( void ) { return _milliSeconds; }
	long	days( void ) { return _days; }
		// Returns the component values for milliSeconds & days

	int	incrementDays( int Days = 1);
	int	incrementMilliSeconds( int MilliSeconds = 1);
		// Increment the time by the value given

	int	extract( unsigned char * buffer );
		// Extracts the time values from a standard 6-byte SFOC
		// time buffer

	int	compress( unsigned char * buffer );
		// Creates a standard 6-byte SFOC time buffer based on the 
		// object time fields

	int	ingestUTC( char * buffer );
		//  Ingests and converts a UTC formmated time string

	const char	*formatted( int JulianFlag = 0 );
		//  Returns a UTC formatted string of the SFOC time

	const char	*errorMessage( void ) { return _errorMessage; }
		//  Returns a text message identifiing the error that occured

	SfocTime	&operator+ (const SfocTime &Time);
	SfocTime	&operator- (const SfocTime &Time);
	int		operator== ( const SfocTime &Time)
			{ if (_days != Time._days) return 0;
			  return (_milliSeconds == Time._milliSeconds);
			}
	int		operator!= ( const SfocTime &Time)
			{ return (!this->operator==(Time)); }
	int		operator> ( const SfocTime &Time)
			{ if (_days > Time._days) return 1;
			  return (_milliSeconds > Time._milliSeconds);
			}
	int		operator< ( const SfocTime &Time)
			{ if (_days < Time._days) return 1;
			  return (_milliSeconds < Time._milliSeconds);
			}
	int		operator>= ( const SfocTime &Time)
			{ return (!this->operator<(Time)); }
	int		operator<= ( const SfocTime &Time)
			{ return (!this->operator>(Time)); }

	//  Default Constructor
	SfocTime ( int epoch = DEFAULT_SFOC_EPOCH, unsigned char *buffer = NULL )
		{ _initialize();
		  _epochYear = epoch;
		  if (buffer != NULL) extract(buffer);
		  return;
		}

	//  Default Constructor
	SfocTime ( unsigned char *buffer )
		{ _initialize();
		  extract(buffer);
		  return;
		}

	//  Assignment Constructor
	SfocTime &operator= (const SfocTime &Time)
		{ if (this == &Time) return *this;
		  _epochYear = Time._epochYear;
		  _milliSeconds = Time._milliSeconds;
		  _days = Time._days;

		  return *this;
		}

	//  Destructor
	~SfocTime ( void ) { }

  protected:
	int	_epochYear;
	long	_milliSeconds;
	long	_days;
	char	_errorMessage[256];

	void	_initialize( void )
		{ _epochYear = DEFAULT_SFOC_EPOCH;
		  _days = _milliSeconds = 0;
		  memset(_errorMessage,0,sizeof(_errorMessage));
		  return;
		}

};

#endif
