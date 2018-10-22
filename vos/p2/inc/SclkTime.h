#ifndef SCLK_TIMECLASS_H
#define SCLK_TIMECLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

#include <strings.h>

//////////////////////////////////////////////////////////////////////////////
//
//				SclkTime.h
//
//	This class defines the standard SCLK time object.  It defines the
//  mechanism to insert, remove and format the time, as well as perform
//  standard arithmetic and relational operations.
//
//////////////////////////////////////////////////////////////////////////////

/* SCLK Default Epoch: 1-1-58 @ 00:00:00.000 */

#define  DEFAULT_SCLK_EPOCH	1958
#define  INVLD_SCLK_TIME	"1900-01-01T00:00:00.000"

class	SclkTime {

  public:
	int	epoch( void ) { return _epochYear; }
		// Returns the epoch used for the sclk

	int	changeEpoch( int epoch, int convert = 1 );
		// Changes the epoch and optionally converts the internal
		// components so that the output time remains constant

	int	partialSeconds( void ) { return _partialSeconds; }
	unsigned long	seconds( void ) { return _seconds; }
		// Returns the component values for seconds & partialSeconds

	int	incrementSeconds( int Seconds = 1);
	int	incrementPartialSeconds( int PartialSeconds = 1);
		// Increment the time by the value given

	void	extract( unsigned char * buffer );
		// Extracts the time values from a standard 5-byte SCLK
		// time buffer

	void	compress( unsigned char * buffer );
		// Creates a standard 5-byte SCLK time buffer based on the 
		// object time fields

	int	ingestUTC( char * buffer );
		//  Ingests and converts a UTC formmated time string

	const char	*formatted( int JulianFlag = 0 );
		//  Returns a UTC formatted string of the SCLK time

	const char	*errorMessage( void ) { return _errorMessage; }
		//  Returns a text message identifiing the error that occured

	SclkTime	&operator+ (const SclkTime &Time);

	SclkTime	&operator- (const SclkTime &Time);
			// operator is realy a delta operator and returns the
			// difference in the count of the sclks.  The epoch
			// will be set to '0' to reflect this

	int		operator== ( const SclkTime &Time)
			{ if (_epochYear != Time._epochYear)
			  { return 0;
			  } else if (_seconds != Time._seconds) return 0;
			  else return (_partialSeconds == Time._partialSeconds);
			}

	int		operator!= ( const SclkTime &Time)
			{ return (!(this->operator == (Time))); }

	int		operator> ( const SclkTime &Time)
			{ if (_seconds > Time._seconds) return 1;
			  return (_partialSeconds > Time._partialSeconds);
			}

	int		operator< ( const SclkTime &Time)
			{ if (_seconds < Time._seconds) return 1;
			  return (_partialSeconds < Time._partialSeconds);
			}

	int		operator>= ( const SclkTime &Time)
			{ return (!(this->operator < (Time))); }

	int		operator<= ( const SclkTime &Time)
			{ return (!(this->operator > (Time))); }

	//  Default Constructor
	SclkTime ( int epoch = DEFAULT_SCLK_EPOCH, unsigned char *buffer = NULL )
		{ _initialize();
		  _epochYear = epoch;
		  if (buffer != NULL) extract(buffer);
		  return;
		}

	//  Default Constructor
	SclkTime ( unsigned char *buffer )
		{ _initialize();
		  extract(buffer);
		  return;
		}

	//  Assignment Constructor
	SclkTime &operator= (const SclkTime &Time)
		{ if (this == &Time) return *this;
		  _epochYear = Time._epochYear;
		  _partialSeconds = Time._partialSeconds;
		  _seconds = Time._seconds;

		  return *this;
		}

	//  Destructor
	~SclkTime ( void ) { }

  protected:
	int	_epochYear;
	int	_partialSeconds;
	unsigned long	_seconds;
	char	_errorMessage[256];

	void	_initialize( void )
		{ _epochYear = DEFAULT_SCLK_EPOCH;
		  _partialSeconds = 0;
		  _seconds = 0;
		  memset(_errorMessage,0,sizeof(_errorMessage));
		  return;
		}

};

#endif
