//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SclkTime.cc
//
//	This class defines the standard SCLK time object.  It defines the
//  mechanism to insert, remove and format the time, as well as perform
//  standard arithmetic and relational operations.
//
//////////////////////////////////////////////////////////////////////////////

#include <time.h>
#include <stdio.h>
#include <string.h>

#include "SclkTime.h"
#include "return_status.h"

/* SCLK Epoch: 1-1-58 @ 00:00:00.000 */
#define  MAX_RANGE_YEARS	136		/* in years	*/
#define  SECONDS_PER_YEAR	31536000	/* 365 day year */
#define  SECONDS_PER_LEAP	31104000	/* 366 day year */
#define  SECONDS_PER_DAY	86400
#define  DAYS_PER_YEAR		365
#define  DAYS_PER_LEAP		366
#define  PS_PER_SECOND		255

//////////////////////////////////////////////////////////////////////////////
//
//				extract
//
//	Extracts the time values from a standard 5-byte SCLK time buffer
//
//////////////////////////////////////////////////////////////////////////////

void	SclkTime::extract(
  unsigned char	*buffer)
{ int	PartialSeconds;
  unsigned long	Seconds;

  Seconds = ((unsigned long)buffer[0] * 16777216) + // This byte should be '0'
            ((unsigned long)buffer[1] * 65536) +
            ((unsigned long)buffer[2] * 256) + (unsigned long)buffer[3];
  PartialSeconds = (int)buffer[4];

  _seconds = Seconds;
  _partialSeconds = PartialSeconds;

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				compress
//	Creates a standard 5-byte SCLK time buffer based on the objects
//  time fields
//
//////////////////////////////////////////////////////////////////////////////

void	SclkTime::compress(
  unsigned char	*buffer)
{
  //  Seconds
  buffer[0] = (_seconds / 16777216) % 256;	// This byte should be '0'
  buffer[1] = (_seconds / 65536) % 256;
  buffer[2] = (_seconds / 256) % 256;
  buffer[3] = _seconds % 256;

  //  Partial seconds
  buffer[4] = _partialSeconds % 256;

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				changeEpoch
//
//	Changes the epoch value of the sclk.  Will also convert the internal
//  components to retain the same time as the default.
//
//////////////////////////////////////////////////////////////////////////////

int	SclkTime::changeEpoch(
  int	epoch,
  int	convert)
{ int	DeltaEpoch = 0,
	LeapDays = 0;

  if (epoch == _epochYear) return RTN_NORMAL;	// no change requested

  if (convert)
  { DeltaEpoch = _epochYear - epoch;

    if (DeltaEpoch < 0)				// not implemented ... yet
    { sprintf(_errorMessage,"Can not convert to more recent EPOCH (%d -> %d)",
              _epochYear, epoch);
      return RTN_INVLD_ARG;
    } else if (((_seconds/SECONDS_PER_YEAR)+DeltaEpoch) >= MAX_RANGE_YEARS)
    { sprintf(_errorMessage,"Second count too large for new EPOCH (%d)",epoch);
      return RTN_INVLD_ARG;
    } else
    { /***  Include Leap-year factors  ***/
      LeapDays = DeltaEpoch/4 - DeltaEpoch/100; // basic; delta<137 years

      /** Correct for out of phase leap year **/
      if ((_epochYear % 4) && (_epochYear % 4) - (DeltaEpoch % 4) <= 0)
         LeapDays++;

      /** Correct for Century crossing **/
      if ((_epochYear % 100) < (DeltaEpoch % 100))
      { if (DeltaEpoch < 100)		// General Century effect excluded
        { if (((_epochYear/100) % 4) != 0) LeapDays--; // non-quad century X-ing
        } else				// General Century effect included
        { LeapDays--;			// Off-phase century leap year
          if (((_epochYear/100) % 4) < 2) LeapDays++; // either century a quad
        }
      }
    }

    _seconds += (DeltaEpoch * SECONDS_PER_YEAR) + (LeapDays * SECONDS_PER_DAY);
    _epochYear = epoch;
  } else _epochYear = epoch;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				formatted
//
//	Returns a UTC formatted string of the SCLK time
//
//////////////////////////////////////////////////////////////////////////////

const char	*SclkTime::formatted(
  int	Julian )
{ long	LeapYear,	// Leap Year flag
	LeapOffset,
	DeltaYear,
	LeapEpoch,
	TDays,
	Year,
	Month,
	Day,		// Day of year/month (as required)
	Hour,
	Minute,
	Second,
	Milli;
  unsigned long	TSeconds = _seconds;	// Total Seconds (from SclkTime class)
  char	dom[12] = {31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30,  31};
  static char	Buffer[64];

  /***  Prep calculations for specific EPOCH year  ***/
  LeapEpoch = ((_epochYear % 4) == 0) && ((_epochYear % 100) ? 1 :
               ((_epochYear % 400) == 0));
  LeapOffset = (_epochYear % 4);	// For Epoch between Leap years

  TDays = TSeconds / SECONDS_PER_DAY;

  if ((LeapYear=LeapEpoch) && TDays < DAYS_PER_LEAP)	// leap-year Epochs are special
  { Year = _epochYear;
    Day = TDays + 1;
  } else
  { DeltaYear = (TDays / DAYS_PER_YEAR);
    Year = _epochYear + DeltaYear;

    /***  Include Leap-year factors  ***/
    Day = (TDays % 365) - DeltaYear/4;
//printf("Y: %d  D: %d\n",Year,Day);
    if (Day < 1 && (Year%100) == 1 && (Year/100) % 4) Day++;
    if (Day < 0)
    { //if ((Year%100) == 1 && (Year/100) % 4) Day++;
      Year--;
      if ((Year%100) == 1 && (Year/100) % 4) Day--;
      LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
      Day += LeapYear ? 366 : 365;
    }

    /**  Epoch is a LeapYear; current year is NOT a leap-year  **/
    if (LeapEpoch && (DeltaYear % 4)) Day--;

    /** Correct for out of phase leap year **/
    if ((LeapOffset + (DeltaYear % 4)) > 4) Day--;

//printf("Y: %d  D: %d\n",Year,Day);
    if (Day < 0)
    { Year--;
      LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
      Day += LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR;
    }
    LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

    if ((Year/100) != (_epochYear/100))		// Crossed a Century
    { if ((_epochYear/100+1) % 4)
         Day++;		// Erroneous Leap-year taken for non quad-century
      if ((Year/100)-(_epochYear/100) > 1 && (Year/100) % 4 && (Year%100))
         Day++;
//printf("Y: %d  D: %d\n",Year,Day);
      if (Day > (DAYS_PER_YEAR-1) && !LeapYear)
      { Day -= DAYS_PER_YEAR;
        Year++;
      }
    }
    Day++;        // Added 1 to Day because it is a zero-offset number used
                  // in a one-offset calculation
  }

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

  /*** Hours, Minutes, Seconds and MilliSeconds are a piece of cake */
  Hour = ((Second=(TSeconds %SECONDS_PER_DAY)) / 3600) % 24;
  Minute = (Second / 60) % 60;
  Second = Second % 60;

  // Unique for SCLK object
  Milli = _partialSeconds * 1000 / 256;


  if (Julian)
     sprintf(Buffer,"%04ld-%03ldT%02ld:%02ld:%02ld.%03ld",
             Year,Day,Hour,Minute,Second,Milli);
  else
  { if (LeapYear) dom[1]++;		//  Add day in Feb. for leap year
    for (Month = 0; Month<12 && Day>dom[Month]; Month++) Day -= dom[Month];
    Month++;
    sprintf(Buffer,"%04ld-%02ld-%02ldT%02ld:%02ld:%02ld.%03ld",
            Year,Month,Day,Hour,Minute,Second,Milli);
  }

  return (Buffer);
}

//////////////////////////////////////////////////////////////////////////////
//
//				ingestUTC
//
//	Ingests and converts a UTC formmated time string
//
//////////////////////////////////////////////////////////////////////////////

int	SclkTime::ingestUTC(
  char	*Time )
{ long	LeapYear,
	LeapDays,
	LeapEpoch,
	LeapOffset,
	DeltaYear,
	Julian,
	Year = _epochYear,
	Month = 0,
	Day = 0,
	Hour = 0,
	Minute = 0,
	Milli = 0;
  unsigned long	Second = 0;
  int	dts[13] = {0,  0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  int	dom[13] = {0, 31, 28, 31, 30,  31,  30,  31,  31,  30,  31,  30,  31};

  /***  Prep calculations for specific EPOCH year  ***/
  LeapOffset = (_epochYear % 4);	// For Epoch between Leap years
  LeapEpoch = ((_epochYear % 4) == 0) && ((_epochYear % 100) ?
              1 : ((_epochYear % 400) == 0));

  if ((Julian=(Time[8] == 'T')))			// Auto-detects Julian Format
     sscanf(Time,"%4ld-%3ldT%2ld:%2ld:%2ld.%3ld",
            &Year,&Day,&Hour,&Minute,&Second,&Milli);
  else sscanf(Time,"%4ld-%2ld-%2ldT%2ld:%2ld:%2ld.%3ld",
              &Year,&Month,&Day,&Hour,&Minute,&Second,&Milli);

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
  DeltaYear = Year - _epochYear;

  /**/
  /***  Verify input values ... some of them  ***/
  /**/
  if (Year < _epochYear)
  { sprintf(_errorMessage,"Supplied year is less than the EPOCH (%ld vs %d)",
            Year,_epochYear);
    return RTN_INVLD_ARG;
  } else if (Year >= (_epochYear + MAX_RANGE_YEARS))
  { sprintf(_errorMessage,
            "Supplied year is larger than EPOCH can handle (%ld vs %d)",
            Year,(_epochYear + MAX_RANGE_YEARS - 1));
    return RTN_INVLD_ARG;
  }

  if (Julian)
  { if ((Day > DAYS_PER_LEAP) || (!LeapYear && Day >DAYS_PER_YEAR))
    { sprintf(_errorMessage,"Day of year out of range: %ld",Day);
      return RTN_INVLD_ARG;
    }
  } else
  { if (Month < 1 || Month > 12)
    { sprintf(_errorMessage,"Month is invalid: %ld",Month);
      return RTN_INVLD_ARG;
    }
    if (Day > dom[Month])
    { if (Day > (dom[Month]+1) || (!LeapYear && Day > dom[Month]))
      { sprintf(_errorMessage,"Invalid number of days (%ld) for month (%ld)",
                Day,Month);
        return RTN_INVLD_ARG;
      }
    }
  }

  /**/
  /***  Convert to SCLK Time  ***/
  /**/
  Day--;		//  Days are an offset from epoch (hence '0' based)
  Day += dts[Month];	//  Julian dates refer to month '0' with no days added
  if (LeapYear && Month > 2) Day++;	// After February, add the leap day

  /***  Include Leap-year factors  ***/
  LeapDays = DeltaYear/4;			// basic definition

  /**  Epoch is a LeapYear; current year is NOT a leap-year  **/
  if (LeapEpoch && (DeltaYear % 4)) LeapDays++;

  /** Correct for out of phase leap year **/
  if ((LeapOffset + (DeltaYear % 4)) > 4) LeapDays++;

  /** Correct for Century crossing **/
  if ((_epochYear/100) != (Year/100) && (Year % 100) > 0)
  { if ((_epochYear/100+1) % 4) LeapDays--;	// not a quad-century

    /* check for a second century crossing */
    if ((Year/100 - _epochYear/100) > 1 && (Year/100) % 4)
       LeapDays--;				// not a quad-century
  }

  Year -= _epochYear;
  Day += (Year * DAYS_PER_YEAR);
  Second += (Day + LeapDays) * SECONDS_PER_DAY + Hour * 3600 + Minute * 60;

  _seconds = Second;
  _partialSeconds = (int)(Milli * 0.256);

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				incrementSeconds
//
//	Increments the day portion of the Sclk Time by the parameter value
//
//////////////////////////////////////////////////////////////////////////////

int	SclkTime::incrementSeconds(
  int	Seconds)
{
  if (Seconds < 0 && (-Seconds) > (int) _seconds)
  { sprintf(_errorMessage,
            "Second increment (%d) would cause a negative count",Seconds);
    return RTN_INVLD_ARG;
  }

  _seconds += Seconds;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				incrementPartialSeconds
//
//	Increments the partial second portion of the Sclk Time by the
//   parameter value
//
//////////////////////////////////////////////////////////////////////////////

int	SclkTime::incrementPartialSeconds(
  int	incPartialSeconds)
{ int	PartialSeconds;
  unsigned long	Seconds;

  PartialSeconds = _partialSeconds + incPartialSeconds;
  if (PartialSeconds < 0 &&
      ((-PartialSeconds) / PS_PER_SECOND + 1) > (int) _seconds)
  { sprintf(_errorMessage,
            "Partial Second increment (%d) would cause a negative count", 
            incPartialSeconds);
    return RTN_INVLD_ARG;
  }

  Seconds = _seconds + PartialSeconds / PS_PER_SECOND;
  PartialSeconds %= PS_PER_SECOND;
  if (PartialSeconds < 0)
  { Seconds--;
    PartialSeconds += PS_PER_SECOND;
  }

  _seconds = Seconds;
  _partialSeconds = PartialSeconds;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				operator +
//
//	This seems somehow useless.  It is left here for unknown reasons
//
//////////////////////////////////////////////////////////////////////////////

SclkTime	&SclkTime::operator+ (const SclkTime &Time)
{ _partialSeconds += Time._partialSeconds;
  _seconds += Time._seconds + (_partialSeconds / PS_PER_SECOND);
  _partialSeconds %= PS_PER_SECOND;

  _epochYear = 0;	// Compromise with seemingly useless function

  return *this;
}

//////////////////////////////////////////////////////////////////////////////
//
//				operator -
//
//	Function returns a delta time between the supplied sclk.  The epoch
//  of the resulting sclk will be set to '0' to represent this fact.
//
//////////////////////////////////////////////////////////////////////////////

SclkTime	&SclkTime::operator- (const SclkTime &Time)
{ 
  /***  Chould verify the epochs are the same, compensate if different  ***/

  if (_seconds < Time._seconds)
  { _seconds = 0;
    _partialSeconds = 0;
  } else _seconds -= Time._seconds;

  if (_partialSeconds >= Time._partialSeconds)
     _partialSeconds -= Time._partialSeconds;
  else
  { _partialSeconds += PS_PER_SECOND - Time._partialSeconds;
    if (_seconds < 2)
    { _seconds = 0;
      _partialSeconds = 0;
    } else _seconds--;
  }

  _epochYear = 0;

  return *this;
}

