//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfocTime.cc
//
//	This class defines the standard SFOC time object.  It defines the
//  mechanism to insert, remove and format the time, as well as perform
//  standard arithmetic and relational operations.
//
//////////////////////////////////////////////////////////////////////////////

#include <time.h>
#include <stdio.h>
#include <string.h>

#include "SfocTime.h"
#include "return_status.h"

/* DEFAULT SFOC Epoch: 1-1-58 @ 00:00:00.000 */
#define  MAX_RANGE_YEARS	180
#define  MS_PER_DAY             86400000
#define  DAYS_PER_YEAR		365
#define  DAYS_PER_LEAP		366
#define  MAX_DAYS		65535
#define  INVLD_SFOC_DAYS        (-21184)

//////////////////////////////////////////////////////////////////////////////
//
//				extract
//
//	Extracts the time values from a standard 6-byte SFOC time buffer
//
//////////////////////////////////////////////////////////////////////////////

int	SfocTime::extract(
  unsigned char	*buffer)
{ long	Days,
	MilliSeconds;

  Days = (buffer[0] * 256) + buffer[1];

  MilliSeconds = (long)(buffer[2] * 16777216) +	// This byte should be '0'
                 (long)(buffer[3] * 65536) +
                 (long)(buffer[4] * 256) + (long)buffer[5];

  _days = Days;
  _milliSeconds = MilliSeconds;

  if (Days < 0 || MilliSeconds < 0 || MilliSeconds >= MS_PER_DAY)
     return RTN_INVLD_ARG_IGNORED;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				compress
//	Creates a standard 6-byte SFOC time buffer based on the objects
//  time fields
//
//////////////////////////////////////////////////////////////////////////////

int	SfocTime::compress(
  unsigned char	*buffer)
{
  if (_days < 0 || _milliSeconds < 0 || _milliSeconds >= MS_PER_DAY)
     return RTN_INVLD_ARG;

  //  Days
  buffer[0] = _days / 256;
  buffer[1] = _days % 256;

  //  Milliseconds
  buffer[2] = (_milliSeconds / 16777216) % 256;	// This byte should be '0'
  buffer[3] = (_milliSeconds / 65536) % 256;
  buffer[4] = (_milliSeconds / 256) % 256;
  buffer[5] = _milliSeconds % 256;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				changeEpoch
//	Changes the epoch value of the sclk.  Will also convert the internal
//  components to retain the same time as the default.
//
//////////////////////////////////////////////////////////////////////////////

int	SfocTime::changeEpoch(
  int	epoch,
  int	convert)
{ int	DeltaEpoch = 0,
	DeltaDays = 0,
	LeapDays = 0;

  if (epoch == _epochYear) return RTN_NORMAL;	// no change requested

  if (convert)
  { DeltaDays = DAYS_PER_YEAR * (DeltaEpoch = _epochYear - epoch);

    if (DeltaEpoch < 0)			// not implemented ... yet
    { sprintf(_errorMessage,"Can not convert to more recent EPOCH (%d -> %d)",
              _epochYear, epoch);
      return RTN_INVLD_ARG;
    } else
    { /***  Include Leap-year factors  ***/
      LeapDays = DeltaEpoch/4 - DeltaEpoch/100;	// basic; delta<137 years

      /** Correct for out of phase leap year **/
      if ((_epochYear % 4) && (_epochYear % 4) - (DeltaEpoch % 4) <= 0)
         LeapDays++;

      /** Correct for Century crossing **/
      if ((_epochYear % 100) < (DeltaEpoch % 100))
      {
        if (DeltaEpoch < 100)		// General Century effect excluded
        { if (((_epochYear/100) % 4) != 0) LeapDays--; // non-quad century X-ing
        } else				// General Century effect included
        { LeapDays--;			// Off-phase century leap year
          if (((_epochYear/100) % 4) < 2) LeapDays++;  // either century a quad
        }
      }
    }

    if (((_days/DAYS_PER_YEAR)+DeltaEpoch) >= MAX_RANGE_YEARS)
    { sprintf(_errorMessage,"Day count too large for new EPOCH (%d)",epoch);
      return RTN_INVLD_ARG;
    }

    _days += DeltaDays + LeapDays;
    _epochYear = epoch;
  } else _epochYear = epoch;

  return RTN_NORMAL;
}


//////////////////////////////////////////////////////////////////////////////
//
//				formatted
//
//	Returns a UTC formatted string of the SFOC time
//
//////////////////////////////////////////////////////////////////////////////

const char	*SfocTime::formatted(
  int	Julian )
{ long	LeapYear,	// Leap Year flag
	LeapOffset,
	DeltaYear,
	LeapEpoch,
	Year,
	Month,
	Day,		// Day of year/month (as required)
	TDays = _days,	// Total Days (from SfocTime class)
	Hour,
	Minute,
	Second,
	Milli;
  char	dom[12] = {31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30,  31};
  static char	Buffer[64];

  /***  Prep calculations for specific EPOCH year  ***/
  LeapEpoch = ((_epochYear % 4) == 0) && ((_epochYear % 100) ? 1 :
               ((_epochYear % 400) == 0));
  LeapOffset = (_epochYear % 4);		// For Epoch between Leap years

  if ((LeapYear=LeapEpoch) && TDays < DAYS_PER_LEAP)	// leap-year Epochs are special
  { Year = _epochYear;
    Day = TDays + 1;
  } else
  { DeltaYear = (TDays / DAYS_PER_YEAR);
    Year = _epochYear + DeltaYear;

    /***  Include leap-year factors  ***/
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
         Day++;         // Erroneous Leap-year taken for non quad-century
      if ((Year/100)-(_epochYear/100) > 1 && (Year/100) % 4 && (Year%100))
         Day++;
//printf("Y: %d  D: %d\n",Year,Day);
      if (Day > (DAYS_PER_YEAR-1) && !LeapYear)
      { Day -= DAYS_PER_YEAR;
        Year++;
      }
    }
    Day++;	// Added 1 to Day because it is a zero-offset number used
		// in a one-offset calculation
  }

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

  /*** Hours, Minutes, Seconds and MilliSeconds are a piece of cake */
  Hour = _milliSeconds / (1000 * 60 * 60);
  Minute = (_milliSeconds / (1000 * 60)) % 60;
  Second = (_milliSeconds / 1000) % 60;

  Milli = _milliSeconds % 1000;

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

int	SfocTime::ingestUTC(
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
	Second = 0,
	Milli = 0;
  int	dts[13] = {0,  0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  int	dom[13] = {0, 31, 28, 31, 30,  31,  30,  31,  31,  30,  31,  30,  31};

  /***  Prep calculations for specific EPOCH year  ***/
  LeapOffset = (_epochYear % 4);	// For Epoch between Leap years
  LeapEpoch = ((_epochYear % 4) == 0) && ((_epochYear % 100) ?
              1 : ((_epochYear % 400) == 0));

  if ((Julian=(Time[8] == 'T')))		// Auto-detects Julian Format
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
  { if (!Julian && (Month < 1 || Month > 12))
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
  /***  Convert to SFOC Time  ***/
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
  Day += (Year * DAYS_PER_YEAR) + LeapDays;
  Milli += (((Hour * 60 + Minute) * 60) + Second) * 1000;

  /* printf ("Time in days: %d\n", Day); */

  _days = Day;
  _milliSeconds = Milli;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				incrementDays
//
//	Increments the day portion of the Sfoc Time by the parameter value
//
//////////////////////////////////////////////////////////////////////////////

int	SfocTime::incrementDays(
  int	Days)
{
  if ((_days+Days) > MAX_DAYS || (_days+Days) < 0)
  { sprintf(_errorMessage,"Day increment (%d) would cause a negative count",
            Days);
    return RTN_INVLD_ARG;
  }

  _days += Days;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				incrementMilliSeconds
//
//	Increments the millisecond portion of the Sfoc Time by the parameter
//  value
//
//////////////////////////////////////////////////////////////////////////////

int	SfocTime::incrementMilliSeconds(
  int	incMilliSeconds)
{ long	Days,
	MilliSeconds;

  MilliSeconds = _milliSeconds + incMilliSeconds;
  Days = _days + MilliSeconds / MS_PER_DAY;
  MilliSeconds %= MS_PER_DAY;
  if (MilliSeconds < 0)
  { Days--;
    MilliSeconds += MS_PER_DAY;
  }

  if (Days > MAX_DAYS || Days < 0)
  { sprintf(_errorMessage,
            "MilliSecond increment (%d) would cause a negative count",
            incMilliSeconds);
    return RTN_INVLD_ARG;
  }

  _days = Days;
  _milliSeconds = MilliSeconds;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				operator +
//
//////////////////////////////////////////////////////////////////////////////

SfocTime	&SfocTime::operator+ (const SfocTime &Time)
{ _milliSeconds += Time._milliSeconds;
  _days += Time._days + (_milliSeconds / MS_PER_DAY);
  _milliSeconds %= MS_PER_DAY;

  return *this;
}

//////////////////////////////////////////////////////////////////////////////
//
//				operator -
//
//////////////////////////////////////////////////////////////////////////////

SfocTime	&SfocTime::operator- (const SfocTime &Time)
{ _days -= Time._days;
  if (_milliSeconds >= Time._milliSeconds)
     _milliSeconds -= Time._milliSeconds;
  else
  { _milliSeconds += MS_PER_DAY - Time._milliSeconds;
    _days--;
  }

  if (_days < 0)
  { _days = INVLD_SFOC_DAYS;
    _milliSeconds = 0;
  }

  return *this;
}
