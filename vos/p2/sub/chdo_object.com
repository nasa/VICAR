$!****************************************************************************
$!
$! Build proc for MIPL module chdo_object
$! VPACK Version 1.9, Monday, December 07, 2009, 16:08:42
$!
$! Execute by entering:		$ @chdo_object
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module chdo_object ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to chdo_object.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("chdo_object.imake") .nes. ""
$   then
$      vimake chdo_object
$      purge chdo_object.bld
$   else
$      if F$SEARCH("chdo_object.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake chdo_object
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @chdo_object.bld "STD"
$   else
$      @chdo_object.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create chdo_object.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack chdo_object.com -mixed -
	-s ChdoBase.cc Chdo002.cc Chdo010.cc Chdo082.cc Chdo128.cc SfocTime.cc -
	   SclkTime.cc -
	-i chdo_object.imake -
	-t tst_chdo_base.cc tst_chdo_base.imake tst_sfoc_time.cc -
	   tst_sclk_time.cc tst_sfoc_time.imake tst_sclk_time.imake -
	   README_tst
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ChdoBase.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				ChdoBase.h
//
//	ChdoBase is the class of an object that contains a generic CHDO
//  header.  It performs the primary CHDO header parsing/extraction, but does
//  not obtain, dump, process, look at, or care about the actual data the CHDO
//  contains.  This class does not validate the correctness of the header.
//  It just parses the header into its defined fields.
//
//	Other CHDO headers that inherit from this class should only be capable
//  of extracting or parsing CHDO specific values from a buffer suplied to 
//  the object.
//
//////////////////////////////////////////////////////////////////////////////

#include "ChdoBase.h"
#include "return_status.h"
//////////////////////////////////////////////////////////////////////////////
//
//				parse
//
//	Ingests a supplied buffer and parses the standard two CHDO fields
//  type & length.  By default, this routine allocates memory for an internal
//  buffer.  If the memory can not be allocated or the routine is instructed
//  by setting the "pointTo" flag (second parameter), the object will point
//  to the buffer passed in as the first parameter and return a non-zero
//   status value.
//	The option of "pointTo" is intended to be a quicker method of
//  ingesting a packet, although more DANGEROUS since the data could
//  disappear out from under the object.
//
//////////////////////////////////////////////////////////////////////////////

int	ChdoBase::parse(
  unsigned char	*buffer,
  int	pointTo )
{ int	Status = RTN_NORMAL;

  /**
  ***  Extract CHDO header information
  **/
  _type = buffer[0] * 256 + buffer[1];
  _length = buffer[2] * 256 + buffer[3];

  /**
  ***  Allocate memory for entire CHDO if needed
  **/
  if (_pointToFlag || _bufferSize < (_length+CHDO_HEADER_LENGTH))
  { if (!_pointToFlag && _buffer != NULL) delete [] _buffer;

    _bufferSize = ((_length+CHDO_HEADER_LENGTH) < DEFAULT_CHDO_SIZE) ?
                  DEFAULT_CHDO_SIZE : (_length+CHDO_HEADER_LENGTH);

    _buffer = new unsigned char [_bufferSize];
  }

  /**
  ***  If memory could not be added, point to memory that was passed in
  ***  be parsed, set return status flag as failure.
  **/
  if ((_pointToFlag = (_buffer == NULL)))
  { _buffer = buffer;
    _bufferSize = 0;
    Status = RTN_ALLOCATE_ERROR;
  } else memcpy(_buffer,buffer,(_length+CHDO_HEADER_LENGTH));

  _data = _buffer + 4;

  return Status;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo002.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo002.h
//
//	CHDO 2 is the Primary Header used for all SFOC-generated CHDO-type
//  SFDUs.  This class adds to the ChdoBase by adding the capability to obtain
//  the the Major & Minor SFDU types, Mission Id, and Format fields from the
//  record_id portion of the CHDO (Ref: SFOC-5-TIS-*DU-SFDU; SFOC0038-1100-05).
//
//////////////////////////////////////////////////////////////////////////////

#include "Chdo002.h"

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo010.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo_010.h
//
//	CHDO 10 is a general data CHDO used to transport data.  This class
//  adds to the ChdoBase by adding the capability to obtain the data
//  porion of the CHDO (Ref: SFOC-5-TIS-*DU-SFDU; SFOC0038-1100-05).
//
//////////////////////////////////////////////////////////////////////////////

#include "Chdo010.h"

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo082.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo_082.h
//
//	CHDO 82 is a Multi-Mission CHDO used as the Packet Telemetry Secondary
//  Header.  This class adds the capability to extract only the ERT and RCT
//  values from the CHDO.  Additional fields are available, but currently not
//  needed.  Consult the TMOD/AMMOS document: SFOC-5-TIS-*DU-MMSFDU for all
//  of the available fields and internal structure of CHDO 82.
//
//////////////////////////////////////////////////////////////////////////////

#include "Chdo082.h"
#include "SfocTime.h"


//////////////////////////////////////////////////////////////////////////////
//
//				extractErt
//
//	Extracts the Earth Receive Time from the CHDO header.  The time is
//  stored in a standard time format (Ref. SFOC-2-SYS-Any-TimeForms;
//  SFOC0038-02-25-05).
//
//////////////////////////////////////////////////////////////////////////////

SfocTime	Chdo_082::ert( void )
{ SfocTime	EarthReceiveTime;

  EarthReceiveTime.extract(&_data[CHDO082_ERT_OFFSET]);

  return (EarthReceiveTime);
}

//////////////////////////////////////////////////////////////////////////////
//
//				extractRct
//
//	Extracts the Record Creation Time from the CHDO header.  The time is
//  stored in a standard time format (Ref. SFOC-2-SYS-Any-TimeForms;
//  SFOC0038-02-25-05).
//
//////////////////////////////////////////////////////////////////////////////

SfocTime	Chdo_082::rct( void )
{ SfocTime	RecordCreationTime;

  RecordCreationTime.extract(&_data[CHDO082_RCT_OFFSET]);

  return (RecordCreationTime);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo128.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo128.h
//
//	CHDO 82 is a Mars '98/01 Mission CHDO used for the TIS Tertiary Packet
//  Header.  This class adds the capability to extract only the APID, SCLK,
//  SCET, and Packet Sequence Count values from the CHDO.  Additional fields
//  are available, but currently not needed.  Consult the TMOD/AMMOS document:
//  SFOC-5-TIS-*DU-M98SFDU for all of the available fields and insternal
//  structure of CHDO 82.
//
//////////////////////////////////////////////////////////////////////////////

#include "Chdo128.h"

//////////////////////////////////////////////////////////////////////////////
//
//				apid
//
//////////////////////////////////////////////////////////////////////////////

int	Chdo_128::apid( void )
{
  return (((int)(_data[0] & 0x07) * 256) + _data[1]);
}

//////////////////////////////////////////////////////////////////////////////
//
//				pktSeqCnt
//
//////////////////////////////////////////////////////////////////////////////

int	Chdo_128::pktSeqCnt( void )
{
  return (((int)_data[6] * 256) + _data[3]);
}

//////////////////////////////////////////////////////////////////////////////
//
//				sclk
//
//////////////////////////////////////////////////////////////////////////////

SclkTime	Chdo_128::sclk( void )
{ SclkTime	SpacecraftClock(&_data[10]);

  return (SpacecraftClock);
}

//////////////////////////////////////////////////////////////////////////////
//
//				scet
//
//////////////////////////////////////////////////////////////////////////////

SfocTime	Chdo_128::scet( void )
{ SfocTime	SpacecraftEventTime(&_data[16]);
  return (SpacecraftEventTime);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfocTime.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SclkTime.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create chdo_object.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE chdo_object
/*
/*   To Create the build file give the command:
/*
/*		$ vimake chdo_object			(VMS)
/*   or
/*		% vimake chdo_object			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE chdo_object		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST SfocTime.cc SclkTime.cc ChdoBase.cc Chdo082.cc Chdo128.cc
/* 002 and 010 define no code (just an #include).  Why do they even exist? */
/* Removed from compile rgd 2/15/07 because they were annoying the mac build. */
#define CLEAN_OTHER_LIST Chdo002.cc Chdo010.cc



#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of chdo_object imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_chdo_base.cc
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
$!-----------------------------------------------------------------------------
$ create tst_chdo_base.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_chdo_base
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_chdo_base			(VMS)
/*   or
/*		% vimake tst_chdo_base			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_chdo_base		/* Only one of these */
/*#define PROCEDURE tst_chdo_base		/* Only one of these */
/*#define SCRIPT tst_chdo_base		/* Only one of these */
#define PROGRAM tst_chdo_base		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_chdo_base.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
/**/
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_chdo_base imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_sfoc_time.cc
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/**  History:                                                           **/
/**  6-16-1998  T. Nguyen  For Y2K task,  added test cases for testing  **/
/**                        time routines.                               **/
/*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SfocTime.h"
#include "rts_time.h"
#include "return_status.h"

#define  MAX_RANGE_YEARS	180
#define  MS_PER_DAY		86400000
#define  DAYS_PER_LEAP		366
#define  DAYS_PER_YEAR		365

void ingest_test(int);
void inverse_test(int);
void specific_test(int);
void test_dates(char *, int);

main()
{ int	lc,
	NewEpoch,
	Year,
	Status; 
  char	utc[24];
  SfocTime	PlayThing;

  for (Year=DEFAULT_SFOC_EPOCH; Year<=DEFAULT_SFOC_EPOCH+30; Year+=9)
  {
    printf("\n*********************************************");
    printf("\n*********************************************");
    printf("\n***                                       ***");
    printf("\n***     Checks for Epoch Year: %-4d       ***",Year);
    printf("\n***                                       ***");
    printf("\n*********************************************");
    printf("\n*********************************************\n");

    ingest_test(Year);

    specific_test(Year);

    inverse_test(Year);
  }


  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***        Epoch Conversion Checks        ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");


  for (lc=1999; lc<2009; lc++)
  { for(Year=1; Year<45; Year+=5)
    { PlayThing.changeEpoch(lc,0);
      sprintf(utc,"%4d-04-15T00:00:00.000",PlayThing.epoch());
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }

      PlayThing.changeEpoch(PlayThing.epoch()-Year);
      if (strcmp(utc,PlayThing.formatted(0)) != 0)
      { printf("\n*** Failure: %s -(%d)-> %s\n",
                utc,PlayThing.days(),PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
//      printf("%s -[%d]-> %s (%d)\n\n",utc,PlayThing.epoch(),
//             PlayThing.formatted(0),PlayThing.days());
    }
    if (RTN_SUCCESS(Status))
    { printf("\nVerified EPOCH conversion from %d to ...\n\t",lc);
      for(Year=1; Year<45; Year+=5) printf("  %04d",(lc-Year));
      printf("\n");
    }
  }

  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***        Misc Capability Checks         ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");

  PlayThing.changeEpoch(DEFAULT_SFOC_EPOCH,0);

  printf("\n*** Increment Time Values\n");
  strcpy(utc,rts_utc_time());
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  } else
  { printf("     Current UTC: %s \n",utc);
    PlayThing.incrementDays(1);
    printf("     Added 1 Day: %s\n",PlayThing.formatted());
    PlayThing.incrementMilliSeconds(MS_PER_DAY/2);
    printf("   Added 1/2 Day: %s\n",PlayThing.formatted());
    PlayThing.incrementMilliSeconds(500);
    printf("Added 1/2 Second: %s\n",PlayThing.formatted());
  }

  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***      Error Identification Checks      ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");


  PlayThing.changeEpoch(DEFAULT_SFOC_EPOCH,0);

  strcpy(utc,rts_utc_time());
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  } else
  { Status = PlayThing.incrementDays(-(PlayThing.days()+10));
    if (RTN_FAILURE(Status))
    { printf("\nDecrementing: %s\n",RTN_DFLT_MSG(Status));
      printf("\t%s\n",PlayThing.errorMessage());
      printf("\t%s\n",utc);
    }
  }

  sprintf(utc,"%4d-13-01T00:00:00.000",PlayThing.epoch()+34);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-02-29T00:00:00.000",PlayThing.epoch()+45);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-429T00:00:00.000",PlayThing.epoch()+50);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-029T00:00:00.000",PlayThing.epoch()-5);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-10-31T00:00:00.000",PlayThing.epoch()+MAX_RANGE_YEARS);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  PlayThing.changeEpoch(DEFAULT_SFOC_EPOCH,0);
  sprintf(utc,"%4d-07-04T00:00:00.000",PlayThing.epoch()+10);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }
  NewEpoch = PlayThing.epoch() + 10;
  Status = PlayThing.changeEpoch(NewEpoch);
  if (RTN_FAILURE(Status))
  { printf("\nEPOCH Change: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%d -> %d for %s\n",PlayThing.epoch(),NewEpoch,utc);
  }

  PlayThing.changeEpoch(DEFAULT_SFOC_EPOCH,0);
  sprintf(utc,"%4d-11-11T00:00:00.000",PlayThing.epoch()+10);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }
  NewEpoch = PlayThing.epoch() - MAX_RANGE_YEARS;
  Status = PlayThing.changeEpoch(NewEpoch);
  if (RTN_FAILURE(Status))
  { printf("\nEPOCH Change: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%d -> %d for %s\n",PlayThing.epoch(),NewEpoch,utc);
  }




  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				inverse_test
//
//////////////////////////////////////////////////////////////////////////////

void	inverse_test(
  int	NewEpoch)
{ int	lc,
	Tdays,
	Ydays,
	Year,
	LeapYear,
	Status; 
  char	utc[24];
  SfocTime	PlayThing(NewEpoch);

  printf("\n***\n***  Iterative Inverse Checks (%d)\n***\n\n",NewEpoch);

  sprintf(utc,"%4d-01-01T00:00:00.000",NewEpoch);
  printf("Start Date: %s  ... incrementing by whole days\n",utc);

  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",utc);
    return;
  }

  if (PlayThing.days() != 0 || PlayThing.milliSeconds() != 0)
  { printf("EPOCH check failed: %s  (%d, %d)\n",
           utc,PlayThing.days(),PlayThing.milliSeconds());
    return;
  }

  Year = NewEpoch;
  Ydays = 0;
  Tdays = 0;
  do
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    Ydays = (LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR);

    for (lc = 1; lc<=Ydays; lc++)
    { sprintf(utc,"%04d-%03dT00:00:00.000",Year,lc);
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }
      if (strcmp(utc,PlayThing.formatted(1)) != 0)
      { printf("*** Failure: %s -(%d)-> %s\n",
                utc,PlayThing.days(),PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
//printf("%s vs %s (%d)\n",utc,PlayThing.formatted(1),PlayThing.days());
//      PlayThing.incrementDays();

      Tdays++;
    }
//printf("%s (%d)\n",PlayThing.formatted(0),PlayThing.days());
    if (RTN_FAILURE(Status)) break;
    Year++;
  } while (Year < NewEpoch+MAX_RANGE_YEARS);

  if (RTN_FAILURE(Status))
     printf("Final Date: %d-%03d (%d) vs %s (%d)\n\n",
            Year,lc,Tdays,PlayThing.formatted(1),PlayThing.days());
  else  printf("Final Date: %d-%03d (%d) vs %s (%d)\n\n",
               (Year-1),(lc-1),(Tdays-1),PlayThing.formatted(1),
               PlayThing.days());

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				ingest_test
//
//////////////////////////////////////////////////////////////////////////////

void	ingest_test(
  int	NewEpoch)
{ int	lc,
	Tdays,
	Ydays,
	Year,
	LeapYear,
	Status; 
  char	utc[24];
  SfocTime	PlayThing(NewEpoch);

  printf("\n***\n***  Time Ingest Checks (%d)\n***\n",NewEpoch);

  Status = RTN_NORMAL;
  Year = PlayThing.epoch();
  Ydays = 0;
  Tdays = 0;
  do
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    Ydays = (LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR);
    for (lc = 1; lc<=Ydays; lc++)
    { sprintf(utc,"%04d-%03dT00:00:00.000",Year,lc);
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }
      if (Tdays != PlayThing.days())
      { printf("\n***Failure: %s -(%d vs %d)-> %s\n",
               utc,Tdays,PlayThing.days(),PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
      Tdays++;
    }
    if (RTN_FAILURE(Status)) break;
    Year++;
  } while (Year < PlayThing.epoch()+MAX_RANGE_YEARS);

  printf("\nChecked: %d -> %d  Total Days: %d(control) vs %d(test)\n\n",
         PlayThing.epoch(),Year,(Tdays-1),PlayThing.days());

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				specific_test
//
//////////////////////////////////////////////////////////////////////////////

void	specific_test(
  int	NewEpoch)
{ char	utc[24];

  printf("\n***\n***  Specific Date Checks (%d)\n***\n",NewEpoch);

  strcpy(utc,rts_utc_time());
  printf("\n Current UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2000-01-01T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2000-02-28T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2000-02-29T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2000-03-01T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2000-12-30T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2000-12-31T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2009-12-31T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2009-12-30T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2100-12-31T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2101-01-01T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  strcpy(utc,"2101-01-02T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc,NewEpoch);

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				test_dates
//
//////////////////////////////////////////////////////////////////////////////

void test_dates(
  char *utcTime,
  int	NewEpoch)
{ int	Status;
  char		TimeBufs[2][128];
  SfocTime	PlayThings[2];

  PlayThings[0].changeEpoch(NewEpoch,0);
  PlayThings[1].changeEpoch(NewEpoch,0);

  strcpy(TimeBufs[0],utcTime);
  Status = PlayThings[0].ingestUTC(TimeBufs[0]);
  if (RTN_FAILURE(Status))
  { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThings[0].errorMessage());
    return;
  }

  printf("  Julian UTC: %s    (%u, %u)\n",
         PlayThings[0].formatted(1),
         PlayThings[0].days(),PlayThings[0].milliSeconds());

  PlayThings[1] = PlayThings[0];
  PlayThings[1].incrementDays();
  printf("Tomorrow UTC: %s  (%d, %d)\n",
         PlayThings[1].formatted(0),
         PlayThings[1].days(),PlayThings[1].milliSeconds());

  if (PlayThings[0] > PlayThings[1])
     printf("Today follows Tomorrow ... wrong\n");
  else if (PlayThings[0] == PlayThings[1])
     printf("Today is the same as Tomorrow ... it only seems that way\n");
  else printf("Tomorrow follows Today\n");

  return;
}

$!-----------------------------------------------------------------------------
$ create tst_sclk_time.cc
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/**  History:                                                           **/
/**  6-16-1998  T. Nguyen  For Y2K task,  added test cases for testing  **/
/**                        time routines.                               **/
/*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SfocTime.h"
#include "SclkTime.h"
#include "rts_time.h"
#include "return_status.h"

#define  MAX_RANGE_YEARS        136
#define  DAYS_PER_LEAP          366
#define  DAYS_PER_YEAR          365
#define  SECONDS_PER_DAY        86400


void	ingest_test(int);
void	inverse_test(int);
void	specific_test(int);
void	test_clock(char *, int);

main()
{ int	lc,
	NewEpoch,
	Status,
	Year;
  char	utc[24];
  SclkTime	PlayThing(1980);


  for (Year=DEFAULT_SCLK_EPOCH; Year<=DEFAULT_SCLK_EPOCH+30; Year+=9)
  {
    printf("\n*********************************************");
    printf("\n*********************************************");
    printf("\n***                                       ***");
    printf("\n***     Checks for Epoch Year: %-4d       ***",Year);
    printf("\n***                                       ***");
    printf("\n*********************************************");
    printf("\n*********************************************\n");

    ingest_test(Year);

    specific_test(Year);

    inverse_test(Year);

  }


  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***        Epoch Conversion Checks        ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");

  for (lc=1999; lc<2009; lc++)
  { for (Year=1; Year<45; Year+=5)
    { PlayThing.changeEpoch(lc,0);
      sprintf(utc,"%4d-01-01T00:00:00.000",PlayThing.epoch());
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }

      PlayThing.changeEpoch(PlayThing.epoch()-Year);
      if (strcmp(utc,PlayThing.formatted(0)) != 0)
      { printf("\n*** Failure: %s -(%u)-> %s\n",
                utc,PlayThing.seconds(),PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }

//      printf("%s -[%d]-> %s (%u)\n\n",
//             utc,PlayThing.epoch(),PlayThing.formatted(0),
//             PlayThing.seconds());
    }
    if (RTN_SUCCESS(Status))
    { printf("\nVerified EPOCH conversion from %d to ...\n\t",lc);
      for(Year=1; Year<45; Year+=5) printf("  %04d",(lc-Year));
      printf("\n");
    }
  }

  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***        Misc Capability Checks         ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");

  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);

  printf("\n*** Increment Time Values\n");
  strcpy(utc,rts_utc_time());
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  } else
  { printf("     Current UTC: %s \n",utc);
    PlayThing.incrementSeconds(SECONDS_PER_DAY);
    printf("     Added 1 Day: %s\n",PlayThing.formatted());
    PlayThing.incrementPartialSeconds(128);
    printf("Added 1/2 Second: %s\n",PlayThing.formatted());
  }


  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***      Error Identification Checks      ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");


  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);

  strcpy(utc,rts_utc_time());
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  } else
  { Status = PlayThing.incrementSeconds(-(PlayThing.seconds()+10));
    if (RTN_FAILURE(Status))
    { printf("\nDecrementing: %s\n",RTN_DFLT_MSG(Status));
      printf("\t%s\n",PlayThing.errorMessage());
      printf("\t%s\n",utc);
    }
  }


  sprintf(utc,"%4d-13-01T00:00:00.000",PlayThing.epoch()+34);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-02-29T00:00:00.000",PlayThing.epoch()+45);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-429T00:00:00.000",PlayThing.epoch()+50);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-029T00:00:00.000",PlayThing.epoch()-5);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-10-31T00:00:00.000",PlayThing.epoch()+MAX_RANGE_YEARS);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);
  sprintf(utc,"%4d-07-04T00:00:00.000",PlayThing.epoch()+10);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }
  NewEpoch = PlayThing.epoch() + 10;
  Status = PlayThing.changeEpoch(NewEpoch);
  if (RTN_FAILURE(Status))
  { printf("\nEPOCH Change: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%d -> %d for %s\n",PlayThing.epoch(),NewEpoch,utc);
  }

  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);
  sprintf(utc,"%4d-11-11T00:00:00.000",PlayThing.epoch()+10);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }
  NewEpoch = PlayThing.epoch() - MAX_RANGE_YEARS;
  Status = PlayThing.changeEpoch(NewEpoch);
  if (RTN_FAILURE(Status))
  { printf("\nEPOCH Change: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%d -> %d for %s\n",PlayThing.epoch(),NewEpoch,utc);
  }

  return RTN_NORMAL;
}
//////////////////////////////////////////////////////////////////////////////
//
//				specific_test
//
//////////////////////////////////////////////////////////////////////////////

void	specific_test(
  int	NewEpoch)
{ char	utc[24];

  printf("\n***\n***  Specific Time and Increment Checks %s\n***\n",
         "- expect millisecond round-off errors");

  strcpy(utc,rts_utc_time());
  printf(" Current UTC: %s \n",utc);
  test_clock(utc,NewEpoch);

  if (NewEpoch < 2000)
  { strcpy(utc,"2000-01-01T00:00:00.001");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-02-28T00:00:00.010");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-02-29T00:00:00.100");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-03-01T00:00:00.020");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-12-30T00:00:00.200");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-12-31T00:00:00.005");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);
  }

  if (NewEpoch < 2009)
  { strcpy(utc,"2009-12-31T00:00:00.500");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2009-12-30T00:00:00.999");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);
  }

  if (NewEpoch < 2100)
  { strcpy(utc,"2100-12-31T00:00:00.000");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2101-01-01T00:00:00.000");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2101-01-02T00:00:00.000");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);
  }

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//                              inverse_test
//
//////////////////////////////////////////////////////////////////////////////

void	inverse_test(
  int	NewEpoch)
{ int	Ydays,
	Year,
	LeapYear,
	Status;
  unsigned int	Tseconds;
  char	utc[24];
  SclkTime	PlayThing(NewEpoch);

  printf("\n***\n***  Iterative Inverse Checks (%d)\n***\n\n",NewEpoch);

  sprintf(utc,"%4d-01-01T00:00:00.000",NewEpoch);
  printf("Start Date: %s  ... incrementing by whole days\n",utc);

  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",utc);
    return;
  }


  Tseconds = 0;
  for (Year=NewEpoch; Year<(NewEpoch+MAX_RANGE_YEARS); Year++)
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    for (Ydays=1; Ydays<=(LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR); Ydays++)
    { sprintf(utc,"%4d-%03dT00:00:00.000",Year,Ydays);
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }

      if (Tseconds != PlayThing.seconds())
      { printf("(%d) %s %u != %u\n\n",NewEpoch,utc,Tseconds,PlayThing.seconds());
        Status = RTN_ERROR;
        break;
      } else if (strcmp(utc,PlayThing.formatted(1)) != 0)
      { printf("*** Failure: %s -(%d)-> %s\n",
               utc,NewEpoch,PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
      Tseconds += SECONDS_PER_DAY;
    }
    if (RTN_FAILURE(Status)) break;
  }
  
  if (RTN_FAILURE(Status))
     printf("Final Date: %d-%03d (%u) vs %s (%u)\n\n",
            Year,Ydays,Tseconds,PlayThing.formatted(1),PlayThing.seconds());
  else  printf("Final Date: %d-%03d (%u) vs %s (%u)\n\n",
               (Year-1),(Ydays-1),(Tseconds-SECONDS_PER_DAY),
               PlayThing.formatted(1),PlayThing.seconds());


  return;
}


//////////////////////////////////////////////////////////////////////////////
//
//				ingest_test
//
//////////////////////////////////////////////////////////////////////////////

void	ingest_test(
  int	NewEpoch)
{ int	lc,
	Tdays,
	Ydays,
	Year,
	LeapYear,
	Status;
  char	utc[24];
  SclkTime	PlayThing(NewEpoch);

  printf("\n***\n***  Time Ingest Checks (%d)\n***\n",NewEpoch);

  Status = RTN_NORMAL;
  Year = PlayThing.epoch();
  Ydays = 0;
  Tdays = 0;
  do
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    Ydays = (LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR);
    for (lc = 1; lc<=Ydays; lc++)
    { sprintf(utc,"%04d-%03dT00:00:00.000",Year,lc);
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }
      if ((Tdays*SECONDS_PER_DAY) != PlayThing.seconds())
      { printf("\n***Failure: %s -(%u vs %u)-> %s\n",
               utc,Tdays*SECONDS_PER_DAY,PlayThing.seconds(),
               PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
      Tdays++;
    }
    if (RTN_FAILURE(Status)) break;
    Year++;
  } while (Year < PlayThing.epoch()+MAX_RANGE_YEARS);

  printf("\nChecked: %d -> %d  Total Seconds: %u(control) vs %u(test)\n\n",
         PlayThing.epoch(),Year,(Tdays-1)*SECONDS_PER_DAY,PlayThing.seconds());

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				test_dates
//
//////////////////////////////////////////////////////////////////////////////

void test_clock(
  char *utcTime,
  int	NewEpoch)
{ int	Status;
  char		TimeBufs[2][128];
  SclkTime	PlayThings[2];

  PlayThings[0].changeEpoch(NewEpoch,0);
  PlayThings[1].changeEpoch(NewEpoch,0);

  strcpy(TimeBufs[0],utcTime);

  Status = PlayThings[0].ingestUTC(TimeBufs[0]);
  if (RTN_FAILURE(Status))
  { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n\n",PlayThings[0].errorMessage());
    return;
  }

  printf("  Julian UTC: %s    (%u.%03d)\n",
         PlayThings[0].formatted(1),
         PlayThings[0].seconds(),PlayThings[0].partialSeconds());

  PlayThings[1] = PlayThings[0];
  PlayThings[1].incrementSeconds(3600*24);
  printf("Tomorrow UTC: %s  (%u.%03d)\n",
         PlayThings[1].formatted(0),
         PlayThings[1].seconds(),PlayThings[1].partialSeconds());

  if (PlayThings[0] > PlayThings[1])
     printf("Today follows Tomorrow ... *** wrong ***\n\n");
  else if (PlayThings[0] == PlayThings[1])
     printf("Today is the same as Tomorrow ... *** wrong ***n\n");
  else printf("Tomorrow follows Today\n\n");

  return;
}

$!-----------------------------------------------------------------------------
$ create tst_sfoc_time.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_sfoc_time
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_sfoc_time			(VMS)
/*   or
/*		% vimake tst_sfoc_time			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_sfoc_time		/* Only one of these */
/*#define PROCEDURE tst_sfoc_time		/* Only one of these */
/*#define SCRIPT tst_sfoc_time		/* Only one of these */
#define PROGRAM tst_sfoc_time		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_sfoc_time.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
/**/
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_sfoc_time imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_sclk_time.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_sclk_time
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_sclk_time			(VMS)
/*   or
/*		% vimake tst_sclk_time			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_sclk_time		/* Only one of these */
/*#define PROCEDURE tst_sclk_time		/* Only one of these */
/*#define SCRIPT tst_sclk_time		/* Only one of these */
#define PROGRAM tst_sclk_time		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_sclk_time.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
/**/
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_sclk_time imake file  **********/
$!-----------------------------------------------------------------------------
$ create README_tst
There are 3 test programs included for this module:
	tst_chdo_base
	tst_sclk_time
    &	tst_sfoc_time

The two time programs (sclk & sfoc) are self contained and just need to be
executed.  They run through checks for most of the available capabilities
and error detection code.  They are success oriented and will notify on
errors.  One quick note regarding the time routines, they work correctly
up until December 31, 2100, after that they start failing.  The tests go
beyond that date, so expect errors at that point.

The CHDO test program is a simple SFDU parser that extracts the basic
components of the CHDO headers from any SFDU.  It will identify all CHDOs
it parses and its length.  If the SFDU file continas CHDOs 82 & 128, a few
fields from each CHDO will be displayed.  No SFDU file is provided, as any
will work.  Bear in mind that there are typically 5 CHDOs per SFDU record,
so the output generated from this program can be large.  The program takes
one arguement, the SFDU file name.
$ Return
$!#############################################################################
