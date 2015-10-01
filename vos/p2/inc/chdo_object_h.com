$!****************************************************************************
$!
$! Build proc for MIPL module chdo_object_h
$! VPACK Version 1.9, Monday, July 24, 2000, 11:28:01
$!
$! Execute by entering:		$ @chdo_object_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module chdo_object_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to chdo_object_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("chdo_object_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @chdo_object_h.bld "STD"
$   else
$      @chdo_object_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create chdo_object_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack chdo_object_h.com -mixed -
	-s ChdoBase.h Chdo002.h Chdo010.h Chdo082.h Chdo128.h SfocTime.h -
	   SclkTime.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ChdoBase.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CHDO_HEADER_CLASS_H
#define CHDO_HEADER_CLASS_H

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

#include <stddef.h>
#include <string.h>

#define  CHDO_HEADER_LENGTH	4
#define  DEFAULT_CHDO_SIZE	1024


class	ChdoBase {

  protected:
	int		_type;		// Type of CHDO
	int		_length;	// Length of data portion of CHDO
	int		_bufferSize;	// Size of object's buffer
	int		_pointToFlag;	// Identifies if buffer is pointing to
					// memory outside of object
	unsigned char	*_buffer;	// Contains entire CHDO
	unsigned char	*_data;		// Points to the data portion of CHDO

	void		initialize( void )
			{ _type = _length = _bufferSize = _pointToFlag = 0;
			  _buffer = _data = NULL;
			}

  public:
	int		parse( unsigned char *buffer, int pointTo = 0 );
	//  Ingests a supplied buffer and parses the standard two CHDO fields
	//  type & length.
	//  By default, this routine allocates memory for an internal buffer.
	//  If the memory can not be allocated or the routine is instructed by
	//  setting the "pointTo" flag (second parameter), the object will
	//  point to the buffer passed in as the first parameter and return a
	//  non-zero status value.
	//  The option of "pointTo" is intended to be a quicker method of
	//  ingesting a packet, although more DANGEROUS since the data could
	//  disappear out from under the object.

	int		type( void ) { return (_type); }
	int		length( void ) { return (_length); }
	//  Returns values for the header fields of this CHDO 

	int		headerLength( void ) { return (CHDO_HEADER_LENGTH); }
	//  Utility functional to return the standard CHDO header length.
	//  This length is not included by the standard length field of a CHDO

	// Default Constructor
	ChdoBase ( unsigned char *buffer = NULL )
		{ initialize();
		  if (buffer != NULL) parse(buffer);
		  return;
		}

	// Copy Constructor
	ChdoBase ( const ChdoBase &chdo )
		{ _pointToFlag = 1;
		  _type = chdo._type;
		  _length = chdo._length;
		  _bufferSize = chdo._bufferSize;
		  _buffer = chdo._buffer;
		  _data = chdo._data;

		  return;
		}

	// Assignment Constructor
	ChdoBase &operator= ( const ChdoBase &chdo )
		{ if (this == &chdo) return *this;
		  if (chdo._buffer != NULL) parse(chdo._buffer);
		  return *this;
		}

	// Destructor
	virtual ~ChdoBase ( void )
		{ if (!_pointToFlag && _buffer != NULL)
		     delete [] _buffer;
		  return;
		}
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo002.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CHDO_002_CLASS_H
#define CHDO_002_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo_002.h
//
//	CHDO 2 is the Primary Header used for all SFOC-generated CHDO-type
//  SFDUs.  This class adds to the ChdoBase by adding the capability to obtain
//  the the Major & Minor SFDU types, Mission Id, and Format fields from the
//  record_id portion of the CHDO.
//  Reference TMOD/AMMOS document: SFOC-5-TIS-*DU-SFDU; SFOC0038-1100-05.
//
//////////////////////////////////////////////////////////////////////////////

#include "ChdoBase.h"

#define  CHDO002_MAJOR_OFFSET	0
#define  CHDO002_MINOR_OFFSET	1
#define  CHDO002_MISSION_OFFSET	2
#define  CHDO002_FORMAT_OFFSET	3

class	Chdo_002 : public ChdoBase {

  public:
	int	majorType( void )
		// Returns the SFDU major type for SFDU data categorization
		{ if (_data != NULL)
                     return (int)_data[CHDO002_MAJOR_OFFSET];
                  else return -1;
                }

	int	minorType( void )
		// Returns the SFDU minor type for SFDU data categorization
		{ if (_data != NULL)
                     return (int)_data[CHDO002_MINOR_OFFSET];
                  else return -1;
                }

	int	missionId( void )
		// Returns the mission identifier code
		{ if (_data != NULL)
                     return (int)_data[CHDO002_MISSION_OFFSET];
                  else return -1;
                }

	int	formatType( void )
		// Returns the SFDU format type; used in conjunction with
		// the Major & Minor Types to specifically define data types
		{ if (_data != NULL)
                     return (int)_data[CHDO002_FORMAT_OFFSET];
                  else return -1;
                }

	// Default Constructor
	Chdo_002 ( unsigned char *buffer = NULL )
		{
		  if (buffer != NULL) parse(buffer);
		  return;
		}

	//  Assignment Constructor
	Chdo_002 &operator= ( const Chdo_002 &chdo)
		{ if (this == &chdo) return *this;
		  if (chdo._buffer == NULL) parse(chdo._buffer);
		  return *this;
		}

	// Destructor
	~Chdo_002 ( void ) { }

  protected:

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo010.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CHDO_010_CLASS_H
#define CHDO_010_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo_010.h
//
//	CHDO 10 is a general data CHDO used to transport data.  This class
//  adds to the ChdoBase by adding the capability to obtain the data
//  porion of the CHDO.
//  Reference SFOC-5-TIS-*DU-SFDU; SFOC0038-1100-05.
//
//////////////////////////////////////////////////////////////////////////////

#include "ChdoBase.h"

class	Chdo_010 : public ChdoBase {

  public:
	const	unsigned char	*data( void ) { return (_data); };
		// Returns a pointer to the data portion of the CHDO,
		// typically the data of importance to the application

	// Default Constructor
	Chdo_010 ( unsigned char *buffer = NULL )
		{
		  if (buffer != NULL) parse(buffer);
		  return;
		}

	// Destructor
	~Chdo_010 ( void ) { }

  protected:

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo082.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CHDO_082_CLASS_H
#define CHDO_082_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo_082.h
//
//      CHDO 82 is a Multi-Mission CHDO used as the Packet Telemetry Secondary
//  Header.  This class adds the capability to extract only the ERT and RCT
//  values from the CHDO.  Additional fields are available, but currently not
//  needed.  Consult the TMOD/AMMOS document: SFOC-5-TIS-*DU-MMSFDU for all
//  of the available fields and internal structure of CHDO 82.
//
//////////////////////////////////////////////////////////////////////////////

#include "ChdoBase.h"
#include "SfocTime.h"

#define  CHDO082_ERT_OFFSET	6
#define  CHDO082_RCT_OFFSET	60

class	Chdo_082 : public ChdoBase {

  public:
	SfocTime	ert( void );
			// Returns the Earth Receive Time associated with
			// the data from this SFDU

	SfocTime	rct( void );
			// Returns the Record Creatrion Time associated with
			// the data from this SFDU

	// Default Constructor
	Chdo_082 ( unsigned char *buffer = NULL )
		{ if (buffer != NULL) parse(buffer);
		  return;
		}

	// Destructor
	~Chdo_082 ( void ) { }

  protected:

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Chdo128.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CHDO_128_CLASS_H
#define CHDO_128_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				Chdo_128.h
//
//	CHDO 82 is a Mars '98/01 Mission CHDO used for the TIS Tertiary Packet
//  Header.  This class adds the capability to extract only the APID, SCLK,
//  SCET, and Packet Sequence Count values from the CHDO.  Additional fields
//  are available, but currently not needed.  Consult the TMOD/AMMOS document:
//  SFOC-5-TIS-*DU-M98SFDU for all of the available fields and insternal
//  structure of CHDO 82.
//
//////////////////////////////////////////////////////////////////////////////

#include "ChdoBase.h"
#include "SfocTime.h"
#include "SclkTime.h"

class	Chdo_128 : public ChdoBase {

  public:
	int	apid( void );
		// Returns the Application Packet ID from the CHDO label

	int	pktSeqCnt( void );
		// Returns the Packet Sequence number (module 16384) from
		// the CHDO label

	SclkTime	sclk( void );
		// Returns Spacecraft Clock from the CHDO label.  The
		// Clock

	SfocTime	scet( void );

	// Default Constructor
	Chdo_128 ( unsigned char *buffer = NULL )
		{
		  if (buffer != NULL) parse(buffer);
		  return;
		}

	// Destructor
	~Chdo_128 ( void ) { }

  protected:

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfocTime.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SclkTime.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
