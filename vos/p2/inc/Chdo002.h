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
