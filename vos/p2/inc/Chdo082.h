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
