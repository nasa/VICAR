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
