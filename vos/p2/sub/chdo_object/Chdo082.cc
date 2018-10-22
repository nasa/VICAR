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
