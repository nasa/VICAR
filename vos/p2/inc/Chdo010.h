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
