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
