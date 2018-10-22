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

