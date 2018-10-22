/*
 * VicarBinaryHeader.cc
 *
 *  Created on: Oct 5, 2011
 *      Author: honghanh
 */

extern "C"
{
#include "applic.h"
#include "zvproto.h"
}

#include "VicarBinaryHeader.h"
#include <cstring>

void jpl::mipl::p2::VicarBinaryHeader::cleanup_()
{
	if (this->header != NULL)
	{
		delete [] this->header;
		this->header = NULL;
	}
	this->size_ = 0;
}

void jpl::mipl::p2::VicarBinaryHeader::clone_(const VicarBinaryHeader& bh)
{
	this->size_ = bh.size_;
	this->header = new unsigned char[this->size_];
	memcpy(this->header, bh.header, this->size_);
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      Constructor                                                    *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    October 10, 2011                                        *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      10-10-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * The default constructor creates an empty binary header object.      *
 *                                                                     *
 ***********************************************************************/
jpl::mipl::p2::VicarBinaryHeader::VicarBinaryHeader()
{
	this->size_ = 0;
	this->header = NULL;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      Constructor                                                    *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    October 10, 2011                                        *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      10-10-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * The constructor reads a VICAR product, and initializes the binary   *
 * header.                                                             *
 *                                                                     *
 ***********************************************************************/
jpl::mipl::p2::VicarBinaryHeader::VicarBinaryHeader(int unitNo)
throw(jpl::mipl::p2::VicarException)
{
	int nl, ns, nlb, nbb, pixSize;

	zvget(unitNo,(char*)"NL",&nl,(char*)"NS",&ns,(char*)"NLB",
			&nlb,(char*)"NBB",&nbb,"PIX_SIZE",&pixSize,(char*)NULL);

	this->size_ = nlb * (nbb + ns) * pixSize;
	this->header = new unsigned char[this->size_];
	memset(this->header, 0, this->size_);

	int offset = (nbb + ns) * pixSize;
	int status;

	for (int j = 0; j < nlb; j++)
	{
		// LINE is 1-based, NLINES is not yet implemented
		status = zvread(unitNo, this->header + j * offset, "LINE", j+1, NULL);
		if (status != SUCCESS)
			throw jpl::mipl::p2::VicarException("Error obtaining binary header");
	}
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      Constructor                                                    *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    October 10, 2011                                        *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      10-10-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * The copy constructor makes a deep copy of every field in the binary *
 * header.                                                             *
 *                                                                     *
 ***********************************************************************/
jpl::mipl::p2::VicarBinaryHeader::VicarBinaryHeader(const VicarBinaryHeader& bh)
{
	this->clone_(bh);
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      Constructor                                                    *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    October 10, 2011                                        *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      10-10-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * The destructor deallocates any field that the constructors          *
 * allocated.                                                          *
 *                                                                     *
 ***********************************************************************/
jpl::mipl::p2::VicarBinaryHeader::~VicarBinaryHeader()
{
	this->cleanup_();
}

jpl::mipl::p2::VicarBinaryHeader& jpl::mipl::p2::VicarBinaryHeader::operator= (const VicarBinaryHeader& bh)
{
	if (this != &bh)
	{
		this->cleanup_();
		this->clone_(bh);
	}
	return *this;
}
