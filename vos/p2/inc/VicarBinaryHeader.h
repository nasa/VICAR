/*
 * VicarBinaryHeader.h
 *
 *  Created on: Oct 5, 2011
 *      Author: honghanh
 */

#ifndef VICARBINARYHEADER_H_
#define VICARBINARYHEADER_H_

#include "VicarException.h"

namespace jpl
{
namespace mipl
{
namespace p2
{
class VicarBinaryHeader
{
private:
	void cleanup_();
	void clone_(const VicarBinaryHeader& bh);

protected:
	size_t size_;
	unsigned char* header;

public:
	VicarBinaryHeader();

	VicarBinaryHeader(int unitNo) throw (jpl::mipl::p2::VicarException);

	VicarBinaryHeader(const VicarBinaryHeader& bh);

	virtual ~VicarBinaryHeader();

	size_t getSize() const { return this->size_; }

	const unsigned char* getHeader() const { return this->header; }

	VicarBinaryHeader& operator= (const VicarBinaryHeader& bh);
};
}
}
}

#endif /* VICARBINARYHEADER_H_ */
