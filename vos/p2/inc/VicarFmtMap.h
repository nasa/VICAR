/*
 * VicarFmtMap.h
 *
 *  Created on: Oct 5, 2011
 *      Author: honghanh
 */

#ifndef VICARFMTMAP_H_
#define VICARFMTMAP_H_

#include <iostream>
#include <map>

namespace jpl
{
namespace mipl
{
namespace p2
{
class VicarFmtMap
{
	std::map<std::string, std::string> fmtMap_;

public:
	VicarFmtMap () {}
	VicarFmtMap(const std::string & filename) throw (std::exception);
	~VicarFmtMap() {}

	bool getFmt (const std::string & bltype, std::string & fmt);
};
}
}
}

#endif /* VICARFMTMAP_H_ */
