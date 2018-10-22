/*
  Copyright 2011-Present
  California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledged. 09-08-2011.

  Auther : Rajesh R. Patel

  History :
  ---------

  Date              Who                        What
  -------------------------------------------------------------------------------
  09/08/2011        Raj       Initial Release

 */
#ifndef __VICAR_IMAGE_UTIL_H__
#define __VICAR_IMAGE_UTIL_H__

#include "applic.h"
#include "zvproto.h"
#include <stdarg.h>
#include <string>
#include <vector>
#include <map>
//using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{
namespace difpic
{
class VicarImageUtil
{
public:
	static std::string getVersion();

	static bool contains(const std::vector<std::string> &v,
			const std::string &element);
	static void clone(const std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &from,
			std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &to);
	static void clone(const std::map < std::string, std::vector < std::string >* > &from,
			std::map < std::string, std::vector < std::string >* > &to);
	static void formatDMsg(const std::string &label,
			const std::string &rval,
			const std::string &lval,
			std::string &outMsg);
	static void formatHdrMsg(const std::string &hdr,
			std::string &outMsg);
	static bool processExtraRHLabels(std::map < std::string, std::vector < std::string >* >& labels,
			const std::vector <std::string> &labelsToIgnore,
			std::string & differences);
	static bool processExtraRHProperties(std::map < std::string,
			std::map < std::string, std::vector < std::string >* >* > &labels,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string & differences);
	static std::string getValue(const std::vector <std::string >& v);
private:
};
};
};
};
};
#endif
