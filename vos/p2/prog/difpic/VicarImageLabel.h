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
#ifndef __VICAR_IMAGE_LABEL_H__
#define __VICAR_IMAGE_LABEL_H__

extern "C"
{
#include "applic.h"
#include "zvproto.h"
}
#include "VicarException.h"
#include <stdarg.h>
#include <string>
#include <vector>
#include <map>

namespace jpl
{
namespace mipl
{
namespace p2
{

/**
 * This class reads the entier VICAR label in memory. The label is stored as map of map of lables and its values
 * (map < string map <string, vector<string> * >* >), where the vector<string*> are all the values associated
 * with the label and the outter map is map with key is properties.
 */
class VicarImageLabel
{

public:

	VicarImageLabel(int unitNo) throw (VicarException);

	virtual ~VicarImageLabel();

	void listLabels(std::string &inThisString) const;

	bool compare(const VicarImageLabel &toThisLabels,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string& differences) const;

	bool getKeyword (const std::string & property, const std::string & keyword, std::vector<std::string> & value);

protected:

	int unitNo_;
	// std::map of properties that contains a key and it's values (more then one if it is an array).
	std::map < std::string, std::map < std::string, std::vector < std::string >* >* > labels_;
	//std::vector<std::string> properties_;

	void cleanUp();
	void init_() throw(VicarException);

	bool compare(std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &lh,
			std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &rh,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string& differences) const;

};
};
};
};
#endif
