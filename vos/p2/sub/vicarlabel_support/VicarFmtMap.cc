/*
 * VicarFmtMap.cc
 *
 *  Created on: Oct 5, 2011
 *      Author: honghanh
 */

extern "C"
{
#include "zvproto.h"
}

#include "VicarFmtMap.h"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <iterator>
#include <algorithm>
#include <vector>

using namespace std;

//====================================================================================
//====================================================================================
//====================================================================================
void trim (string & s)
{
	string whitespaces(" \t\f\v\n\r");
	size_t found;

	found = s.find_last_not_of(whitespaces);
	if (found != string::npos)
	{
		s.erase(found+1);
		found = s.find_first_not_of(whitespaces);
		if (found != string::npos)
			s.erase(0, found);
	}
	else
		s.clear();
}

//====================================================================================
//====================================================================================
//====================================================================================
void split(const string & s, vector< string >& tokens)
{
	istringstream iss(s);
	copy(istream_iterator<string>(iss), istream_iterator<string>(), back_inserter<vector<string> >(tokens));
	return;
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
 * The constructor opens an FMT mapping file, parses it and populates  *
 * the mapping table.                                                  *
 *                                                                     *
 ***********************************************************************/
jpl::mipl::p2::VicarFmtMap::VicarFmtMap(const string & filename)
throw(exception)
{
	ifstream mapfile;
	string line;

	mapfile.open(filename.c_str());

	if (mapfile.is_open())
	{
		vector<string> tokens;
		const int max_val_size = 512;
		char expanded[max_val_size];

		while (mapfile.good())
		{
			getline(mapfile, line);
			trim(line);

			// reject blank lines and comment lines
			if (line.empty() || line.at(0) == '#')
				continue;

			tokens.clear();
			split(line, tokens);
			zvfilename(const_cast<char*>(tokens.at(1).c_str()), expanded, max_val_size);
			this->fmtMap_.insert(pair< string, string >(tokens.at(0), string(expanded)));
		}
		mapfile.close();
	}
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      getFmt                                                         *
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
 * Given a BLTYPE value, return the appropriate FMT to use.            *
 * Returns true if the FMT is found, false otherwise.                  *
 *                                                                     *
 ***********************************************************************/
bool jpl::mipl::p2::VicarFmtMap::getFmt (const string & bltype, string & fmt)
{
	map < string, string >::iterator it = this->fmtMap_.find(bltype);
	if (it == this->fmtMap_.end())
		return false;
	fmt.clear();
	fmt.append(it->second);
	return true;
}
