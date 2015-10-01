$!****************************************************************************
$!
$! Build proc for MIPL module vicarlabel_support
$! VPACK Version 1.9, Monday, October 10, 2011, 22:26:55
$!
$! Execute by entering:		$ @vicarlabel_support
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module vicarlabel_support ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vicarlabel_support.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("vicarlabel_support.imake") .nes. ""
$   then
$      vimake vicarlabel_support
$      purge vicarlabel_support.bld
$   else
$      if F$SEARCH("vicarlabel_support.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vicarlabel_support
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vicarlabel_support.bld "STD"
$   else
$      @vicarlabel_support.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vicarlabel_support.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vicarlabel_support.com -mixed -
	-s VicarBinaryHeader.cc VicarFmtMap.cc -
	-i vicarlabel_support.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create VicarBinaryHeader.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarFmtMap.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vicarlabel_support.imake
/*****************************************************************************/
/*****************************************************************************/
/** Rajesh R. Patel                                                         **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

#define SUBROUTINE vicarlabel_support

#define MODULE_LIST VicarBinaryHeader.cc VicarFmtMap.cc


#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define P2_SUBLIB

#define LIB_RTL
#define LIB_TAE
/*#define LIB_P2SUB  for local build*/

$ Return
$!#############################################################################
