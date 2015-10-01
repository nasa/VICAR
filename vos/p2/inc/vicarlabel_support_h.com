$!****************************************************************************
$!
$! Build proc for MIPL module vicarlabel_support_h
$! VPACK Version 1.9, Monday, October 10, 2011, 22:25:03
$!
$! Execute by entering:		$ @vicarlabel_support_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module vicarlabel_support_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vicarlabel_support_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("vicarlabel_support_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @vicarlabel_support_h.bld "STD"
$   else
$      @vicarlabel_support_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vicarlabel_support_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vicarlabel_support_h.com -mixed -
	-s VicarBinaryHeader.h VicarFmtMap.h VicarException.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create VicarBinaryHeader.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarFmtMap.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarException.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
#ifndef __VICAR_EXCEPTION_H__
#define __VICAR_EXCEPTION_H__

#include <exception>
#include <string>

//using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{
class VicarException : public std::exception
{
private:
	std::string what_;
public:
	VicarException(std::string what) : what_(what) {}
	virtual ~VicarException() throw () {}
	const char* what() const throw () { return this->what_.c_str(); }
};
}
}
}
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
