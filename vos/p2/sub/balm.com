$!****************************************************************************
$!
$! Build proc for MIPL module balm
$! VPACK Version 1.9, Monday, October 10, 2011, 12:54:25
$!
$! Execute by entering:		$ @balm
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
$ write sys$output "*** module balm ***"
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
$ write sys$output "Invalid argument given to balm.com file -- ", primary
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
$   if F$SEARCH("balm.imake") .nes. ""
$   then
$      vimake balm
$      purge balm.bld
$   else
$      if F$SEARCH("balm.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake balm
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @balm.bld "STD"
$   else
$      @balm.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create balm.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack balm.com -mixed -
	-s balm.cc -
	-i balm.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create balm.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
 * balm.cc
 *
 *  Created on: Aug 17, 2011
 *      Author: honghanh
 */

#include "balm.h"
#include <memory>
#include <cstdlib>

using namespace std;
namespace jpl
{
namespace mipl
{
namespace p2
{
bool BalmField::initialized = false;
unsigned long BalmField::masks[sizeof(unsigned long) << 3] = {0};

BalmField::BalmField()
{
	this->name_ = "";
	this->type_ = _UNSET;
	this->start_ = 0;
	this->length_ = 0;

	BalmField::init();
}

// for debugging
BalmField::BalmField(string iname, bool iisBit, int itype, size_t istart, size_t ilength)
{
	BalmField::init();

	this->name_ = iname;
	this->isBit_ = iisBit;
	this->type_ = itype;
	this->start_ = istart;	// should check, so that start is greater than 0
	this->length_ = ilength;
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
 *      1.0    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * The constructor takes a node in an BalmFormat and interpret it to   *
 * populate the Field object.                                          *
 *                                                                     *
 ***********************************************************************/
BalmField::BalmField(BalmFormat tree_node, string base_name)
throw (bad_fmt_exception)
{

	BalmField::init();

	KEYWORD* key = tree_node->first_keyword;
	size_t len;	// temporary variable
	unsigned short valueType;
	char* str;	// temporary variable
	string error;
	string prefix("Field::Field(BalmFormat, string): ");

	// set type first to check if type is set twice
	this->type_ = _UNSET;
	// initialize other value
	this->name_ = "";
	this->start_ = this->length_ = 0;
	// uninitialized isBit will be either true or false,
	// and it's meaningless either way

	str = tree_node->class_name;
	if (!setBitType(str))
	{
		// str maybe turned into all upper case by now
		prefix.append("Class_name is not 'COLUMN' or 'BIT_COLUMN' [");
		prefix.append(str, strlen(str));
		prefix.append("].");
		throw bad_fmt_exception(prefix);
	}

	while (key != NULL)
	{
		// turn the keyword to upper case for comparison
		str = key->name;
		len = strlen(str);

		// stricmp is system-dependent, so I'm not using that
		for (size_t i = 0; i < len; i++)
			str[i] = toupper(str[i]);

		// process the keyword of interest
		if (strcmp(str, "NAME") == 0)
		{
			valueType = OdlGetKwdValueType(key);
			str = key->value;
			len = strlen(str);
			if (valueType == ODL_SYMBOL)
			{
				if (!symbolizeOdlSymbol(str, error))
				{
					prefix.append("Malformed NAME token. ").append(error);
					throw bad_fmt_exception(prefix);
				}
				setName(str, base_name);
			}
			else if (valueType == ODL_TEXT)
			{
				if (!symbolizeOdlText(str, error))
				{
					prefix.append("Malformed NAME token. ").append(error);
					throw bad_fmt_exception(prefix);
				}
				setName(str, base_name);
			}
			else
			{
				prefix.append("NAME token must be an identifier [");
				prefix.append(str, len);
				prefix.append("].");
				throw bad_fmt_exception(prefix);
			}

		}
		else if ((!this->isBit_ && strcmp(str, "DATA_TYPE") == 0)
				|| strcmp(str, "BIT_DATA_TYPE") == 0)
		{
			str = key->value;
			len = strlen(str);
			// stricmp is not very standardized, so not using that
			for (size_t i = 0; i < len; i++)
				str[i] = toupper(str[i]);

			if (!stripQuotes(str, error))
			{
				prefix.append("Malformed DATA_TYPE or BIT_DATA_TYPE token. ").append(error);
				throw bad_fmt_exception(prefix);
			}

			if (!setType(str))
			{
				prefix.append("Unknown type [");
				prefix.append(str, strlen(str));
				prefix.append("].");
				throw bad_fmt_exception(prefix);
			}
		}
		else if ((!this->isBit_ && strcmp(str, "START_BYTE") == 0)
				|| (this->isBit_ && strcmp(str, "START_BIT") == 0))
		{
			str = key->value;
			if (OdlGetKwdValueType(key) == ODL_INTEGER)
			{
				this->start_ = atoi(str);
				if (this->start_ == 0)
				{
					prefix.append("The value of START_BYTE or START_BIT is not an integer [");
					prefix.append(str, strlen(str));
					prefix.append("].");
					throw bad_fmt_exception(prefix);
				}
			}
			else
			{
				prefix.append("The value of START_BYTE or START_BIT is not an ODL_INTEGER [");
				prefix.append(str, strlen(str));
				prefix.append("].");
				throw bad_fmt_exception(prefix);
			}
		}
		else if ((!this->isBit_ && strcmp(str, "BYTES") == 0)
				|| (this->isBit_ && strcmp(str, "BITS") == 0))
		{
			str = key->value;
			if (OdlGetKwdValueType(key) == ODL_INTEGER)
			{
				this->length_ = atoi(str);
				if (this->length_ == 0)
				{
					prefix.append("The value of BYTES or BITS is not an integer [");
					prefix.append(str, strlen(str));
					prefix.append("].");
					throw bad_fmt_exception(prefix);
				}
			}
			else
			{
				prefix.append("The value of BYTES or BITS is not an ODL_INTEGER [");
				prefix.append(str, strlen(str));
				prefix.append("].");
				throw bad_fmt_exception(prefix);
			}
		}

		key = key->right_sibling;
	}

}

BalmField::~BalmField()
{

}

// not thread-safe
/**
 * Provide bit masks for getBitString method.
 */
void BalmField::init ()
{
	if (!initialized)
	{
		size_t longsize = sizeof(unsigned long) << 3;
		for (size_t i = 0; i < longsize; i++)
			masks[i] = 1UL << i;
		initialized = true;
	}
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      setBitType                                                     *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.1    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Change to return bool.                           *
 *      08-24-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * This function sets the isBit field by perform a case-insensitive    *
 * comparison with the string "COLUMN" and "BIT_COLUMN". If the first  *
 * matches, then isBit is false. If the second matches, then isBit     *
 * is true. If neither matches, this function will return false        *
 * to indicate an error.                                               *
 *                                                                     *
 ***********************************************************************/
bool BalmField::setBitType(OUT char* object_class)	// object_class will be modified as a side-effect
{
	size_t len = strlen(object_class);

	for (size_t i = 0; i < len; i++)
		object_class[i] = toupper(object_class[i]);

	if (strcmp(object_class, "COLUMN") == 0)
		this->isBit_ = false;
	else if (strcmp(object_class, "BIT_COLUMN") == 0)
		this->isBit_ = true;
	else return false;
	return true;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      setType                                                        *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.1    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Eliminates the checking code, and change return  *
 *                    type to bool, and allows type to be set multiple *
 *                    times.                                           *
 *      08-24-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * This function assumes that symbol has been correctly                *
 * parsed from the PDS label and the quotes, if any, are stripped.     *
 *                                                                     *
 * This function checks the symbol against a set of allowed types.     *
 * If the type matches, this function will set the type of the field   *
 * and returns true. Otherwise, this function will leave the type as   *
 * as UNSET, and returns false.                                        *
 *                                                                     *
 * The allowable field are the combined of allowable field for         *
 * DATA_TYPE and BIT_DATA_TYPE according to PDS 3 standard.            *
 *                                                                     *
 ***********************************************************************/
bool BalmField::setType(char* symbol)	// symbol will be modified as a side-effect
{
	size_t len = strlen(symbol);
	if (len == 0) return false;

	if (strcmp(symbol, "BINARY") == 0)
	{
		this->type_ = _BYTE;	// byte stream
		return true;
	}

	if (strcmp(symbol, "BOOLEAN") == 0)
	{
		this->type_ = _BOOLEAN;	// can either be byte or bit
		return true;
	}

	if (strcmp(symbol, "MSB_INTEGER") == 0)
	{
		this->type_ = _MSB_SIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "MSB_SIGNED_INTEGER") == 0)
	{
		this->type_ = _MSB_SIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "MSB_UNSIGNED_INTEGER") == 0)
	{
		this->type_ = _MSB_UNSIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "INTEGER") == 0)
	{
		// ternary expression here doesn't work for some reason
		if (isLsb())
			this->type_ = _LSB_SIGNED_INTEGER;
		else
			this->type_ = _MSB_SIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "LSB_INTEGER") == 0)
	{
		this->type_ = _LSB_UNSIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "LSB_UNSIGNED_INTEGER") == 0)
	{
		this->type_ = _LSB_UNSIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "LSB_SIGNED_INTEGER") == 0)
	{
		this->type_ = _LSB_SIGNED_INTEGER;
		return true;
	}

	if (strcmp(symbol, "N/A") == 0)
	{
		this->type_ = _BYTE;
		return true;
	}

	if (strcmp(symbol, "BIT_STRING") == 0)
	{
		this->type_ = _BYTE;
		return true;
	}

	if (strcmp(symbol, "CHARACTER") == 0)
	{
		this->type_ = _BYTE;
		return true;
	}

	if (strcmp(symbol, "FLOAT") == 0 || strcmp(symbol, "IEEE_REAL") == 0)
	{
		if (isLsb())
			this->type_ = _LSB_FLOAT;
		else
			this->type_ = _MSB_FLOAT;
		return true;
	}

	return false;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      setName                                                        *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.1    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Remove checking for quote because checking is    *
 *                    done is the caller.                              *
 *      08-24-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * This function assumes that symbol is an identifier according to     *
 * PDS 3 standard.                                                     *
 *                                                                     *
 * This will prepend a base_name to the field's name according to C's  *
 * dot notation if base_name is not empty.                             *
 *                                                                     *
 ***********************************************************************/
void BalmField::setName (char* symbol, string base_name)
{
	this->name_.append(symbol, strlen(symbol));
	if (!base_name.empty())
		this->name_ = base_name.append(".").append(this->name_);
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      symbolizeOdlText                                               *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * This function takes a string assume to be an ODL_TEXT and check     *
 * if this token is a valid ODL_TEXT (have both quotes at the          *
 * beginning and end), and appropriate characteristics so that once    *
 * the quotes are stripped, the string becomes an identifier.          *
 * Return true if everything is valid, and odlText is converted into   *
 * an identifier.                                                      *
 * Return false with error message.                                    *
 *                                                                     *
 ***********************************************************************/
bool BalmField::symbolizeOdlText (char* odlText, OUT string& error)
{
	string prefix("Field::symbolizeOdlText: ");
	size_t len = strlen(odlText);

	// check if proper OdlText type
	if (len < 2 || odlText[0] != '"' || odlText[len-1] != '"')
	{
		error = prefix;
		error.append("odlText does not contain both begin quote and end quote [");
		error.append(odlText, len);
		error.append("].");
		return false;
	}

	// check if proper identifier style
	size_t last = len-1;

	// according to PDS 3 standard, an identifier first character
	// can only be a letter; I'll stick to that
	if (!isalpha(odlText[1]))
	{
		error = prefix;
		error.append("First character after the quote of odlText is not a letter [");
		error.append(odlText, len);
		error.append("].");
		return false;
	}

	for (size_t i = 2; i < last; i++)
	{
		if (!(isalpha(odlText[i]) || isdigit(odlText[i]) || odlText[i] == '_'))
		{
			error = prefix;
			error.append("Second and later characters after the quote of odlText is not a letter, a digit or an underscore [");
			error.append(odlText, len);
			error.append("].");
			return false;
		}
	}

	// if everything is appropriate, strip the quote
	// last become the length of the new string,
	// it's 2 less than the length of the first string
	last--;
	for (size_t i = 0; i < last; i++)
		// shouldn't use strmemcpy because we don't know the order of copying
		odlText[i] = odlText[i+1];	// shift everything over
	// add terminating character
	odlText[last] = '\0';

	return true;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      symbolizeOdlSymbol                                             *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * This function takes a string assume to be an ODL_SYMBOL and check   *
 * if this token is a valid ODL_SYMBOL and have appropriate            *
 * characteristics so that once the quotes, if any, are stripped, the  *
 * string becomes an identifier.                                       *
 * Return true if everything is valid, and odlText is converted into   *
 * an identifier.                                                      *
 * Return false with error message.                                    *
 *                                                                     *
 ***********************************************************************/
bool BalmField::symbolizeOdlSymbol (char* odlSymbol, OUT string& error)
{
	string prefix("Field::symbolizeOdlSymbol: ");
	size_t len = strlen(odlSymbol);

	// check if empty len
	if (len == 0)
	{
		error = prefix;
		error.append("odlSymbol is empty.");
		return false;
	}

	if (len == 1 && odlSymbol[0] == '\'')
	{
		error = prefix;
		error.append("Quotes must appear both at the beginning and at the end of odlSymbol ['].");
		return false;
	}

	// if quote present at the beginning or end, if at all
	if ((odlSymbol[0] == '\'' && odlSymbol[len-1] != '\'')
			|| (odlSymbol[0] != '\'' && odlSymbol[len-1] == '\''))
	{
		error = prefix;
		error.append("Quotes must appear both at the beginning and at the end of odlSymbol [");
		error.append(odlSymbol, len);
		error.append("].");
		return false;
	}

	// have both quotes or none at all at this point
	size_t first, last;
	if (odlSymbol[0] == '\'')
	{
		first = 1;
		last = len -1;
	}
	else
	{
		first = 0;
		last = len;
	}

	// check first character to be a letter
	if (!isalpha(odlSymbol[first]))	// if the symbol contains only quote, it'll be catch here
	{
		error = prefix;
		error.append("First character after the quote must be a letter [");
		error.append(odlSymbol, len);
		error.append("].");
		return false;
	}

	// subsequent character must be a letter, a digit or an underscore
	for (size_t i = first+1; i < last; i++)
	{
		if (!(isalpha(odlSymbol[i]) || isdigit(odlSymbol[i]) || odlSymbol[i] == '_'))
		{
			error = prefix;
			error.append("Second and later characters after the quote must be a letter, a digit, or an underscore [");
			error.append(odlSymbol, len);
			error.append("].");
			return false;
		}
	}

	// everything checks out, so strip the quote is there's any
	if (first == 1)	// first is 1 mean there are quotes
	{
		last--;
		for (size_t i = 0; i < last; i++)
			odlSymbol[i] = odlSymbol[i+1];
		odlSymbol[last] = '\0';
	}

	return true;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      stripQuotes                                                    *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * This function takes a string assume to be an ODL_SYMBOL or ODL_TEXT *
 * and check if single quote or double quote exists at both the        *
 * beginning and the end of the string or not at all.                  *
 * odlTextOrSymbol is allowed to be empty, since it's assumed to be    *
 * a string.                                                           *
 *                                                                     *
 * If the quotes exist, strip them; if not, leave the string alone.    *
 * If the quotes exist at only 1 end of the string, return false.      *
 * Otherwise return true.                                              *
 *                                                                     *
 ***********************************************************************/
bool BalmField::stripQuotes (char* odlTextOrSymbol, OUT string& error)
{
	string prefix("Field::stripQuotes: ");
	size_t len = strlen(odlTextOrSymbol);
	if (len == 0) return 0;	// nothing to be done

	if ((odlTextOrSymbol[0] == '\'' && odlTextOrSymbol[len-1] != '\'')
			|| (odlTextOrSymbol[0] != '\'' && odlTextOrSymbol[len-1] == '\'')
			|| (odlTextOrSymbol[0] == '"' && odlTextOrSymbol[len-1] != '"')
			|| (odlTextOrSymbol[0] != '"' && odlTextOrSymbol[len-1] == '"'))
	{
		error = prefix;
		error.append("Mismatched quotes [");
		error.append(odlTextOrSymbol, len);
		error.append("].");
		return false;
	}

	// strip if have quotes
	if (odlTextOrSymbol[0] == '\'' || odlTextOrSymbol[0] == '"')
	{
		len -= 2;
		for (size_t i = 0; i < len; i++)
			odlTextOrSymbol[i] = odlTextOrSymbol[i+1];
		odlTextOrSymbol[len] = '\0';
	}
	return true;
}

/**
 * Retrieves the value for every type, and do byte swapping to correct
 * Endianness if needed.
 * @param bytearr: the input buffer
 * @param size: size of bytearr
 * @param value: the output buffer
 * @param value_byte_size: the size of output buffer. This field must be equal
 * to this->length_, otherwise error code fld_incorrect_output_size is returned.
 * Although it is not necessary to enter this field, since it must be equal to
 * this->length_, I hope having this field here will make future developer
 * cautious about allocating @param value.
 */
int BalmField::getGenericValue (const void* bytearr, size_t size, OUT void* value, size_t value_byte_size)
{
	int lsbTypes = BalmField::_LSB_SIGNED_INTEGER | BalmField::_LSB_UNSIGNED_INTEGER | BalmField::_LSB_FLOAT;
	int msbTypes = BalmField::_MSB_SIGNED_INTEGER | BalmField::_MSB_UNSIGNED_INTEGER | BalmField::_MSB_FLOAT;
	int otherTypes = BalmField::_BOOLEAN | BalmField::_BYTE;
	int acceptedTypes = lsbTypes | msbTypes | otherTypes;

	if (!(this->type_ & acceptedTypes)) return BalmField::fld_wrong_type;

	if (this->isBit_) return BalmField::fld_wrong_type;

	if (this->length_ != value_byte_size) return BalmField::fld_incorrect_output_size;

	size_t start = this->start_ - 1;	// FMT start attribute is 1-based,
	//  local start is 0-based

	if (size < (this->length_ + start)) return BalmField::fld_input_size_too_small;

	bool localLsb = isLsb();
	bool fieldLsb = this->type_ & lsbTypes;
	bool isOtherType = this->type_ & otherTypes;

	memcpy((char*)value, (char*)bytearr + start, value_byte_size);

	if (isOtherType) return BalmField::fld_success;

	// reverse byte others if endianness of the two platforms
	// are opposite
	if (localLsb != fieldLsb)
	{
		unsigned char c;
		unsigned int l1 = this->length_ - 1;
		char* myvalue = (char*)value;
		for (unsigned int i = 0, j = this->length_ >> 1; i < j; i++)
		{
			c = myvalue[i];
			myvalue[i] = myvalue[l1-i];
			myvalue[l1-i] = c;
		}
	}

	return BalmField::fld_success;
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getRealValue (float)                                                                   *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a 32-bit floating point number using C++ casting   *
 * feature. This will interpret all types to a floating point number. It's the caller's   *
 * job to make sure the value should be interpreted as a float.                           *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getRealValue (const void* bytearr, size_t size, OUT float* value)
{
	return this->getValueHelper_ <float> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getRealValue (double)                                                                  *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a 64-bit floating point number using C++ casting   *
 * feature. This will interpret all types to a floating point number. It's the caller's   *
 * job to make sure the value should be interpreted as a double.                          *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getRealValue (const void* bytearr, size_t size, OUT double* value)
{
	return this->getValueHelper_ <double> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getIntegerValue (short)                                                                *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a short integer using C++ casting feature. This    *
 * will interpret all types to a floating point number. It's the caller's job to make     *
 * sure the value should be interpreted as a short.                                       *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getIntegerValue(const void* bytearr, size_t size, OUT short* value)
{
	return this->getValueHelper_ <short> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getIntegerValue (int)                                                                  *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as an integer using C++ casting feature. This will    *
 * interpret all types to a floating point number. It's the caller's job to make sure the *
 * value should be interpreted as an int.                                                 *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getIntegerValue(const void* bytearr, size_t size, OUT int* value)
{
	return this->getValueHelper_ <int> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getIntegerValue (long)                                                                 *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a long integer using C++ casting feature. This     *
 * will interpret all types to a floating point number. It's the caller's job to make     *
 * sure the value should be interpreted as a long.                                        *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getIntegerValue(const void* bytearr, size_t size, OUT long* value)
{
	return this->getValueHelper_ <long> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getIntegerValue (unsigned short)                                                       *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a unsigned short using C++ casting feature. This   *
 * will interpret all types to a floating point number. It's the caller's job to make     *
 * sure the value should be interpreted as an unsigned short.                             *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getIntegerValue(const void* bytearr, size_t size, OUT unsigned short* value)
{
	return this->getValueHelper_ <unsigned short> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getIntegerValue (unsigned int)                                                         *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a unsigned int using C++ casting feature. This     *
 * will interpret all types to a floating point number. It's the caller's job to make     *
 * sure the value should be interpreted as an unsigned int.                               *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getIntegerValue(const void* bytearr, size_t size, OUT unsigned int* value)
{
	return this->getValueHelper_ <unsigned int> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getIntegerValue (unsigned long)                                                        *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve and interpret the value as a unsigned long using C++ casting feature. This    *
 * will interpret all types to a floating point number. It's the caller's job to make     *
 * sure the value should be interpreted as an unsigned long.                              *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getIntegerValue(const void* bytearr, size_t size, OUT unsigned long* value)
{
	return this->getValueHelper_ <unsigned long> (bytearr, size, value);
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getBitString                                                                           *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve the value of a bit string, and place it in a buffer the size of an unsigned   *
 * long. The start bit of the bit string will be the first bit (highest order bit) in the *
 * output buffer.                                                                         *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform. Or if the  *
 * field is not a bit string.                                                             *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 * fld_output_size_too_small: if the bit string is longer than an unsigned long.          *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getBitString (const void* buffer, size_t bufsiz, OUT unsigned long* value)
{
	// Other execution
	if (!this->isBit_) return fld_wrong_type;

	size_t len = this->length_;
	if (len > (sizeof(unsigned long) << 3)) return fld_output_size_too_small;

	unsigned int startBit = this->start_-1 + len;	// start is 1-based, startBit is 0-based and start from the end
	if (startBit >= (bufsiz << 3)) return fld_input_size_too_small;

	startBit--;
	unsigned char bitIndex = 7 - (startBit & 7);	// bit index is 0-based, and the lower order bit is
	// further away from the memory address

	size_t i = 0;
	unsigned int startByte = startBit >> 3;

	unsigned char* bytearr = (unsigned char*)buffer;

	*value = 0;
	// the next 2 loops is a loop unrolling technique
	// because as soon as the bitIndex crosses byte boundary
	// we need a left shift instead of a right shift
	while (i < len)
	{
		*value |= (bytearr[startByte] & masks[bitIndex]) >> (bitIndex - i);
		i++;
		bitIndex = (bitIndex + 1) & 7;
		if (bitIndex == 0)
		{
			startByte--;
			break;
		}
	}
	while (i < len)
	{
		*value |= (bytearr[startByte] & masks[bitIndex]) << (i - bitIndex);
		i++;
		bitIndex = (bitIndex + 1) & 7;
		if (bitIndex == 0) startByte--;
	}

	return fld_success;
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getByteValue (char)                                                                    *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve a single byte and interpret it as an unsigned char.                           *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getByteValue (const void* bytearr, size_t size, OUT unsigned char* value)
{
	unsigned char* v = new unsigned char[this->length_];
	int result = this->getGenericValue(bytearr, size, v, this->length_);

	if (result == BalmField::fld_success)
		*value = v[0];	// length_ is a positive number

	delete [] v;

	return result;
}

/******************************************************************************************
 *                                                                                        *
 * Component:                                                                             *
 *                                                                                        *
 * getByteValue (char)                                                                    *
 *                                                                                        *
 * Author:                                                                                *
 *                                                                                        *
 *      Haley Nguyen (Jet Propulsion Laboratory)                                          *
 *                                                                                        *
 *  Version:                                                                              *
 *                                                                                        *
 *      1.0    October 10, 2011                                                           *
 *                                                                                        *
 *  Change History:                                                                       *
 *                                                                                        *
 *      10-10-2011    Original code                                                       *
 *                                                                                        *
 *----------------------------------------------------------------------------------------*
 * Retrieve a single byte and interpret it as a char.                                     *
 *                                                                                        *
 * Return:                                                                                *
 * fld_success: if the value is successfully read and interpreted.                        *
 * fld_wrong_type: if the field is a type that is unavailable on the platform.            *
 * fld_input_size_too_small: if @param bytearr is smaller than the field's specification. *
 *                                                                                        *
 ******************************************************************************************/
int BalmField::getByteValue (const void* bytearr, size_t size, OUT char* value)
{
	char* v = new char[this->length_];
	int result = this->getGenericValue(bytearr, size, v, this->length_);

	if (result == BalmField::fld_success)
		*value = v[0];	// length_ is a positive number

	delete [] v;

	return result;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      getByteStream                                                  *
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
 * This function retrieves the value of a field into an array of       *
 * bytes.                                                              *
 * @param bytearr: input buffer                                        *
 * @param size: size of input buffer in bytes.                         *
 * @param value: the output buffer.                                    *
 * @param value_byte_size: the length of the output array in bytes.    *
 * Return:                                                             *
 * fld_success: if value is successfully read.                         *
 * fld_wrong_type: if this field's type can't be handled.              *
 * fld_output_size_too_small: if the output buffer cannot accommodate  *
 * the data. This is different from getting numeric field because      *
 * byte stream cannot be casted.                                       *
 *                                                                     *
 ***********************************************************************/
int BalmField::getByteStream (const void* bytearr, size_t size, OUT void* value, unsigned int value_byte_size)
{
	if (this->length_ > value_byte_size) return BalmField::fld_output_size_too_small;

	return getGenericValue(bytearr, size, value, value_byte_size);
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      getBoolean                                                     *
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
 * This function retrieves the value of a field and return true if the *
 * value is not 0, and false otherwise.                                *
 * @param bytearr: input buffer                                        *
 * @param size: size of input buffer in bytes.                         *
 * @param value: the output.                                           *
 *                                                                     *
 * Return:                                                             *
 * fld_success: if value is successfully read.                         *
 * fld_wrong_type: if this field's type can't be handled.              *
 * fld_output_size_too_small: if the output buffer cannot accommodate  *
 * the data. This only happens if field is a bit string that exceeds   *
 * 32 bits.                                                            *
 *                                                                     *
 ***********************************************************************/
int BalmField::getBoolean(const void* bytearr, size_t size, bool* value)
{
	int retVal;

	if (this->isBit_)
	{
		unsigned long buf = 0;

		retVal = getBitString(bytearr, size, &buf);
		if (retVal == fld_success)
			*value = buf != 0;
	}
	else
	{
		unsigned char* buf = new unsigned char[this->length_];
		retVal = getGenericValue(bytearr, size, buf, this->length_);
		if (retVal == fld_success)
		{
			*value = false;
			for (size_t i = 0; i < this->length_; i++)
			{
				if (buf[i])
				{
					*value = true;
					break;
				}
			}
		}
		delete [] buf;
	}
	return retVal;
}

ostream& operator<< (ostream& os, const BalmField& f)
{
	os << f.name_ << ":" << f.start_ << ":" << f.length_;
	return os;
}


/*************************** Global functions *****************************/
BalmFormat parseFormat (const char* formatFileName)
{
	// error will be printed to standard output
	// because our FMT is not 1 entire PDS label
	// we cannot have any error printed because
	// there will be error since it's not one entire PDS label.
	return OaParseLabelFile((char*)formatFileName, NULL, ODL_EXPAND_STRUCTURE, 1);
}

void freeFormat(BalmFormat format)
{
	OalFreeSDT(format);
	return;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      getAllFields                                                   *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * Takes the format in the form of an BalmFormat and output a list of  *
 * objects, each describe a data field.                                *
 *                                                                     *
 * If everything goes well, this function returns true. Otherwise,     *
 * it will return false and an error message.                          *
 *                                                                     *
 ***********************************************************************/
bool getAllFields (BalmFormat format, OUT vector<BalmField>& fields, OUT string& error)
{
	BalmFormat node = format->first_child;	// there is always a root node
	stack<string> field_names;
	BalmField field;
	string base_name;

	field_names.push("");

	do
	{
		base_name = field_names.top();
		try
		{
			field = BalmField(node, base_name);
		}
		catch (bad_fmt_exception& e)
		{
			error = string(e.what());
			return false;
		}

		fields.push_back(field);

		if (node->first_child != NULL)	// go to children first
		{
			node = node->first_child;
			field_names.push(field.getName());
		}
		else if (node->right_sibling != NULL)	// go to the next sibling
		{
			node = node->right_sibling;
		}
		else
		{
			field_names.pop();
			while (true)
			{
				node = node->parent;
				if (node == format) break;

				if (node->right_sibling != NULL)
				{
					node = node->right_sibling;
					break;
				} else field_names.pop();
			}
		}

	} while (node != format);

	return true;
}

/***********************************************************************
 *                                                                     *
 *  Component:                                                         *
 *                                                                     *
 *      getField                                                       *
 *                                                                     *
 *  Author:                                                            *
 *                                                                     *
 *      Haley Nguyen (Jet Propulsion Laboratory)                       *
 *                                                                     *
 *  Version:                                                           *
 *                                                                     *
 *      1.0    September 7, 2011                                       *
 *                                                                     *
 *  Change History:                                                    *
 *                                                                     *
 *      09-07-2011    Original code                                    *
 *                                                                     *
 *---------------------------------------------------------------------*
 *                                                                     *
 *  Description:                                                       *
 *                                                                     *
 * Takes the format in the form of an BalmFormat and output a list of  *
 * objects, each describe a data field.                                *
 *                                                                     *
 * If everything goes well, this function returns true. Otherwise,     *
 * it will return false and an error message.                          *
 *                                                                     *
 * @param fields a vector to be search                                 *
 * @param name the name of the field to be searched.                   *
 * @param closestStartValue since the name SPARE tends to not be       *
 * unique, to help identify the appropriate field that is most likely  *
 * be the field we're looking for, this value is used as an origin     *
 * from which the field of the same name that is closest to this value *
 * is chosen.
 *                                                                     *
 ***********************************************************************/
const BalmField* getField (const vector<BalmField>& fields, const string& name, size_t closestStartValue)
{
	size_t length = fields.size();
	const BalmField* result = NULL;
	long int s = (long int) closestStartValue;

	for (size_t i = 0; i < length; i++)
	{
		if (iequals(name, fields.at(i).getName()))
		{
			if (result == NULL)
				result = &fields.at(i);
			else if (abs((long int)fields.at(i).getStart() - s) < abs((long int)result->getStart() - s))
				result = &fields.at(i);
		}
	}

	return result;
}

bool iequals (const string& str1, const string& str2)
{
	size_t len1 = str1.length(), len2 = str2.length();

	if (len1 != len2) return false;

	for (size_t i = 0; i < len1; i++)
		if (toupper(str1.at(i)) != toupper(str2.at(i))) return false;

	return true;
}

inline bool isLsb () { int num = 1; return *((char*)&num) == 1; }
}
}
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create balm.imake
/*****************************************************************************/
/*****************************************************************************/
/** Rajesh R. Patel                                                         **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

#define SUBROUTINE balm

#define MODULE_LIST balm.cc


#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define USES_ANSI_C

#define P2_SUBLIB

#define LIB_RTL
#define LIB_TAE
/* #define LIB_P2SUB for local build*/

#define LIB_PDS
$ Return
$!#############################################################################
