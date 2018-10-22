#include <iostream>
#include <cstring>
#include <string>
#include "VicarImageLabel.h"
#include "VicarHistoryLabel.h"
#include "difpic_bridges.h"
#include "DifpicParameters.h"
#include "VicarFmtMap.h"
#include "VicarBinaryHeader.h"
#include "balm.h"

extern "C"
{
#include "xvmaininc.h"
#include "applic.h"
#include "ftnbridge.h"
#include "defines.h"
}

using namespace std;
using namespace jpl::mipl::p2::difpic;
using namespace jpl::mipl::p2;

bool found (const vector<string>& source, const string target);
void print (const string& msg);

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
void print (const string& msg)
{
	const size_t max = 250;
	char buffer[max];
	size_t len = msg.size();
	size_t i = 0, j = 0;
	char c;

	do
	{
		c = msg.at(i);
		i++;
		buffer[j] = c;
		j++;
		if ( j == (max - 1) || c == '\n' || i == len)
		{
			buffer[j] = '\0';
			zvmessage(buffer, NULL);
			j = 0;
		}
	} while (i < len);
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" bool labeldifC()
{
	bool same=true;
	int iunit1=-1, iunit2=-2, status=0;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;
		if (params->labelDiffEnabled())
		{
			status = zvunit(&iunit1, const_cast< char *>("INP"), 1,NULL);
			status = zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			status = zvunit(&iunit2, const_cast<char*>("INP"), 2,NULL);
			status = zvopen(iunit2,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			VicarImageLabel ld1(iunit1);
			VicarImageLabel ld2(iunit2);

			same=ld1.compare(ld2,
					params->propertiesToIgnore(),
					params->labelsToIgnore(),
					differences);
			if (!params->silent())
			{
				if (!same)
				{
					zvmessage("Found following label differences between given images:\n", NULL);
					print(differences);
					zvmessage("\n------------ Vicar label differs. ------------\n", NULL);
				}
				else
					zvmessage("------- Vicar label comparison passed. -------\n", NULL);
				zvmessage("\n", NULL);
			}
		}
	}
	catch (const exception& e)
	{
		zvmessage(const_cast<char*>(e.what()),const_cast<char*>("Exception::labeldifC"));
	}
	if (iunit1!=-1)
		status = zvclose(iunit1,NULL);
	if (iunit2!=-2)
		status = zvclose(iunit2,NULL);

	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" void FTN_NAME2(labeldif, LABELDIF) (int *status)
{
	*status = labeldifC() ? 0 : 1;
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" bool histdifC()
{
	bool same=true;
	int iunit1=-1, iunit2=-2, status=0;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;

		if (params->historyDiffEnabled())
		{
			status = zvunit(&iunit1, const_cast<char*>("INP"), 1,NULL);
			status = zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			status = zvunit(&iunit2, const_cast<char*>("INP"), 2,NULL);
			status = zvopen(iunit2,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			/*
	VicarHistoryLabel ld1(iunit1);
	VicarHistoryLabel ld2(iunit2);

	same=ld1.compare(ld2,
	params->propertiesToIgnore(),
	params->labelsToIgnore(),
	differences);
			 */
			same=false;
			if (!params->silent())
				zvmessage("***** History label comparision not implemented yet. *****\n", "ERROR");
		}
	}
	catch (const exception &e)
	{
		zvmessage(const_cast<char*>(e.what()),const_cast<char*>("Exception::histdifC"));
	}
	if (iunit1!=-1)
		status = zvclose(iunit1,NULL);
	if (iunit2!=-2)
		status = zvclose(iunit2,NULL);
	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" void FTN_NAME2(histdif, HISTDIF) (int *status)
{
	*status = histdifC() ? 0 : 1;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool getBHFMTs (string & fmt1, string & fmt2, int iunit1, int iunit2, string & errmsg)
{
	const DifpicParameters * params = DifpicParameters::instance();
	const vector<string>& fmtnames = params->getBinaryHeaderFMTFileNames();

	// Check if BHFMTFILES are specified
	if (fmtnames.size() != 2 && fmtnames.size() != 0)
	{
		errmsg.append("Need exactly two file names for BHFMTFILES (Binary Header FMT File Names)");
		return false;
	}

	// ------- Begin: BHFMTFILES are not specified, use FMT mapping -----
	if (fmtnames.size() == 0)
	{
		// Check if BHFMTMAP exists,
		if (params->getBinaryHeaderFMTMappingFile().empty())
		{
			// BHFMTMAP does not exist, report error and quit
			errmsg.append("Need BHFMTMAP");
			return false;
		}

		// BHFMTMAP exist, read and parse map
		VicarFmtMap fmtMap(params->getBinaryHeaderFMTMappingFile());
		string bltype1, bltype2;

		// check if BLTYPES are specified
		if (params->getBlTypes().size() != 0 && params->getBlTypes().size() != 2)
		{
			errmsg.append("Need exactly two strings for BLTYPES.");
			return false;
		}

		// ----- Begin: BLTYPES are not specified, read Vicar system label -----
		if (params->getBlTypes().size() != 2)
		{
			char value[MAX_LABEL_VALUE_SIZE+1];
			zlget(iunit1, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype1 = string(value);
			zlget(iunit2, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype2 = string(value);
		}
		// ----- End: BLTYPES are not specified, read Vicar system label -------
		else
		{
			bltype1 = params->getBlTypes().at(0);
			bltype2 = params->getBlTypes().at(1);
		}

		// Using BLTYPES to read FMT file names from FMT map
		if (!fmtMap.getFmt(bltype1, fmt1))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype1);
			return false;
		}
		if (!fmtMap.getFmt(bltype2, fmt2))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype2);
			return false;
		}
	}
	// ------- End: BHFMTFILES are not specified, use FMT mapping -------
	// BHFMTFILES are specified, then obtain the FMT's file names from parameters
	else
	{
		fmt1 = fmtnames.at(0);
		fmt2 = fmtnames.at(1);
	}

	return true;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool getUnionOfFields (BalmFormat format1, BalmFormat format2,
		const string& input1, const string& input2,
		const vector< string >& ignored, vector< BalmField >& allFields,
		string& differences, string& errmsg, bool* same)
{
	vector<BalmField> allFields1;
	vector<BalmField> allFields2;

	if (!getAllFields(format1, allFields1, errmsg))
		return false;

	if (!getAllFields(format2, allFields2, errmsg))
		return false;

	size_t numField = allFields1.size();

	// make a intersection of fields to check
	for (size_t i = 0; i < numField; i++)
	{
		const BalmField* field1 = &allFields1.at(i);
		const BalmField* field2 = getField(allFields2, field1->getName(), field1->getStart());

		if (!found(ignored, field1->getName()))
		{
			if (field2 == NULL)
			{
				differences += "Field ";
				differences += field1->getName();
				differences += " doesn't exist in file ";
				differences += input2;
				differences += ".\n";
				*same = false;
			}
			else if (field2 != NULL)
			{
				allFields.push_back(*field1);
				allFields.push_back(*field2);
			}
		}
		// if field is ignored, then we don't if it's missing
	}

	numField = allFields2.size();
	for (size_t i = 0; i < numField; i++)
	{
		const BalmField* field2 = &allFields2.at(i);
		const BalmField* field1 = getField(allFields1, field2->getName(), field2->getStart());
		if (field1 == NULL && !found(ignored, field2->getName()))	// if field is ignored, then we don't if it's missing
		{
			differences += "Field ";
			differences += field2->getName();
			differences += " doesn't exist in file ";
			differences += input1;
			differences += ".\n";
			*same = false;
		}
	}

	return true;
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
// helper functions
bool found (const vector<string>& source, const string target)
{
	// this silly function is here because I can't figured out
	// out to use the algorithm library in 5 minutes.
	size_t len = source.size();

	for (size_t i = 0; i < len; i++)
	{
		if (iequals(source.at(i), target))
		{
			return true;
		}
	}

	return false;
}
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
void printGetFieldError (int result, const string& field)
{
	string error("Error while comparing field ");
	error += field;
	error += ": ";

	switch (result)
	{
	case BalmField::fld_wrong_type:
		error += "Wrong type.";
		break;
	case BalmField::fld_output_size_too_small:
		error += "Output size too small.";
		break;
	case BalmField::fld_input_size_too_small:
		error += "Input buffer too small.";
		break;
	case BalmField::fld_bitstr_too_long:
		error += "Bit string too long.";
		break;
		// fld_incorrect_output_size won't be return by getValue* functions
	default:
		error += "Unknown.";
	}

	zvmessage(const_cast<char*>(error.c_str()), NULL);
}
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool bitStrEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	// this assumes fields are bit string
	unsigned long bs1, bs2;
	int result;

	result = field1.getBitString(buf1, size1, &bs1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getBitString(buf2, size2, &bs2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field2.getName());
		return false;
	}

	return bs1 == bs2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool unsignedIntEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	unsigned long l1, l2;
	int result;

	result = field1.getIntegerValue(buf1, size1, &l1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getIntegerValue(buf2, size2, &l2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	return l1 == l2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool integerEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	long l1, l2;
	int result;
	result = field1.getIntegerValue(buf1, size1, &l1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getIntegerValue(buf2, size2, &l2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	return l1 == l2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool realEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	double d1, d2;
	int result;

	// this assumes fields are floating point number
	result = field1.getRealValue(buf1, size1, &d1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}

	result = field2.getRealValue(buf2, size2, &d2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field2.getName());
		return false;
	}

	return d1 == d2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool boolEquals (BalmField & field1, const unsigned char* buf1, size_t size1,
		BalmField & field2, const unsigned char* buf2, size_t size2)
{
	// this assumes fields are boolean
	bool b1, b2;
	int result;
	result = field1.getBoolean(buf1, size1, &b1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getBoolean(buf2, size2, &b2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field2.getName());
		return false;
	}
	return b1 == b2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool byteEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	const size_t len = field1.getLength();
	// this assumes fields are byte arrays
	if (len != field2.getLength())
		return false;

	unsigned char* a1 = new unsigned char [len];
	unsigned char* a2 = new unsigned char [len];
	int result;

	bool success = (result = field1.getByteStream(buf1, size1, a1, len)) == BalmField::fld_success;
	if (!success)
	{
		printGetFieldError(result, field1.getName());
	}
	if (success)
	{
		success = (result = field2.getByteStream(buf2, size2, a2, len)) == BalmField::fld_success;
		if (!success)
		{
			printGetFieldError(result, field2.getName());
		}
	}
	success = success && (memcmp(a1,a2,len) == 0);

	delete [] a1;
	delete [] a2;

	return success;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool fieldEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	if (field1.isBitField() != field2.isBitField())
	{
		zvmessage((char*)"Type mismatch", (char*)"");
		return false;
	}

	if (field1.getType() != field2.getType())
	{
		zvmessage((char*)"Type mismatch", (char*)"");
		return false;
	}

	if (field1.isBitField())
		return bitStrEquals(field1, buf1, size1, field2, buf2, size2);
	else
	{
		switch (field1.getType())
		{
		case BalmField::BOOLEAN: return boolEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::BYTE: return byteEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::FLOAT: return realEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::SIGNED_INTEGER: return integerEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::UNSIGNED_INTEGER: return unsignedIntEquals(field1, buf1, size1, field2, buf2, size2);
		default:
			zvmessage((char*)"Fields are UNSET.", (char*)"");
		}
	}
	return false;	// so that the caller will print error messages with field's name.

}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" bool binaryheaderdifC()
{
	bool same = true;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;
		if (params->binaryHeaderDiffEnabled())
		{
			bool got_error = false;
			string errmsg;
			int iunit1, iunit2, status;
			string fmt1, fmt2;
			BalmFormat format1 = NULL;
			BalmFormat format2 = NULL;
			vector<BalmField> allFields;
			VicarBinaryHeader bh1, bh2;

			status = zvunit(&iunit1,(char*) "INP", 1,NULL);
			zvsignal(iunit1, status, 1);
			zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", "COND", "BINARY", NULL);
			status = zvunit(&iunit2, (char*)"INP", 2, NULL);
			zvsignal(iunit2, status, 1);
			zvopen(iunit2, "OP", "READ", "OPEN_ACT", "SA", "IO_ACT", "SA", "COND", "BINARY", NULL);

			got_error = !(getBHFMTs(fmt1, fmt2, iunit1, iunit2, errmsg));

			// -------------------- Begin: Parse format files ----------------------
			if (!got_error)
			{
				format1 = parseFormat(fmt1.c_str());
				if (format1 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt1;
					got_error = true;
				}
			}

			if (!got_error)
			{
				format2 = parseFormat(fmt2.c_str());
				if (format2 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt2;
					got_error = true;
				}
			}
			// -------------------- End: Parse format files ------------------------

			// Read ignore fields
			const vector<string>& ignored = params->bhFieldsToIgnore();

			// Set up fields' union list
			if (!got_error)
			{
				string input1 = params->getInputFileNames().at(0);
				string input2 = params->getInputFileNames().at(1);
				got_error = !getUnionOfFields (format1, format2, input1, input2,
						ignored, allFields, differences, errmsg, &same);
			}

			// Get binary header
			if (!got_error)
			{
				try
				{
					// Read binary headers
					bh1 = VicarBinaryHeader(iunit1);
					bh2 = VicarBinaryHeader(iunit2);
				}
				catch (const exception& e1)
				{
					errmsg.append(e1.what());
					got_error = true;
				}

			}

			// --------------- Begin: Traverse union list and diff ----------------
			if (!got_error)
			{
				size_t numField = allFields.size();
				BalmField* field1;
				BalmField* field2;
				const unsigned char* buf1 = bh1.getHeader();
				const unsigned char* buf2 = bh2.getHeader();
				const size_t bin_hdr_size1 = bh1.getSize();
				const size_t bin_hdr_size2 = bh2.getSize();

				for (size_t i = 0; i < numField; i += 2)
				{
					field1 = &allFields.at(i);
					field2 = &allFields.at(i+1);
					if (!fieldEquals(*field1, buf1, bin_hdr_size1, *field2, buf2, bin_hdr_size2))
					{
						// for niceties, the values from respective files ought to be printed.
						// however, because FMT file is still messed up at this time,
						// it's better not to print them yet because the value maybe wrong.
						differences += "Field ";
						differences += field1->getName();
						differences += " differs.\n";
						same = false;

						// don't break here because I want to see all fields that are different
					}
				}
			}
			// --------------- End: Traverse union list and diff ------------------

			if (format1 != NULL) freeFormat(format1);
			if (format2 != NULL) freeFormat(format2);
			status = zvclose(iunit1,NULL);
			status = zvclose(iunit2,NULL);
			if (got_error)
			{
				zvmessage((char*)errmsg.c_str(), (char*)"ERROR");
				return false;
			}

			if (!params->silent())
			{
				if (!same)
				{
					zvmessage("Found following binary header differences between given images:\n", NULL);
					print(differences);
					zvmessage("\n--------------- Binary header differs. ----------------\n", NULL);
				}
				else
					zvmessage("----------- Binary header comparison passed. ----------\n", NULL);
				zvmessage("\n", NULL);
			}
		}
	}
	catch (const exception& e)
	{
		zvmessage(const_cast<char*>(e.what()), (char*)"Exception::binaryheaderdifC");
	}

	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" void FTN_NAME2(binaryheaderdif, BINARYHEADERDIF) (int *unit1, int *unit2, int *status)
{
	*status = binaryheaderdifC() ? 0 : 1;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool getLPFMTs (string & fmt1, string & fmt2, int iunit1, int iunit2, string & errmsg)
{
	const DifpicParameters * params = DifpicParameters::instance();
	const vector<string>& fmtnames = params->getLinePrefixFMTFileNames();

	// Check if LPFMTFILES are specified
	if (fmtnames.size() != 2 && fmtnames.size() != 0)
	{
		errmsg.append("Need exactly two file names for LPFMTFILES (Line Prefix FMT File Names)");
		return false;
	}

	// ------- Begin: LPFMTFILES are not specified, use FMT mapping -----
	if (fmtnames.size() == 0)
	{
		// Check if LPFMTMAP exists,
		if (params->getLinePrefixFMTMappingFile().empty())
		{
			// LPFMTMAP does not exist, report error and quit
			errmsg.append("Need LPFMTMAP");
			return false;
		}

		// LPFMTMAP exist, read and parse map
		VicarFmtMap fmtMap(params->getLinePrefixFMTMappingFile());
		string bltype1, bltype2;

		// check if BLTYPES are specified
		if (params->getBlTypes().size() != 0 && params->getBlTypes().size() != 2)
		{
			errmsg.append("Need exactly two strings for BLTYPES.");
			return false;
		}

		// ----- Begin: BLTYPES are not specified, read Vicar system label -----
		if (params->getBlTypes().size() != 2)
		{
			char value[MAX_LABEL_VALUE_SIZE+1];
			zlget(iunit1, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype1 = string(value);
			zlget(iunit2, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype2 = string(value);
		}
		// ----- End: BLTYPES are not specified, read Vicar system label -------
		else
		{
			bltype1 = params->getBlTypes().at(0);
			bltype2 = params->getBlTypes().at(1);
		}

		// Using BLTYPES to read FMT file names from FMT map
		if (!fmtMap.getFmt(bltype1, fmt1))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype1);
			return false;
		}
		if (!fmtMap.getFmt(bltype2, fmt2))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype2);
			return false;
		}
	}
	// ------- End: LPFMTFILES are not specified, use FMT mapping -------
	// LPFMTFILES are specified, then obtain the FMT's file names from parameters
	else
	{
		fmt1 = fmtnames.at(0);
		fmt2 = fmtnames.at(1);
	}

	return true;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
// helper functions
bool compareOneLine (const unsigned char* buf1, size_t size1,
		const unsigned char* buf2, size_t size2,
		vector<BalmField> & allFields, string & differences)
{
	size_t numField = allFields.size();
	BalmField* field1;
	BalmField* field2;
	bool same = true;

	// go through the fields of the first file
	for (size_t i = 0; i < numField; i += 2)
	{
		field1 = &allFields.at(i);
		field2 = &allFields.at(i+1);
		if (!fieldEquals(*field1, buf1, size1, *field2, buf2, size2))
		{
			// for niceties, the values from respective files ought to be printed.
			// however, because FMT file is still messed up at this time,
			// it's better not to print them yet because the value maybe wrong.
			differences += " ";
			differences += field1->getName();
			same = false;

			// don't break here because I want to see all fields that are different
		}
	}

	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" bool lineprefixdifC()
{
	bool same=true;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;
		bool got_error = false;
		string errmsg;

		if (params->linePrefixDiffEnabled())
		{
			string fmt1, fmt2;
			BalmFormat format1 = NULL;
			BalmFormat format2 = NULL;
			vector<BalmField> allFields;
			int nl1, ns1, nlb1, nbb1;
			int nl2, ns2, nlb2, nbb2;
			int pixSize1=0, pixSize2=0;
			int iunit1, iunit2, status;

			status = zvunit(&iunit1,(char*) "INP", 1,NULL);
			zvsignal(iunit1, status, 1);
			zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", "COND", "BINARY", NULL);
			status = zvunit(&iunit2, (char*)"INP", 2,NULL);
			zvsignal(iunit2, status, 1);
			zvopen(iunit2, "OP", "READ", "OPEN_ACT", "SA", "IO_ACT", "SA", "COND", "BINARY", NULL);

			// Get FMT file
			got_error = !(getLPFMTs(fmt1, fmt2, iunit1, iunit2, errmsg));

			// ----------------- Begin: Parse FMT file --------------------
			if (!got_error)
			{
				format1 = parseFormat(fmt1.c_str());
				if (format1 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt1;
					got_error = true;
				}
			}

			if (!got_error)
			{
				format2 = parseFormat(fmt2.c_str());
				if (format2 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt2;
					got_error = true;
				}
			}

			// ----------------- End: Parse FMT file ----------------------

			// Get ignore fields
			const vector<string>& ignored = params->lpFieldsToIgnore();

			// get union of unignored fields
			if (!got_error)
			{
				string input1 = params->getInputFileNames().at(0);
				string input2 = params->getInputFileNames().at(1);
				got_error = !getUnionOfFields (format1, format2, input1, input2,
						ignored, allFields, differences, errmsg, &same);
			}


			// --------------- Begin: Diff line prefix for all lines --------------
			if (!got_error)
			{
				// get dimensions of data
				zvget(iunit1,"NL",&nl1,"NS",&ns1,
						"NLB",&nlb1,"NBB",&nbb1,
						"PIX_SIZE",&pixSize1,NULL);
				zvget(iunit2,"NL",&nl2,"NS",
						&ns2,"NLB",&nlb2,"NBB",&nbb2,
						"PIX_SIZE",&pixSize2,NULL);
				if (nl1 != nl2)
				{
					same = false;
					differences += "Files have different number of lines.\n";
				}
				else
				{
					size_t size1 = (nbb1 + ns1) * pixSize1;
					size_t size2 = (nbb2 + ns2) * pixSize2;
					unsigned char* buf1 = new unsigned char[size1];
					unsigned char* buf2 = new unsigned char[size2];
					size_t el1 = nl1 + nlb1;
					char tempErrMsg[64];
					bool tempSame;

					// ----------------- Begin: Loop through every lines of image ------------------
					// the two files have the same number of lines
					// so we just need to check the maximum line number of one file.
					for (size_t j = nlb1 + 1, k = nlb2 + 1; j <= el1; j++, k++)
					{
						string tempDifference;
						zvread(iunit1, buf1, (char*)"LINE", j, NULL);
						zvread(iunit2, buf2, (char*)"LINE", k, NULL);
						// Loop through every fields and diff
						tempSame = compareOneLine (buf1, size1, buf2, size2, allFields, tempDifference);
						same = same && tempSame;
						if (!tempSame)
						{
							sprintf(tempErrMsg, "Line %ld differs at fields:", j-nlb1);
							differences += tempErrMsg;
							differences += tempDifference;
							differences += ".\n";
						}
					}

					// ----------------- End: Loop through every lines of image --------------------

					delete [] buf1;
					delete [] buf2;
				}
			}
			// --------------- End: Diff line prefix for all lines ----------------

			status = zvclose(iunit1,NULL);
			status = zvclose(iunit2,NULL);

			if (format1 != NULL) freeFormat(format1);
			if (format2 != NULL) freeFormat(format2);

			if (got_error)
			{
				zvmessage((char*)errmsg.c_str(), (char*)"ERROR");
				return false;
			}
			if (!params->silent())
			{
				if (!same)
				{
					zvmessage("Found following line prefix differences between given images:\n", NULL);
					print(differences);
					zvmessage("\n--------------- Line prefix differs. ----------------\n", NULL);
				}
				else
					zvmessage("----------- Line prefix comparison passed. ----------\n", NULL);
				zvmessage("\n", NULL);
			}
		}
	}
	catch (const exception &e)
	{
		zvmessage(const_cast<char*>(e.what()), (char*)"Exception::lineprefixdifC");
	}
	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" void FTN_NAME2(lineprefixdif, LINEPREFIXDIF) (int *status)
{
	*status = lineprefixdifC() ? 0 : 1;
}
