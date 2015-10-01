/*
 * balm.h
 *
 *  Created on: Aug 17, 2011
 *      Author: honghanh
 */
#ifndef BALM_H_
#define BALM_H_

#define OUT

extern "C"
{
#include "oal.h"
#include "lablib3.h"
}

#include <cstring>
#include <cctype>
#include <string>
#include <vector>
#include <stack>
#include <iostream>

using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{

typedef ODLTREE BalmFormat;

class bad_fmt_exception : public exception
{
public:
	string msg;

	bad_fmt_exception(string m) : msg(m) {}
	~bad_fmt_exception() throw() {}
	const char* what() const throw () { return msg.c_str(); }
};

class BalmField
{
	string name_;
	bool isBit_;
	int type_;
	size_t start_;
	size_t length_;

	static unsigned long masks[sizeof(unsigned long) << 3];
	static bool initialized;

	static void init();

	bool symbolizeOdlText (char* odlText, string& error);
	bool symbolizeOdlSymbol (char* odlSymbol, string& error);
	bool stripQuotes(char* odlTextOrSymbol, string& error);
	bool setBitType (char* object_class);
	bool setType(char* symbol);
	void setName (char* symbol, string base_name);
	int getGenericValue (const void* bytearr, size_t size, OUT void* value, size_t value_byte_size);
	// because this is a template function
	// its definition has to go with its declaration
	template <class myType>
	int getValueHelper_ (const void* bytearr, size_t size, OUT myType* value)
	{
		size_t len = this->length_;
		char* v = new char[len];
		int result = this->getGenericValue(bytearr, size, v, len);

		if (result == BalmField::fld_success)
		{
			switch (this->type_)
			{
			case BalmField::_LSB_FLOAT:
			case BalmField::_MSB_FLOAT:
			{
				// using if-else is better than using switch
				// because the size of some type maybe equal to each other
				// and switch statement is not allowed to have duplicate
				// although this doesn't tend to happen with floating point type
				if (len == sizeof(float))
					*value = *((float*)v);
				else if (len == sizeof(double))
					*value = *((double*)v);
				else
					result = BalmField::fld_wrong_type;
			}
			break;
			case BalmField::_MSB_UNSIGNED_INTEGER:
			case BalmField::_LSB_UNSIGNED_INTEGER:
			{
				// using if-else is better than using switch
				// because the size of some type maybe equal to each other
				// and switch statement is not allowed to have duplicate
				if (len == sizeof (unsigned char))
					*value = *((unsigned char*)v);
				else if (len == sizeof (unsigned short))
					*value = *((unsigned short*)v);
				else if (len == sizeof (unsigned int))
					*value = *((unsigned int*)v);
				else if (len == sizeof (unsigned long))
					*value = *((unsigned long*)v);
				else
					result = BalmField::fld_wrong_type;
			}
			break;
			case BalmField::_MSB_SIGNED_INTEGER:
			case BalmField::_LSB_SIGNED_INTEGER:
			{
				// using if-else is better than using switch
				// because the size of some type maybe equal to each other
				// and switch statement is not allowed to have duplicate
				if (len == sizeof(char))
					*value = *v;
				else if (len == sizeof (short))
					*value = *((short*)v);
				else if (len == sizeof (int))
					*value = *((int*)v);
				else if (len == sizeof (long))
					*value = *((long*)v);
				else
					result = BalmField::fld_wrong_type;
			}
			break;
			case BalmField::_BYTE:
			{
				if (len >= sizeof(myType))
					*value = *((myType*)v);
				else
				{
					*value = 0;
					memcpy(value, v,len);
				}
			}
			break;
			case BalmField::_BOOLEAN:
			{
				// a sane person should never call this method on BOOLEAN type
				// but who am I to force people to be sane?
				*value = *((bool*)v);
			}
			break;
			default:
				result = BalmField::fld_wrong_type;
			}
		}

		return result;
	}

protected:
	// type constants
	// there are less than 16 primitive types in C/C++
	// so this mechanism should be okay even on platform with int size of 2 bytes

	const static int _UNSET = 0;
	// it's VERY important that these actual values are non-zero and a power of 2
	const static int _BYTE = 1;
	const static int _LSB_UNSIGNED_INTEGER = 1 << 1;
	const static int _MSB_UNSIGNED_INTEGER = 1 << 2;
	const static int _LSB_SIGNED_INTEGER = 1 << 3;
	const static int _MSB_SIGNED_INTEGER = 1 << 4;
	const static int _BOOLEAN = 1 << 5;
	const static int _LSB_FLOAT = 1 << 6;
	const static int _MSB_FLOAT = 1 << 7;

	// this constructor is for testing purposes
	BalmField(string name, bool isBit, int type, size_t start, size_t length);

public:

	const static int UNSET = _UNSET;
	const static int BYTE = _BYTE;
	const static int UNSIGNED_INTEGER = _LSB_UNSIGNED_INTEGER | _MSB_UNSIGNED_INTEGER;
	const static int SIGNED_INTEGER = _LSB_SIGNED_INTEGER | _MSB_SIGNED_INTEGER;
	const static int BOOLEAN = _BOOLEAN;
	const static int FLOAT = _LSB_FLOAT | _MSB_FLOAT;

	// these constants are status code from get*Value functions.
	// they should be in uppercase as they are constant.
	// however, both OK and SUCCESS are taken as defined macro
	// so I have to write mine in lower case.
	const static int fld_success = 0;
	const static int fld_wrong_type = 1;
	const static int fld_output_size_too_small = 2;
	const static int fld_input_size_too_small = 3;
	const static int fld_bitstr_too_long = 4;
	const static int fld_incorrect_output_size = 5;

	BalmField();
	BalmField(BalmFormat tree_node, string base_name) throw (bad_fmt_exception);
	~BalmField();

	inline string getName() const { return this->name_; }
	inline int getType() const
	{
		switch (this->type_)
		{
		case _BOOLEAN: return BOOLEAN;
		case _BYTE: return BYTE;
		case _LSB_FLOAT:
		case _MSB_FLOAT: return FLOAT;
		case _LSB_UNSIGNED_INTEGER:
		case _MSB_UNSIGNED_INTEGER: return UNSIGNED_INTEGER;
		case _LSB_SIGNED_INTEGER:
		case _MSB_SIGNED_INTEGER: return SIGNED_INTEGER;
		default: return UNSET;
		}
	}
	inline size_t getStart() const { return this->start_; }
	inline size_t getLength() const { return this->length_; }
	inline bool isBitField() const { return this->isBit_; }
	friend ostream& operator<< (ostream& os, const BalmField& f);

	int getByteValue (const void* bytearr, size_t size, OUT unsigned char* value);
	int getByteValue (const void* bytearr, size_t size, OUT char* value);
	int getByteStream (const void* bytearr, size_t size, OUT void* value, unsigned int value_byte_size);

	int getRealValue (const void* bytearr, size_t size, OUT float* value);
	int getRealValue (const void* bytearr, size_t size, OUT double* value);

	int getIntegerValue (const void* bytearr, size_t size, OUT short* value);
	int getIntegerValue (const void* bytearr, size_t size, OUT int* value);
	int getIntegerValue (const void* bytearr, size_t size, OUT long* value);
	int getIntegerValue (const void* bytearr, size_t size, OUT unsigned short* value);
	int getIntegerValue (const void* bytearr, size_t size, OUT unsigned int* value);
	int getIntegerValue (const void* bytearr, size_t size, OUT unsigned long* value);

	int getBitString (const void* bytearr, size_t size, OUT unsigned long* value);

	int getBoolean (const void* bytearr, size_t size, OUT bool* value);
};

// try unsigned long long
// routine to read binary header (lp per line) in 1,2 lines
BalmFormat parseFormat (const char* formatFileName);
void freeFormat(BalmFormat format);
bool getAllFields (BalmFormat format, OUT vector<BalmField>& fields, OUT string& error);
const BalmField* getField (const vector<BalmField>& fields, const string& name, size_t closestStartValue);
inline bool isLsb ();
ostream& operator<< (ostream& os, const BalmField& f);
bool iequals (const string& str1, const string& str2);
};
};
};

#endif /* BALM_H_ */
