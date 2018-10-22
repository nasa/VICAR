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
09/13/2011        Raj       Initial Release

 */

#include "DifpicParameters.h"
#include "VicarException.h"
#include <cstdio>
#include <iostream>

// defines.h has some really horrible macros such as U
// which incidentally is a template item in C++ standard
// library. Thus the code won't compile if these C 
// includes are put before the C++ includes.
extern "C"
{
#include "defines.h"
#include "zvproto.h"
}

using namespace std;
/*
string jpl::mipl::p2::VicarImageLabel::INP_VAR_NAME="INP";
string jpl::mipl::p2::VicarImageLabel::LBLCMP_VAR_NAME="DIFF_LABELS";
string jpl::mipl::p2::VicarImageLabel::IGNORE_LABELS_VAR_NAME="IGNORE_LABELS";
string jpl::mipl::p2::VicarImageLabel::IGNORE_PROPERTIES_VAR_NAME="IGNORE_PROPERTIES";
 */
jpl::mipl::p2::difpic::DifpicParameters* jpl::mipl::p2::difpic::DifpicParameters::instance_=(jpl::mipl::p2::difpic::DifpicParameters*)NULL;
//====================================================================================
//====================================================================================
//====================================================================================
/**/
jpl::mipl::p2::difpic::DifpicParameters* 
jpl::mipl::p2::difpic::DifpicParameters::instance() throw(exception)
{
	if (jpl::mipl::p2::difpic::DifpicParameters::instance_==NULL)
	{
		jpl::mipl::p2::difpic::DifpicParameters::instance_=new DifpicParameters();
	}
	return jpl::mipl::p2::difpic::DifpicParameters::instance_;
}

//====================================================================================
//====================================================================================
//====================================================================================
jpl::mipl::p2::difpic::DifpicParameters::
DifpicParameters()
{
	this->getParameters();
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::difpic::DifpicParameters::
getMultiStringValueParam(const std::string& name,
		std::vector<std::string>& values,
		int maxLength,
		int minCount,
		int maxCount,
		bool fillTillMax)
{
	int count=0;
	int status = zvpcnt((char*)name.c_str(),&count);
	if (status != SUCCESS)
	{
		string emsg="Error obtaining count for ";
		emsg.append(name).append(" keyword.");
		zvmessage((char*)emsg.c_str(),NULL);
	}
	else
	{
		char *val = new char[maxLength];
		if (count<minCount || count>maxCount)
		{
			sprintf(val,"%s count=%d outside the range of [%d,%d]",name.c_str(),count,minCount,maxCount);
			zvmessage(val,const_cast<char*>("ERROR"));
		}
		else
		{
			int i=1;
			for (; i<=count; ++i)
			{
				status = zvpone((char*)name.c_str(),val,i,maxLength);
				if (status != SUCCESS)
				{
					sprintf(val,"Error obtaining value number %d for %s keyword. Return status=%d",i,name.c_str(),status);
					zvmessage(val,const_cast<char*>("ERROR"));
					values.clear();
					break;
				}
				else
				{
					values.push_back(string(val));
				}
			}
			if (fillTillMax)
				for (; i<=maxCount; ++i)
					values.push_back(string(val));
		}

		delete [] val;
	}
}
//====================================================================================
//====================================================================================
//====================================================================================
void jpl::mipl::p2::difpic::DifpicParameters::getParameters()
{

	int  status;
	const int max_val_size = 512;
	char val[max_val_size];
	char expanded[max_val_size];

	status = zvpone(const_cast<char*>("BHFMTMAP"), val, 1, max_val_size);
	if (status == SUCCESS)
	{
		status = zvfilename(val, expanded, max_val_size);
		if (status == SUCCESS)
			this->bhFmtMappingFile_ = string(expanded);
	}
	// if status fail, we ought to throw exception or something

	status = zvpone(const_cast<char*>("LPFMTMAP"), val, 1, max_val_size);
	if (status == SUCCESS)
	{
		status = zvfilename(val, expanded, max_val_size);
		if (status == SUCCESS)
			this->lpFmtMappingFile_ = string(expanded);
	}
	// if status fail, we ought to throw exception or something

        this->getMultiStringValueParam("BLTYPES", this->bltypes_, 32, 0, 2);

	if (zvptst(const_cast<char*>("ALL")))
		this->histDiffEnabled_=this->binDiffEnabled_=
				this->linePreDiffEnabled_=this->labelDiffEnabled_=pixDiffEnabled_=true;
	else
	{
		this->histDiffEnabled_=zvptst(const_cast<char*>("HSTCMP"));
		this->binDiffEnabled_=zvptst(const_cast<char*>("BINCMP"));
		this->linePreDiffEnabled_=zvptst(const_cast<char*>("LPRCMP"));
		this->pixDiffEnabled_=zvptst(const_cast<char*>("PIXCMP"));
		this->labelDiffEnabled_ = zvptst(const_cast<char*>("LBLCMP"));
	}

	this->silent_=zvptst(const_cast<char*>("SILENT"));

	this->getMultiStringValueParam("INP", this->inputFileNames_, 1024, 2,2, true);

	if (this->binaryHeaderDiffEnabled())
	{
		this->getMultiStringValueParam("BHFMTFILES",this->bhFMTFileNames_,1024,0,2);
		this->getMultiStringValueParam("BHK_IGNORE", this->bhFieldsToIgnore_, 128, 0, 600);
	}

	if (this->linePrefixDiffEnabled())
	{
		this->getMultiStringValueParam("LPFMTFILES",this->lpFMTFileNames_,1024,0,2);
		this->getMultiStringValueParam("LPK_IGNORE", this->lpFieldsToIgnore_, 128, 0, 600);
	}

	if (this->labelDiffEnabled())
	{
		this->getMultiStringValueParam("LBL_IGNORE",this->labelsToIgnore_,32,0,600);
		this->getMultiStringValueParam("PROP_IGNORE",this->propertiesToIgnore_,32,0,600);
	}
}
