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
#ifndef __DIFF2_PARAMETERS_H__
#define __DIFF2_PARAMETERS_H__

#include <cstdarg>
#include <string>
#include <vector>
//using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{
namespace difpic
{
class DifpicParameters
{

public:
	//DifpicParameters() throw (std::exception);
	virtual ~DifpicParameters() {}
	static DifpicParameters* instance() throw (std::exception);
	/*
	    {
	      if (instance_==NULL)
		instance_=new DifpicParameters();
	      return instance_;
	    }
	 */
	bool  silent() const { return this->silent_; }
	bool  pixDiffEnabled() const { return this->pixDiffEnabled_; }
	bool  labelDiffEnabled() const { return this->labelDiffEnabled_; }
	bool  historyDiffEnabled() const { return this->histDiffEnabled_; }
	bool  binaryHeaderDiffEnabled() const { return this->binDiffEnabled_; }
	bool  linePrefixDiffEnabled() const { return this->linePreDiffEnabled_; }
	const std::string& getBinaryHeaderFMTMappingFile() const { return this->bhFmtMappingFile_; }
	const std::string& getLinePrefixFMTMappingFile() const { return this->lpFmtMappingFile_; }
	const std::vector<std::string>& getBlTypes() const { return this->bltypes_; }
	const std::vector<std::string>& getBinaryHeaderFMTFileNames() const { return this->bhFMTFileNames_; }
	const std::vector<std::string>& getLinePrefixFMTFileNames() const { return this->lpFMTFileNames_; }
	const std::vector<std::string>& labelsToIgnore() const { return this->labelsToIgnore_; }
	const std::vector<std::string>& propertiesToIgnore() const { return this->propertiesToIgnore_; }

	const std::vector<std::string>& getInputFileNames() const { return this->inputFileNames_; }
	const std::vector<std::string>& bhFieldsToIgnore() const { return this->bhFieldsToIgnore_; }
	const std::vector<std::string>& lpFieldsToIgnore() const { return this->lpFieldsToIgnore_; }
protected:

	DifpicParameters();
	static DifpicParameters* instance_;
	virtual void getParameters();

	std::vector <std::string> labelsToIgnore_;
	std::vector <std::string> propertiesToIgnore_;
	std::vector<std::string> bhFMTFileNames_;
	std::vector<std::string> lpFMTFileNames_;
	std::vector<std::string> bhFieldsToIgnore_;
	std::vector<std::string> lpFieldsToIgnore_;
	std::vector<std::string> inputFileNames_;
	std::vector<std::string> bltypes_;
	std::string bhFmtMappingFile_;
	std::string lpFmtMappingFile_;

	void getMultiStringValueParam(const std::string& name,
			std::vector<std::string>& values,
			int maxLength,
			int minCount,
			int maxCount,
			bool fillTillMax=false);

	bool silent_, labelDiffEnabled_,
	histDiffEnabled_,pixDiffEnabled_,
	binDiffEnabled_, linePreDiffEnabled_;
};
};
};
};
};
#endif
