//////////////////////////////////////////////////////////////////////
//                                                                  //
//   SgPrintFileTypeCmd.h: Cmd class to select file format for      //
//                        printing (VICAR or POSTSCRIPT)            //
//                                                                  //
//////////////////////////////////////////////////////////////////////
#ifndef SGPRINTFILETYPECMD_H
#define SGPRINTFILETYPECMD_H

#include "RadioCmd.h"
#include "SgPrintValue.h"

class SgPrintFileTypeCmd : public RadioCmd {

  protected:

    SgPrintValue   *_printValue;

    virtual void doit() 
	{
	    if(_value) {
		if(!strcmp(name(), "Vicar") )
		    _printValue->setFileType(VIC_FILE);
		else if(!strcmp(name(), "PostScript") )
		    _printValue->setFileType(PS_FILE);
		else
		    _printValue->setFileType(UNDEF_FILE);
	    }
	}


  public:

    SgPrintFileTypeCmd(const char *name, int active, CmdValue startState,
		 CmdList *radioCmdList, SgPrintValue *printValue)
	: RadioCmd(name, active, startState, radioCmdList)
	{ _printValue = printValue; }

    virtual const char *const className() { return("SgPrintFileTypeCmd"); }

};

#endif
