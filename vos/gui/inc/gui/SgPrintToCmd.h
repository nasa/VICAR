//////////////////////////////////////////////////////////////////////
//                                                                  //
//   SgPrintToCmd.h: Cmd class to select the destination for        //
//                        printing (File or Printer)                //
//                                                                  //
//////////////////////////////////////////////////////////////////////
#ifndef SGPRINTTOCMD_H
#define SGPRINTTOCMD_H

#include "RadioCmd.h"
#include "SgPrintValue.h"

class SgPrintToCmd : public RadioCmd {

  protected:

    SgPrintValue   *_printValue;

    virtual void doit() 
	{
	    if(_value) {
		if(!strcmp(name(), "Printer") )
		    _printValue->setPrintTo(DEST_PRINTER);
		else if(!strcmp(name(), "File") )
		    _printValue->setPrintTo(DEST_FILE);
		else
		    _printValue->setPrintTo(DEST_UNDEF);
	    }
	}


  public:

    SgPrintToCmd(const char *name, int active, CmdValue startState,
		 CmdList *radioCmdList, SgPrintValue *printValue)
	: RadioCmd(name, active, startState, radioCmdList)
	{ _printValue = printValue; }

    virtual const char *const className() { return("SgPrintToCmd"); }

};

#endif
