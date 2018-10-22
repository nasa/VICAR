//////////////////////////////////////////////////////////////////////
//                                                                  //
//   SgPrintPaperSizeCmd.h: Cmd class to select paper size for      //
//                        printing (A2, A3, A4, LETTER, or LEGAL    //
//                                                                  //
//////////////////////////////////////////////////////////////////////
#ifndef SGPRINTPAPERSIZECMD_H
#define SGPRINTPAPERSIZECMD_H

#include "RadioCmd.h"
#include "SgPrintValue.h"

class SgPrintPaperSizeCmd : public RadioCmd {

  protected:

    SgPrintValue   *_printValue;

    virtual void doit() 
	{
	    if(_value) {
		if(!strcmp(name(), "A2") )
		    _printValue->setPaperSize(A2);
		else if(!strcmp(name(), "A3") )
		    _printValue->setPaperSize(A3);
		else if(!strcmp(name(), "A4") )
                    _printValue->setPaperSize(A4);
		else if(!strcmp(name(), "Letter") )
                    _printValue->setPaperSize(LETTER);
		else if(!strcmp(name(), "Legal") )
                    _printValue->setPaperSize(LEGAL);
		else
		    _printValue->setPaperSize(UNDEF_PAPER_SIZE);
	    }
	}


  public:

    SgPrintPaperSizeCmd(const char *name, int active, CmdValue startState,
		 CmdList *radioCmdList, SgPrintValue *printValue)
	: RadioCmd(name, active, startState, radioCmdList)
	{ _printValue = printValue; }

    virtual const char *const className() { return("SgPrintPaperSizeCmd"); }

};

#endif
