//////////////////////////////////////////////////////////////////////
//                                                                  //
//   SgPrintPrinterCmd.h:                                          //
//   Cmd class to select the printer file name  for printing        //
//   into a file given by _value.                                   //
//                                                                  //
//////////////////////////////////////////////////////////////////////
#ifndef SGPRINTPRINTERCMD_H
#define SGPRINTPRINTERCMD_H

#include "NoUndoCmd.h"
#include "SgPrintValue.h"

class SgPrintPrinterCmd : public NoUndoCmd {

  protected:

    SgPrintValue   *_printValue;

    virtual void doit() 
	{ _printValue->setPrinterCmd((char *)_value); }

  public:

    SgPrintPrinterCmd(const char *name, int active,
		       SgPrintValue *printValue) 
	: NoUndoCmd(name, active)
	{ _printValue = printValue; }

    virtual void freeValue(CmdValue value) 
	{ if (value) delete [] (char *)value; }

    virtual const char *const className() { return ("SgPrintPrinterCmd"); }

};

#endif
