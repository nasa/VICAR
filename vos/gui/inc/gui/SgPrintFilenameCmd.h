//////////////////////////////////////////////////////////////////////
//                                                                  //
//   SgPrintFilenameCmd.h:                                          //
//   Cmd class to select the printer file name  for printing        //
//   into a file given by _value.                                   //
//                                                                  //
//////////////////////////////////////////////////////////////////////
#ifndef SGPRINTFILENAMECMD_H
#define SGPRINTFILENAMECMD_H

#include "NoUndoCmd.h"
#include "SgPrintValue.h"

class SgPrintFilenameCmd : public NoUndoCmd {

  protected:

    SgPrintValue   *_printValue;

    virtual void doit() 
	{ _printValue->setFilename((char *)_value); }

  public:

    SgPrintFilenameCmd(const char *name, int active,
		       SgPrintValue *printValue) 
	: NoUndoCmd(name, active)
	{ _printValue = printValue; }

    virtual void freeValue(CmdValue value) 
	{ if (value) delete [] (char *)value; }

    virtual const char *const className() { return ("SgPrintFilenameCmd"); }

};

#endif
