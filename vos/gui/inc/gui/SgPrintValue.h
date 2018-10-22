//////////////////////////////////////////////////////////////////////////
// SgPrintValue.h: Contains all print parameter which can be set
// by using the Print Dialog
//////////////////////////////////////////////////////////////////////////

#ifndef SGPRINTVALUE_H
#define SGPRINTVALUE_H

#include "BasicComponent.h"
#include <Xm/Xm.h>              // only for Boolean!

enum SgPrintTo { DEST_PRINTER, DEST_FILE, DEST_UNDEF };
enum SgPrintFileType { VIC_FILE, PS_FILE, UNDEF_FILE };
enum SgPrintPaperSize { A2, A3, A4, LETTER, LEGAL, UNDEF_PAPER_SIZE };

class SgPrintValue {

  protected:
    
    char            *_filename;
    char            *_printerCmd;
    SgPrintTo        _printTo;
    SgPrintFileType  _fileType;
    SgPrintPaperSize _paperSize;
    
  public:
    
    SgPrintValue() {
	_filename = strdup("scratch");
	_printerCmd = strdup("lp");
	_printTo = DEST_PRINTER;
	_fileType = VIC_FILE;
	_paperSize = LETTER;
    }

    SgPrintValue(SgPrintValue &value) {
	if (value._filename)
	    _filename = strdup(value._filename);
	else
	    _filename = NULL;

	if (value._printerCmd)
	    _printerCmd = strdup(value._printerCmd);
	else 
	    _printerCmd = NULL;

	_printTo = value._printTo;
	_fileType = value._fileType;
	_paperSize = value._paperSize;
    }

    virtual ~SgPrintValue() {
	if (_filename) delete [] _filename;
	if (_printerCmd) delete [] _printerCmd;
    }
    
    void setFilename(char *name) 
	{ if (_filename) delete [] _filename;
	  _filename = strdup(name); 
	}
    char *getFilename() { return strdup(_filename); }

    void setPrinterCmd(char *name) 
	{ if (_printerCmd) delete [] _printerCmd;
	  _printerCmd = strdup(name); 
	}
    char *getPrinterCmd() { return strdup(_printerCmd); }

    void setPrintTo(SgPrintTo dest) { _printTo = dest; }
    SgPrintTo getPrintTo() { return _printTo; }

    void setFileType(SgPrintFileType type) { _fileType = type; }
    SgPrintFileType getFileType() { return _fileType; }

    void setPaperSize(SgPrintPaperSize paper) { _paperSize = paper; }
    SgPrintPaperSize getPaperSize() { return _paperSize; }
    
    virtual const char *const className() { return "SgPrintValue"; }
    
};
#endif

