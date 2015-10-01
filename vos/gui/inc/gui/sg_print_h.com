$!****************************************************************************
$!
$! Build proc for MIPL module sg_print_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:38
$!
$! Execute by entering:		$ @sg_print_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module sg_print_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sg_print_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sg_print_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @sg_print_h.bld "STD"
$   else
$      @sg_print_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_print_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_print_h.com -mixed -
	-s SgPrintDialog.h VicarToPs.h SgPrintValue.h SgPrintToCmd.h -
	   SgPrintFileTypeCmd.h SgPrintPaperSizeCmd.h SgPrintPrinterCmd.h -
	   SgPrintFilenameCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgPrintDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SgPrintDialog.h: Creates a dialog for printing 
///////////////////////////////////////////////////////////////////
#ifndef SGPRINTDIALOG_H
#define SGPRINTDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class SgPrintValue;
class Cmd;
class RadioCmdBox;

class SgPrintDialog: public CustomDialog {

  private:
  
    SgPrintValue    *_printValue;
    Cmd               *_printCmd;

    Cmd               *_toPrinterCmd;
    Cmd               *_toFileCmd;

    Cmd               *_filenameCmd;
    Cmd               *_printerCmd;

    Cmd               *_asVicarCmd;
    Cmd               *_asPsCmd;

    Cmd               *_paperA2Cmd;
    Cmd               *_paperA3Cmd;
    Cmd               *_paperA4Cmd;
    Cmd               *_paperLetterCmd;
    Cmd               *_paperLegalCmd;

    CmdList           *_radioListDest;
    RadioCmdBox       *_radioCmdBoxDest;

    CmdList           *_radioListFormat;
    RadioCmdBox       *_radioCmdBoxFormat;

    CmdList           *_radioListPaper;
    RadioCmdBox       *_radioCmdBoxPaper;

    virtual void apply();

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    SgPrintDialog(const char *name, Cmd *printCmd, SgPrintValue *printValue = NULL);
    ~SgPrintDialog();

    virtual Widget createWorkArea(Widget);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarToPs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// VicarToPostScript - function which converts a VICAR-File
// into a PostScript-File in order to print the main widget
// either into a file or send it imidiatly to the printer.
// Author: Rainer Berlin, DLR
// Modifications: Vadim Parizher, JPL-MIPS
//////////////////////////////////////////////////////////////////////////////

#ifndef VICAR_TO_PS_H
#define VICAR_TO_PS_H

#include <stdio.h>

struct PaperSize {
    const int width;
    const int height;
    const int xpos;
    const int ypos;
    PaperSize(int w, int h, int x, int y)
        : width(w), height(h), xpos(x), ypos(y) { }
};
 
const PaperSize PaperA4(540, 780, 30, 30);
const PaperSize PaperA3(780, 1080, 30, 30);
const PaperSize PaperA2(1080, 1560, 30, 30);
const PaperSize PaperLetter(540, 720, 36, 36);
const PaperSize PaperLegal(720, 1008, 36, 36);

int vicImageToPostScript(char *vicFile, char *psFile, char *title, 
			 PaperSize psize = PaperLetter);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgPrintValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgPrintToCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgPrintFileTypeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgPrintPaperSizeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgPrintPrinterCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgPrintFilenameCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
