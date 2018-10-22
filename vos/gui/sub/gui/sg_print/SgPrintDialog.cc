////////////////////////////////////////////////////////////////////////
//  PrintDialog.cc: Source file for Print Dialog Box
////////////////////////////////////////////////////////////////////////
#include "SgPrintDialog.h"

#include "SgPrintValue.h"

#include "SgPrintToCmd.h"
#include "SgPrintFileTypeCmd.h"
#include "SgPrintPaperSizeCmd.h"
#include "SgPrintPrinterCmd.h"
#include "SgPrintFilenameCmd.h"

#include "SgKeyinCmdInterface.h"

#include "RadioCmdBox.h"
#include "CmdList.h"

#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>

SgPrintDialog::SgPrintDialog(const char *name, Cmd *printCmd, SgPrintValue *printValue)
    : CustomDialog(name, Visible, Visible, Visible, Visible, Invisible)
{
    _printValue = printValue ? printValue : new SgPrintValue();

    _printCmd = printCmd;
    
    // Create commands for selection of print destination

    _radioListDest = new CmdList();
    _toPrinterCmd = new SgPrintToCmd("Printer", True, 
			(CmdValue)(_printValue->getPrintTo() == DEST_PRINTER),
			_radioListDest, _printValue);
    _toFileCmd = new SgPrintToCmd("File", True, 
			(CmdValue)(_printValue->getPrintTo() == DEST_FILE),
			_radioListDest, _printValue);

    // Create commands for selection of file type for printing

    _radioListFormat = new CmdList();
    _asVicarCmd = new SgPrintFileTypeCmd("Vicar", True, 
			(CmdValue)(_printValue->getFileType() == VIC_FILE),
			_radioListFormat, _printValue);
    _asPsCmd = new SgPrintFileTypeCmd("PostScript", True, 
			(CmdValue)(_printValue->getFileType() == PS_FILE),
			_radioListFormat, _printValue);
    
    // Create commands for selection of paper size

    _radioListPaper = new CmdList();
    _paperA2Cmd = new SgPrintPaperSizeCmd("A2", True, 
			(CmdValue)(_printValue->getPaperSize() == A2),
			_radioListPaper, _printValue);
    _paperA3Cmd = new SgPrintPaperSizeCmd("A3", True, 
			(CmdValue)(_printValue->getPaperSize() == A3),
			_radioListPaper, _printValue); 
    _paperA4Cmd = new SgPrintPaperSizeCmd("A4", True,
                        (CmdValue)(_printValue->getPaperSize() == A4),
                        _radioListPaper, _printValue);
    _paperLetterCmd = new SgPrintPaperSizeCmd("Letter", True,
                        (CmdValue)(_printValue->getPaperSize() == LETTER),
                        _radioListPaper, _printValue);
    _paperLegalCmd = new SgPrintPaperSizeCmd("Legal", True,
                        (CmdValue)(_printValue->getPaperSize() == LEGAL),
                        _radioListPaper, _printValue);

    // Create commands for selection of printer file name

    _filenameCmd = new SgPrintFilenameCmd("PrntFile", True, _printValue);

    // Create a command for selection of the printer name / command

    _printerCmd = new SgPrintPrinterCmd("PrntName", True, _printValue);

    _toPrinterCmd->addToDeactivationList(_filenameCmd);
    _toPrinterCmd->addToActivationList(_printerCmd);
    _toPrinterCmd->addToDeactivationList(_asVicarCmd);
    _toPrinterCmd->addToDeactivationList(_asPsCmd);
 
    _toFileCmd->addToDeactivationList(_printerCmd);
    _toFileCmd->addToActivationList(_filenameCmd);
    _toFileCmd->addToActivationList(_asVicarCmd);
    _toFileCmd->addToActivationList(_asPsCmd);
}

SgPrintDialog::~SgPrintDialog()
{
    delete _toPrinterCmd;
    delete _toFileCmd;

    delete _asVicarCmd;
    delete _asPsCmd;

    delete _paperA2Cmd;
    delete _paperA3Cmd;
    delete _paperA4Cmd;
    delete _paperLetterCmd;
    delete _paperLegalCmd;

    delete _filenameCmd;
    delete _printerCmd;
    
    delete _radioListDest;
    delete _radioCmdBoxDest;

    delete _radioListFormat;
    delete _radioCmdBoxFormat;

    delete _radioListPaper;
    delete _radioCmdBoxPaper;
}

void SgPrintDialog::apply()
{
    // Copy from CustomDialog class (private !!)

    if (_applyCmdList) { 
	_applyCmdList->execute();
	_applyCmdList->clear();
    }

    // Execute printing with new print value

    SgPrintValue *value = new SgPrintValue(*_printValue);
    _printCmd->execute((CmdValue)value);
}

Widget SgPrintDialog::createWorkArea(Widget parent)
{
    /////////////////////////////////////////////////////////
    // First, create all the containers
    /////////////////////////////////////////////////////////

    Widget big_rc = XtVaCreateWidget("PrintDialog", 
				     xmRowColumnWidgetClass, parent,
				     XmNorientation, XmVERTICAL, 
				     NULL);
    
    // Create a frame with title for print destination

    Widget printToFrame = XtVaCreateManagedWidget("PrintToFrame",
			xmFrameWidgetClass, big_rc,
			NULL );
    XtVaCreateManagedWidget ("PrintToFrameLabel",
			xmLabelGadgetClass, printToFrame,
			XmNchildType, XmFRAME_TITLE_CHILD,
			XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
			NULL );
    Widget rcDest = XtVaCreateManagedWidget("PrintToRc1",
			xmRowColumnWidgetClass, printToFrame,
			XmNorientation, XmVERTICAL,
			NULL);
    Widget rcDest1 = XtVaCreateManagedWidget("PrintToRc2",
			xmRowColumnWidgetClass, rcDest,
			NULL);
    Widget rcDest2 = XtVaCreateManagedWidget("DestDialog2",
			xmRowColumnWidgetClass, rcDest,
			XmNorientation, XmVERTICAL,
			NULL);
    
    // Create a frame with title for print file format  

    Widget fileFormatFrame = XtVaCreateManagedWidget("FileFormatFrame",
			xmFrameWidgetClass, big_rc,
			NULL );
    XtVaCreateManagedWidget ("FileFormatFrameLabel",
			xmLabelGadgetClass, fileFormatFrame,
			XmNchildType, XmFRAME_TITLE_CHILD,
			XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
			NULL );
    Widget rcFile = XtVaCreateManagedWidget("FileDialog",
			xmRowColumnWidgetClass, fileFormatFrame,
			XmNorientation, XmVERTICAL,
			NULL);
    
    // Create a frame with title for print execution type

    Widget paperFrame = XtVaCreateManagedWidget("PaperSizeFrame",
			xmFrameWidgetClass, big_rc,
			NULL );
    XtVaCreateManagedWidget ("PaperSizeFrameLabel",
			xmLabelGadgetClass, paperFrame,
			XmNchildType, XmFRAME_TITLE_CHILD,
			XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
			NULL );
    Widget rcPaper = XtVaCreateManagedWidget("PaperSizeRc",
			xmRowColumnWidgetClass, paperFrame,
			XmNorientation, XmVERTICAL,
			NULL);

    /////////////////////////////////////////////////////////
    // Now create interfaces
    /////////////////////////////////////////////////////////

    _radioCmdBoxDest = new RadioCmdBox(rcDest1, "RadioCmdBox",
				       _radioListDest);
    _radioCmdBoxFormat = new RadioCmdBox(rcFile, "RadioCmdBox",
				       _radioListFormat);
    _radioCmdBoxPaper = new RadioCmdBox(rcPaper, "RadioCmdBox",
				       _radioListPaper);
    
    // Create interfaces for print file selection and selection of 
    // the printer name/command

    CmdInterface *ci1 = new SgKeyinCmdInterface(rcDest2, _filenameCmd);
    ci1->manage();

    CmdInterface *ci2 = new SgKeyinCmdInterface(rcDest2, _printerCmd);
    ci2->manage();

    // Manage Commands

    _radioCmdBoxDest->manage();
    _radioCmdBoxFormat->manage();
    _radioCmdBoxPaper->manage();

    _filenameCmd->execute((CmdValue)_printValue->getFilename());
    _printerCmd->execute((CmdValue)_printValue->getPrinterCmd());
    _asVicarCmd->execute((CmdValue)(_printValue->getFileType() == VIC_FILE));
    _asPsCmd->execute((CmdValue)(_printValue->getFileType() == PS_FILE));
    _paperA2Cmd->execute((CmdValue)(_printValue->getPaperSize() == A2));
    _paperA3Cmd->execute((CmdValue)(_printValue->getPaperSize() == A3));
    _paperA4Cmd->execute((CmdValue)(_printValue->getPaperSize() == A4));
    _paperLetterCmd->execute((CmdValue)(_printValue->getPaperSize() ==LETTER));
    _paperLegalCmd->execute((CmdValue)(_printValue->getPaperSize() == LEGAL));

    if (_printValue->getPrintTo() == DEST_PRINTER)
	_toPrinterCmd->execute(
	    (CmdValue)(_printValue->getPrintTo() == DEST_PRINTER));
    else if (_printValue->getPrintTo() == DEST_FILE)
	_toFileCmd->execute(
	    (CmdValue)(_printValue->getPrintTo() == DEST_FILE));

    return big_rc;
}



