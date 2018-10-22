///////////////////////////////////////////////////////////////////////////////
// TpPrintCmd.cc: This is a command that allows user to print visual part of 
// tp images as ps files or save them vicar rgb files.  The value for this 
// command should be provided via the print dialog.
///////////////////////////////////////////////////////////////////////////////
#include "TpPrintCmd.h"
#include "TpDisplayer.h"
#include "TpSubDisplayer.h"
#include "TpImageView.h"
#include "ImageData.h"
#include "Application.h"
#include "WidgetSnapshot.h"
#include "VicarToPs.h"
#include "XvicImage.h"          // for Xvic functions
#include "XImageToVicar.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#if !defined(__VMS)
  #include <unistd.h>
#else
  #include <unixlib.h>
#endif

TpPrintCmd::TpPrintCmd(const char *name, int active, TpDisplayer *displayer,
		       SgPrintValue *printValue)
    : NoUndoCmd(name, active)
{
    _displayer = displayer;
    _value = (CmdValue)printValue;
    _printValue = NULL;
}

void TpPrintCmd::doit()
{
    theApplication->setBusyCursor();

    _printValue = (SgPrintValue *)_value;

    const PaperSize *paperSize;
    if (_printValue->getPaperSize() == A2)
	paperSize = &PaperA2;
    if (_printValue->getPaperSize() == A3)
        paperSize = &PaperA3;
    if (_printValue->getPaperSize() == A4)
        paperSize = &PaperA4;
    if (_printValue->getPaperSize() == LETTER)
        paperSize = &PaperLetter;
    if (_printValue->getPaperSize() == LEGAL)
        paperSize = &PaperLegal;

    for (int i = 0; i < _displayer->getNumImages(); i++) {

	char title[1024];
	if ((_printValue->getFileType() == PS_FILE) ||
	    (_printValue->getPrintTo() == DEST_PRINTER)) {
	    
	    // Get the time in seconds
	    
	    time_t aclock;
	    time(&aclock);
	    struct tm *newtime;
	    newtime = localtime(&aclock);
	    char *currDate = sdup(asctime(newtime));
	    char *date = &currDate[4];

	    sprintf(title, "Image: %s       Date: %s",
		    _displayer->getImage(i)->getImageData()->getInputDataSourceName(), 
		    date);
	}

	char red[1024], grn[1024], blu[1024], ps[1024];

	if ((_printValue->getPrintTo() == DEST_FILE)) {
	    sprintf(red, "%s%d.red", _printValue->getFilename(), i);
	    sprintf(grn, "%s%d.grn", _printValue->getFilename(), i);
	    sprintf(blu, "%s%d.blu", _printValue->getFilename(), i); 
	    sprintf(ps, "%s%d.ps", _printValue->getFilename(), i);
	}
	else {
#if defined(__VMS)
            sprintf(red, "%s_%d_%d.red", "tp", getpid() % 1000, i);
            sprintf(grn, "%s_%d_%d.grn", "tp", getpid() % 1000, i);
            sprintf(blu, "%s_%d_%d.blu", "tp", getpid() % 1000, i);
            sprintf(ps, "%s_%d_%d.ps", "tp", getpid() % 1000, i);
#else
	    sprintf(red, "/tmp/%s%d%d.red", "tp", getpid(), i);
            sprintf(grn, "/tmp/%s%d%d.grn", "tp", getpid(), i);
            sprintf(blu, "/tmp/%s%d%d.blu", "tp", getpid(), i);
            sprintf(ps, "/tmp/%s%d%d.ps", "tp", getpid(), i);
#endif
        }

	Widget iw = _displayer->getImage(i)->getImageView()->getWidget();
	unsigned char wpp;
	XtVaGetValues(iw, XvicNworkProcPolicy, &wpp, NULL);
	XtVaSetValues(iw, XvicNworkProcPolicy, XvicNONE, NULL);
	XImageHandler *imgHandler = takeWidgetSnapshot(iw);
	writeXImageToVicarFile(imgHandler, red, grn, blu, 0, 0, 0, 0);
	XtVaSetValues(iw, XvicNworkProcPolicy, wpp, NULL);

	if( (_printValue->getFileType() == PS_FILE) ||
	    (_printValue->getPrintTo() == DEST_PRINTER)) { 
	    vicImageToPostScript(grn, ps, title, *paperSize);

	    char command[1024];
#ifndef __VMS
	    sprintf(command, "rm %s", red);
#else
	    sprintf(command, "delete %s;*", red);
#endif
	    system(command);

#ifndef __VMS
	    sprintf(command, "rm %s", grn);
#else
            sprintf(command, "delete %s;*", grn);
#endif
	    system(command);

#ifndef __VMS
	    sprintf(command, "rm %s", blu);
#else
            sprintf(command, "delete %s;*", blu);
#endif
	    system(command);

	    if( _printValue->getPrintTo() == DEST_PRINTER) { 
		sprintf(command, "%s %s", _printValue->getPrinterCmd(), ps);
		system(command);
#ifndef __VMS
		sprintf(command, "rm %s", ps);
#else
		sprintf(command, "delete %s;*", ps);
#endif
		system(command);
	    }
	}
	freeWidgetSnapshot(imgHandler);
    }

    theApplication->removeBusyCursor();
}
