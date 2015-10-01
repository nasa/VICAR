$!****************************************************************************
$!
$! Build proc for MIPL module sg_print
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:34
$!
$! Execute by entering:		$ @sg_print
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module sg_print ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sg_print.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sg_print.imake") .nes. ""
$   then
$      vimake sg_print
$      purge sg_print.bld
$   else
$      if F$SEARCH("sg_print.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sg_print
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sg_print.bld "STD"
$   else
$      @sg_print.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_print.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_print.com -mixed -
	-s SgPrintDialog.cc VicarToPs.cc -
	-i sg_print.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgPrintDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarToPs.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// VicarToPostScript - function which converts a VICAR-File
// into a PostScript-File in order to print the main widget
// either into a file or send it imidiatly to the printer
///////////////////////////////////////////////////////////////////////////////
#include "VicarToPs.h"
#include "zvproto.h"    // prototypes of RTL (zvopen ...)

#include <iostream>
using namespace std;
#include <string.h>
#include <time.h> 
#include <stdlib.h>

/*-------------------------------------------------------------------------*/
/*       Writes a title above and a frame around the VICAR image           */
/*-------------------------------------------------------------------------*/
void WriteFrameAndTitle(FILE *psFile, char *title)
{
    fprintf(psFile, "%s\n", "%-------------- Image Frame ---------------");
    fprintf(psFile, "gsave\n");
    fprintf(psFile, "XPos YPos translate\n");
    fprintf(psFile, "width height\n");
    fprintf(psFile, "printFrame\n");
    fprintf(psFile, "%s\n", "%-------------- Image Title ---------------");
    fprintf(psFile, "height 5 add\n");
    fprintf(psFile, "(%s) printTitle\n", title);
    fprintf(psFile, "grestore\n\n");
}

/*-------------------------------------------------------------------------*/
/*            Writes the header into the PostScript-File                   */
/*-------------------------------------------------------------------------*/
void WriteHeader(FILE *psFile)
{
    char dateStr[80];
    char currDate[50];
    struct tm *newtime;
    time_t aclock;
    
    // Get the curent system date

    time(&aclock);                      // get the time in seconds
    newtime = localtime( &aclock);      // converts time to struct tm form
    strcpy(currDate, asctime(newtime));
    sprintf(dateStr, "%%CreationDate:   %s", currDate);
    
    // Set up the ADOBE POSTSCRIPT (R)  Page description Comments
    
    fprintf( psFile, "%s\n",       "%!PS-Adobe-2.0");
    fprintf( psFile, "%s\n",       "%%Creator:       TP Tiepoint Program");
    fprintf( psFile, "%s\n",       "%%Title:         TP Printout");
    fprintf( psFile, "%s",         dateStr);
    fprintf( psFile, "%s\n",       "%%Pages:         1");
    fprintf( psFile, "%s\n",       "%%DocumentFonts: Times-Roman");
    fprintf( psFile, "%s\n\n",     "%%EndComments");
}

/*-------------------------------------------------------------------------*/
/*        Writes the variable definitions into the PostScript-File         */
/*-------------------------------------------------------------------------*/
void WriteVariableDefinitions(FILE *psFile, int nl, int ns, PaperSize psize)
{
    int h, w, x, y;           // used page size and position
    float lsq, slq;           // Ratios nl/ns and ns/nl
    float hwq;                // Ratio height/width
    
    lsq = (float)nl / (float)ns;
    slq = (float)ns / (float)nl;
    hwq = (float)psize.height / (float)psize.width;

    if(lsq > hwq) { 
	w = (int) ( ((float)psize.height * (float)ns) / (float)nl );
	h = psize.height;
	x = (int)( (float)(psize.width - w) / 2.0 ) + psize.xpos;
	y = psize.ypos;
    }
    else if(lsq < hwq) {
	w = psize.width;
	h = (int) ( ((float)psize.width * (float)nl) / (float)ns );
	x = psize.xpos;
	y = (int)( (float)(psize.height - h) / 2.0 ) + psize.ypos;
    }
    else { 
	w = psize.width;
	h = psize.height;
	x = psize.xpos;
	y = psize.ypos;
    }
    
    /*  Define Variables */
    
    fprintf(psFile, "%s\n", "%--------- Define Variables ----------------");
    fprintf(psFile, "/width    %3d def  %s\n", w, "%Page Width");
    fprintf(psFile, "/height   %3d def  %s\n", h, "%page Height");
    fprintf(psFile, "/XPos     %3d def  %s\n", x, "%left start X-Position");
    fprintf(psFile, "/YPos     %3d def  %s\n", y, "%bottom start Y-Position");
    fprintf(psFile, "/Margin    10 def  %s\n", "%margin beetwin plots");
    fprintf(psFile, "/basefont  /Times-Roman findfont def\n\n");
}

/*-------------------------------------------------------------------------*/
/*            Writes the PostScript procedures into the file               */
/*-------------------------------------------------------------------------*/
void WriteProcedureDefinitions(FILE *psFile)
{
    /* Define Procedures */
    
    fprintf(psFile, "%s\n", "%--------- Define Procedures ---------------");
    fprintf(psFile, "/printFrame       %s\n", "%stack -> height, width");
    fprintf(psFile, "  {  newpath\n");
    fprintf(psFile, "     2 copy\n");
    fprintf(psFile, "     0 0 moveto\n");
    fprintf(psFile, "     0 exch rlineto\n");
    fprintf(psFile, "     0 rlineto\n");
    fprintf(psFile, "     0 exch neg rlineto\n");
    fprintf(psFile, "     neg 0 rlineto\n");
    fprintf(psFile, "     closepath\n");
    fprintf(psFile, "     stroke  } def\n\n");
    
    fprintf(psFile, "/printTitle       %s\n", 
	    "%stack -> title string, height");
    fprintf(psFile, "  { /title exch def\n");
    fprintf(psFile, "    /hoch exch def\n");
    fprintf(psFile, "    basefont findfont 15 scalefont setfont\n");
    fprintf(psFile, "    .2 setgray\n");
    fprintf(psFile, "    width\n");
    fprintf(psFile, "    title stringwidth pop sub 2 div  %s\n",
	    "%centers string, x for moveto");
    fprintf(psFile, "    hoch moveto                      %s\n",
	    "%y for moveto (height)");
    fprintf(psFile, "    title show } def\n\n");
}

/*-------------------------------------------------------------------------*/
/*            Writes the VICAR image into the PostScript-File              */
/*-------------------------------------------------------------------------*/
void WriteImage(FILE *psFile, int InUnit, 
                unsigned char *byteBuff, unsigned short *halfBuff,
                int nl, int ns, int ps)
{ 
    int i,s;
    unsigned char *bytePtr;
    unsigned short *halfPtr;
    
    fprintf(psFile, "%s\n", "%----------------- Image -----------------");
    fprintf(psFile, "gsave\n");
    fprintf(psFile, "XPos YPos translate\n");
    fprintf( psFile, "width height scale\n");
    fprintf( psFile, "%s%d%s\n",
	     " /picstr ", ns, " string def");
    fprintf( psFile, "%s\n",       " /vicarimage");
    fprintf( psFile, " { %d %d %d [ %d 0 0 %d 0 %d ] \n",
	     ns,nl,ps*8, ns,-nl,nl);
    fprintf( psFile, "     {currentfile picstr readhexstring pop}\n");
    fprintf( psFile, "     image\n");
    fprintf( psFile, "     }  def\n");
    fprintf( psFile, " vicarimage\n");
    if(byteBuff) { 
	for(i=0; i<nl; i++) { 
	    zvread(InUnit, byteBuff, "LINE", i+1, NULL);
	    bytePtr=byteBuff;
	    for(s=0; s<ns; s++, bytePtr++) { 
		fprintf( psFile, "%02x", *bytePtr );
		if(((s+1)%30) == 0)
		    fprintf( psFile, "\n" );
	    }
	    fprintf( psFile, "\n" );
	}
    }
    else { 
	for(i=0; i<nl; i++) { 
	    zvread(InUnit, halfBuff, "LINE", i+1, NULL);
	    halfPtr=halfBuff;
	    for(s=0; s<ns; s++, halfPtr++) { 
		fprintf( psFile, "%02x", *halfPtr );
		if(((s+1)%30) == 0)
		    fprintf( psFile, "\n" );
	    }
	    fprintf( psFile, "\n" );
	}
    }
    fprintf( psFile, "\n" );
    fprintf(psFile, "grestore\n\n");
}

/*-------------------------------------------------------------------------*/
/*    Converts a given VICAR-File into the PostScript-File                 */
/*-------------------------------------------------------------------------*/
int vicImageToPostScript(char *vicFile, char *psFile, char *title, 
			 PaperSize pSize)
{
    int vicUnit;
    int nl, ns;
    static int instance = 1;
    char format[8];
    int ps;                          /* pixel size in bytes      */
    FILE *psFilePtr;
    unsigned char  *byteBuff;        /* pointer to image data of type BYTE*/
    unsigned short *halfBuff;        /* buffer for image data of type HALF */
    
    byteBuff = NULL;
    halfBuff = NULL;
    
    // Open the VICAR image file for reading

    zvunit(&vicUnit, (char *)"VicarToPs", instance++, "u_name", vicFile, NULL);
    zvopen(vicUnit, NULL);
    zvget(vicUnit, 
	  "NL",       &nl,
	  "NS",       &ns,
	  "PIX_SIZE", &ps,
	  "FORMAT",   format,
	  NULL);

    // Open the PostScript-File for writing

    psFilePtr = fopen(psFile, "w");
    if (!psFilePtr) {
	cerr << "Error opening output file" << endl;
	return -1;
    }

    // Allocation of memory for one image line 

    if(!strcmp(format, "BYTE")) { 
	byteBuff=(unsigned char *)malloc(ns*sizeof(char));
	if(byteBuff==NULL) { 
	    cerr << "Sorry, memory problems !!" << endl;
	    return -2;
	}
    }
    else if(!strcmp(format, "HALF")) { 
	halfBuff=(unsigned short *)malloc(ns*sizeof(short));
	if(halfBuff==NULL) { 
	    cerr << "Sorry, memory problems !!" << endl;
	    return -2;
	}
    }
    else { 
	cerr << "Sorry, only BYTE and HALF supported for PostScript !!" 
	     << endl;
	return -3;
    } 

    // Write the header, variables and PostScript procedures 

    WriteHeader(psFilePtr);
    WriteVariableDefinitions(psFilePtr, nl, ns, pSize);
    WriteProcedureDefinitions(psFilePtr);
    
    // Write the VICAR image into the PS-file

    WriteImage(psFilePtr, vicUnit, byteBuff, halfBuff, nl, ns, ps);

    // Write a title above and a frame around the VICAR-image

    WriteFrameAndTitle(psFilePtr, title);

    // Insert the showpage command to print out the page

    fprintf(psFilePtr, "showpage   %s\n\n", "%prints the current page");
  
    // Close the VICAR file and the PostsScript-File

    zvclose(vicUnit, NULL);
    if(byteBuff) free((char *)byteBuff);
    if(halfBuff) free((char *)halfBuff);
    fclose(psFilePtr);  
    
    return(0);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sg_print.imake
#define SUBROUTINE sg_print
#define MODULE_LIST \
  SgPrintDialog.cc VicarToPs.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_SAGE_BASE
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
$ Return
$!#############################################################################
