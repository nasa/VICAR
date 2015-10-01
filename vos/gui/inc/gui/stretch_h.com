$!****************************************************************************
$!
$! Build proc for MIPL module stretch_h
$! VPACK Version 1.8, Tuesday, June 10, 1997, 00:20:16
$!
$! Execute by entering:		$ @stretch_h
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
$ write sys$output "*** module stretch_h ***"
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
$ write sys$output "Invalid argument given to stretch_h.com file -- ", primary
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
$   if F$SEARCH("stretch_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @stretch_h.bld "STD"
$   else
$      @stretch_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretch_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretch_h.com -mixed -
	-s StretchListCmd.h StretchCmd.h StretchDialog.h StretchRadioBtn.h -
	   Function.h StretchValue.h TableValue.h StretchCmdInterface.h -
	   StretchParmInpView.h StretchBandChooser.h StretchCheckBox.h -
	   StretchParmListDialog.h StretchListInterface.h StretchParmList.h -
	   CurListValue.h ListControl.h StretchFun.h -
	   StretchPercValuesDialog.h StretchPercValuesIf.h LoadLutFileCmd.h -
	   SaveLutFileCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create StretchListCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////
// StretchListCmd.h
///////////////////////////////////////////////////
#ifndef STRETCHLISTCMD_H
#define STRETCHLISTCMD_H
#include "NoUndoCmd.h"
#include "TableValue.h"
#include "StretchValue.h"

class StretchCmdInterface;

class StretchListCmd : public NoUndoCmd {

  protected:

    StretchCmdInterface *_stretchCmdInterface;
    StretchType _stretchType;

  public:

    StretchListCmd(const char *, int, StretchCmdInterface *, StretchType);

    virtual void doit();

    void freeValue(CmdValue);
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchCmd.h: This command applies LUT(s) to the image
//////////////////////////////////////////////////////////////
#ifndef STRETCHCMD
#define STRETCHCMD
#include "Cmd.h"

class Lut;
class SiHistogram;
class StretchValue;

class StretchCmd : public Cmd {

  protected:

    int _created;

    Lut *_lutR, *_lutG, *_lutB;
    SiHistogram *_histR, *_histG, *_histB;

    StretchValue *_currentValue;
    StretchValue *_undoValue;
    StretchValue *_redValue, *_grnValue, *_bluValue, *_allValue;

    void stretchOneBand(Lut *, StretchValue *, SiHistogram *,
                        double &, double &);

  public:

    StretchCmd(const char *, int, Lut *, SiHistogram *, 
	Lut *, SiHistogram *, Lut *, SiHistogram * ); // for three bands
    StretchCmd(const char *, int, Lut *, SiHistogram *);  // for one band only
    ~StretchCmd();

    virtual void doit();
    virtual void undoit();

    virtual void freeValue(CmdValue);

    Lut *getLutRed() { return _lutR; }
    Lut *getLutGrn() { return _lutG; }
    Lut *getLutBlu() { return _lutB; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// StretchDialog.h: This class creates a work area for stretch dialog.
//////////////////////////////////////////////////////////////////////
#ifndef STRETCHDIALOG_H
#define STRETCHDIALOG_H
#include "MenuDialog.h"
#include "HelpBrowser.h"

class Cmd;
class Lut;

class StretchDialog : public MenuDialog {

  private:

    Cmd *_cmd;

    Lut *_lutR, *_lutG, *_lutB;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    StretchDialog(const char *name, Cmd *cmd, Lut *lutR, Lut *lutG, Lut *lutB);

    virtual Widget createWorkArea(Widget parent);
    virtual void createMenuPanes();

    virtual const char *const className() { return "StretchDialog"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchRadioBtn.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchRadioBtn.h: A component class to show a Stretch type radio button
/////////////////////////////////////////////////////////////
#ifndef STRETCHRADIOBTN_H
#define STRETCHRADIOBTN_H
#include "UIComponent.h"
#include "StretchValue.h"  // For StretchType definition

class StretchCmdInterface;

class StretchRadioBtn : public UIComponent {

  private:

    static void valueChangedCallback(Widget, XtPointer, XtPointer);

  protected:

    StretchType _type;
    StretchCmdInterface *_stretchCmdInterface;

  public:

    StretchRadioBtn(Widget, const char *, StretchType,
		    StretchCmdInterface *);

    virtual void valueChanged();
    virtual void setValue(StretchType);

    virtual const char *const className() { return "StretchRadioBtn"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Function.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* function.c -- contains code for compiling and executing user-specified
 * functions.
 */
#ifndef FUNCTION_H
#define FUNCTION_H
#include <string.h>

extern "C" int zknuth(char *,int *);
extern "C" void zxknuth(float *, float *);

void BlockFill(int val, char *dst, int len);

typedef struct FunctionDef
{
  float         args[20];       /* list of arguments to the function    */
  char          buf[980];       /* buffer to hold compiled function     */
}FunctionDef;

int ParseFunction(char *string, FunctionDef *func);
float ApplyFunction(FunctionDef *func, float arg);
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// StretchValue.h: Contains type of stretch and all parameters needed
// by the function to execute the stretch.  The object of this class
// is passed to the command as a value.
///////////////////////////////////////////////////////////////////
#ifndef STRETCHVALUE_H
#define STRETCHVALUE_H
#include <Xm/Xm.h>

enum StretchType { RAW, LINEAR, CLIP, 
		   CONTOUR, ITABLE, 
		   PSTRETCH, FUNCTION, LOGARITHM, 
		   SMOOTH, GAUSS, ELLIPSE, POWER, 
		   PEAK, MEAN, ASTRETCH, 
		   TABLE, ALARM, COMP, OFF };

enum StretchBand { STR_ALL, STR_RED, STR_GREEN, STR_BLUE };

#define HSIZE	256

class SiHistogram;

struct StretchValue {

    StretchType stretchName;

    StretchBand band;
    Boolean     changeOnlyBand;
    
    double	low, high;
    double 	dnmin, dnmax;
    int 	dnValue;
    int 	nbits;
    double 	curve;
    int 	interval, maxnum; 
    double      lPerc, hPerc;
    Boolean     stretched;
    
    int 	*inTable, *outTable, tableNoVals;
    int	        *inITable, *outITable, itableNoVals;
    int         *alarmValues, alarmNoVals;
    
    int 	backgnd;
    char 	*func;
    double 	mean, pmean, ampl, freq, phi;
    
    int         range;		// used in 
    double      factor, percent; // mean & peak
    
    double      lpercent, hpercent; // used in astretch
    
    double      gsigma, gmean;
    
    // Post stretches
    Boolean table, alarm, comple, off;
    
    StretchValue(StretchBand = STR_ALL);
    StretchValue(StretchValue &);		// copy ctor
    ~StretchValue ();
    
    StretchValue &operator=(StretchValue &val);
    Boolean operator==(StretchValue &val);

    double lPercValueRed, hPercValueRed;
    double lPercValueGrn, hPercValueGrn;
    double lPercValueBlu, hPercValueBlu;

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TableValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TableValue.h: Contains pairs of "in" and "out" values, also 
// keeps the value of the type of the table.  Everything is 
// public.
///////////////////////////////////////////////////////////////////
#ifndef TABLEVALUE_H
#define TABLEVALUE_H

struct TableValue {

    int *inTable, *outTable;
    int count;
    
    TableValue () 
    {
	inTable = outTable = NULL; 
	count = 0;
    };

    ~TableValue ()
    {
	if (inTable) delete [] inTable;
	if (outTable) delete [] outTable;
    };
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchCmdInterface.h: Command interface subclass that fills 
// StretchValue structure end calls execution on the command with 
// filled-in value.
//////////////////////////////////////////////////////////////
#ifndef STRETCHCMDINTERFACE
#define STRETCHCMDINTERFACE
#include "CmdInterface.h"
#include "StretchValue.h"
#include "Cmd.h"

class StretchParmInpView;
class StretchRadioBtn;
class StretchCheckBox;
class StretchParmListDialog;

const int Stretch_MAX_RADIOS = 13;
const int Stretch_MAX_CHECKS = 4;
const int Stretch_MAX_INP_VIEWS = 22;

class StretchCmdInterface : public CmdInterface {

  private:

    static XtResource _resources[];
    
  protected: 

    StretchValue *_stretchValue;

    StretchRadioBtn *_radioBtnList[Stretch_MAX_RADIOS];
    int _numRadios;

    StretchCheckBox *_checkBoxList[Stretch_MAX_CHECKS];
    int _numChecks;

    StretchParmInpView *_inpViewList[Stretch_MAX_INP_VIEWS];
    int _numInpViews;

    StretchParmListDialog *_itableListDialog;
    StretchParmListDialog *_tableListDialog;
    StretchParmListDialog *_alarmListDialog;

    CmdInterface *_itableValueList;

    // Obtained from resource file

    int _alarmValue;
    char *_selectedParmColor, *_unselectedParmColor;

  public:

    StretchCmdInterface(Widget, Cmd *);
    virtual ~StretchCmdInterface() { }

    void stretchIt(StretchValue *);
    void stretchIt();

    virtual void setValue(CmdValue);
    virtual void setSensitivity(StretchType);

    // Operations on saved stretch value

    void setBand(StretchBand band) 
	{ _stretchValue->band = band;}
    void setType(StretchType name) 
	{ _stretchValue->stretchName = name;}
    void setLow(double v) 
	{ _stretchValue->low = v;}
    void setHigh(double v) 
	{ _stretchValue->high = v;}
    void setMin(double v) 
	{ _stretchValue->dnmin = v;}
    void setMax(double v) 
	{ _stretchValue->dnmax = v;}
    void setDnValue(int v) 
	{ _stretchValue->dnValue = v;}
    void setNumBits(int v) 
	{ _stretchValue->nbits = v;}
    void setCurve(double v) 
	{ _stretchValue->curve = v;}
    void setInterval(int v) 
	{ _stretchValue->interval = v;}
    void setMaxNum(int v) 
	{ _stretchValue->maxnum = v;}
    void setLowPerc(double v) 
	{ _stretchValue->lPerc = v;}
    void setHighPerc(double v) 
	{ _stretchValue->hPerc = v;}
    void setStretched(Boolean v) 
	{ _stretchValue->stretched = v;}
    void setTable(int *inTable, int *outTable, int size) 
	{ _stretchValue->inTable = inTable;
	  _stretchValue->outTable = outTable;
	  _stretchValue->tableNoVals = size; }
    void setITable(int *inTable, int *outTable, int size) 
	{ _stretchValue->inITable = inTable;
          _stretchValue->outITable = outTable;
          _stretchValue->itableNoVals = size; }
    void setAlarmTable(int *alarmTable, int size) 
	{ _stretchValue->alarmValues = alarmTable;
	  _stretchValue->alarmNoVals = size; }
    void setBackground(int v) 
	{ _stretchValue->backgnd = v; }
    void setFunction(char *f) 
	{ _stretchValue->func = f; }
    void setMean(double v) 
	{ _stretchValue->mean = v; }
    void setPMean(double v) 
	{ _stretchValue->pmean = v; }
    void setAmpl(double v) 
	{ _stretchValue->ampl = v; }
    void setFreq(double v) 
	{ _stretchValue->freq = v; }
    void setPhi(double v) 
	{ _stretchValue->phi = v; }
    void setRange(int v) 
	{ _stretchValue->range = v; }
    void setFactor(double v) 
	{ _stretchValue->factor = v; }
    void setPercent(double v) 
	{ _stretchValue->percent = v; }
    void setLowPercent(double v) 
	{ _stretchValue->lpercent = v; }
    void setHighPercent(double v) 
	{ _stretchValue->hpercent = v; }
    void setGsigma(double v) 
	{ _stretchValue->gsigma = v; }
    void setGmean(double v) 
	{ _stretchValue->gmean = v; }
    void setTableOn(Boolean on) 
	{ _stretchValue->table = on; }
    void setAlarmOn(Boolean on) 
	{ _stretchValue->alarm = on; }
    void setComplOn(Boolean on) 
	{ _stretchValue->comple = on; }
    void setOffOn(Boolean on)
	{ _stretchValue->off = on; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchParmInpView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchParmInpView.h: A component class to show keyin fields
/////////////////////////////////////////////////////////////
#ifndef STRETCHPARMINPVIEW_H
#define STRETCHPARMINPVIEW_H
#include "KeyinView.h"
#include "StretchValue.h"

class StretchCmdInterface;

enum StretchParmType { StretchInt, StretchDouble, StretchString };

class StretchParmInpView : public KeyinView {

  private:

    static StretchValue _defaultStretchValue;	// holds default values

  protected:

    StretchValue *_stretchValue;
    int _offset;
    StretchParmType _type;
    StretchCmdInterface *_stretchCmdInterface;

  public:

    StretchParmInpView ( Widget, const char *, StretchValue *, int,
			 StretchParmType, StretchCmdInterface * );

    virtual void update( XtPointer = NULL );
    virtual void setValue(StretchValue *);
    virtual void setForeground(char *color);

    virtual const char *const className() { return "StretchParmInpView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchBandChooser.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchRadioBtn.h: A component class to choose which band will be 
// stretched.  The choices are Red, Green, Blue, All.  Only one option
// can be chosen.
/////////////////////////////////////////////////////////////
#ifndef STRETCHBANDCHOOSER_H
#define STRETCHBANDCHOOSER_H
#include "CmdInterface.h"
#include "StretchValue.h"

class StretchCmdInterface;

class StretchBandChooser : public CmdInterface {

  private:

    static void valueChangedCallback(Widget, XtPointer, XtPointer);
    
  protected:
    
    Widget _all, _red, _grn, _blu;
    
  public:
    
    StretchBandChooser(Widget, Cmd *);
    virtual ~StretchBandChooser() { }
    
    virtual void valueChanged();

    virtual void setValue(CmdValue);

    virtual const char *const className() { return "StretchBandChooser"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchCheckBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchCheckBtn.h: A component class to show a Stretch type check button
/////////////////////////////////////////////////////////////
#ifndef STRETCHCHECKBOX_H
#define STRETCHCHECKBOX_H
#include "UIComponent.h"
#include "StretchValue.h" // For StretchType definition

class StretchCmdInterface;

class StretchCheckBox : public UIComponent {

 private:

   static void valueChangedCallback(Widget, XtPointer, XtPointer);

 protected:

   StretchType _type;
   StretchCmdInterface *_stretchCmdInterface;

 public:

   StretchCheckBox(Widget, const char *, StretchType, StretchCmdInterface *);

   virtual void valueChanged();
   virtual void setValue(Boolean);
   virtual StretchType getType() { return _type; }

   virtual const char *const className() { return "StretchCheckBox"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchParmListDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// StretchParmListDialog.h: Custom dialog for selecting 
// values for table, itable and alarm stretches.
/////////////////////////////////////////////////////////////////
#ifndef STRETCHPARMLISTDIALOG_H
#define STRETCHPARMLISTDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"
#include "StretchValue.h" // For StretchType definition

class Cmd;

class StretchParmListDialog : public CustomDialog {

  private:

    StretchType _stretchType; 	// ITABLE or TABLE or ALARM
    int _defValue;

    Cmd *_cmd;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    StretchParmListDialog(const char *name, Cmd *, StretchType, int defValue=0);

    virtual Widget createWorkArea(Widget);

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchListInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchListInterface.h: Fills list with values 
//////////////////////////////////////////////////////////////
#ifndef STRETCHLISTINTERFACE_H
#define STRETCHLISTINTERFACE_H
#include "CmdInterface.h"
#include "StretchValue.h" // For StretchType definition

class ListControl;

class StretchListInterface : public CmdInterface {

  protected:
    
    int _defValue;
    ListControl *_control;
    StretchType _stretchType;

  public:
    
    StretchListInterface ( Widget, Cmd *, StretchType, int defValue=0);
    
    virtual void setValue(CmdValue);
    
    void runIt();

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchParmList.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchParmList.h: A component class to implement list widget
/////////////////////////////////////////////////////////////
#ifndef STRETCHPARMLIST_H
#define STRETCHPARMLIST_H
#include "UIComponent.h"

class CurListValue;

class StretchParmList : public UIComponent {

  protected:

    int *_in;
    int *_out;
    int _count;

    CurListValue *_inSingleValue;
    CurListValue *_outSingleValue;

    static void selectCallback ( Widget, XtPointer, XtPointer);

  public:

    StretchParmList ( Widget, const char *, CurListValue *, CurListValue * );
    ~StretchParmList ( ) { };

    void update ( int *, int *, int );

    virtual void select ( int );

    virtual const char *const className() { return "StretchParmList"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CurListValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// CurListValue.h: To show current list values
/////////////////////////////////////////////////////////////
#ifndef CURLISTVALUE_H
#define CURLISTVALUE_H
#include "KeyinView.h"

class CurListValue : public KeyinView {

  protected:

    int _defValue;
    int _value;

  public:

    CurListValue ( Widget, const char *, int=0);

    void setValue ( int );
    int getValue();

    virtual void update( XtPointer = NULL );

    virtual const char *const className() { return "CurListValue"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ListControl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// ListControl.h
///////////////////////////////////////////////////////////////
#ifndef LISTCONTROL_H
#define LISTCONTROL_H
#include "UIComponent.h"

class CurListValue;
class StretchParmList;
class StretchListInterface;
class TableValue;

class ListControl : public UIComponent {
    
  protected:

    CurListValue *_curListInValue, *_curListOutValue;
    StretchParmList *_list;
    StretchListInterface *_interface;
    TableValue *_tableValue;
    
    static void addToListCallback ( Widget, 
				    XtPointer, 
				    XtPointer );
    static void deleteFromListCallback ( Widget,
					 XtPointer,
					 XtPointer );
    
  public:
    
    ListControl ( Widget, const char *, CurListValue *, CurListValue *, 
		  StretchParmList *, StretchListInterface * );
    
    void setTableValue ( TableValue * );
    
    void addToList ( XtPointer );
    void deleteFromList ( XtPointer );
    
    int *getInAsArray();
    int *getOutAsArray();
    int getCount();
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchFun.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// StretchFun.h: This is a collection of functions that do all types
// of stretches supported by the stretch tool.
///////////////////////////////////////////////////////////////////
#ifndef STRETCHFUN_H
#define STRETCHFUN_H
#include <Xm/Xm.h>		// only for Boolean types

class Lut;
class SiHistogram;

// Non-histogram Stretches

void stretch_linear ( Lut *lut, double dnmin, double dnmax );
void stretch_alarm ( Lut *lut, int *alarmValues, int nVals, int dnValue );
void stretch_comp ( Lut *lut );
void stretch_off ( Lut *lut );
void stretch_clip ( Lut *lut, int nbits );
void stretch_log ( Lut *lut, double curve, double dnmin, double dnmax );
void stretch_contour ( Lut *lut, int start, int end, int interval, int maxnum, 
		       int dnValue, Boolean stretched=False );
void stretch_table ( Lut *lut, int *inTable, int *outTable,
		     int nVals);
void stretch_itable ( Lut *lut, int *inTable, int *outTable,
		      int nVals, int background = False );
void stretch_function ( Lut *lut, char* v);
void stretch_period ( Lut *lut, double mean, double ampl, 
		      double freq, double phi );

// Histogram Stretches

void stretch_percent ( Lut *lut, SiHistogram *hist,
		       double lPerc, double hPerc,
		       int lExclude, int hExclude,
		       double &low, double &high );
void stretch_gauss ( Lut *lut, SiHistogram *hist,
                     double gsigma, double mean,
                     int low, int high );
void stretch_smooth ( Lut *lut, SiHistogram *hist,
		      int low, int high );
void stretch_ellipse ( Lut *lut, SiHistogram *hist,
		       int low, int high );
void stretch_power( Lut *lut, SiHistogram *hist,
		    int low, int high );
void stretch_peak ( Lut *lut, SiHistogram *hist,
		    int dnmin, int dnmax );
void stretch_mean ( Lut *lut, SiHistogram *hist,
		    int low, int high );

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchPercValuesDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// StretchPercValuesDialog.h: Custom dialog for displaying  
// percent stretch upper and lower limits.
/////////////////////////////////////////////////////////////////
#ifndef STRETCHPERCVALUESDIALOG_H
#define STRETCHPERCVALUESDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;

class StretchPercValuesDialog : public CustomDialog {

  private:

    Cmd *_cmd;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    StretchPercValuesDialog(const char *name, Cmd *);

    virtual Widget createWorkArea(Widget);

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchPercValuesIf.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchPercValuesIf.h: Shows current percent stretch limits.
//////////////////////////////////////////////////////////////
#ifndef STRETCHPERCVALUESIF_H
#define STRETCHPERCVALUESIF_H
#include "CmdInterface.h"

class KeyinView;

class StretchPercValuesIf : public CmdInterface {

  private:

    KeyinView *_lPercValueRed, *_lPercValueGrn, *_lPercValueBlu;
    KeyinView *_hPercValueRed, *_hPercValueGrn, *_hPercValueBlu;

  public:
    
    StretchPercValuesIf(Widget, Cmd *);

    virtual void setValue(CmdValue);

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadLutFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// LoadLutFileCmd: A Command class that loads an IBIS file.  The Command 
// value is a dynamically allocated single string.
/////////////////////////////////////////////////////////
#ifndef LOADSINGLEFILECMD_H
#define LOADSINGLEFILECMD_H
#include "NoUndoCmd.h"

class Cmd;

class LoadLutFileCmd : public NoUndoCmd {

  protected:

    Cmd *_stretchCmd;
    
  public:
    
    LoadLutFileCmd(const char *, int, Cmd *);
    
    virtual void doit();
    
    virtual void freeValue(CmdValue value) 
	{ if (value) delete [] (char *)value; }
    
    virtual const char *const className () { return "LoadLutFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SaveLutFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// SaveLutFileCmd: A Command class that saves pseudocolor tables in  an 
// IBIS file format.  The Command value is a dynamically allocated single 
// string.
/////////////////////////////////////////////////////////
#ifndef SAVELUTFILECMD_H
#define SAVELUTFILECMD_H
#include "NoUndoCmd.h"

class Lut;

class SaveLutFileCmd : public NoUndoCmd {

 protected:

    Lut *_lutRed, *_lutGrn, *_lutBlu;
    
 public:

   SaveLutFileCmd(const char *, int, Lut *lutRed, Lut *lutGrn, Lut *lutBlu);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) 
       { if (value) delete [] (char *)value; }

   virtual const char *const className () { return "SaveLutFileCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
