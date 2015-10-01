$!****************************************************************************
$!
$! Build proc for MIPL module si_histogram_h
$! VPACK Version 1.9, Friday, February 13, 1998, 11:37:59
$!
$! Execute by entering:		$ @si_histogram_h
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
$ write sys$output "*** module si_histogram_h ***"
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
$ write sys$output "Invalid argument given to si_histogram_h.com file -- ", primary
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
$   if F$SEARCH("si_histogram_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @si_histogram_h.bld "STD"
$   else
$      @si_histogram_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_histogram_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_histogram_h.com -mixed -
	-s SiHistDefs.h SiHistView.h SiHistMenuCmd.h SiHistWindow.h -
	   SiHistogram.h SiHistSingleBox.h SiHistBox.h SiHistGraph.h -
	   SiHistAxis.h SiHistStat.h SiHistBtnInterface.h SiHistSpikeDialog.h -
	   SiHistSpikeArrowIf.h SiImageToHistGlue.h SiRawHistToStrHistGlue.h -
	   SiLutToStrHistGlue.h SiCollectStretchedHist.h SiCollectHist.h -
	   SiCollectHistBG.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiHistDefs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SiHistDefs.h: Type definitions for histogram components.
///////////////////////////////////////////////////////////////////
#ifndef SIHISTDEFS_H
#define SIHISTDEFS_H

typedef unsigned char MethodType;
#define STACKED 0
#define BLEND 1
#define POPUP 2
typedef unsigned char PopupDirectionType;
#define ROW 0
#define COLUMN 1
typedef unsigned char VerAxisDirType;
#define ASC 0
#define DESC 1
typedef unsigned char OrientType;
#define HORIZONTAL 0
#define VERTICAL 1

// Define X resource names

#define XvicNmethod		"method"
#define XvicCMethod		"Method"
#define XvicRMethodType		"MethodType"

#define XvicNorientation	"histOrientation"
#define XvicCOrientation	"HistOrientation"
#define XvicROrientType         "OrientType"

#define XvicNpopupDirection	"popDirection"
#define XvicCPopupDirection	"PopDirection"
#define XvicRPopupDirectionType "PopupDirectionType,"

#define XvicNshowAxis           "showAxis"
#define XvicCShowAxis           "ShowAxis"
#define XvicRShowAxisType       "Boolean"

#define XvicNshowStat           "showStat"
#define XvicCShowStat           "ShowStat"
#define XvicRShowAxisType       "Boolean"

#define XvicNshowHist           "showHist"
#define XvicCShowHist           "ShowHist"
#define XvicRShowHistType       "Boolean"

#define XvicNspike              "spike"
#define XvicCSpike              "Spike"
#define XvicRSpikeType          "Int"

#define XvicNverAxisDir		"verAxisDir"
#define XvicCVerAxisDir		"VerAxisDir"
#define XvicRVerAxisDirType	"VerAxisDirType"

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// SiHistView.h:  View objects that depend on histogram model.
///////////////////////////////////////////////////////////////////
#ifndef SiHistVIEW_H
#define SiHistVIEW_H

class SiHistogram;

class SiHistView {

  protected:

    SiHistogram *_histR, *_histG, *_histB;

    SiHistView ( SiHistogram *r, SiHistogram *g, SiHistogram *b ) 
	{ _histR = r; _histG = g; _histB = b; }
    virtual ~SiHistView () { }

  public:

    virtual void update ()=0;
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistMenuCmd.h: Include file to handle the HIST command
//                button from the Image widget.
//		This class is also used to deal with the 
//		Stretched Histograms
/////////////////////////////////////////////////////////////
#ifndef SiHistMENUCMD_H
#define SiHistMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class SiHistogram;

class SiHistMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_histWindow;
     SiHistogram *_histR, *_histG, *_histB;
     const char *_title;

  protected:
    
    virtual void doit();

  public:
    
    SiHistMenuCmd ( const char*, const char*, int, SiHistogram*, SiHistogram*, SiHistogram* );
    virtual const char *const className () { return "SiHistMenuCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// SiHistWindow.h: Histogram window component
////////////////////////////////////////////////////////////////////
#ifndef SiHistWINDOW_H
#define SiHistWINDOW_H
#include "MenuWindow.h"

class Cmd;
class SiHistogram;
class SiHistBox;
class PopupMenu;

class SiHistWindow : public MenuWindow {

  protected:
    
    SiHistogram *_histR, *_histG, *_histB;
    SiHistBox *_histBox;
    Widget _form;
    PopupMenu *_popup;

    Cmd *_stackNoBlend;
    Cmd *_stackBlend;
    Cmd *_row;
    Cmd *_column;
    Cmd *_spikeDialog;
    Cmd *_stats;
    Cmd *_axis;
    Cmd *_hist;
    Cmd *_horizontal;
    Cmd *_vertical;
    Cmd *_ascending;
    Cmd *_descending;
    Cmd *_logScale;

    virtual Widget createWorkArea ( Widget );
    virtual void createMenuPanes();

  public:

    SiHistWindow ( const char *, SiHistogram*, SiHistogram *, SiHistogram * );
    virtual ~SiHistWindow();
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistogram.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////
// SiHistogram.h
// Note that of all the functions which change the histogram, only
// clear() calls updateViews automatically.  This is because it is
// likely that the entire histogram will be updated at once, which
// would result in many calls to updateViews.  It is therefore the
// caller's responsibility to call updateViews() when it is done
// modifying the histogram.  This includes changing the data range
// or the number of bins!
// The data range can be specified either for integral types
// (in which case it is [min..max] inclusive, e.g. 0..255), or for
// floating types (in which case it is [min..max) where max is just
// higher than the maximum value, e.g. 0..1.0).  Internally, the
// max is stored as max+1 (which makes it an exclusive range), but
// getUpperLimit() returns the value you sent in.
///////////////////////////////////////////////////
#ifndef SiHISTOGRAM_H
#define SiHISTOGRAM_H
#include "assert.h"
#include <X11/Intrinsic.h>		// only for Boolean!

class SiHistView;

class SiHistogram	{

    protected:

	double 	_binSize;
	double 	_lowerLimit; 
	double	_upperLimit;
	int	_numBins;
	Boolean	_useBinSize;	// True: binSize const.  False: _numBins const.
	Boolean _isIntRange;

	int	*_bin; 		// Pointer to a hist array

	int _numViews;		// Number of dependent views
	SiHistView **_views;	// View objects that depend on this model

	void ctor(double ll, double ul);
	void allocate ();

    public:

	SiHistogram ( double ll, double ul=256, double bs=1 );
	SiHistogram ( int ll, int ul=255, double bs=1 );
	SiHistogram();
	~SiHistogram();

        SiHistogram &operator=(SiHistogram &hist);
	int &operator[] ( int i );	// _bins[i]

	void attachView (SiHistView *);	// Add dependent view object
	void detachView (SiHistView *);   // Delete dependent view object
	void updateViews();		// Called whenever the model's data changes

	// Parameters for the model

	void setNumBins ( int );	// setting one of #bins, binSize sets the other
	int numBins() { return _numBins; }

	int *getAsArray();

	void setBinSize ( double );
 	double getBinSize() { return _binSize; };

	void setLimits ( double minval, double maxval );
	void setLimits ( double minval, double maxval, double binSize );
	void setLimits ( double minval, double maxval, int numBins );
	void setLimits ( int minval, int maxval );
	void setLimits ( int minval, int maxval, double binSize );
	void setLimits ( int minval, int maxval, int numBins );
	double getLowerLimit() { return _lowerLimit; }
	double getUpperLimit()
		{ return (_isIntRange ? _upperLimit-1 : _upperLimit); }
	double getUpperLimitBound() { return _upperLimit; }

	Boolean isIntRange() { return _isIntRange; }

	void clear();
	void clear_noupdate();

	int getBinNumber ( double value ) // get the "address"
	    {
		if ( value < _lowerLimit ) value = _lowerLimit;
		return (value >= _upperLimit) ? (_numBins - 1) 
			: (int ((value - _lowerLimit) / _binSize));
	    };

	double getBinValue ( int bin )	// get domain value for bin #
	    { return (bin * _binSize) + _lowerLimit; }

	int getBin ( int b )
	    {
		if (b < 0 || b >= _numBins)
		    b = 0;
		return _bin[b];
	    };

	void setBin ( int b, int value ) 
	    {
		if (b < 0 || b >= _numBins)
		    b = 0;
		_bin[b] = value; 
	    };

	void incBin ( int b )
	    {
		if (b < 0 || b >= _numBins)
                    b = 0;
		_bin[b] ++;
	    };

	double getMean();
	double getStDev();
	int getMaxValue();

	int spike ( int n );

	inline static int min (int i, int j) { return (i<j) ? i: j; };
	inline static int max (int i, int j) { return (i>j) ? i: j; };	

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSingleBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiHistSingleBox.h: Container class for histogram view components.
////////////////////////////////////////////////////////////////
#ifndef SiHistSINGLEBOX_H
#define SiHistSINGLEBOX_H
#include "UIComponent.h"
#include "SiHistDefs.h"

class SiHistogram;
class SiHistAxis;
class SiHistGraph;
class SiHistStat;

class SiHistSingleBox : public UIComponent {

  private:

    static int _histBoxInit;           // flag for class initialization

    static XtResource _resources[];

  protected:

    SiHistogram 	*_histR;

    SiHistAxis 		*_axis;
    SiHistGraph 	*_graph;
    SiHistStat		*_statView;

    OrientType 		_histOrient;
    VerAxisDirType	_verAxisDir;
    Boolean		_logScale;

    static String 	_defaults[];

    Boolean 		_showAxis;
    Boolean 		_showHist;
    Boolean 		_showStat;

    Boolean 		_shell_resize;

    static Widget findShellWidget ( Widget w );
    void setShellResize ( Boolean value );
    void restoreShellResize();

  public:

    SiHistSingleBox ( Widget, const char *, SiHistogram * );
    virtual ~SiHistSingleBox();

    virtual void setHistBox();

    virtual void layComponents();       // Shows only specified components
    virtual void showComponents();      // Manages components
    virtual void hideComponents();      // Unmanages components

    virtual void setOrientType ( OrientType );
    virtual void setVerAxisDirType ( VerAxisDirType );
    virtual void setSpike ( int spike );

    OrientType getOrientType() { return _histOrient; }
    VerAxisDirType getVerAxisDirType() { return _verAxisDir; }
    int getSpike();

    virtual void setLogScale(Boolean log);
    Boolean logScaleIsSet ();

    virtual void showAxis (Boolean show);
    virtual void showStat (Boolean show);

    Boolean axisIsDisplayed () { return _showAxis; }
    Boolean histIsDisplayed () { return _showHist; }
    Boolean statIsDisplayed () { return _showStat; }

    SiHistogram *getHistR() { return _histR; }

    virtual const char *const className() { return "SiHistSingleBox"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiHistBox.h: Container class for histogram view components.
////////////////////////////////////////////////////////////////
#ifndef SiHistBOX_H
#define SiHistBOX_H
#include "SiHistSingleBox.h"

class SiHistBox : public SiHistSingleBox {

  private:

    static int _histBoxInit;           // flag for class initialization

    static XtResource _resources[];

  protected:

    SiHistogram 		*_histG;
    SiHistogram 		*_histB;

    SiHistSingleBox	*_histBoxR;
    SiHistSingleBox	*_histBoxG;
    SiHistSingleBox	*_histBoxB;

    Widget 		_sepRG, _sepGB;

    PopupDirectionType 	_popDirection;  // Meaningfull only for Pop-up
    MethodType 		_method;

    void layComponentsRow();
    void layComponentsCol();

  public:

    SiHistBox ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    virtual ~SiHistBox();

    virtual void setHistBox();

    virtual void layComponents();       // Shows only specified components
    virtual void showComponents();      // Manages components
    virtual void hideComponents();      // Unmanages components

    void setOrientType ( OrientType );
    void setVerAxisDirType ( VerAxisDirType );
    void setSpike ( int spike );
    void setLogScale ( Boolean log );
    void setPopupDirectionType ( PopupDirectionType );
    void setMethodType ( MethodType );

    PopupDirectionType getPopupDirectionType() { return _popDirection; }
    MethodType getMethodType() { return _method; }

    virtual void showAxis (Boolean show);
    virtual void showStat (Boolean show);

    SiHistogram *getHistG() { return _histG; }
    SiHistogram *getHistB() { return _histB; }

    virtual const char *const className() { return "SiHistBox"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistGraph.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// SiHistGraph.h:  This is a view component that draws graphs.
//////////////////////////////////////////////////////////////////////////
#ifndef SIHISTGRAPH_H
#define SIHISTGRAPH_H
#include "SgGraphView.h"
#include "SiHistView.h"

class SiHistGraph : public SgGraphView, public SiHistView {

  public:

    SiHistGraph ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    SiHistGraph ( Widget, const char *, SiHistogram * );
    virtual ~SiHistGraph();

    virtual void update();

    virtual const char *const className() { return "SiHistGraph"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistAxis.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// SiHistAxis.h:  This is a view component that draws axis.
//////////////////////////////////////////////////////////////////////////
#ifndef SIHISTAXIS_H
#define SIHISTAXIS_H
#include "SgAxisView.h"
#include "SiHistView.h"

class SiHistAxis : public SgAxisView, public SiHistView {

  public:

    SiHistAxis ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    SiHistAxis ( Widget, const char *, SiHistogram * );
    virtual ~SiHistAxis();

    virtual void update();

    virtual const char *const className() { return "SiHistAxis"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistStat.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SiHistStatView.h: A component class to show histogram statistics
/////////////////////////////////////////////////////////////

#ifndef SIHISTSTAT_H
#define SIHISTSTAT_H
#include "UIComponent.h"
#include "SiHistView.h"
#include "SiHistDefs.h"

class SiHistogram;

class SiHistStat : public UIComponent, public SiHistView {

  private:

    SiHistogram *_hist;
    SiHistogram *_hist1;
    SiHistogram *_hist2;

    Widget _frameMean, _frameStDev;   
    Widget _frameMean1, _frameStDev1;
    Widget _frameMean2, _frameStDev2;

    Widget _mean, _stDev;		// Input area
    Widget _mean1, _stDev1;
    Widget _mean2, _stDev2;

    Widget _labelMean, _labelStDev;	// The label
    Widget _labelMean1, _labelStDev1;
    Widget _labelMean2, _labelStDev2;

    char *_red, *_green, *_blue;

  public:

    SiHistStat ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    ~SiHistStat();

    void setStat ( double, double );
    void setStatColor ( double mR, double mG, double mB, 
			double sdR, double sdG, double sdB );

    virtual void update ();

    virtual const char *const className() { return "SiHistStat"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistBtnInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SiHistBtnInterface.h: A "push button" interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SIHISTBTNINTERFACE_H
#define SIHISTBTNINTERFACE_H
#include "SgDrawAreaInterface.h"

class SiHistogram;

class SiHistBtnInterface : public SgDrawAreaInterface {

  public:
    
    SiHistBtnInterface ( Widget, Cmd *, 
		SiHistogram*, SiHistogram*, SiHistogram* );
    virtual ~SiHistBtnInterface() { }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSpikeDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SiHistSpikeDialog.h: Include file to create the Spike Dialog Box
//////////////////////////////////////////////////////////////
#ifndef SIHISTSPIKEDIALOG_H
#define SIHISTSPIKEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class SiHistBox;

class SiHistSpikeDialog : public CustomDialog {

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w);}
   
    SiHistBox *_histBox;

  public:

    SiHistSpikeDialog ( SiHistBox *, const char * );
    virtual ~SiHistSpikeDialog() { }

    virtual Widget createWorkArea(Widget);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSpikeArrowIf.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SiHistSpikeArrowIf.h: An arrow button interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SIHISTSPIKEARROWIF_H
#define SIHISTSPIKEARROWIF_H
#include "ArrowButtonInterface.h"

class SiHistogram;

class SiHistSpikeArrowIf : public ArrowButtonInterface {

  protected:    

    int _step;
    SiHistogram *_histogram;

    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:
    
    SiHistSpikeArrowIf ( Widget, int, SiHistogram *, Cmd * );
    virtual ~SiHistSpikeArrowIf() { }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiImageToHistGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef SIIMAGETOHISTGLUE_H
#define SIIMAGETOHISTGLUE_H
#include "BasicImageView.h"

class ImageData;
class SiHistogram;

class SiImageToHistGlue : public BasicImageView {

 protected:

   SiHistogram *_histR;
   SiHistogram *_histG;
   SiHistogram *_histB;

   void *_collectionActive;

 public:

   SiImageToHistGlue (ImageData *model,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);
   virtual ~SiImageToHistGlue ();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "SiImageToHistGlue"; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiRawHistToStrHistGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// RawHistToStrHistGlue: class that serves as a "glue" class between a
// set of Histogram objects (the unstretched ones) and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the unstretched Histogram, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef SIRAWHISTTOSTRHISTGLUE_H
#define SIRAWHISTTOSTRHISTGLUE_H
#include "SiHistView.h"

class SiHistogram;
class Lut;

class SiRawHistToStrHistGlue : public SiHistView {

 protected:

   SiHistogram *_strhistR;
   SiHistogram *_strhistG;
   SiHistogram *_strhistB;

   Lut *_lutR;
   Lut *_lutG;
   Lut *_lutB;

 public:

   SiRawHistToStrHistGlue (
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
	SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
	Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "SiRawHistToStrHistGlue"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiLutToStrHistGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// LutToStrHistGlue: class that serves as a "glue" class between a
// set of LUT objects and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the LUT, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef SILUTTOSTRHISTGLUE_H
#define SILUTTOSTRHISTGLUE_H
#include "LutView.h"

class SiHistogram;

class SiLutToStrHistGlue : public LutView {

 protected:

   SiHistogram *_histR;
   SiHistogram *_histG;
   SiHistogram *_histB;

   SiHistogram *_strhistR;
   SiHistogram *_strhistG;
   SiHistogram *_strhistB;

 public:

   SiLutToStrHistGlue ( 
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
	SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
	Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "SiLutToStrHistGlue"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCollectStretchedHist.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// SiCollectStretchedHist.h
//////////////////////////////////////////////////////////////////////
#ifndef SICOLLECTSTRETCHEDHIST_H
#define SICOLLECTSTRETCHEDHIST_H

class Lut;
class SiHistogram;

void SiCollectStretchedHist ( 
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
	SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
	Lut *lutR, Lut *lutG, Lut* lutB);


void SiCollectStretchedHist ( SiHistogram *hist, 
		SiHistogram *strhist, Lut *lut);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCollectHist.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiCollectHist.h: These subroutines can be used to fill in histogram 
// model.  The caller should pass a pointer to existing histogram 
// model object.
////////////////////////////////////////////////////////////////
#ifndef SICOLLECTHIST_H
#define SICOLLECTHIST_H

#include "ImageDefs.h"

class ImageData;
class SiHistogram;

// Calculate histogram from the image

void SiCollectHist(ImageData *imageModel, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);
void siGetHistPtr (SiHistogram *, ImageData *, int, int, int, ColorType);


void SiCollectHist(int arrayR[], int arrayG[], int arrayB[],
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);


// Calculate histogram from the array of arbitrary size

void SiCollectHist(int *arrayR, int *arrayG, int *arrayB, int size,
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);

void siGetHistPtr (SiHistogram *, int *array, int size);

// Actually read the pixels for one line's worth and update the histogram

void SiCollectHistLine(SiHistogram *hist, unsigned char *buffer, int size,
		ImagePixelType type);

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiCollectHistBG.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiCollectHistBG.h
////////////////////////////////////////////////////////////////
#ifndef SICOLLECTHISTBG_H
#define SICOLLECTHISTBG_H

class ImageData;
class SiHistogram;
class ImageTile;

void SiCollectHistBG(ImageData *imageModel, 
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB, 
	void **active);

void SiCollectHistFromTile(SiHistogram *hist, int bufferIndex, 
	ImageTile &tile, int width, int height);

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
