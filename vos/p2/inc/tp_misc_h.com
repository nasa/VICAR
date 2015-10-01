$!****************************************************************************
$!
$! Build proc for MIPL module tp_misc_h
$! VPACK Version 1.9, Friday, June 04, 2010, 14:01:57
$!
$! Execute by entering:		$ @tp_misc_h
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
$ write sys$output "*** module tp_misc_h ***"
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
$ write sys$output "Invalid argument given to tp_misc_h.com file -- ", primary
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
$   if F$SEARCH("tp_misc_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @tp_misc_h.bld "STD"
$   else
$      @tp_misc_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_misc_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_misc_h.com -mixed -
	-s TpDefs.h TpApplication.h TpWindow.h TpDisplayer.h TpSubDisplayer.h -
	   ImageOverlayView.h TpImageView.h TpMatchManager.h TpMatch.h -
	   DrawObject.h TpPoint.h TpPointModel.h TpZoomControl.h -
	   TpContrastControl.h TpPosView.h TpCursorModel.h TpImageInfo.h -
	   TpImageReference.h TpSelectionMgr.h TpWedgeOverlayView.h -
	   TpMatchBrowseControl.h TpContrastControlPrefView.h -
	   TpZoomControlPrefView.h TpPrefView.h TpMatchManagerPrefView.h -
	   TpDisplayerPrefView.h TpAutofindRes.h TpMatchModeRes.h -
	   TpMatchModeValues.h TpMatchModeValuesCmdIf.h TpStripPanTool.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpDefs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpDefs.h: General definitions for tp program.
////////////////////////////////////////////////////////////

#ifndef TPDEFS_H
#define TPDEFS_H

//const int TP_MAX_IMAGES = 12;
const int TP_MAX_IMAGES = 25;
const int TP_MAX_DISPLAYS = 6;

typedef unsigned char TpAutofindMode;
#define MANUAL 0
#define AFFINE 1
#define SPICE  2
#define XtpRAutofindMode "TpAutofindMode"

typedef unsigned char TpMatchMode;
#define AUTO_CORR     0
#define AFFINE_ONLY   1
#define XtpRMatchMode "TpMatchMode"

typedef unsigned char TpPointSymbolShapeType;
#define CrossWithDot                0
#define Rectangle                   1
#define Dot                         2
#define Cross45                     3
#define CrossWithHole45             4
#define RectangleWithCrossesWithDot 5
#define XtpRPointSymbolShapeType "TpPointSymbolShapeType"

typedef unsigned char TpTagPosition;
#define NorthEast 0
#define NorthWest 1
#define SouthEast 2
#define SouthWest 3
#define Center 4

#define FULL "FULL"
#define REAL "REAL"
#define TEXT "A256"
 
#define BLANK_FULL -1
#define BLANK_REAL (float)-1.0
#define BLANK_TEXT "NONE"

#ifndef STRDUP_SIM_DEFINED
#define STRDUP_SIM_DEFINED
#include <string.h>
 
inline char *sdup(const char *str)
{
    // not robust, but neither is strdup
    char *newStr = new char[strlen(str) + 1];
    strcpy(newStr, str);
    return (newStr);
}
#endif

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpApplication.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpApplication.h: Special application class for tp application.
// It allows user to issue commands of type 
// % tp -images image1, image2, ..., imageN -pfile point_filename 
// -config config_file -v
//
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAPPLICATION_H
#define TPAPPLICATION_H
#include "Application.h"

#define XtpNpfile "pfile"
#define XtpCPfile "Pfile"

#define XtpNconfig "config"
#define XtpCConfig "Config"

#define XtpNverbose "verbose"
#define XtpCVerbose "Verbose"

class TpApplication : public Application {

  private:

    static XrmOptionDescRec _options[3];
    static XtResource _resources[];

  protected:

    // Functions to handle Xt interface

    virtual void initialize_hook ();  
    
    char *_pfile;
    char *_config;
    Boolean _verbose;
    int _exitStatus;

    // Allow subclasses to use command-line arguments for resources
    // If the array is dynamically allocated, the destructor can free it

    virtual XrmOptionDescList getOptions() { return _options; }
    virtual Cardinal getNumOptions() { return XtNumber(_options); }

  public:
    
    TpApplication ( const char *appClassName );
    virtual ~TpApplication();     

    char *getPfile() { return _pfile; }
    char *getConfig() { return _config; }
    Boolean getVerbose() { return _verbose; }
    void setExitStatus(int status) { _exitStatus = status; }
    int getExitStatus() { return _exitStatus; }

    virtual const char *const className() { return "TpApplication"; }
};
#endif

// Create pointer to single global instance of TpApplication class

extern TpApplication *theTpApplication;
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpWindow.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPWINDOW_H
#define TPWINDOW_H
#include "MenuWindow.h"
#include "Cmd.h"
#include "TpDefs.h"
#include "TpAutofindResultsDialog.h"
#include "TpMatchModeResultsDialog.h"
#include "TpMatchModeDialog.h"

class TpWindow : public MenuWindow {

  private:

    static XtResource _resources[];

  protected:

    Boolean _enablePrintWidgetTree;
    Boolean _enableSetSpecialStatus;

    char *_config;

    Cmd *_savePointCmd;
    Cmd *_deletePointCmd;
    Cmd *_listPointsCmd;
    Cmd *_redoMatchIdsCmd;
    Cmd *_autoSyncPointsCmd;
    Cmd *_quitCmd;
    Cmd *_exitStatusCmd;
    Cmd *_shiftRightCmd;
    Cmd *_shiftLeftCmd;
    Cmd *_postLoadImageCmd;
    Cmd *_postRemoveImageCmd;
    Cmd *_postLoadPointFileCmd;
    Cmd *_closePointFileCmd;

    Cmd *_postDisplayModeCmd;
    Cmd *_postAutofindResultsCmd;
    Cmd *_postMatchModeResultsCmd;
    Cmd *_postQualFormatDialogCmd;
    Cmd *_postPointEditorOptsDialogCmd;
    Cmd *_postPointSymbolsDialogCmd;
    Cmd *_postPointTagsDialogCmd;
    Cmd *_postCursorSymbolsDialogCmd;
    Cmd *_postAutofindDialogCmd;
    Cmd *_postMatchModeDialogCmd;

    Cmd *_savePointFileCmd;
    Cmd *_saveAndExitCmd;
    Cmd *_postSavePointFileAsCmd;
    Cmd *_loadConfigCmd;
    Cmd *_postLoadConfigCmd;
    Cmd *_saveConfigCmd;
    Cmd *_saveConfigAsCmd;
    Cmd *_postSaveConfigAsCmd;
    Cmd *_printCmd;

    CmdList *_modeRadioList;
    CmdList *_findRadioList;

    TpAutofindResultsDialog *_autofindResultsDialog;
    TpMatchModeResultsDialog *_matchModeResultsDialog;
    TpMatchModeDialog *_matchModeDialog;

    void initErrorManager() const;
    void initPrefManager() const;
    virtual Widget createWorkArea(Widget parent);
    virtual void createMenuPanes();

    void deleteWindowResponse();

  public:

    TpWindow(const char *name);
    ~TpWindow();

    void resetSaveCmd(Boolean enable);

    void activateSavePointCmd() { _savePointCmd->activate(); }
    void deactivateSavePointCmd() { _savePointCmd->deactivate(); }

    void loadConfig(char *filename=NULL);
    void saveConfig(char *filename=NULL);
    char *getConfigFileName() { return _config; }

    TpAutofindResultsDialog *getAutofindResultsDialog() 
	{ return _autofindResultsDialog; }

    TpMatchModeResultsDialog *getMatchModeResultsDialog()
        { return _matchModeResultsDialog; }

    TpMatchModeDialog *getMatchModeDialog()
	{ return _matchModeDialog; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpDisplayer.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpDisplayer.cc: This class manages multiple subdisplayer windows, of which
// at most three can be displayed on the screen at one time.
//////////////////////////////////////////////////////////////////////////////
#ifndef TPDISPLAYER_H
#define TPDISPLAYER_H
#include "UIComponent.h"
#include "TpDefs.h"
#include "RotationDefs.h"

class TpSubDisplayer;
class TpImageReference;
class TpMatchManager;
class TpMatch;

class TpDisplayer : public UIComponent {

  private:

    static XtResource _resources[];

    String _cursor;
    String _cursorColor;

    Widget _form;

    TpMatchManager *_matchManager;
    TpImageReference *_imageReference;
    TpSubDisplayer *_image[TP_MAX_IMAGES];
    
    // A maximum of TP_MAX_DISPLAYS images can be displayed at one time
 
    TpSubDisplayer *_displayed[TP_MAX_DISPLAYS];

    int _nimages;	// Total in memory
    int _numWin;	// Visible on screen
    int _win[TP_MAX_DISPLAYS];	// In this order
    Boolean _locks[TP_MAX_DISPLAYS];	// Some may be locked during shiftings

    void layoutComponents() const;
    void showComponents() const;
    void hideComponents() const;

  public:

    TpDisplayer(Widget parent, const char *name, 
		TpMatchManager *, TpImageReference *);
    ~TpDisplayer();

    void reload(TpDisplayer *);

    String getCursor() { return _cursor; }
    void setCursor(String);

    String getCursorColor() { return _cursorColor; }
    void setCursorColor(String);

    int addImage(char *filename);
    int deleteImage(int n);

    int getNumImages() { return _nimages; }
    TpSubDisplayer *getImage(int i) { return _image[i]; }

    void setNumWin(int numWin);
    int getNumWin() { return _numWin; }

    void shiftLeft();
    void shiftRight();
    void setLock(int image);
    void unSetLock(int image);
    void setNumDisplays(int);
    TpSubDisplayer *getSubDisplayer(int i) { return _image[i]; }
    void newMatchSelected(TpMatch *);

    virtual const char * const className() { return ("TpDisplayer"); } 
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSubDisplayer.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpSubDisplayer.h: Keeps track of all the image views and all the 
// tiepoint matches in relation to a single image.  One, two or three
// of these will comprise the display.
///////////////////////////////////////////////////////////////////
#ifndef TPSUBDISPLAYER_H
#define TPSUBDISPLAYER_H
#include "UIComponent.h"
#include "TpImageInfo.h"
#include "RotatedImageData.h"

class TpPosView;
class TpImageView;
class TpStripPanTool;
class TpZoomControl;
class TpMatchManager;
class TpContrastControl;
class TpDisplayer;

class TpSubDisplayer : public UIComponent {

  private:

    TpDisplayer *_displayer;
    TpMatchManager *_matchManager;

    char *_filename;
    int _number;

    RotatedImageData *_imageData;
    RotationType _rotationMode;		// left, right, flip
    Boolean _locked;			// for shifts

    Widget _panZoomForm, _form;

    TpPosView *_posView;
    Widget _lock;
    TpImageInfo *_imageInfo;
    TpImageView *_imageView;
    TpStripPanTool *_panView;
    TpImageView *_zoomView;
    TpZoomControl *_mainImageZoom;
    TpZoomControl *_zoomControl;
    TpContrastControl *_contrastControl;

    Boolean _failed;

    // ImageData values set via resources
    static XtResource _resources[];
    Boolean _autoMinRange;
    Boolean _autoMaxRange;
    String _minValue;                  // Strings so we can 
    String _maxValue;                  // detect no-value

    void layoutComponents() const;
    void showComponents() const;
    void hideComponents() const;

  public:

    TpSubDisplayer(Widget parent, const char *name, 
		char *filename, int number, 
		TpMatchManager *matchManager, TpDisplayer *);
    ~TpSubDisplayer();

    char *getFilename() { return _filename; }
    int getNumber() { return _number; }

    void setNumber(int n) { _number = n; _imageInfo->setNumber(n); }

    void setRotationMode(RotationType rotMode);
    void setLock(Boolean);
    void setCursor(String);
    void setCursorColor(String);

    ImageData *getImageData() const { return (ImageData *)_imageData; }
    TpImageView *getImageView() const { return _imageView; }
    TpImageView *getZoomView() const { return _zoomView; }
    TpContrastControl *getContrastControl() const { return _contrastControl; }
    TpPosView *getPosView() const { return _posView; }

    void makeCurrentPointVisible();

    Boolean failed() { return _failed; }

    virtual const char * const className() { return ("TpSubDisplayer"); } 
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageOverlayView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageOverlayView.h: This class is responsible for displaying 
// imaging widget with overlayed objects.
////////////////////////////////////////////////////////////////
#ifndef IMAGEOVERLAYVIEW_H
#define IMAGEOVERLAYVIEW_H
#include "ImageDisplayView.h"
#include "XvicImage.h"
#include "sl_lists.h"

class DrawObject;

class ImageOverlayView : public ImageDisplayView {

  protected:

    SL_List<DrawObject *> *_objects;

    XvicColor getColor(XColor *xcolor);
    XvicColor getColor(int red, int green, int blue);

  public:

    ImageOverlayView(Widget parent, const char *name, ImageData *imageData);
    virtual ~ImageOverlayView();

    XvicID addObject(DrawObject *object);
    void moveObject(DrawObject *object, double newx, double newy);
    void deleteObject(DrawObject *object);

    void colorObject(DrawObject *object, XColor *xcolor);
    void colorObject(DrawObject *object, int red, int green, int blue);

    XvicID groupObjects(SL_List<DrawObject *> *objects);
    void ungroupObjects(XvicID id);

    XvicID raiseObject(DrawObject *object);

    virtual const char *const className() { return "ImageOverlayView"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpImageView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpImageView.h: This class is responsible for displaying 
// imaging widget with overlayed points.
////////////////////////////////////////////////////////////////
#ifndef TPIMAGEVIEW_H
#define TPIMAGEVIEW_H
#include "ImageOverlayView.h"

class ImageData;
class TpMatchManager;
class TpSubDisplayer;

class TpImageView : public ImageOverlayView {

  private:

    static void inputCallback(Widget, XtPointer, XtPointer);

  protected:

    static Boolean _actionsAdded;

    static void tpMoveUp(Widget, XEvent *, String *, Cardinal *);
    static void tpMoveDown(Widget, XEvent *, String *, Cardinal *);
    static void tpMoveLeft(Widget, XEvent *, String *, Cardinal *);
    static void tpMoveRight(Widget, XEvent *, String *, Cardinal *);

    TpMatchManager *_matchManager;
    TpSubDisplayer *_subDisplayer;
    Boolean _saved;

    Time _lastClickTime;

    void input(XtPointer);

  public:

    TpImageView(Widget parent, const char *name, ImageData *imageData, 
		TpMatchManager *, TpSubDisplayer *);
    virtual ~TpImageView();

    virtual void setCenter(const double x, const double y);
    virtual void setTitle(char *title);

    virtual const char *const className() { return "TpImageView"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchManager.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpMatchManager.h: Keeps track of all the tiepoint matches.
///////////////////////////////////////////////////////////////////
#ifndef TPMATCHMANAGER_H
#define TPMATCHMANAGER_H
#include "TpMatch.h"
#include "TpDisplayer.h"
#include "TpMatchBrowseControl.h"
#include "TpMatchModeValues.h"
#include "TpDefs.h"
#include "sl_lists.h"
#include <iostream>

class TpWindow;
class ImageData;
class TpPointModel;
class TpSelectionMgr;
class TpSubDisplayer;
class TpQualGroupMgr;

class TpMatchManager {

  private:

    static Boolean _firstTime;
    static XtResource _resources[];

    TpWindow *_window;
    TpDisplayer *_displayer;
    TpMatchBrowseControl *_matchBrowser;

    int _nextId;
    int _startId;
    SL_List<TpMatch *> *_matches;

    TpSelectionMgr *_selectionMgr;
    TpQualGroupMgr *_genQualMgr;
    TpQualGroupMgr *_pointQualMgr;

    int _clickPrecision;
    Boolean _fullMatchIsEnforced;
    Boolean _autoSyncPoints;
    Boolean _checkingGenQualUnique;

    char *_ibisFilename;
    Boolean _dirtyFlag;

    TpAutofindMode _autofindMode;
    TpMatchMode _matchMode;
    TpMatchModeValues *_matchModeValues;
    TpSubDisplayer *_referencedSubDisp;

    TpPointSymbolShapeType _pointShape;
    int _pointWidth;
    int _pointHeight;
    char *_pointColor;
    char *_pointColorSel;
    TpTagPosition _tagPosition;

    double _autofindResults[6];
    float _matchModeResults[5];

    int _pmkCorrParm;
    int _lsmCorrParm;
    int _winCorrParm;
    float _accCorrParm;
    float _corCorrParm;

    void panZoomWindow(int x, int y, TpSubDisplayer *);

    static Boolean isInImage(const double x, const double y, TpSubDisplayer *);
    static TpPointModel *selectedPoint(TpSelectionMgr *, TpSubDisplayer *);

    int manual(const double x, const double y, TpSubDisplayer *, 
		Boolean, Boolean genByAlgorithm = False);
    void affine(const double x, const double y, TpSubDisplayer *, Boolean);
    void assist(const double x, const double y, TpSubDisplayer *, Boolean);
    void automatch(const double x, const double y, TpSubDisplayer *, Boolean);

    // Support routine to do affine transformation

    int doAffine(const double x, const double y,
		 TpSubDisplayer *in, TpSubDisplayer *out,
		 double &xOut, double &yOut);

    // Support routine to de the correlation

    int doMatch(const double x, const double y, TpSubDisplayer *in, 
		const double xs, const double sy, TpSubDisplayer *out,
		double &xOut, double &yOut);

  public:

    TpMatchManager(TpWindow *);
    virtual ~TpMatchManager();

    void reload(TpMatchManager *);

    void setDisplayer(TpDisplayer *displayer) { _displayer = displayer; }
    void setMatchBrowser(TpMatchBrowseControl *b) { _matchBrowser = b; }

    TpSelectionMgr *getSelectionMgr() { return _selectionMgr; }
    TpQualGroupMgr *getGenQualMgr() { return _genQualMgr; }
    TpQualGroupMgr *getPointQualMgr() { return _pointQualMgr; }

    void readInitialPoints();
    void listPoints();
    void writePointsIbis(char *filename=NULL);
    void readPointsIbis(char *filename=NULL);
    void closePointsIbis();

    void processNewPoint(const double x, const double y, 
			 TpSubDisplayer *, Boolean genInMain = True);
    void processSelectPoint(const double x, const double y, 
			    TpSubDisplayer *);
    void processSelectMatch(const double x, const double y, 
			    TpSubDisplayer *);
    void processScrollAll(const double x, const double y,
                            TpSubDisplayer *);
    void processMovePoint(TpSubDisplayer *, const double, const double);

    void newMatchSelected();
    void selectMatch(int number);

    void createPointViews(TpPointModel *, TpSubDisplayer *);

    int getNextId() { return _nextId; }
    void setNextId(int nextId) { _nextId = nextId - 1; }
    int getStartId() { return _startId; }
    void setStartId(int startId) { _startId = startId; redoIds(); }

    int numMatches() { return _matches->get_length(); }
    int getMatchNumber(TpMatch *);
    TpMatch *getNthMatch(int);
    void addMatch();
    void addMatch(TpMatch *match);
    void deleteCurrentSelection();
    void deleteLastAddedMatch();
    void deleteCurrentMatch();
    void deleteAllPoints(ImageData *);
    Boolean isExistingMatch(TpMatch *match);

    char *getIbisFileName() { return _ibisFilename; }
    Boolean isDirty() { return _dirtyFlag; }

    void setAutofindMode(TpAutofindMode mode) { _autofindMode = mode; }
    TpAutofindMode getAutofindMode() { return _autofindMode; }

    double getAutofindResult(int i);
    float getMatchModeResult(int i);

    void setMatchMode(TpMatchMode mode) { _matchMode = mode; }
    TpMatchMode getMatchMode() { return _matchMode; }

    TpMatchModeValues *getMatchModeValues() { return _matchModeValues; }
    void setMatchModeValues(TpMatchModeValues *value)
        { _matchModeValues = value;
	std::cout << _matchModeValues->_pmk << std::endl;
	std::cout << _matchModeValues->_lsm << std::endl;
	std::cout << _matchModeValues->_searchWindow << std::endl;
	std::cout << _matchModeValues->_accuracy << std::endl;
	std::cout << _matchModeValues->_threshold << std::endl; }

    void setPointShape(TpPointSymbolShapeType shape);
    TpPointSymbolShapeType getPointSymbolShape() { return _pointShape; }

    void setPointWidth(int);
    int getPointWidth() { return _pointWidth; }

    void setPointHeight(int);
    int getPointHeight() { return _pointHeight; }

    void setPointDimensions(int w, int h);

    void setPointColor(char *);
    void setPointColorSel(char *);
    char *getPointColor() { return _pointColor; }
    char *getPointColorSel() { return _pointColorSel; }

    void colorCodePointsGen(int n);
    void colorCodePointsPoint(int n);

    void setTagPosition(TpTagPosition tagPosition);
    TpTagPosition getTagPosition() { return _tagPosition; }

    void setPmkCorrParm(int p);
    void setLsmCorrParm(int p);
    void setWinCorrParm(int p);
    void setAccCorrParm(float p);
    void setCorCorrParm(float p);

    void showPointLabels(Boolean show);
    void rotationChanged(ImageData *id);

    void redoIds();

    void setAutoSyncPoints(Boolean v) { _autoSyncPoints = v; }

    Boolean isCheckingGenQualUnique() { return _checkingGenQualUnique; }
    void setCheckingGenQualUnique(Boolean v) { _checkingGenQualUnique = v; }

    virtual const char *const className() { return ("TpMatchManager"); } 
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatch.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpMatch.h: This class keeps a list of a single tiepoint set.
///////////////////////////////////////////////////////////////
#ifndef TPMATCH_H
#define TPMATCH_H
#include "sl_lists.h"
#include "TpDefs.h"
#include <Xm/Xm.h>

class TpMatchManager;
class TpPointModel;
class TpSelectionMgr;
class ImageData;
class TpQualGroup;
class TpQualGroupMgr;

class TpMatch {

  protected:

    TpMatchManager *_manager;

    SL_List<TpPointModel *> *_list;
    Boolean _current;

    TpQualGroup *_genQual;

  public:

    TpMatch(TpMatchManager *);
    virtual ~TpMatch();

    void setId(int id);
    void redrawAll();

    Boolean addPoint(char *filename, int n, ImageData *, 
		     const double x, const double y,
		     TpQualGroupMgr *qualMgr,
		     TpPointModel *&added);
    void addPoint(TpPointModel *);
    void deletePoint(TpPointModel *);
    void deletePoint(ImageData *);
    void clear();
    void setCurrent(TpSelectionMgr *);
    void listPoints();
    void setPointShape(TpPointSymbolShapeType shape);
    void setTagPosition(TpTagPosition tagPosition);
    void setPointWidth(int);
    void setPointHeight(int);
    void setPointDimensions(int w, int h);
    void setPointColor(char *);
    void setPointColorSel(char *);
    void showPointLabels(Boolean show);
    void rotationChanged(ImageData *);
    void colorCodePointsGen(int n, float a, float b);
    void colorCodePointsPoint(int n, float a, float b);

    int initScan(SL_ListWatch<TpPointModel *> *);
    int scanDone();
    TpPointModel *next();
    int getNumPoints() { return _list->get_length(); }

    Boolean isEmpty() 
	{ if(_list->get_length() == 0) return TRUE; else return FALSE; }
    SL_List<TpPointModel *> *getList() { return _list; }

    static int matchCmp(TpMatch *match, void *toMatch);

    TpQualGroup *getGenQual() { return _genQual; }
    TpMatchManager *getMatchManager() { return _manager; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DrawObject.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// DrawObject.h: Abstract class for imaging widget graphical overlay object.
// The subclasses have to specify the drawing routine.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef DrawObject_H
#define DrawObject_H
#include "XvicImage.h"

class DrawObject {

  protected:

    Widget _iw;
    XvicID _id;
    XvicID _gid;
    XvicGC _gc;
    XvicColor _color;

    double _x;
    double _y;
    int _width;
    int _height;
    int _offsetx;
    int _offsety;

  public:

    DrawObject(Widget iw, double x=0.0, double y=0.0);
    virtual ~DrawObject();

    virtual XvicID draw()=0;
    virtual void move(double x, double y);
    virtual void erase();
    virtual Boolean isGroup() { return FALSE; }

    void setColor(XColor *color);
    void setColor(int r, int g, int b);
    void setColor(const char *str);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPoint.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPoint.h: This class is responsible for drawing a visual representation
// of a tiepoint over the image.  It supports different sizes and shapes.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINT_H
#define TPPOINT_H
#include "DrawObject.h"
#include "TpDefs.h"

class TpPointModel;

class TpPoint : public DrawObject {

  private:

    static int _tpPointInit;           // flag for class initialization

    static XtResource _resources[];

  protected:

    TpPointModel *_model;

    XFontStruct *_fontStruct;
    char *_fontname;

    char *_labelString;

    int _width, _height;

    XvicID drawCrossDot();
    XvicID drawRectangle();
    XvicID drawDot();
    XvicID drawCross45();
    XvicID drawCrossWithHole45();

    XvicID drawLabel();

  public:

    TpPoint(TpPointModel *model, Widget iw);
    virtual ~TpPoint();

    virtual XvicID draw();
    void update(TpPointModel *model);

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointModel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpPointModel.h: This class serves as a Model for individual 
// points (observations).  
///////////////////////////////////////////////////////////////
#ifndef TPPOINTMODEL_H
#define TPPOINTMODEL_H
#include "TpDefs.h"
#include <Xm/Xm.h>

class TpPoint;
class TpMatch;
class ImageData;
class TpQualGroup;
class TpQualGroupMgr;

class TpPointModel {

  protected:

    TpPoint **_views;
    int _numViews;

    Boolean _isCurrent;

    double _x;
    double _y;

    ImageData *_image;
    char *_filename;
    int _imageNumber;

    TpMatch *_match;
    TpQualGroup *_pointQual;

    Pixel _size;
    static TpPointSymbolShapeType _pointShape;
    static TpTagPosition _tagPosition;

    static int _pointWidth; 
    static int _pointHeight;
    static char *_pointColor;
    static char *_pointColorSel;

    static Boolean _showLabel;

    static char *_colorCodeFilename;

  public:

    TpPointModel(char *filename, int imageNumber, ImageData *image, 
		 TpMatch *match, TpQualGroupMgr *qualMgr,
		 double x=0.0, double y=0.0);
    virtual ~TpPointModel();

    void attachView(TpPoint *);
    void detachView(TpPoint *);
    void updateViews();

    double getX() { return _x; }
    double getY() { return _y; }

    void setX(double x);// { _x = x; updateViews(); }
    void setY(double y);// { _y = y; updateViews(); }
    void setXY(double x, double y);// { _x = x; _y = y; updateViews(); }

    char *getFilename() { return _filename; }
    ImageData *getImageData() { return _image; }
    int getImageNumber() { return _imageNumber; }

    void setCurrent(Boolean isCurrent);
    Boolean isCurrent() { return _isCurrent; }

    void listPoint();

    void setPointShape(TpPointSymbolShapeType shape);
    static void setPointShapeStatic(TpPointSymbolShapeType shape);
    TpPointSymbolShapeType getPointShape() { return _pointShape; }

    void setTagPosition(TpTagPosition position);
    static void setTagPositionStatic(TpTagPosition position);
    TpTagPosition getTagPosition() { return _tagPosition; }

    void setPointWidth(int);
    static void setPointWidthStatic(int);
    int getPointWidth() { return _pointWidth; }

    void setPointHeight(int);
    static void setPointHeightStatic(int);
    int getPointHeight() { return _pointHeight; }

    void setPointDimensions(int, int);
    static void setPointDimensionsStatic(int, int);
    
    void setPointColor(char *);
    static void setPointColorStatic(char *);
    char *getPointColor() { return _pointColor; }

    void setPointColorSel(char *);
    static void setPointColorSelStatic(char *);
    char *getPointColorSel() { return _pointColorSel; }

    void showPointLabels(Boolean show);
    Boolean getShowPointLabels() { return _showLabel; }

    void rotationChanged();

    void colorCodePointsGen(int n, float a, float b);
    void colorCodePointsPoint(int n, float a, float b);

    void setColorCodeFilename(char *string) { _colorCodeFilename = string; }

    static int pointCmp(TpPointModel *point, void *toPoint);

    TpMatch *getMatch() { return _match; }
    TpQualGroup *getPointQual() { return _pointQual; }
};
#endif        
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpZoomControl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  TpZoomControl.h
///////////////////////////////////////////////////////////////////
#ifndef TPZOOMCONTROL_H
#define TPZOOMCONTROL_H
#include "UIComponent.h"

class TpImageView;

class TpZoomControl : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    Widget _keyinZoom;
    TpImageView *_zoomView;

    int _zoomValue;

    static void incZoomCallback(Widget w, XtPointer clientData,
			XtPointer callData);
    static void setZoomCallback(Widget w, XtPointer clientData,
                        XtPointer callData);
    static void decZoomCallback(Widget w, XtPointer clientData,
			XtPointer callData);

    void setZoom(int i);
    void incZoom(int i);

  public:

    TpZoomControl(Widget parent, const char *name, TpImageView *_zoomView);
    virtual ~TpZoomControl() { }

    void reload(TpZoomControl *);
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpContrastControl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpContrastControl.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPCONTRASTCONTROL_H
#define TPCONTRASTCONTROL_H
#include "CmdInterface.h"

class ImageData;
class TpWedgeOverlayView;

class TpContrastControl : public CmdInterface {

  protected:

    static XtResource _resources[];

    int _min, _max;

    Widget _keyinLowContrast;
    Widget _keyinHighContrast;

    TpWedgeOverlayView *_wedge;

    static void setHighContrastCallback(Widget w, XtPointer clientData,
			XtPointer callData);
    static void setLowContrastCallback(Widget w, XtPointer clientData,
			XtPointer callData);

    void setHighContrast(int i);
    void setLowContrast(int i);

  public:

    TpContrastControl(Widget parent, Cmd *cmd);
    virtual ~TpContrastControl() { }

    void reload(TpContrastControl *);

    void setValue(CmdValue);
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPosView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpPosView.h: This class displays coordinates of cursor location 
// if no point is selected or point coordinates and point qualifier if 
// any point is selected.  Typing new coordinates should change point's 
// location.
////////////////////////////////////////////////////////////////
#ifndef TPPOSVIEW_H
#define TPPOSVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"

class TpSubDisplayer;
class TpCursorModel;
class KeyinView;
class TpMatchManager;
class ImageData;

class TpPosView : public UIComponent {

  private:

    static void cursorMovedCallback(Widget,
                       XtPointer clientData, XtPointer callData);
    static void changePosCallback(Widget, 
                       XtPointer clientData, XtPointer callData);
    static void changeQualCallback(Widget, 
                       XtPointer clientData, XtPointer callData);

  protected:

    KeyinView *_linePosition;
    KeyinView *_sampPosition;
    KeyinView *_qualPosition;

    TpSubDisplayer *_subDisplayer;
    ImageData *_imageData;
    TpCursorModel *_cursorModel;
    TpMatchManager *_matchManager;

    Boolean _isOnImage;

    virtual void startTracking();
    virtual void stopTracking();

    Boolean isCursorOnImage(XvicImageCallbackStruct * cb);

    void changePos();
    void changeQual();

  public:

    TpPosView(Widget parent, const char *name,
		TpCursorModel *cursorModel, ImageData *imageData,
		TpMatchManager *mm, TpSubDisplayer *subDisplayer);
    virtual ~TpPosView();

    void cursorIsOnImage(Boolean onImage);

    virtual void cursorMoved(XvicImageCallbackStruct * cb );
    virtual void cursorMoved(int x, int y);
    virtual void cursorMoved(double x, double y);

    virtual void blankDisplay();

    void displayValues();

    virtual const char *const className() { return "TpPosView"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpCursorModel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpCursorModel.h: Calls updates on cursor views such as position 
// view, dn view etc., when something cursor-related happens.
////////////////////////////////////////////////////////////////
#ifndef TPCURSORMODEL_H
#define TPCURSORMODEL_H
#include "CursorModel.h"

class TpCursorModel : public CursorModel {

  protected:

    int _n;
    Widget *_aiw;

  public:
  
    TpCursorModel(Boolean trackingEnabled, Widget iw, Widget *aiw, int n);
    virtual ~TpCursorModel();

    void getImageWidgets(Widget *&aiw, int &n);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpImageInfo.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpImageInfo.h: This component displays image filename and image number.
/////////////////////////////////////////////////////////////////////////////
#ifndef TPIMAGEINFO_H
#define TPIMAGEINFO_H
#include "UIComponent.h"

class TpImageInfo : public UIComponent {

  protected:

    Widget _imageNumber;

  public:

    TpImageInfo(Widget parent, const char *name, char *filename, int number);
    virtual ~TpImageInfo() { }

    void setNumber(int n);

    virtual const char *const className() { return "TpImageInfo"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpImageReference.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// TpImageReference
///////////////////////////////////////////////////////
#ifndef TpImageReference_H
#define TpImageReference_H
#include "UIComponent.h"

class TpImageReference : public UIComponent {

  private:

  protected:

    int _numLabels;
    Widget *_labels1;
    Widget *_labels2;

    Pixel _fg, _bg;
    char *_bgVisible;
    char *_fgLoaded;

    void setToInvisible(Widget w);
    void setToVisible(Widget w);
    void indicateLoadedImage(Widget w);

  public:

    TpImageReference(Widget, const char *);
    virtual ~TpImageReference();

    void setToVisible(int i);
    void setAllToInvisible();
    void setReferencedImage(int i);
    void indicateLoadedImage(int i);

    virtual const char *const className() { return "TpImageReference"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSelectionMgr.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpSelectionMgr.h: This class keeps a list of selected points.
///////////////////////////////////////////////////////////////
#ifndef TPSELECTIONMGR_H
#define TPSELECTIONMGR_H

#include "TpMatch.h"
#include "sl_lists.h"
#include <Xm/Xm.h>

class TpMatchManager;
class TpPointModel;

class TpSelectionMgr {

  protected:

    TpMatchManager *_matchManager;
    SL_List<TpPointModel *> *_points;
    TpMatch *_match;

  public:

    TpSelectionMgr(TpMatchManager *);
    virtual ~TpSelectionMgr();

    void addMatch(TpMatch *);
    void addPoint(TpPointModel *);
    void selectPoint(TpPointModel *);
    void selectMatchAndPoints(TpMatch *);
    void deletePoint(TpPointModel *);
    void clear();

    int initScan(SL_ListWatch<TpPointModel *> *);
    TpPointModel *nextPoint();
    int scanDone();

    Boolean isEmpty() 
	{ if (_points->get_length() == 0) 
	    return True; 
	  else
	    return False; }

    int getNumPoints() { return _points->get_length(); }

    Boolean isFullHouse();

    TpMatch *getMatch() { return _match; }

    void listPoints();

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpWedgeOverlayView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpWedgeOverlayView.h: Provides overlay graphics support for wedge class
// The class allows to manupulate two marks that are used by contrasting 
// tool in TP.
/////////////////////////////////////////////////////////////////////////////
#ifndef TPWEDGEOVERLAYVIEW_H
#define TPWEDGEOVERLAYVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"

class TpContrastValue;
class Cmd;

class TpWedgeOverlayView : public UIComponent {

  private:

    static String _defaults[];
    static XtResource _resources[];

    static void inputCallback(Widget, XtPointer, XtPointer);
    void input(XtPointer callData);

  protected:

    Cmd *_cmd;

    int _min, _max;

    Widget _iw;
    
    XvicGC    _lineGC;
    XvicColor _lineColor;
    
    String    _markColor;
    Dimension _markLength, _markThickness;

  public:

    TpWedgeOverlayView(Widget, const char *, Cmd *);
    
    virtual void update(TpContrastValue *);
    
    virtual const char *const className() { return ("TpWedgeOverlayView"); }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchBrowseControl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchBrowseControl.h: This component allows user to browse through 
// different matches, displays general qualifier for the current match and 
// allows user to change it.
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHBROWSECONTROL_H
#define TPMATCHBROWSECONTROL_H
#include "UIComponent.h"
#include "TpQualifier.h" // for TpQualType def

class TpSelectionMgr;
class TpMatchManager;
class TpMatch;

class TpMatchBrowseControl : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    Boolean _enableGotoQual;

    static String _defaults[];

    TpSelectionMgr *_selMgr;
    TpMatchManager *_matchManager;

    Widget _numberField; // Match's running id number
    Widget _valueField;  // General qualifier's value
    Widget _gotoLabel;	// Label for Goto
    Widget _gotoField;	// Goto general qual value

    int _numberInt;

    void setNumberField(char *text);
    void setValueField(char *text);

    static void setNumberCallback(Widget, XtPointer, XtPointer);
    static void setValueCallback(Widget, XtPointer, XtPointer);
    static void gotoQualCallback(Widget, XtPointer, XtPointer);
    static void incNumberCallback(Widget, XtPointer, XtPointer);
    static void decNumberCallback(Widget, XtPointer, XtPointer);

    static void confirm(void *);

    void setNumber();
    void setNumber(int);
    void gotoQual();
    void gotoQual(char *);
    void setValue(Boolean confirmed);
    void displayValue();
    void incNumber();
    void decNumber();

  public:

    TpMatchBrowseControl(Widget parent, const char *name, 
			 TpSelectionMgr *, TpMatchManager *);
    virtual ~TpMatchBrowseControl() { }

    void newMatchSelected(TpMatch *);

    virtual const char *const className() { return "TpMatchBrowseControl"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpContrastControlPrefView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpContrastControlPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPCONTRASTCONTROLPREFVIEW_H
#define TPCONTRASTCONTROLPREFVIEW_H

#include "PrefView.h"
#include "TpContrastControl.h"

class TpContrastControlPrefView : public PrefView {
 
  public:

    TpContrastControlPrefView() { }
    virtual ~TpContrastControlPrefView() { }
    
    virtual int copySize() { return sizeof(TpContrastControl); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpContrastControl *)object)->reload((TpContrastControl *)copy); }
    
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpZoomControlPrefView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpZoomControlPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPZOOMCONTROLPREFVIEW_H
#define TPZOOMCONTROLPREFVIEW_H

#include "PrefView.h"
#include "TpZoomControl.h"

class TpZoomControlPrefView : public PrefView {
 
  public:

    TpZoomControlPrefView() { }
    virtual ~TpZoomControlPrefView() { }
    
    virtual int copySize() { return sizeof(TpZoomControl); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpZoomControl *)object)->reload((TpZoomControl *)copy); }
    
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPrefView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPPREFVIEW_H
#define TPPREFVIEW_H

#include "PrefView.h"

template <class T>
class TpPrefView : public PrefView {
 
  public:

    TpPrefView() { }
    virtual ~TpPrefView() { }
    
    virtual int copySize() { return sizeof(T); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((T *)object)->reload((T *)copy); }
    
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchManagerPrefView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchManagerPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPMATCHMANAGERPREFVIEW_H
#define TPMATCHMANAGERPREFVIEW_H

#include "PrefView.h"
#include "TpMatchManager.h"

class TpMatchManagerPrefView : public PrefView {
 
  public:

    TpMatchManagerPrefView() { }
    virtual ~TpMatchManagerPrefView() { }
    
    virtual int copySize() { return sizeof(TpMatchManager); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpMatchManager *)object)->reload((TpMatchManager *)copy); }
    
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpDisplayerPrefView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpDisplayerPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPDISPLAYERPREFVIEW_H
#define TPDISPLAYERPREFVIEW_H

#include "PrefView.h"
#include "TpDisplayer.h"

class TpDisplayerPrefView : public PrefView {
 
  public:

    TpDisplayerPrefView() { }
    virtual ~TpDisplayerPrefView() { }
    
    virtual int copySize() { return sizeof(TpDisplayer); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpDisplayer *)object)->reload((TpDisplayer *)copy); }
    
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutofindRes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAutofindResults
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOFINDRESULTS_H
#define TPAUTOFINDRESULTS_H
#include "UIComponent.h"

class TpMatchManager;
class KeyinView;

class TpAutofindResults : public UIComponent {

  protected:

    TpMatchManager *_matchManager;
    KeyinView *_labels[6];

  public:

    TpAutofindResults(Widget, const char *, TpMatchManager *);
    virtual ~TpAutofindResults();

    void setValues(double [6]);

    virtual const char *const className() { return "TpAutofindResults"; }

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeRes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchModeResults
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODERESULTS_H
#define TPMATCHMODERESULTS_H
#include "UIComponent.h"

class TpMatchManager;
class KeyinView;

class TpMatchModeResults : public UIComponent {

  protected:

    TpMatchManager *_matchManager;
    KeyinView *_labels[5];
    Widget _dumpStdout;

  public:

    TpMatchModeResults(Widget, const char *, TpMatchManager *);
    virtual ~TpMatchModeResults();

    void setValues(float [5]);

    Boolean isDumpToStdout();	// True if we should dump results to stdout too
				// We should really dump in here, but for
				// headers and such we must dump outside.

    virtual const char *const className() { return "TpMatchModeResults"; }

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeValues.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpMatchModeValues.h: A structure that holds match mode parameters
/////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODEVALUES_H
#define TPMATCHMODEVALUES_H

class TpMatchModeValues {
  public:
    int _pmk;          // Product Momentum Correlation matching patch size
    int _lsm;          // Least Squares Momentum matching patch size
    int _searchWindow; // Search Area 
    float _accuracy;   // Point Accuracy
    float _threshold;    // Threshold

    TpMatchModeValues(int pmk = 7, int lsm = 15, int sw = 20, 
		      float acc = 0.3, float threshold = 0.5) { 
	_pmk = pmk; 
	_lsm = lsm; 
	_searchWindow = sw; 
	_accuracy = acc; 
	_threshold = threshold; 
    }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeValuesCmdIf.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmdIf.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHMODEVALUESCMDIF_H
#define TPSETMATCHMODEVALUESCMDIF_H
#include "CmdInterface.h"

class KeyinView;

class TpSetMatchModeValuesCmdIf : public CmdInterface {

  protected:

    KeyinView *_pmk;
    KeyinView *_lsm;
    KeyinView *_sw;
    KeyinView *_accuracy;
    KeyinView *_threshold;

    static void setPMKCallback(Widget, XtPointer, XtPointer);
    static void setLSMCallback(Widget, XtPointer, XtPointer);
    static void setSWCallback(Widget, XtPointer, XtPointer);
    static void setAccuracyCallback(Widget, XtPointer, XtPointer);
    static void setThresholdCallback(Widget, XtPointer, XtPointer);

  public:

    TpSetMatchModeValuesCmdIf(Widget parent, Cmd *cmd);
    virtual ~TpSetMatchModeValuesCmdIf() { }

    void setValue(CmdValue);

    void setPMK();
    void setLSM();
    void setSW();
    void setAccuracy();
    void setThreshold();
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpStripPanTool.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// TpStripPanTool.h: This component is very similar to StripPanTool and 
// should eventually be replaced with StripPanTool, but after there is 
// a fix to Motif bug that causes crashes when setUserZoom2Fit() is called 
// and scrollbar policy is set.
////////////////////////////////////////////////////////////////////////
#ifndef TPSTRIPPANTOOL_H
#define TPSTRIPPANTOOL_H

#include "ImageDisplayView.h"
#include "XvicImage.h"

class Lut;
class LutToImageWidgetGlue;

#define XvicNpanBoxColor	"panBoxColor"
#define XvicCPanBoxColor	"PanBoxColor"
#define XvicNapplyStretchToPan	"applyStretchToPan"
#define XvicCApplyStretchToPan	"ApplyStretchToPan"

class TpStripPanTool : public ImageDisplayView {
 private:
   static void bigWidgetChangeCallback(Widget, XtPointer, XtPointer);
   static void inputCallback(Widget, XtPointer, XtPointer);

   static XtResource _resources[];

   Dimension computeSizeX(int, int, Boolean, Widget);
   Dimension computeSizeY(int, int, Boolean, Widget);

 protected:
   Widget _big_iw;
   XvicID _box_id;
   String _box_color_string;	// From the resource
   XvicColor _box_color;
   XvicGC _box_gc;
   int _box_x, _box_y;		// Image coords of upper left
   int _input_x, _input_y;	// Saved coords for mouse-panning
   Boolean _preserve_aspect;	// True if aspect ratio of shell should be set
   Boolean _apply_stretch_to_pan;	// From the resource

   Lut *_rlut, *_glut, *_blut;
   Lut *_rplut, *_gplut, *_bplut;
   LutToImageWidgetGlue *_lutGlue, *_pseudoLutGlue;

   // Calculate zoom factor needed for image to fit window
                                                  
   virtual void calcZoomToFit(Widget iw, int *in, int *out);

   void bigWidgetChange(XtPointer);
   void input(XtPointer);

   void drawNewBox();
   void moveBox();

   void setAspectRatio();
   void copyDisplayModeResources();
   void copyDataRangeResources();

   void newSize();

 public:

   TpStripPanTool(Widget parent, const char *name, ImageData *model,
		Widget _big_iw,
		Dimension view_height, Dimension view_width,
		Boolean preserve_aspect = True,
		Lut *rlut=NULL, Lut *glut=NULL, Lut *blut=NULL,
		Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);
   ~TpStripPanTool();

   virtual void setUserZoom2Fit();
};

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
