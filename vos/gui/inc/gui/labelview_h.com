$!****************************************************************************
$!
$! Build proc for MIPL module labelview_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:31
$!
$! Execute by entering:		$ @labelview_h
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
$ write sys$output "*** module labelview_h ***"
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
$ write sys$output "Invalid argument given to labelview_h.com file -- ", primary
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
$   if F$SEARCH("labelview_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @labelview_h.bld "STD"
$   else
$      @labelview_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create labelview_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack labelview_h.com -mixed -
	-s LabelCmd.h ListLabelCmd.h ClearLabelCmd.h LabelWindow.h -
	   TextDisplayModel.h TextDisplayView.h ImageToLabelGlue.h -
	   LabelClearEveryRunCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create LabelCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
//
//   LabelCmd.h: 
//
//   This is a class derived from NoUndoCmd.
//   It is for setting up the image label display window.
//
////////////////////////////////////////////////////////////
#ifndef LABELCMD_H
#define LABELCMD_H
#include <X11/Intrinsic.h>
#include "NoUndoCmd.h"

class LabelWindow;
class ImageData;

class LabelCmd : public NoUndoCmd {

  private:

     int _created;
     ImageData *_image;
     LabelWindow *_labelWindow;

  protected:
    
     virtual void doit();

  public:
    
     LabelCmd( const char *, int, ImageData *);
     virtual ~LabelCmd();
     void resetLabelWindow();
     int isCreated() { return _created; }
     void setImage(ImageData *image) { _image = image; }
     virtual const char *const className () { return "LabelCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ListLabelCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
//
//   ListLabelCmd.h: 
//
//   This is a class derived from NoUndoCmd.
//   It displays image labels.
//
///////////////////////////////////////////////////////////////
#ifndef LISTLABELCMD_H
#define LISTLABELCMD_H
#include "ImageLabel.h"
#include "NoUndoCmd.h"

class TextDisplayModel;

class ListLabelCmd : public NoUndoCmd {

  protected:

    char *_key;

    char *_value;

    int _maxLabelSize;

    TextDisplayModel *_textM;

    virtual void doit();

  public:

    ListLabelCmd( const char *name, int active, char *key,
					TextDisplayModel *textM );

    virtual void freeValue (CmdValue);

    virtual const char *const className() { return ("ListLabelCmd"); }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ClearLabelCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
//
//   ClearLabelCmd.h: 
//
//   This is a class derived from NoUndoCmd.
//   It clears image label display.
//
///////////////////////////////////////////////////////////////
#ifndef CLEARLABELCMD_H
#define CLEARLABELCMD_H
#include "NoUndoCmd.h"

class TextDisplayModel;

class ClearLabelCmd : public NoUndoCmd {

  protected:

    TextDisplayModel *_textM;

    virtual void doit();

  public:

    ClearLabelCmd( const char *, int, TextDisplayModel *textM );

    virtual const char *const className() { return ("ClearLabelCmd"); }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LabelWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
//
//     LabelWindow.h: 
//
//     This is a derived from MenuWindow class.
//     It sets up and manages the window for label display.
//
////////////////////////////////////////////////////////////////////
#ifndef LABELWINDOW_H
#define LABELWINDOW_H
#include "MenuWindow.h"

class Cmd;
class TextDisplayView;
class TextDisplayModel;
class ImageData;
class ImageLabel;

class SgSearchTextDialog;
class SgSearchAgainCmd;
class SgSearchTextCmd;

class LabelWindow : public MenuWindow {

  private:

    Widget _form, _textArea;

  protected:

    static String _defaults[];

    SgSearchTextDialog *_searchDialog;
    SgSearchAgainCmd *_searchAgainCmd;
    SgSearchTextCmd *_searchCmd;

    TextDisplayView *_view;
    TextDisplayModel *_textM;
    Cmd *_clearOutputCmd, *_clearEveryRunCmd, *_saveOutputCmd;
    void setLabelMenu( MenuCmdList *, ImageLabel * );

  public:

    Widget createWorkArea ( Widget );
    void   createMenuPanes();
    void   setDefault();
    void reset();
    
    LabelWindow( const char *name, ImageData *image );
    virtual ~LabelWindow();

    virtual const char *const className () { return "LabelWindow"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TextDisplayModel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//
//    TextDisplayModel.h: 
//
//    This is the model class for text display .
//
///////////////////////////////////////////////////////////////////
#ifndef TEXTDISPLAYMODEL_H
#define TEXTDISPLAYMODEL_H

typedef enum _TextStyle { NORMAL, HIGHLIGHT } TextStyle;

class ImageData;

class TextDisplayView;

class TextDisplayModel {

  protected:
    
    ImageData *_image;
    char *_text;
    
    int _numViews;
    TextDisplayView **_views;
    
    void updateViews();
    void updateViews(char *newText, int size, TextStyle style);
    
  public:
    
    TextDisplayModel(ImageData *);
    virtual ~TextDisplayModel();
    
    void attachView(TextDisplayView *); // Add dependent view object
    void detachView(TextDisplayView *); // Remove dependent view object

    void setText(char *, int, TextStyle = NORMAL);
    char *getText() { return _text; };
    ImageData *getImage() { return _image; };
    void clear();

    virtual const char *const className () { return "TextDisplayModel"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TextDisplayView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
//
//    TextDisplayView.h
//
//    This is a class derived from BasicImageView.
//    It is the view for text output.
//
/////////////////////////////////////////////////////////////
#ifndef TEXTDISPLAYVIEW_H
#define TEXTDISPLAYVIEW_H

#include "TextDisplayModel.h"
#include <Xm/Xm.h>
#include "UIComponent.h"
#include "BasicImageView.h"
//#include "lists.h"

class CmdInterface;

class TextDisplayView : public BasicImageView {

 private:
    
    static XtResource _resources[];
    static String _defaults[];

    Widget _textW;
    Boolean _clearEveryRun;
    char *_newRunString;

 protected:

    TextDisplayModel *_textM;

 public:
  
    TextDisplayView(const char *, ImageData *);

    virtual ~TextDisplayView();

    void update();
    Widget createTextArea( Widget );

    void registerTextModel(TextDisplayModel *); 
    void unregisterTextModel();
    void addText(char *newText, int size, TextStyle style);
    void setClearEveryRun( Boolean value ) { _clearEveryRun = value; }
    Boolean getClearEveryRun( ) { return _clearEveryRun; }

    virtual const char *const className () { return "TextDisplayView"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToLabelGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
//
//    ImageToLabelGlue.h: 
//
//    This is a class derived form BasicImageView that serves as 
//    a "glue" class between an ImageData object and a TextDisplayModel object.
//
//    This class, though a UIComponent, creates no widget, and therefore 
//    should never be managed.
//
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETOLABELGLUE_H
#define IMAGETOLABELGLUE_H
#include "BasicImageView.h"

class ImageData;
class LabelCmd;

class ImageToLabelGlue : public BasicImageView {

 protected:

   LabelCmd *_labelCmd;

 public:

   ImageToLabelGlue (ImageData *model, LabelCmd *labelCmd);

   virtual ~ImageToLabelGlue ();

   virtual void update();
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToLabelGlue"; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LabelClearEveryRunCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
//
//   LabelClearEveryRunCmd.h: 
//
//   This is a class derived from Cmd.
//   It sets the toggled value for clearing label output
//   everytime new output is generated.
//
///////////////////////////////////////////////////////////////
#ifndef LABELCLEAREVERYRUNCMD_H
#define LABELCLEAREVERYRUNCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class TextDisplayView;

class LabelClearEveryRunCmd : public Cmd {

   protected:

      TextDisplayView *_view;

      virtual void doit();

      virtual void undoit( );

   public:

      LabelClearEveryRunCmd( const char *, int, TextDisplayView * );

      virtual const char *const className() { return ("LabelClearEveryRunCmd"); }

};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
