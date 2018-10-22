////////////////////////////////////////////////////////////////
// ImageWindow.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEWINDOW_H
#define IMAGEWINDOW_H
#include "MenuWindow.h"
#include "PrefDialog.h"

class ImageDisplayer;
class SiHistogram;
class ImageData;
class RotatedImageData;
class Lut;
class SiImageToHistGlue;
class SiRawHistToStrHistGlue;
class SiLutToStrHistGlue;
class ImageToPseudoGlue;
class BasicImageView;

class ImageToImageWindowGlue;

class Cmd;
class CmdList;
class QuitCmd;
class ZoomCmdSet;
class ZoomDialog;
class BorderCmd;
class MenuBarCmd;
class LabelCmd;

class ImageWindow : public MenuWindow {

private:

	static XtResource _resources[];

	Widget _form;
	static void postFileLoadEventHandler(Widget, XtPointer,
					     XEvent *, Boolean *);
	static void setUserPrefsEventHandler(Widget, XtPointer,
					     XEvent *, Boolean *);

protected:

	PrefDialog              *_prefDialog;

	RotatedImageData	*_image;
	SiHistogram		*_histR, *_histG, *_histB;
	SiHistogram		*_strhistR, *_strhistG, *_strhistB;
	Lut			*_lutR, *_lutG, *_lutB;
	Lut                     *_pseudoR, *_pseudoG, *_pseudoB;
	SiImageToHistGlue		*_imageToHistGlue;
	SiRawHistToStrHistGlue	*_rawHistToStrHistGlue;
	SiLutToStrHistGlue	*_lutToStrHistGlue;
	ImageToPseudoGlue	*_imageToPseudoGlue;

	ImageToImageWindowGlue  *_imageToImageWindowGlue;

	Boolean                 _showFullScreenDisplay;
	Boolean                 _showMotifBorder;
	Boolean                 _showMenuBar;

	Boolean                 _cmdLineHelpReqd;

       	CmdList *               _cmdListFile;

	ImageDisplayer          *_imageDisplayer;

	QuitCmd *               _quit;
	Cmd *		        _loadFileCmd;
	Cmd *	 	        _loadMenuCmd;
	Cmd *		        _histCmd;
	Cmd *		        _lutCmd;
	Cmd *		        _stretchCmd;
	Cmd *		        _cursorStretchEnableCmd;
	Cmd *                   _pseudoCmd;
	Cmd *		        _pseudoGraphCmd;
	Cmd *		        _pseudoModeCmd;
	Cmd *		        _stretchHist;
	Cmd *		        _panCmd;
	Cmd *		        _magCmd;
	Cmd *		        _prefDialogCmd;
	Cmd *		        _fullScreenDisplayCmd;
	Cmd *		        _sideBarCmd;
	BorderCmd *    	        _borderCmd;
	MenuBarCmd *            _menuCmd;
	Cmd *		        _postStretchCmd;
	Cmd *		        _postPseudoCmd;
	Cmd *		        _postZoomCmd;
	ZoomCmdSet *	        _zoomCmdSet;
	ZoomDialog *	        _zoomDialog;

	Cmd *		        _minAutoCmd;
	Cmd *		        _maxAutoCmd;
	Cmd *		        _minValueCmd;
	Cmd *		        _maxValueCmd;
	Cmd *		        _postRangeCmd;
	Cmd *                   _latLonCmd;
	Cmd *                   _reloadCmd;
        LabelCmd *              _labelDisplayCmd;

	BasicImageView * _imageView;

	// Application resources

	Boolean		_enablePrintWidgetTree;
        int             _CMapRedLevels, _CMapGreenLevels, _CMapBlueLevels;
        int             _CMapGrayLevels;
	Boolean		_autoMinRange;
	Boolean		_autoMaxRange;
	String		_minValue;	// Strings so we can detect no-value
	String		_maxValue;
	Boolean		_setInitialZoomFit;
	Boolean		_enableScriptCommand;
	String		_scriptCommand;
	Boolean		_enableSaveCommand;
	String		_saveCommand;
	Boolean		_enablePrintCommand;
	String		_printCommand;

	virtual Widget createWorkArea( Widget parent );
	void createMenuPanes();

public:

	ImageWindow( const char * name ) : MenuWindow(name) { };

	~ImageWindow();
//	virtual void initialize();

	ImageData *getImageData() { return (ImageData *)_image; }
	BasicImageView *getImageView() { return _imageView; }
	
	ImageDisplayer *getImageDisplayer() { return _imageDisplayer; }
	
	SiHistogram *getHistR() { return _histR; }
	SiHistogram *getHistG() { return _histG; }
	SiHistogram *getHistB() { return _histB; }

	Lut	  *getLutR() { return _lutR; }
	Lut	  *getLutG() { return _lutG; }
	Lut	  *getLutB() { return _lutB; }

	Lut       *getPseudoR() { return _pseudoR; }
        Lut       *getPseudoG() { return _pseudoG; }
        Lut       *getPseudoB() { return _pseudoB; }

        Cmd       *getHistCmd() { return _histCmd; }
        Cmd       *getLutCmd() { return _lutCmd; }
	Cmd	  *getStretchCmd() { return _stretchCmd; }
	Cmd       *getPseudoCmd() { return _pseudoCmd; }
	Cmd	  *getPostStretchCmd() { return _postStretchCmd; }

	virtual void enableLatLonIFs(Boolean);
};
#endif
