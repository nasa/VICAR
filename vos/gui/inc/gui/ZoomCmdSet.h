/////////////////////////////////////////////////////////////////
// ZoomCmdSet.h - creates and maintains all the Zoom commands
////////////////////////////////////////////////////////////////
#ifndef ZOOMCMDSET_H
#define ZOOMCMDSET_H

class BasicImageView;
class MenuCmdList;
class CmdList;
class ZoomSpecialCmdList;
class ZoomSpecialCmd;
class ZoomFitCmd;
class ZoomCmd;

class ZoomCmdSet {

 protected:

   MenuCmdList *_menuCmdList;
   CmdList *_radioList;
   ZoomSpecialCmdList *_zoomSpecialCmdList;
   ZoomSpecialCmd *_zoomSpecialCmd;
   ZoomCmd *_zoomCmd;
   ZoomFitCmd *_zoomFitCmd;
   BasicImageView *_imageView;

 public:

   ZoomCmdSet(BasicImageView *imageView);

   MenuCmdList *getMenuList() { return _menuCmdList; }
   CmdList *getRadioList() { return _radioList; }
   ZoomCmd *getZoomCmd() { return _zoomCmd; }
   ZoomSpecialCmdList *getZoomSpecialCmdList() { return _zoomSpecialCmdList; }
   ZoomSpecialCmd *getZoomSpecialCmd() { return _zoomSpecialCmd; }
   ZoomFitCmd *getZoomFitCmd() { return _zoomFitCmd; }
   BasicImageView *getImageView() { return _imageView; }
};
#endif

