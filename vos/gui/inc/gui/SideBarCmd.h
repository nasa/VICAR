/////////////////////////////////////////////////////////////
// SideBarCmd.h: Include file for turning XVicDisp side bar
//                on and off
/////////////////////////////////////////////////////////////
#ifndef SIDEBARCMD_H
#define SIDEBARCMD_H
#include "Cmd.h"

class ImageDisplayer;

class SideBarCmd : public Cmd {
  private:
    int _oldValue;    // Last valid command for Undo
    ImageDisplayer *_imageView;
  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SideBarCmd( const char *, int, ImageDisplayer * );
    virtual const char *const className () { return "SideBarCmd"; }
};
#endif
