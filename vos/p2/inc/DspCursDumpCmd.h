/////////////////////////////////////////////////////////////
// DspCursDumpCmd.h: Include file to handle the CursorPos command
//                button from the Browse app.
/////////////////////////////////////////////////////////////
#ifndef DSPCURSDMPCMD_H
#define DSPCURSDMPCMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>
#include "DspCursDump.h"
#include "ImageData.h"
#include "ImageDisplayView.h"

class MainWindow;
class ImageData;
class ImageDisplayView;

class DspCursDumpCmd : public NoUndoCmd {

  private:
     
     DspCursDump *	_cursDump;
     
  protected:

    int                 _created;
    char *		_fileName;
 
    
    virtual void 	doit();
    
  public:

    DspCursDumpCmd ( Widget parent, const char*, const char*, int,  ImageData*, 
    				ImageDisplayView*, unsigned char, int );
    static  void 	setNameLink(XtPointer obj, char * fileName);
    virtual void	setName(char * fileName);
    virtual const char *const className () { return "DspCursDumpCmd"; }
};
#endif

