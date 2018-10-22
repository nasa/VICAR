////////////////////////////////////////////////////////////////////////
// ReloadFileCmd: A Command class, the purpose of which
// is simply to call execute(filename) on a LoadFileCmd
// object.
////////////////////////////////////////////////////////////////////////
#include "ReloadFileCmd.h"
#include "ImageWindow.h"
#include "ImageToReloadGlue.h"
#include "ImageData.h"

ReloadFileCmd::ReloadFileCmd(const char *name, int active, ImageData *image, 
			     Cmd *loadFileCmd)
  : NoUndoCmd(name, active)
{
  _image = image;
  _loadFileCmd = loadFileCmd;
   
  _glue = new ImageToReloadGlue(_image, this);
}

void ReloadFileCmd::doit()
{
  if(_loadFileCmd && _image->getReloadDataSourceName()) {
    char *temp = strdup(_image->getReloadDataSourceName());
    // we make a copy, since the Cmd will try to free the string
    _loadFileCmd->execute(temp);
  }

}

ReloadFileCmd::~ReloadFileCmd()
{
  delete _glue;
}
