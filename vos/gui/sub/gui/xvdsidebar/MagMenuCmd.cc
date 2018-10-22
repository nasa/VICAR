///////////////////////////////////////////////////////////
// MagMenuCmd.cc: 
//////////////////////////////////////////////////////////
#include "MagMenuCmd.h"
#include "MagTool.h"
#include "ImageData.h"
#include "Lut.h"
#include "CursorModel.h"
#include "MagInfo.h"

MagMenuCmd::MagMenuCmd(const char *name, int active, Widget parent,
                ImageData *imageData, Widget iw,
                Lut *rlut, Lut *glut, Lut *blut,
		Lut *rpseudo, Lut *gpseudo, Lut *bpseudo,
                MagInfo *magInfo,
                CursorPositionView *posView, CursorDnView *dnView)
        : NoUndoCmd(name, active)
{
    _parent = parent;
    _imageData = imageData;
    _iw = iw;
    _rlut = rlut;
    _glut = glut;
    _blut = blut;
    _rpseudo = rpseudo;
    _gpseudo = gpseudo;
    _bpseudo = bpseudo;
    _posView = posView;
    _dnView = dnView;
    _magInfo = magInfo;

    _magTool = NULL;
}

MagMenuCmd::~MagMenuCmd()
{
    if (_magTool) {
        _magTool->unmanage();
        delete _magTool;
    }
}

void MagMenuCmd::doit()
{
    if (_value) {
        _magTool = new MagTool(_parent, "magTool", _imageData, _iw, 
			_rlut, _glut, _blut,
			_rpseudo, _gpseudo, _bpseudo,
                        _magInfo, _posView, _dnView);

        _magTool->manage();
    }
    else {
        if (!_magTool) return;
        _magTool->unmanage();
        delete _magTool;
    }
}
