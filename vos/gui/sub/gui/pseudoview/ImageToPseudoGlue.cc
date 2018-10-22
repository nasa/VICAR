////////////////////////////////////////////////////////////////////////
// ImageToPseudoGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Pseudocolor objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it disables the pseudocolor tool if the image is color and enables it
// if the image is b&w.  This class, even though it's a UIComponent,
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ImageToPseudoGlue.h"
#include "Cmd.h"
#include "MenuDialog.h"
#include "ImageData.h"

ImageToPseudoGlue::ImageToPseudoGlue (ImageData *model,
				      Cmd *modeCmd, MenuDialog *dialog, 
				      Cmd *pseudoCmd, Cmd *postLutCmd, 
				      Cmd *postPseudoCmd)
    : BasicImageView("glue", model)
{
    _pseudoModeCmd = modeCmd;
    _pseudoDialog = dialog;
    _pseudoCmd = pseudoCmd;
    _postLutCmd = postLutCmd;
    _postPseudoCmd = postPseudoCmd;

    _model->attachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the image changes,
// recompute the histogram.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void ImageToPseudoGlue::update()
{
    if ( (_model->isDataSourceOpened()) && (_model->getMode() == BWmode) ) {
	_pseudoModeCmd->activate();
	_pseudoCmd->activate();
	_postLutCmd->activate();
	_postPseudoCmd->activate();
    }
    else {		// Deactivate all the commands and pop-down dialog
	_pseudoModeCmd->execute((CmdValue)False);
	_pseudoModeCmd->deactivate();
	_pseudoDialog->unpost();
	_pseudoCmd->deactivate();
	_postLutCmd->deactivate();
	_postPseudoCmd->deactivate();
    }
}

