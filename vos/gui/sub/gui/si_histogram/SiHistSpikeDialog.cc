////////////////////////////////////////////////////////////////////////
// SiHistSpikeDialog.cc: Source file for creating Spike Dialog Box
////////////////////////////////////////////////////////////////////////
#include "SiHistSpikeDialog.h"
#include "SiHistBox.h"
#include "SiHistogram.h"
#include "SgIntKeyinInterface.h"
#include "SiHistSpikeArrowIf.h"
#include "SiHistSpikeCmd.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

SiHistSpikeDialog::SiHistSpikeDialog ( SiHistBox *box, const char *name ) 
	: CustomDialog ( name, Invisible, Invisible, Invisible, 
			 Visible, Visible )
{
   _histBox = box;
}

Widget SiHistSpikeDialog::createWorkArea ( Widget parent)
{
   SiHistogram *histogram;
   histogram = _histBox->getHistR();

   Widget rc = XtVaCreateWidget("workArea", 
				xmRowColumnWidgetClass, parent,
				XmNorientation, 	XmHORIZONTAL,
				XmNpacking,     	XmPACK_TIGHT,
				NULL);

   Cmd *spikeCmd = new SiHistSpikeCmd ("SpikeCmd", TRUE, _histBox);

   SgIntKeyinInterface  *keyIn; 
   keyIn = new SgIntKeyinInterface ( rc, spikeCmd );

   CmdInterface *incButton;
   incButton = new SiHistSpikeArrowIf ( rc, 1, histogram,  spikeCmd );

   CmdInterface *decButton;
   decButton = new SiHistSpikeArrowIf ( rc, -1, histogram, spikeCmd );

   keyIn->manage();
   incButton->manage();
   decButton->manage();

   return rc;
}
