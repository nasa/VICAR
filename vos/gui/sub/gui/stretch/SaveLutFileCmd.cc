/////////////////////////////////////////////////////////
// SaveLutFileCmd: A Command class that saves pseudocolor tables in  an
// IBIS file format.  The Command value is a dynamically allocated single
// string.
/////////////////////////////////////////////////////////
#include "SaveLutFileCmd.h"
#include "Lut.h"
#include "ErrorManager.h"
#include "UIComponent.h"                // only for strdup()
#include "ibisfile.h"
#include "zvproto.h"
#include "file_no_path.h"
#include <stdio.h>
#include <assert.h>

SaveLutFileCmd::SaveLutFileCmd(const char *name, int active, 
			       Lut *lutRed, Lut *lutGrn, Lut *lutBlu)
    : NoUndoCmd(name, active)
{
    _lutRed = lutRed;
    _lutGrn = lutGrn;
    _lutBlu = lutBlu;
}

void SaveLutFileCmd::doit()
{
    const int lut_size = 256; //!!! Make this more general by passing lut

    char *filename = (char *)_value;

    int unit, i, ibis, status, record;
    int lutcols[3];
    int lut[lut_size][3];
 
    if (!filename) return;
    
    for (i = 0; i < 3; i++) 
	lutcols[i] = i+1;
 
    status = zvunit(&unit, (char *)"out_file",  1, "u_name", filename, NULL);
    if (status != 1) {
	theErrorManager->process(Error, "zvunit", "Error saving file", 
				 filename);
	return;
    }

    status = IBISFileOpen(unit, &ibis, (char *)IMODE_WRITE, 3,
			  lut_size, 0, (char *)IORG_COLUMN);
    if (status != 1) {
	theErrorManager->process(Error, "IBISFileOpen", 
				 "Error saving file", filename);
	return;
    }
    
    status = IBISFileSet(ibis, (char *)IFILE_TYPE, (char *)"LOOKUP_TABLE", 0);
    if (status != 1) {
        theErrorManager->process(Error, "IBISFileSet",
                                 "Error saving file", filename);
	return;
    }

    status = ICLNewRGB(ibis, 1, 2, 3, 0);
    if (status < 0) {
	theErrorManager->process(Error, "ICLNewRGB",
                                 "Error saving file", filename);
	return;
    }

    char *stripFilename = strdup(filename);
    file_no_path(stripFilename);

    status = ICLNewLOOKUP_TABLE(ibis, lutcols, 3, 0, 0, 
				(char *)"PSEUDOCOLOR", stripFilename);
    if (status < 0) {
        theErrorManager->process(Error, "ICLNewLOOKUP_TABLE",
                                 "Error saving file", filename);
	return;
    }

    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							filename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
    
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
			    0, 0, (char *)IFMT_FULL);
    if (status != 1) {
	theErrorManager->process(Error, "IBISRecordOpen",
                                 "Error saving file", filename);
	return;
    }
    
    for (i = 0; i < lut_size; i++) {
	lut[i][0] = (*_lutRed)[i];
	lut[i][1] = (*_lutGrn)[i];
	lut[i][2] = (*_lutBlu)[i];
    }
    for (i = 0; i < lut_size; i++) {
	status = IBISRecordWrite(record, (char*)lut[i], i+1);
	if (status != 1) {
	    theErrorManager->process(Error, "IBISRecordWrite",
				     "Error saving file", filename);
	    return;
	}
    }
    
    // close up shop

    status = IBISFileClose( ibis, 0 );
    if (status != 1) {
	theErrorManager->process(Error, "IBISFileClose",
				 "Error saving file", filename);
    }

    delete [] stripFilename;
}
