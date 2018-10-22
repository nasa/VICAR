/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "return_status.h"
#include "lbl_gen_api.h"

#include "lbl_articulation.h"
#include "lbl_camera_model.h"
#include "lbl_command.h"
#include "lbl_compression.h"
#include "lbl_coordinate.h"
#include "lbl_derived_geometry.h"
#include "lbl_derived_image.h"
#include "lbl_ground_support.h"
#include "lbl_identification.h"
#include "lbl_image_data.h"
#include "lbl_image_geometry.h"
#include "lbl_instrument_state.h"
#include "lbl_surface_model.h"
#include "lbl_surface_projection.h"
#include "lbl_telemetry.h"

#include <zvproto.h>

typedef	struct	{
		char	*Name;
		int	(*Process)();
		void	(*Load)();
		void	(*Print)();
		void	*Items;
		} TestTable_typ;

LblArticulation_typ	Articulation;
LblCameraModel_typ 	CameraModel;
LblCommand_typ 		Command;
LblCompression_typ	Compression;
LblCoordinate_typ	Coordinate;
LblDerivedGeometry_typ 	DerivedGeometry;
LblDerivedImage_typ 	DerivedImage;
LblGroundSupport_typ	GroundSupport;
LblIdentification_typ	Identification;
LblImageData_typ 	ImageData;
LblImageGeometry_typ 	ImageGeometry;
LblInstrumentState_typ 	InstrumentState;
LblSurfaceModel_typ	SurfaceModel;
LblTelemetry_typ 	Telemetry;
LblSurfaceProjection_typ	SurfaceProjection;

/***  TestTable_typ Labels[15];  ****/
/****  Quicker way, but Alpha VMS doesn't like it  ***/
TestTable_typ Labels[] = {
		{ "COMMAND",
		  *LblCommand,
                  *LblTestCommand,
                  *LblPrintCommand,
		  &Command },
		{ "COMPRESSION",
		  *LblCompression,
                  *LblTestCompression,
                  *LblPrintCompression,
		  &Compression },
		{ "COMPRESSION_PARMS",
		  *LblCompressionParms,
                  *LblTestCompression,
                  *LblPrintCompression,
		  &Compression },
		{ "CAMERA_MODEL",
		  *LblCameraModel,
                  *LblTestCameraModel,
                  *LblPrintCameraModel,
		  &CameraModel },
		{ "GEOMETRIC_CAMERA",
		  *LblGeometricCameraModel,
                  *LblTestCameraModel,
                  *LblPrintCameraModel,
		  &CameraModel },
		{ "DERIVED_GEOMETRY",
		  *LblDerivedGeometry,
                  *LblTestDerivedGeometry,
                  *LblPrintDerivedGeometry,
		  &DerivedGeometry },
		{ "IDENTIFICATION",
		  *LblIdentification,
                  *LblTestIdentification,
                  *LblPrintIdentification,
		  &Identification },
		{ "IMAGE_DATA",
		  *LblImageData,
                  *LblTestImageData,
                  *LblPrintImageData,
		  &ImageData },
		{ "IMAGE_GEOMETRY",
		  *LblImageGeometry,
                  *LblTestImageGeometry,
                  *LblPrintImageGeometry,
		  &ImageGeometry },
		{ "INSTRUMENT_STATE",
		  *LblInstrumentState,
                  *LblTestInstrumentState,
                  *LblPrintInstrumentState,
		  &InstrumentState },
		{ "INSTRUMENT_STATE_PARMS",
		  *LblInstrumentStateParms,
                  *LblTestInstrumentState,
                  *LblPrintInstrumentState,
		  &InstrumentState },
		{ "SURFACE_MODEL",
		  *LblSurfaceModel,
                  *LblTestSurfaceModel,
                  *LblPrintSurfaceModel,
		  &SurfaceModel },
		{ "SURFACE_MODEL_PARMS",
		  *LblSurfaceModelParms,
                  *LblTestSurfaceModel,
                  *LblPrintSurfaceModel,
		  &SurfaceModel },
		{ "SURFACE_PROJECTION",
		  *LblSurfaceProjection,
                  *LblTestSurfaceProjection,
                  *LblPrintSurfaceProjection,
		  &SurfaceProjection },
		{ "SURFACE_PROJECTION_PARMS",
		  *LblSurfaceProjectionParms,
                  *LblTestSurfaceProjection,
                  *LblPrintSurfaceProjection,
		  &SurfaceProjection },
		{ "TELEMETRY",
		  *LblTelemetry,
                  *LblTestTelemetry,
                  *LblPrintTelemetry,
		  &Telemetry },
		{ "ARTICULATION",
		  *LblArticulation,
                  *LblTestArticulation,
                  *LblPrintArticulation,
		  &Articulation },
		{ "COORDINATE",
		  *LblCoordinate,
                  *LblTestCoordinate,
                  *LblPrintCoordinate,
		  &Coordinate },
		{ "GROUND_SUPPORT",
		  *LblGroundSupport,
                  *LblTestGroundSupport,
                  *LblPrintGroundSupport,
		  &GroundSupport },
		{ "DERIVED_IMAGE",
		  *LblDerivedImage,
                  *LblTestDerivedImage,
                  *LblPrintDerivedImage,
		  &DerivedImage },
		{ "DERIVED_IMAGE_PARMS",
		  *LblDerivedImageParms,
                  *LblTestDerivedImage,
                  *LblPrintDerivedImage,
		  &DerivedImage },
		{ 0, 0, 0, 0, 0 }
              };
/***/

/*******************************************************************************
 *				MAIN
 *
 ******************************************************************************/
int main(
  int	argc,
  char	*argv[])
{ int	cnt,
	Idx,
	Line,
	OutUnit,
	status;
  char	Pixels[100],
	Choice[128];;

  zveaction("SA","");		// maks sure error overrides work

  if (argc > 1)
  { if (strcmp(argv[1],"help") == 0 || strcmp(argv[1],"HELP") == 0)
    { printf("tst_lbl_routines [<file_name> [READ | WRITE [<property label>]]]\n");
      return 1;
    }
    strcpy(Pixels,argv[1]);
  } else strcpy(Pixels,"label_test.img");

  if (argc > 2) strcpy(Choice,argv[2]);
  else strcpy(Choice,"READ_WRITE");

  status = zvunit(&OutUnit,"NA",1,"U_NAME",Pixels,NULL);
  if (status != 1)
  { zvmessage("Could not open output file"," ");
    zabend();
  }

  if (strstr(Choice,"WRITE"))
  { status = zvopen(OutUnit, "OP","WRITE", "O_FORMAT","BYTE", "U_FORMAT","BYTE",
                    "OPEN_ACT","AS", "U_NL",10, "U_NS",10, "U_NB",1,  NULL);
    zvsignal(OutUnit, status, 1);

    for (Idx=0; Labels[Idx].Name; Idx++)
    { if (argc > 3 && !strstr(Labels[Idx].Name,argv[3])) continue;
      if (Labels[Idx].Load) Labels[Idx].Load(Labels[Idx].Items);
      status = Labels[Idx].Process(OutUnit,LBL_WRITE,Labels[Idx].Items,1);
      if (RTN_FAILURE(status))
         printf("Label Processing failed: %d\n%s\n",
                RTN_NUMBER(status), LblErrorMessage());
      if (Labels[Idx].Print) Labels[Idx].Print(Labels[Idx].Items);
    }

    for (Line=1; Line<=10; Line++)
        status =  zvwrit(OutUnit, Pixels, NULL);
    zvclose(OutUnit, NULL);
  }

  if (strstr(Choice,"READ"))
  { status = zvopen(OutUnit, "OP","READ", "OPEN_ACT","AS",  NULL);
    zvsignal(OutUnit, status, 1);

    for (Idx=0; Labels[Idx].Name; Idx++)
    { if (argc > 3 && !strstr(Labels[Idx].Name,argv[3])) continue;
      status = Labels[Idx].Process(OutUnit,LBL_READ,Labels[Idx].Items,1);
      if (RTN_FAILURE(status))
         printf("Label Processing failed: %d\n%s\n",
                RTN_NUMBER(status), LblErrorMessage());
      if (Labels[Idx].Print) Labels[Idx].Print(Labels[Idx].Items);
    }
    zvclose(OutUnit, NULL);
  }


  return 1;
}

