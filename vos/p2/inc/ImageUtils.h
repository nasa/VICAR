#ifndef IMAGE_UTILS
#define IMAGE_UTILS

#define IU_MAX_FNAME_LEN 250
#define IU_MAX_PARM       20
#define IU_SKIP_LINES      0
#define IU_BILINEAR_INTERP 1
#define IU_BICUBIC_INTERP  2
#define IU_NEAR_NEIGHBOR   3

/******************************************************************************/
typedef struct
{
   int unit;
   int nl, ns, pixsize;
   char format[8];
   char fname[IU_MAX_FNAME_LEN];

   double *buffer;
   unsigned char **valid;
   long long int valid_cnt;
   int curr_line_in_buff;
}VICAR_IMAGE;

/******************************************************************************/
typedef struct
{
   VICAR_IMAGE *vi;

   double **buffer;
   double **tile;
   int buffer_size;
   int last_line_requested;
   int tile_nl, tile_ns;
   int startPos;
}VICAR_TILE_IMAGE;

/******************************************************************************/
typedef struct
{
   VICAR_IMAGE *from;
   VICAR_IMAGE *to;

   /* buffer is assumed to be 4 sequential lines */
   double **buffer;
   int curr_lines_in_buff[4];
   int resample_mode;
}VICAR_RESAMPLE_IMAGE;

/******************************************************************************/
//void getValidMask(VICAR_IMAGE **vi);

/******************************************************************************/
// getVI_inp: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + inst
//    - the instance of vicar input file opened
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_inp(int inst);

/******************************************************************************/
// getVI_inp_by_fname: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + fname
//    - filename
// + inst
//    - the instance of vicar input file opened
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_fname(char *fname, int inst);

/******************************************************************************/
// getVI_inp_by_parmName: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + parmName
//    - parameter name in the pdf file
// + inst
//    - the instance of vicar input file opened
//
// output:
// =======
// + NULL
//    - if inst is less than the number of parameter given
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_parmName(char *parmName, int inst);

/******************************************************************************/
// getVI_out: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + format
//    - format of the output file
// + inst
//    - the instance of vicar input file opened
// 
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_out(char *format, int inst, int nl, int ns);

/******************************************************************************/
// getVI_out_by_fname: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + fname
//    - filename
// + type
//    - group name of inst
// + format
//    - format of the output file
// + inst
//    - instance of vicar output file opened
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_out_by_fname(char *fname, char *type, char *format, int inst, int nl, int ns);

/******************************************************************************/
// getVI_out_by_parmName: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + parmName
//    - parameter name in the calling pdf file
// + format
//    - format of the output file
// + inst
//    - instance of vicar output file opened
//    - !!! every new parm should begin w/ 1 !!! - corresponds to nth parm
//      in a multidimensional parm
//
// output:
// =======
// + NULL
//    - if inst is less than the number of parameters given
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_out_by_parmName(char *parmName, char *format, int inst, int nl, int ns);

/******************************************************************************/
// (depricated - use getVI function) 
// getImage: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + unit
//    - an opened VICAR file handle
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getImage(int unit);

/******************************************************************************/
// deleteImage: deletes a VICAR_IMAGE struct
//
// input:
// ======
// + *vi
//    - pointer to VICAR_IMAGE to delete
/******************************************************************************/
void deleteImage(VICAR_IMAGE **vi);

/******************************************************************************/
// getVRI: return an initialized VICAR_RESAMPLE_IMAGE struct pointer
//
// input:
// ======
// + from
//    - image to resample from
// + to
//    - image to resample to
// + resample_mode
//    - method used to resample
//
// output:
// =======
// + vri
//    - initialized VICAR_RESAMPLE_IMAGE
/******************************************************************************/
VICAR_RESAMPLE_IMAGE* getVRI(VICAR_IMAGE *from, VICAR_IMAGE *to, int resample_mode);

/******************************************************************************/
// getVTI: returns an initialized VICAR_TILE_IMAGE struct
//
// input:
// ======
// + vi
//    - an initialzed VICAR_IMAGE
//    - call getImage to initialize vi before calling the function
//
// output:
// =======
// + vti
//    - initialized VICAR_TILE_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_TILE_IMAGE* getVTI(VICAR_IMAGE *vi, int tile_nl, int tile_ns);

/******************************************************************************/
// readVicarTileImage: reads the tile for given line samp
//
// input:
// ======
// + vti
//    - an initialzed VICAR_TILE_IMAGE
//    - call getVTI before calling this function
//
// output:
// =======
// + vti
//    - buffer inside vti will hold image tile
/******************************************************************************/
void readVicarTileImage(VICAR_TILE_IMAGE *vti, int line, int samp);

/******************************************************************************/
// deleteVTI: deletes the VICAR_RESAMPLE_IMAGE structure
//            (!! DOES NOT DELETE THE VICAR_IMAGE STRUCT !!)
//
// input:
// ======
// + vti
//    - VICAR_TILE_IMAGE struct
/******************************************************************************/
void deleteVTI(VICAR_TILE_IMAGE **vti);

/******************************************************************************/
// getSkippedLine: returns a downsampled line by skipping
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
// + to_line
//    - line at offset 0 specifying the output line
//
// output:
// =======
// + buf
//    - output buffer to put the downsampled line into
/******************************************************************************/
void getSkippedLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int to_line);

/******************************************************************************/
// getInterpolatedLine: returns an interpolated line *** NOT YET IMPLEMENTED ***
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
// + to_line
//    - line at offset 0 specifying the output line
//
// output:
// =======
// + buf
//    - output buffer to put the downsampled line into
/******************************************************************************/
void getBilinInterpLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int line);

/******************************************************************************/
// getSkippedLine: returns a downsampled line by skipping
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
// + to_line
//    - line at offset 0 specifying the output line
//
// output:
// =======
// + buf
//    - output buffer to put the downsampled line into
/******************************************************************************/
void getDownSampledLine(VICAR_RESAMPLE_IMAGE *vri, double *ds_buf, int line);

/******************************************************************************/
// writeVicarImageLine: writes a VICAR line onto disk
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct (writes the data stored in vi->buffer onto disk)
// + line
//    - line at offset 0 specifying the line to read (offset = 0)
//
/******************************************************************************/
void writeVicarImageLine(VICAR_IMAGE *vi, int line);

/******************************************************************************/
// readVicarResampleImageLine: reads a line from "from" image in vri and stores it
//                             inside the vri buffer
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLED_IMAGE with from and to set
// + buf_index
//    - specifies which of the 4 buffers in the 4 X NS vri buffer to read into
// + line
//    - specifies the line number in the "from" file to read (offset = 0)
//
// output:
// =======
// + vri->buffer[buf_index] stores the line read from "from" image
/******************************************************************************/
void readVicarResampleImageLine(VICAR_RESAMPLE_IMAGE *vri, int buf_index, int line);

/******************************************************************************/
// createDownSampledImage: creates a downsampled image
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLED_IMAGE with from and to set
//
// output:
// =======
// + vri->to image
/******************************************************************************/
void createDownSampledImage(VICAR_RESAMPLE_IMAGE *vri);

/******************************************************************************/
// readVicarImageLine: read a VICAR line and stores it in vi->buffer
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct
// + line
//    - line at offset 0 specifying the line to read
//
// output:
// =======
// + vi->buffer
//    - output buffer
/******************************************************************************/
void readVicarImageLine(VICAR_IMAGE *vi, int line);

/******************************************************************************/
// deleteAndCloseImage: closes the VICAR_IMAGE and frees memory
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct
/******************************************************************************/
void deleteAndCloseImage(VICAR_IMAGE **vi);

/******************************************************************************/
// deleteVRI: deletes the VICAR_RESAMPLE_IMAGE structure
//            (!! DOES NOT DELETE THE TO AND FROM FILES !!)
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
/******************************************************************************/
void deleteVRI(VICAR_RESAMPLE_IMAGE **vri);

/******************************************************************************/
// getValidMask: gets valid pixels of images by looking at non-zero pixels
//               (*UNTESTED FUNCTION*)
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct
//
// output:
// =======
// + vi valid buffer
/******************************************************************************/
void getValidMask(VICAR_IMAGE **vi);

#endif
