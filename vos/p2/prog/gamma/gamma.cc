//////////////////////////////////////////////////////////////////////////
// gamma
//
// This program provides the functionalities of gamma correction. Input
// image must be 1-band or 3-band. The gamma corrected output will be
// in BYTE format. The gamma value needs to be provided for non-srgb
// gamma correction.
//////////////////////////////////////////////////////////////////////////

#include "vicmain_c"
#include "SimpleImage.h"
#include "lbl_image_data.h"
#include "lbl_derived_image.h"

#include <stdlib.h>
#include <cfloat>
#include <math.h>
//////////////////////////////////////////////////////////////////////////
// Find and return the maximum value in an image.
///////////////////////////////////////////////////////////////
double getMax(SimpleImage<double> *image);

//////////////////////////////////////////////////////////////////////////
// Scale a value from old range to a new range.
//////////////////////////////////////////////////////////////////////////
double normalize(double value, double old_min, double old_max, double new_min, 
                 double new_max);

//////////////////////////////////////////////////////////////////////////
// sRGB gamma correction based on the specification of sRGB. The constant
// values used in this function are all defined in the specification of 
// sRGB. The output image is scaled to [0, maxout].
// TODO: make srgb mode support inverse correction.
//////////////////////////////////////////////////////////////////////////
void srgbGammaCorrection(SimpleImage<double> *inp_img, 
                         SimpleImage<double> *out_img, 
                         double maxin, double maxout, short int is_float);

//////////////////////////////////////////////////////////////////////////
// Simple gamma correction based on the pure exponential function. The 
// output image is scaled to [0, maxout].
//////////////////////////////////////////////////////////////////////////
void simpleGammaCorrection(SimpleImage<double> *inp_img, 
                           SimpleImage<double> *out_img, double gamma, 
                           double maxin, double maxout, short int do_inverse, 
                           short int is_float);

void main44() {
    zvmessage("Gamma version 1.0.0", "");

    char msg[256];
    char inp_filename[256], out_filename[256];
    int status, count, def;
    int inp_nl, inp_ns, inp_nb;
    int inp_unit[3], inp_band[3];
    int out_unit[3], out_band[3];

    // Read input image's filename.
    zvp("INP", inp_filename, &count);
    sprintf(msg, "Processing image: %s", inp_filename);
    zvmessage(msg, "");

    // Get input image's nl, ns, and nb.
    zvunit(&inp_unit[0], "INP", 1, "u_name", inp_filename, NULL);
    zvopen(inp_unit[0], "op", "read", "u_format", "doub", "open_act", "sa", NULL);
    zvget(inp_unit[0], "nl", &inp_nl, "ns", &inp_ns, "nb", &inp_nb, NULL);

    // validation for input image's band.
    // only 1-band and 3-band input images are supported.
    if (inp_nb != 1 && inp_nb != 3) {
        sprintf(msg, "input images must be 1-band or 3-band. "
                "However, %d-band input is provided", inp_nb);
        zvmessage(msg, "");
        zabend();
    }
    inp_unit[2] = inp_unit[1] = inp_unit[0];
    inp_band[0] = 1;
    inp_band[1] = 2;
    inp_band[2] = 3;

    // Validation for sRGB case
    short int do_srgb = zvptst("SRGB");
    if (do_srgb && inp_nb != 3) {
        sprintf(msg, "sRGB gamma correction requires 3-band input. "
                "However, %d-band input is provided.", inp_nb);
        zvmessage(msg, "");
        zabend();
    } 

    // Read and validate parameter GAMMA 
    double gamma;
    zvparmd("GAMMA", &gamma, &count, &def, 1, 0);
    if (!do_srgb && count == 0) {
        zvmessage("Paramerter GAMMA is required for non-srgb gamma "
                  "correction.", "");
        zabend();
    } 

    // Read INVERSE parameter
    short int do_inverse = zvptst("INVERSE"); 

    // Find out input format, and the maximum number can be represented 
    // by this format
    char inp_fmt[8];
    double max_in_value = 0.0;
    zvget(inp_unit[0], "FORMAT", inp_fmt, NULL);
    
    if (strcmp(inp_fmt, "BYTE") == 0) max_in_value = 255.0;
    if (strcmp(inp_fmt, "HALF") == 0) max_in_value = 32767.0;
    if (strcmp(inp_fmt, "FULL") == 0) max_in_value = 2147483647.0;
    if (strcmp(inp_fmt, "REAL") == 0) max_in_value = 1.0;
    if (strcmp(inp_fmt, "DOUB") == 0) max_in_value = 1.0;

    // Find out output format, and the maximum number can be represented 
    // by this format.
    char *out_fmt = "BYTE";
    double max_out_value = 255.0;
    short int is_float = 0;

    if (zvptst("HALF")) {
        out_fmt = "HALF";
        max_out_value = 32767.0;
    }
    if (zvptst("FULL")) {
        out_fmt = "FULL";
        max_out_value = 2147483647.0;
    }
    if (zvptst("REAL")) {
        out_fmt = "REAL";
        max_out_value = 1.0;
        is_float = 1;
    }
    if (zvptst("DOUB")) {
        out_fmt = "DOUB";
        max_out_value = 1.0;
        is_float = 1;
    }

    // Read input image
    SimpleImage<double> inp_img;
    inp_img.alloc(inp_nb, inp_nl, inp_ns);
    for (int b = 0; b < inp_nb; b++) {
        for (int l = 0; l < inp_nl; l++) {
            zvread(inp_unit[b], inp_img.linePtr(b, l), "band", inp_band[b], 
                   "line", l + 1, "NSAMPS", inp_ns, NULL);
        }
    }

    // Create output image which has the exact same dimension as input image. 
    SimpleImage<double> out_img;
    out_img.alloc(inp_nb, inp_nl, inp_ns);

    // init label routine ImageData for reading from input
    LblImageData_typ *InpImageData = new LblImageData_typ;
    LblImageData(inp_unit[0], LBL_READ, InpImageData, 1);

    // Find out the maximum values for input and output
    double max_in, max_out;  // values read from parameters
    double maxin = 0.0, maxout = 0.0;  

    // Read parameter MAX_IN 
    zvparmd("MAX_IN", &max_in, &count, &def, 1, 0);
    if (count == 1) {
        maxin = max_in;
    } 
    if (maxin == 0.0 && InpImageData->SampleBitMask.Valid) {
        // figure out the image's depth from sample_bit_mask label.
        char *bit_mask = InpImageData->SampleBitMask.Value;
        char *p = strtok(bit_mask, "#");
        char substr_bit_mask[32];
        while (p != NULL) {
            strcpy(substr_bit_mask, p);
            p = strtok(NULL, "#");
        }

        // convert binary to base 10 number.
        maxin = (double)strtol(substr_bit_mask, NULL, 2);
    }
    if (maxin == 0.0) {
        maxin = max_in_value;
    }

    // Read parameter MAX_OUT
    zvparmd("MAX_OUT", &max_out, &count, &def, 1, 0);
    if (count == 1) maxout = max_out;
    if (maxout == 0.0) maxout = max_out_value;

    /*
    // Find out the depth of the input image 
    double max_value;
    if (InpImageData->SampleBitMask.Valid) {
        // figure out the image's depth from sample_bit_mask label.
        char *bit_mask = InpImageData->SampleBitMask.Value;
        char *p = strtok(bit_mask, "#");
        char substr_bit_mask[32];
        while (p != NULL) {
            strcpy(substr_bit_mask, p);
            p = strtok(NULL, "#");
        }
        // convert binary to base 10 number.
        max_value = (double)strtol(substr_bit_mask, NULL, 2);
    } else {
        // find out the nearest next power of 2 number for the maximum value  
        max_value = getMax(&inp_img);
        max_value = pow(2, ceil(log(max_value) / log(2)));
    } */

    // gamma correction
    if (do_srgb) {
        zvmessage("Gamma correction mode: sRGB", "");
        srgbGammaCorrection(&inp_img, &out_img, maxin, maxout, is_float);
    } else {
        zvmessage("Gamma correction mode: simple", "");
        simpleGammaCorrection(&inp_img, &out_img, gamma, maxin, maxout, 
                              do_inverse, is_float);
    }

    // open output file
    zvunit(&out_unit[0], "OUT", 1, NULL);
    zvopen(out_unit[0], "op", "write", "u_ns", inp_ns, "u_nl", inp_nl, 
           "u_nb", inp_nb, "open_act", "sa", "u_org", "bsq", "u_format", "doub",
           "o_format", out_fmt, NULL);
    zvplabel(out_unit[0], 0, 1);
    out_unit[2] = out_unit[1] = out_unit[0];
    out_band[0] = 1;
    out_band[1] = 2;
    out_band[2] = 3;

    // init label routine DerivedImage for writing
    LblDerivedImage_typ DerivedImage;
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    // write ENCODED_DISPLAY_GAMMA label
    if (do_srgb) {
        strcpy(DerivedImage.EncodedDisplayGamma.Value,"sRGB");
    } else {
        char exp_str[32];
        sprintf(exp_str, "%.3f", gamma);
        strcpy(DerivedImage.EncodedDisplayGamma.Value, exp_str);
    }
    DerivedImage.EncodedDisplayGamma.Valid = 1;
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    LblDerivedImageApi(out_unit[0], LBL_AUGMENT, &DerivedImage, 1);

    // write SAMPLE_BIT_MASK label
    LblImageData_typ OutImageData;
    memset(&OutImageData, 0 , sizeof(LblImageData_typ));
    strcpy(OutImageData.SampleBitMask.Value, "2#11111111#");
    OutImageData.SampleBitMask.Valid = 1;
    LblImageData(out_unit[0], LBL_AUGMENT, &OutImageData, 1);
   
    // write output
    for (int l = 0; l < inp_nl; l++) {
        for (int b = 0; b < inp_nb; b++) {
            zvwrit(out_unit[b], out_img.linePtr(b, l), "line", l + 1, "band", 
                   out_band[b], "NSAMPS", inp_ns, NULL);
        }
    } 
    zvmessage("Save output.", "");

    // free memory
    inp_img.free();
    out_img.free();
    zvclose(inp_unit[0], NULL);
    zvclose(out_unit[0], NULL);
    delete InpImageData;
}

/*
// Find the maximum value in an image.
double getMax(SimpleImage<double> *image)
{
    int nb = image->getNB();
    int nl = image->getNL();
    int ns = image->getNS();
    double max = -DBL_MAX;

    for (int b = 0; b < nb; b++) {
        for (int l = 0; l < nl; l++) {
            for (int s = 0; s < ns; s++) {
                double value = image->get(b, l, s);
                if (value >= max) {
                    max = value;
                }
            }
        }
    }

    return max;
} */

// Scale a value from old range to a new range.
double normalize(double value, double old_min, double old_max, double new_min, 
                 double new_max)
{
    if (old_max - old_min == 0.0) return 0.0;

    return ((value - old_min) * (new_max - new_min)) / (old_max - old_min)
           + new_min;
}

// sRGB gamma correction
void srgbGammaCorrection(SimpleImage<double> *inp_img, 
                         SimpleImage<double> *out_img, 
                         double maxin, double maxout, short int is_float)
{
    // Values used to normalize data
    double inter_min = 0.0;
    double inter_max = 1.0;
    double final_min = 0.0;
    double final_max = maxout;

    // These values are defined in the specification of sRGB.
    // In order to use these values directly, data must be normalized to [0, 1].
    double srgb_constant = 12.92;
    double srgb_cutoff = 0.0031308;
    double srgb_a = 0.055;
    double srgb_gamma = 1.0 / 2.4;
    double linear_v, srgb_v;

    int nb = inp_img->getNB();
    int nl = inp_img->getNL();
    int ns = inp_img->getNS();
    
    for (int b = 0; b < nb; b++) {
        for (int l = 0; l < nl; l++) {
            for (int s = 0; s < ns; s++) {
                // normalize to [0, 1]
                linear_v = inp_img->get(b, l, s);
                linear_v = normalize(linear_v, 0.0, maxin, inter_min, inter_max);

                // gamma correction
                if (linear_v <= srgb_cutoff) {
                    srgb_v = srgb_constant * linear_v;
                } else {
                    srgb_v = (1 + srgb_a) * pow(linear_v, srgb_gamma) - srgb_a;
                } 

                // clip srgb value to [0, 1]
                if (srgb_v < inter_min) srgb_v = inter_min;
                if (srgb_v > inter_max) srgb_v = inter_max;

                // normalize srgb value to [0, final_max]
                srgb_v = normalize(srgb_v, inter_min, inter_max, final_min, final_max);

                // add 0.5 to integral types. The vicar runtime library (RTL)
                // will truncate it when writing output. 
                if (!is_float) srgb_v += 0.5;

                // save the srgb value in output image.
                out_img->set(b, l, s, srgb_v);
            }
        }
    }
}

// Simple gamma correction
void simpleGammaCorrection(SimpleImage<double> *inp_img,
                           SimpleImage<double> *out_img, double gamma, 
                           double maxin, double maxout, short int do_inverse,
                           short int is_float)
{
    // Values used to normalize data
    double inter_min = 0.0;
    double inter_max = 1.0;
    double final_min = 0.0;
    double final_max = maxout;

    int nb = inp_img->getNB();
    int nl = inp_img->getNL();
    int ns = inp_img->getNS();
    double value;
    double exp;
    char msg[256];

    if (gamma != 0.0) {
        exp = 1.0 / gamma;
        if (do_inverse) exp = 1.0 / exp;
    } else {
        exp = 0.0;
        zvmessage("WARNING: parameter GAMMA should not normally be 0.","");
    }

    for (int b = 0; b < nb; b++) {
        for (int l = 0; l < nl; l++) {
            for (int s = 0; s < ns; s++) {
                // normalize input value to [0, 1]
                value = inp_img->get(b, l, s);
                value = normalize(value, 0.0, maxin, inter_min, inter_max);
 
                // gamma correction
                value = pow(value, exp);

                // clip the value to [0, 1]
                if (value < inter_min) value = inter_min;
                if (value > inter_max) value = inter_max;

                // normalize the value to [0, final_max]
                value = normalize(value, inter_min, inter_max, final_min, final_max);

                // add 0.5 to integral types. The vicar runtime library (RTL)
                // will truncate it when writing output.
                if (!is_float) value += 0.5;

                // save the value in output image
                out_img->set(b, l, s, value);
            }
        }
    }
}
