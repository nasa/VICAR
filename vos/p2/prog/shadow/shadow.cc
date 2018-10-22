#include "vicmain_c"
#include "SimpleImage.h"

#include <cmath>
#include <cfloat>
#include <math.h>

using namespace std;

#define PI 3.14159265

SimpleImage<double> create_gaussian_kernel(int gau_kernel_size, double sigma);
double convolution2D(SimpleImage<double> &source_img, SimpleImage<double> &gau_kernel, int target_line, int target_samp);
double findMax(SimpleImage<double> &source_img);
double findMin(SimpleImage<double> &source_img);
double normalize(double source_value, double source_min, double source_max);
int* compute_histogram(SimpleImage<double> &illumination_enhanced, double max_dn);
double* compute_pdf(int *histogram, int total_pixels, double max_dn);
double* compute_moving_average(double *pdf, int filter_size, double max_dn);

void main44(){
    zvmessage("Shadow version 1.0.1\n", "");
   
    char msg[256]; 
    char inp_filename[256], out_filename[256];
    int status, count, def;
    int inp_unit, inp_nl, inp_ns, inp_nb, out_unit;

    zvp("OUT", out_filename, &count);
    if(count == 0){
        zvmessage("Parameter OUT is undefined.", "");
        zabend();
    }

    zvp("INP", inp_filename, &count);
    sprintf(msg, "Image: %s\n", inp_filename);
    zvmessage(msg, "");

    zvunit(&inp_unit, "INP", 1, "u_name", inp_filename, NULL);
    zvopen(inp_unit, "op", "read", "u_format", "doub", "open_act", "sa", NULL);
    zvget(inp_unit, "NL", &inp_nl, "NS", &inp_ns, "NB", &inp_nb, NULL);

    SimpleImage<double> inp_img;
    inp_img.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        zvread(inp_unit, inp_img.linePtr(line), "LINE", line + 1, "BAND", inp_nb, NULL);
    }
    zvclose(inp_unit, NULL);

    //compute Gaussian kernel
    //sigma is the gaussian standard deviation 
    double sigma;
    int gau_kernel_size;

    zvparmd("SIGMA", &sigma, &count, &def, 1, 0);
    zvp("KSIZE", &gau_kernel_size, &count);

    SimpleImage<double> gau_kernel = create_gaussian_kernel(gau_kernel_size, sigma);

    sprintf(msg, "Create gaussian %d x %d kernel with stand deviation equals %f.", gau_kernel_size, gau_kernel_size, sigma);
    zvmessage(msg, "");

    double illumination_value, absdiff_value;
    SimpleImage<double> illumination_estimate;
    SimpleImage<double> absdiff_img;
    absdiff_img.alloc(inp_nl, inp_ns);
    illumination_estimate.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            illumination_value = convolution2D(inp_img, gau_kernel, line, samp);
            illumination_estimate.set(line, samp, illumination_value);
            absdiff_value = fabs(inp_img.get(line, samp) - illumination_value);
            absdiff_img.set(line, samp, absdiff_value);
        }
    }

    double weighted_value; 
    SimpleImage<double> illumination_weighted;
    illumination_weighted.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            weighted_value = convolution2D(absdiff_img, gau_kernel, line, samp);
            illumination_weighted.set(line, samp, weighted_value);
        }
    }

    //Normalize the weighted illumination image to [0, 1].
    double illumination_weighted_max = findMax(illumination_weighted);
    double illumination_weighted_min = findMin(illumination_weighted);
    SimpleImage<double> illumination_weighted_normalized;
    illumination_weighted_normalized.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            double normalized_value = normalize(illumination_weighted.get(line, samp), illumination_weighted_min, illumination_weighted_max);
            illumination_weighted_normalized.set(line, samp, normalized_value);
        }
    }

    double modified_value;
    SimpleImage<double> illumination_modified;
    illumination_modified.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            modified_value = illumination_weighted_normalized.get(line, samp) * inp_img.get(line, samp) + (1 - illumination_weighted_normalized.get(line, samp)) * illumination_estimate.get(line, samp);
            illumination_modified.set(line, samp, modified_value);
        }
    }

    zvmessage("Decomposed illumination image from the input image using gaussian kernel.", "");

    //Compute reflectance image
    SimpleImage<double> reflectance_img;
    reflectance_img.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            if(illumination_modified.get(line, samp) != 0.0){
                double reflectance_value = inp_img.get(line, samp) / illumination_modified.get(line, samp);
                reflectance_img.set(line, samp, reflectance_value);      
            } else {
                reflectance_img.set(line, samp, 0.0);
            }
        }
    }

    zvmessage("Decomposed reflectance image from the input image.", "");

    //Output the reflectance image if OUT_REFLECT is defined.
    char out_reflectance[256];
    int out_reflectance_unit;
    
    zvp("OUT_REFLECT", out_reflectance, &count);
    if(count == 1){
        zvunit(&out_reflectance_unit, "OUT_REFLECT", 1, "u_name", out_reflectance, NULL);
        zvopen(out_reflectance_unit, "op", "write", "u_nl", inp_nl, "u_ns", inp_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

        for(int line = 0; line < inp_nl; line++){
            zvwrit(out_reflectance_unit, reflectance_img.linePtr(line), "band", 1, "line", line + 1, NULL);
        }

        zvclose(out_reflectance_unit, NULL);
    }

    //Gamma correction
    double intensity_sum = 0.0;
    double inp_img_max = findMax(inp_img);
    double inp_img_min = findMin(inp_img);
    double normalized_value;

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            normalized_value = normalize(inp_img.get(line, samp), inp_img_min, inp_img_max);
            intensity_sum += normalized_value;
        }
    }
  
    double intensity_mean = intensity_sum / (inp_nl * inp_ns);
    double illumination_modified_max = findMax(illumination_modified);
    double illumination_modified_min = findMin(illumination_modified);
    SimpleImage <double> illumination_modified_normalized;
    illumination_modified_normalized.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            normalized_value = normalize(illumination_modified.get(line, samp), illumination_modified_min, illumination_modified_max);
            illumination_modified_normalized.set(line, samp, normalized_value);
        }
    }

    SimpleImage<double> illumination_enhanced_normalized;
    illumination_enhanced_normalized.alloc(inp_nl, inp_ns);
    double global_gamma, adaptive_gamma, alpha, enhanced_value, gamma_value;
    int adaptive_gamma_correction = 1;

    zvmessage("Illumination image gamma brightness correction.", "");

    zvparmd("GAMMA", &global_gamma, &count, &def, 1, 0);
    if(count == 1){
        adaptive_gamma_correction = 0;
        zvmessage("The program enters global gamma brightness correction mode.", "");
    } else {
        zvmessage("The program enters adaptive gamma brightness correction mode.", "");
    }

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            if(adaptive_gamma_correction){
                alpha = 1 + illumination_modified_normalized.get(line, samp);
                gamma_value = (illumination_modified_normalized.get(line, samp) + alpha) / (1 + alpha);
            } else {
                gamma_value = global_gamma;
            }

            enhanced_value = pow(illumination_modified_normalized.get(line, samp), gamma_value);
            illumination_enhanced_normalized.set(line, samp, enhanced_value);
        }
    }

    //Contrast enhancement using probability density function
    SimpleImage<double> illumination_enhanced;
    illumination_enhanced.alloc(inp_nl, inp_ns);
    double normalize_factor = findMax(inp_img);

    zvmessage("Illumination image contrast enhancement.", "");

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            double value = round(illumination_enhanced_normalized.get(line, samp) * normalize_factor);
            illumination_enhanced.set(line, samp, value);
        }
    } 

    //Compute histogram based on enhanced illumination image.
    int *histogram = compute_histogram(illumination_enhanced, normalize_factor);

    //Compute probability density function.
    double *pdf = compute_pdf(histogram, inp_nl * inp_ns, normalize_factor);

    //Compute moving average of the probability density function.
    int filter_size = 2;
    double *pdf_ma = compute_moving_average(pdf, filter_size, normalize_factor);

    //Create a uniform probability density function.
    double *pdf_uniform = new double[(int)normalize_factor + 1];
    for(int i = 0; i < (int)normalize_factor + 1; i++){
        pdf_uniform[i] = 1.0 / normalize_factor;
    }

    //Create modified probability density function.
    double *pdf_mod = new double[(int)normalize_factor + 1];
    for(int i = 0; i < (int)normalize_factor + 1; i++){
        if(pdf_ma[i] < pdf_uniform[i]){
            pdf_mod[i] = pdf_ma[i];
        } else {
            pdf_mod[i] = pdf_uniform[i];
        }
    }

    //Compute final probability density function.
    double *pdf_final = new double[(int)normalize_factor + 1];
    double weighted_factor = 0.0;
    for(int i = 0; i < (int)normalize_factor + 1; i++){
        weighted_factor += (pdf_uniform[i] - pdf_mod[i]);
    }

    for(int i = 0; i < (int)normalize_factor + 1; i++){
        pdf_final[i] = weighted_factor * pdf[i] + (1 - weighted_factor) * pdf_uniform[i];        
    }

    //Compute cumulative probability density function.
    double *cumulative_pdf = new double[(int)normalize_factor + 1];
    for(int i = 0; i < (int)normalize_factor + 1; i++){
        double cumulative_value = 0.0;   
 
        for(int j = 0; j <= i; j++){
            cumulative_value += pdf_final[j];
        }
  
        cumulative_pdf[i] = floor(normalize_factor * cumulative_value + 0.5);
    }

    //Compute final illumination image.
    SimpleImage<double> illumination_final;
    illumination_final.alloc(inp_nl, inp_ns);
    int original_value;
    double mapped_value;

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            original_value = (int)illumination_enhanced.get(line, samp);
            mapped_value = cumulative_pdf[original_value];
            illumination_final.set(line, samp, mapped_value);
        }
    }

    //Output final illumination image if OUT_ILLUM is defined.
    char out_illufinal[256];
    int out_illufinal_unit;

    zvp("OUT_ILLUM", out_illufinal, &count);
    if(count == 1){
        zvunit(&out_illufinal_unit, "OUT_ILLUM", 1, "u_name", out_illufinal, NULL);
        zvopen(out_illufinal_unit, "op", "write", "u_nl", inp_nl, "u_ns", inp_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

        for(int line = 0; line < inp_nl; line++){
            zvwrit(out_illufinal_unit, illumination_final.linePtr(line), "band", 1, "line", line + 1, NULL);
        }

        zvclose(out_illufinal_unit, NULL);
    }

    //Compute the output image based on retinex theory.
    SimpleImage<short int> out_img;
    out_img.alloc(inp_nl, inp_ns);

    zvmessage("Combine illumination image and reflectance image.", "");

    for(int line = 0; line < inp_nl; line++){
        for(int samp = 0; samp < inp_ns; samp++){
            int out_value = (int)(reflectance_img.get(line, samp) * illumination_final.get(line, samp));
            out_img.set(line, samp, out_value);
        }
    }

    //Write out image
    zvp("OUT", out_filename, &count);
    zvunit(&out_unit, "OUT", 1, "u_name", out_filename, NULL);
    zvopen(out_unit, "op", "write", "u_nl", inp_nl, "u_ns", inp_ns, "u_nb", 1, "open_act", "sa", "u_format", "half", "u_org", "bsq", "o_format", "half", NULL);

    for(int line = 0; line < inp_nl; line++){
        zvwrit(out_unit, out_img.linePtr(line), "band", 1, "line", line + 1, NULL);
    }

    zvclose(out_unit, NULL);

    zvmessage("Write output.", "");
}

SimpleImage<double> create_gaussian_kernel(int gau_kernel_size, double sigma){
    SimpleImage<double> gau_kernel;
    double sum = 0.0;
    double kernel_value = 0.0;
    double m, n = 2.0 * sigma * sigma;
    int half_kernel_size = (int)floor(gau_kernel_size / 2);
    gau_kernel.alloc(gau_kernel_size, gau_kernel_size);
    
    //generate N x N gaussian kernel
    for(int line = -half_kernel_size; line <= half_kernel_size; line++){
        for(int samp = -half_kernel_size; samp <= half_kernel_size; samp++){
            m = sqrt(line * line + samp * samp);
            kernel_value = exp((-(m * m) / n)) / (PI * n); 
            gau_kernel.set(line + half_kernel_size, samp + half_kernel_size, kernel_value);
            sum += kernel_value;
        }
    }

    //normalize the kernel so that it adds up to 1
    for(int line = 0; line < gau_kernel_size; line++){
        for(int samp = 0; samp < gau_kernel_size; samp++){
            gau_kernel.set(line, samp, gau_kernel.get(line, samp) / sum);
        }
    }

    return gau_kernel;
}

double convolution2D(SimpleImage<double> &source_img, SimpleImage<double> &gau_kernel, int target_line, int target_samp){
    double result = 0.0;
    int gau_kernel_size = gau_kernel.getNS(); //The width and height of the gaussian kernel window is the same.

    for(int m = 0; m < gau_kernel_size; m++){
        int mm = gau_kernel_size - 1 - m;
        for(int n = 0; n < gau_kernel_size; n++){
            int nn = gau_kernel_size - 1 - n;
            int ii = target_line + (m - (int)floor(gau_kernel_size / 2));
            int jj = target_samp + (n - (int)floor(gau_kernel_size / 2));

            if(ii >= 0 && ii < source_img.getNL() && jj >= 0 && jj < source_img.getNS()){
                result += source_img.get(ii, jj) * gau_kernel.get(mm, nn);
            }
        }
    }

    return result;
}

double findMax(SimpleImage<double> &source_img){
    double max = -DBL_MAX;

    for(int line = 0; line < source_img.getNL(); line++){
        for(int samp = 0; samp < source_img.getNS(); samp++){
            if(source_img.get(line, samp) >= max){
                max = source_img.get(line, samp);
            }
        }
    }

    return max;
}

double findMin(SimpleImage<double> &source_img){
    double min = DBL_MAX;

    for(int line = 0; line < source_img.getNL(); line++){
        for(int samp = 0; samp < source_img.getNS(); samp++){
            if(source_img.get(line, samp) <= min){
                min = source_img.get(line, samp);
            }
        }
    }

    return min;
}

//normalize the source_value into range of [0, 1]
double normalize(double source_value, double source_min, double source_max){
    return (source_value - source_min) / (source_max - source_min);
}

//Compute histogram
int* compute_histogram(SimpleImage<double> &illumination_enhanced, double max_dn){
    int* histogram = new int[(int)max_dn + 1];

    for(int i = 0; i < (int)max_dn + 1; i++){
        histogram[i] = 0;
    }

    for(int line = 0; line < illumination_enhanced.getNL(); line++){
        for(int samp = 0; samp < illumination_enhanced.getNS(); samp++){
            int img_value = (int)illumination_enhanced.get(line, samp);
            histogram[img_value] += 1;
        }
    }

    return histogram;
}

//Compute probability density function
double* compute_pdf(int *histogram, int total_pixels, double max_dn){
    double *pdf = new double[(int)max_dn + 1];

    for(int i = 0; i < (int)max_dn + 1; i++){
        pdf[i] = (double)histogram[i] / total_pixels;
    }

    return pdf;
}

//Compute moving average of probability density function
double* compute_moving_average(double *pdf, int filter_size, double max_dn){
    double* pdf_ma = new double[int(max_dn) + 1];
    int counter;
    double sum;

    for(int i = 0; i < (int)max_dn + 1; i++){
        counter = i + filter_size > max_dn ? (int)max_dn + 1 : i + filter_size;
        sum = 0;
   
        for(int j = i; j < counter; j++){
            sum += pdf[j];
        }

        pdf_ma[i] = sum / (counter - i);
    }

    return pdf_ma;
}
