#include "vicmain_c"
#include "SimpleImage.h"

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <math.h>

#include <cstdlib>
#include <iomanip>
#include <cmath>
#include <ctime>
#include <cstring>
#include <float.h>

using namespace std;

#define PI 3.14159265;

typedef enum {MIN, MEDIAN, MAX} FilterType;

SimpleImage<double> trim_image(SimpleImage<double> inp_img, int top, int bot, int left, int right);
double conv2D(SimpleImage<double> &image, int kernel[3][3], int line, int samp);
double magnitude(double gradient_x, double gradient_y);
double direction(double gradient_x, double gradient_y);
SimpleImage<double> filter(SimpleImage<double> &image, int filter_width, int filter_height, FilterType filter_type);
double determinant(double **matrix, int n);
void eigen(int n, double matrix[], int it_max, double eigen_vectors[], double eigen_values[], int &it_num, int &rot_num);
void identity_matrix(int n, double matrix[]);
void diagonal_vector(int n, double matrix[], double vector[]);
double** compute_sigma(int n, double diff);
double** compute_sigma_sum(int n, double **sigma, double **sigma_sum);
double** compute_sigma_avg(int n, double **sigma_sum, int nonzero_count);
bool border_cleanup(int border[], int start_index, int end_index, double avg_thresh1, double avg_thresh2, double absdiff_thresh1, double absdiff_thresh2);

void main44(){
    zvmessage("HORIZON version 1.0.3\n", "");

    char inp_filename[256], out_filename[256], out_gradient_magnitude[256], out_gradient_direction[256];
    char msg[256];
    int status, count, def;
    int inp_unit, inp_band, inp_nl, inp_ns, inp_nb;
    int out_gradient_magnitude_unit, out_gradient_direction_unit, out_gradient_nl, out_gradient_ns, out_gradient_nb;
    
    zvp("OUT", out_filename, &count);
    if(count == 0){
        zvmessage(msg, "Parameter OUT is undefined.");
        zabend();
    }

    zvp("inp", inp_filename, &count);
    sprintf(msg, "Detecting horizon for image: %s\n", inp_filename);
    zvmessage(msg, "");

    zvunit(&inp_unit, "inp", 1, "u_name", inp_filename, NULL); 
    zvopen(inp_unit, "op", "read", "u_format", "doub", "open_act", "sa", NULL);
    zvget(inp_unit, "NL", &inp_nl, "NS", &inp_ns, "NB", &inp_nb, NULL);

    SimpleImage<double> inp_img;
    inp_img.alloc(inp_nl, inp_ns);

    for(int line = 0; line < inp_nl; line++){
        zvread(inp_unit, inp_img.linePtr(line), "LINE", line + 1, "BAND", inp_nb, NULL);
    }

    zvclose(inp_unit, NULL);

    //trim the input image to remove the borders
    int trim_top, trim_bot, trim_left, trim_right;
    zvp("TRIMTOP", &trim_top, &count);
    zvp("TRIMBOT", &trim_bot, &count);
    zvp("TRIMLEFT", &trim_left, &count);
    zvp("TRIMRIGHT", &trim_right, &count);

    if(trim_top < 0 || trim_bot < 0 || trim_left < 0 || trim_right < 0){
        zvmessage("trim factors cannot be smaller than 0.", "");
        zabend();
    }

    if(trim_top + trim_bot > inp_nl || trim_left + trim_right > inp_ns){
        zvmessage("trim factors exceed the maximum size of the image.", "");
        zabend();
    }

    SimpleImage<double> trimed_img = trim_image(inp_img, trim_top, trim_bot, trim_left, trim_right); 
    int trimed_nl = trimed_img.getNL();
    int trimed_ns = trimed_img.getNS();
    char out_trimed[256];
    int out_trimed_unit;

    zvp("TRIM_IMG", out_trimed, &count);
    if(count == 1){
        zvunit(&out_trimed_unit, "TRIM_IMG", 1, "u_name", out_trimed, NULL);
        zvopen(out_trimed_unit, "op", "write", "u_nl", trimed_nl, "u_ns", trimed_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

        for(int line = 0; line < trimed_nl; line++){
            zvwrit(out_trimed_unit, trimed_img.linePtr(line), "BAND", 1, "LINE", line + 1, NULL);
        }

        zvclose(out_trimed_unit, NULL);
    }

    //filter the inp_img using filter to remove salt-and-pepper noise
    int filter_size; 
    zvp("FILTER_SIZE", &filter_size, &count);
    if(filter_size < 3){
        zvmessage("Filter size cannot be smaller than 3. The smallest size of the filter kernel is 3 x 3.", "");
        zabend();
    }

    FilterType filter_type;
    if(zvptst("MIN")){
        filter_type = MIN;
    } else if(zvptst("MAX")){
        filter_type = MAX;
    } else {
        filter_type = MEDIAN;
    }

    SimpleImage<double> smooth_img_uncut = filter(trimed_img, filter_size, filter_size, filter_type);
    int smooth_trim_offset = (int)floor(filter_size / 2);
    SimpleImage<double> smooth_img = trim_image(smooth_img_uncut, smooth_trim_offset, smooth_trim_offset, smooth_trim_offset, smooth_trim_offset);
   
    int smooth_nl = smooth_img.getNL();
    int smooth_ns = smooth_img.getNS();
    char out_smooth[256];
    int out_smooth_unit;
    
    //if SMOOTH_IMG is defined, then output the smoothed image.
    zvp("smooth_img", out_smooth, &count);
    if(count == 1){
        zvunit(&out_smooth_unit, "smooth_img", 1, "u_name", out_smooth, NULL);
        zvopen(out_smooth_unit, "op", "write", "u_nl", smooth_nl, "u_ns", smooth_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

        for(int line = 0; line < smooth_nl; line++){
            zvwrit(out_smooth_unit, smooth_img.linePtr(line), "BAND", 1, "LINE", line + 1, NULL);
        }

        zvclose(out_smooth_unit, NULL);
    }

    //calculate gradient magnitude for the input image
    //define kernels for sobel operator
    int kernel_x[3][3] = {{-1, 0, 1}, {-2, 0, 2}, {-1, 0, 1}};
    int kernel_y[3][3] = {{-1, -2, -1}, {0, 0, 0}, {1, 2, 1}};

    SimpleImage<double> gradient_magnitude_uncut;
    SimpleImage<double> gradient_direction_uncut;
    int cut_offset = 1;
    double gradient_x, gradient_y;

    gradient_magnitude_uncut.alloc(smooth_nl, smooth_ns);
    gradient_direction_uncut.alloc(smooth_nl, smooth_ns);

    for(int line = cut_offset; line < smooth_nl - cut_offset; line++){
        for(int samp = cut_offset; samp < smooth_ns - cut_offset; samp++){
            gradient_x = conv2D(smooth_img, kernel_x, line, samp); 
            gradient_y = conv2D(smooth_img, kernel_y, line, samp);          
            gradient_magnitude_uncut.set(line, samp, magnitude(gradient_x, gradient_y));
            gradient_direction_uncut.set(line, samp, direction(gradient_x, gradient_y));
        }
    }

    //cut the borders in gradient image caused by 2D convolution
    SimpleImage<double> gradient_magnitude = trim_image(gradient_magnitude_uncut, cut_offset, cut_offset, cut_offset, cut_offset);
    SimpleImage<double> gradient_direction = trim_image(gradient_direction_uncut, cut_offset, cut_offset, cut_offset, cut_offset);

    //if GM_IMG is defined, then output gradient_magnitude
    zvp("GM_IMG", out_gradient_magnitude, &count);
    int gradient_nl = gradient_magnitude.getNL();
    int gradient_ns = gradient_magnitude.getNS();
 
    if(count == 1){
        zvunit(&out_gradient_magnitude_unit, "gm", 1, "u_name", out_gradient_magnitude, NULL);
        zvopen(out_gradient_magnitude_unit, "op", "write", "u_nl", gradient_nl, "u_ns", gradient_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);
        
        for(int line = 0; line < gradient_nl; line++){
            zvwrit(out_gradient_magnitude_unit, gradient_magnitude.linePtr(line), "BAND", 1, "LINE", line + 1, NULL);
        }

        zvclose(out_gradient_magnitude_unit, NULL);
    }

    gradient_magnitude_uncut.free();

    //if GD is defined, then output gradient_direction
    zvp("GD_IMG", out_gradient_direction, &count);

    if(count == 1){
        zvunit(&out_gradient_direction_unit, "gd", 1, "u_name", out_gradient_direction, NULL);
        zvopen(out_gradient_direction_unit, "op", "write", "u_nl", gradient_nl, "u_ns", gradient_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

        for(int line = 0; line < gradient_nl; line++){
            zvwrit(out_gradient_direction_unit, gradient_direction.linePtr(line), "BAND", 1, "LINE", line + 1, NULL);
        }

        zvclose(out_gradient_direction_unit, NULL);
    }

    gradient_direction_uncut.free();

    //calculate border
    double Jn_max = -DBL_MAX; 
    int strong_horizon_thresh_min, strong_horizon_thresh_max, strong_horizon_search_increment_factor;
    int border_tmp[gradient_ns];
    int border_opt[gradient_ns];
    double sky_gradient_magnitude_avg_opt = 0.0, ground_gradient_magnitude_avg_opt = 0.0;

    zvp("SH_THRESH_MIN", &strong_horizon_thresh_min, &count);
    zvp("SH_THRESH_MAX", &strong_horizon_thresh_max, &count);
    zvp("SH_SIF", &strong_horizon_search_increment_factor, &count);

    int n = (int)((strong_horizon_thresh_max - strong_horizon_thresh_min) / strong_horizon_search_increment_factor) + 1;

    SimpleImage<double> sky_gradient_magnitude;
    SimpleImage<double> ground_gradient_magnitude;

    int iter_opt, thresh_opt; //for printing purpose only.

    zvmessage("*** Detecting Strong Horizon ***", "");
    sprintf(msg, "Strong horizon thresh min: %d\nStrong horizon thresh max: %d\nStrong horizon search increment factor: %d\n", strong_horizon_thresh_min, strong_horizon_thresh_max, strong_horizon_search_increment_factor);
    zvmessage(msg, "");

    sprintf(msg, "%-15s%-20s%-20s%-20s%-15s", "Iteration", "Current Thresh", "Sky Business", "Ground Business", "Difference");
    zvmessage(msg, "");

    for(int k = 1; k <= n; k++){
        int t = (int)(strong_horizon_thresh_min + ((strong_horizon_thresh_max - strong_horizon_thresh_min) / (n - 1)) * (k - 1));
        
        for(int samp = 0; samp < gradient_ns; samp++){
            border_tmp[samp] = gradient_nl - 1;

            for(int line = 0; line < gradient_nl; line++){
                if(gradient_magnitude.get(line, samp) > t){
                    border_tmp[samp] = line;
                    break;
                }
            }
        }

        double sky_gradient_magnitude_total = 0.0, ground_gradient_magnitude_total = 0.0;
        double gradient_magnitude_value, image_value;
        int sky_nonzero_counter = 0, ground_nonzero_counter = 0;

        for(int samp = 0; samp < gradient_ns; samp++){
            for(int line = 0; line < gradient_nl; line++){
                gradient_magnitude_value = gradient_magnitude.get(line, samp);

                if(line <= border_tmp[samp]){
                    //sky region
                    sky_gradient_magnitude_total += gradient_magnitude_value;
                    sky_nonzero_counter++;
                } else {
                    //ground region
                    ground_gradient_magnitude_total += gradient_magnitude_value;
                    ground_nonzero_counter++;
                } 
            }
        }

        //Calculate the sky and ground average busyness level from gradient magnitude image.
        double sky_gradient_magnitude_avg = sky_gradient_magnitude_total / sky_nonzero_counter;
        double ground_gradient_magnitude_avg = ground_gradient_magnitude_total / ground_nonzero_counter;

        //calculate energe function Jn based on sky and ground average values from gradient magnitude image
        double Jn = ground_gradient_magnitude_avg - sky_gradient_magnitude_avg;

        sprintf(msg, "%-15d%-20d%-20.5f%-20.5f%-15.5f", k, t, sky_gradient_magnitude_avg, ground_gradient_magnitude_avg, Jn);
        zvmessage(msg, "");

        if(Jn > Jn_max){
            Jn_max = Jn;
            sky_gradient_magnitude_avg_opt = sky_gradient_magnitude_avg;
            ground_gradient_magnitude_avg_opt = ground_gradient_magnitude_avg;

            iter_opt = k;
            thresh_opt = t;            

            for(int samp = 0; samp < gradient_ns; samp++){
                border_opt[samp] = border_tmp[samp];
            }
        }
    }

    sprintf(msg, "Optimal strong horizon is detected with threshold value %d at iteration %d\n", thresh_opt, iter_opt);
    zvmessage(msg, "");

    //Preliminary horizon borders will be marked with DN value -10000
    for(int samp = 0; samp < gradient_ns; samp++){
        inp_img.set(border_opt[samp] + trim_top + smooth_trim_offset + cut_offset, samp + trim_left + smooth_trim_offset + cut_offset, -10000.0);
    }

    //identify outliers and the column with possible weak horizon
    int outlier_thresh, outlier_search_interval;
    int weak_horizon_magnitude_thresh, weak_horizon_direction_thresh, weak_horizon_search_interval;
    int iteration_max, master_iteration_max = 30, iteration_counter = 0;
    int weak_horizon_thresh_min;
    int weak_horizon_thresh_max;
    int weak_horizon_search_increment_factor;
    bool is_weak_horizon[gradient_ns];
    bool is_outlier[gradient_ns];
    
    zvp("OUTLIER_THRESH", &outlier_thresh, &count);
    zvp("OUTLIER_SIF", &outlier_search_interval, &count);
    zvp("WH_MAG_THRESH", &weak_horizon_magnitude_thresh, &count);
    zvp("WH_DIR_THRESH", &weak_horizon_direction_thresh, &count);
    zvp("WH_DIR_SIF", &weak_horizon_search_interval, &count);
    zvp("WH_ITER_MAX", &iteration_max, &count);
    zvp("WH_THRESH_MIN", &weak_horizon_thresh_min, &count);
    zvp("WH_THRESH_MAX", &weak_horizon_thresh_max, &count);
    zvp("WH_SIF", &weak_horizon_search_increment_factor, &count);

    zvmessage("*** Refining Weak Horizon and Outliers ***", "");
    
    sprintf(msg, "Outlier definition: %d pixels.", outlier_thresh);
    zvmessage(msg, "");

    sprintf(msg, "Outlier range: %d columns.", outlier_search_interval);
    zvmessage(msg, "");

    sprintf(msg, "Weak horizon detection gradient magnitude threshold: %d pixels.", weak_horizon_magnitude_thresh);
    zvmessage(msg, "");

    sprintf(msg, "Weak horizon detection gradient direction threshold: %d degrees.", weak_horizon_direction_thresh);
    zvmessage(msg, "");

    sprintf(msg, "Weak horizon detection gradient direction search interval: %d pixels.", weak_horizon_search_interval);
    zvmessage(msg, "");

    sprintf(msg, "Weak horizon thresh min: %d", weak_horizon_thresh_min);
    zvmessage(msg, "");

    sprintf(msg, "Weak horizon thresh max: %d", weak_horizon_thresh_max);
    zvmessage(msg, "");

    sprintf(msg, "Weak horizon search increment factor: %d\n", weak_horizon_search_increment_factor);
    zvmessage(msg, "");

    //validate input iteration_max
    if(iteration_max > master_iteration_max){
        sprintf(msg, "Parameter WH_ITER_MAX %d exceeds maximum iteration allowed. Iteration_max resets to 30.\n", iteration_max);
        zvmessage(msg, "");
        iteration_max = master_iteration_max;
    }

    if(iteration_max == 0){
        zvmessage("Weak horizon detection and outlier refinement feature is turned off.\n", "");
    }

    while(iteration_counter < iteration_max){
        //init is_weak_horizon and is_outlier flag arrays to false
        for(int samp = 0; samp < gradient_ns; samp++){
            is_weak_horizon[samp] = false;
            is_outlier[samp] = false;
        }

        for(int samp = 0; samp < gradient_ns; samp++){
            int border = border_opt[samp];
            int left_index = 0, right_index = 0;
            if(samp < outlier_search_interval){
                left_index = 0;
                right_index = samp + outlier_search_interval;
            } else if(samp > gradient_ns - outlier_search_interval){
                left_index = samp - outlier_search_interval;
                right_index = gradient_ns - 1;
            } else {
                left_index = samp - outlier_search_interval;
                right_index = samp + outlier_search_interval;
            }

            //check if the current point is an outlier
            double border_local_total = 0;
            double border_local_avg = 0;
            double border_local_counter = 0;

            for(int i = left_index; i < right_index; i++){
                if(border_opt[i] < 20){
                    continue;
                }
 
                border_local_total += border_opt[i];
                border_local_counter += 1;
            }
            border_local_avg = border_local_total / border_local_counter;
        
            if(abs(border - border_local_avg) > outlier_thresh){
                border_opt[samp] = border_local_avg;
                is_outlier[samp] = true;
            }

            //if the current point is not an outlier, then check if the area above the current point contains weak horizon
            if(!is_outlier[samp]){
                double weak_gradient_total = 0;
                double weak_gradient_avg = 0;
                double weak_gradient_counter = 0;
            
                for(int line = border_opt[samp] - 1; line > 20; line--){ //if a point is within 20 line, it is defined as outlier.
                    if(weak_gradient_counter < 5){  //calculate the average value of 10 points above the border
                        weak_gradient_total += gradient_magnitude.get(line, samp);
                        weak_gradient_counter += 1;
                        weak_gradient_avg = weak_gradient_total / weak_gradient_counter;
                    } else {
                        if(gradient_magnitude.get(line, samp) - weak_gradient_avg > weak_horizon_magnitude_thresh){
                            int top_index = 0, bot_index = 0;
                            if(line < weak_horizon_search_interval){
                                top_index = 0;
                                bot_index = line + weak_horizon_search_interval;
                            } else if(line > gradient_nl - weak_horizon_search_interval) {
                                top_index = line - weak_horizon_search_interval;
                                bot_index = gradient_nl - 1;
                            } else {
                                top_index = line - weak_horizon_search_interval;
                                bot_index = line + weak_horizon_search_interval;
                            }

                            double source_value = gradient_direction.get(line, samp);
                            int similarity_counter = 0;
                            for(int sub_samp = left_index; sub_samp <= right_index; sub_samp++){
                                for(int sub_line = top_index; sub_line <= bot_index; sub_line++){
                                    double target_value = gradient_direction.get(sub_line, sub_samp);
                                    if(source_value >= target_value - weak_horizon_direction_thresh && source_value <= target_value + weak_horizon_direction_thresh){
                                        similarity_counter += 1;
                                    }
                                }
                            }

                            int sub_size = (bot_index - top_index) * (right_index - left_index);
                            if(similarity_counter > (int)(sub_size / 3)){
                                //there is a weak horizon in this samp.
                                is_weak_horizon[samp] = true;
                                break;
                            }
                        } else {
                            weak_gradient_total += gradient_magnitude.get(line, samp);
                            weak_gradient_counter += 1;
                            weak_gradient_avg = weak_gradient_total / weak_gradient_counter;
                        }
                    }
                }
            }
        }

        //Check for each column to see if weak horizon or outlier exists.
        //if yes, then continue running the algorithm to refine the horizon.
        //if no, break out the algorithm, and go writing the result.
        bool iteration_flag = false;
        for(int samp = 0; samp < gradient_ns; samp++){
            if(is_weak_horizon[samp] || is_outlier[samp]){
                iteration_flag = true;
                break;
            }
        }

        if(iteration_flag == false){
            break;
        }

        int weak_n = (int)((weak_horizon_thresh_max - weak_horizon_thresh_min) / weak_horizon_search_increment_factor) + 1;
        int weak_border_tmp[gradient_ns];
        int weak_border_opt[gradient_ns];
        double weak_Jn_max = 0;

        sprintf(msg, "%-15s%-20s%-20s%-20s%-15s", "Iteration", "Current Thresh", "Sky Business", "Ground Business", "Difference");
        zvmessage(msg, "");

        for(int samp = 0; samp < gradient_ns; samp++){
            weak_border_tmp[samp] = border_opt[samp];
            weak_border_opt[samp] = 0;
        }   

        for(int weak_k = 1; weak_k <= weak_n; weak_k++){
            int weak_t = (int)(weak_horizon_thresh_min + ((weak_horizon_thresh_max - weak_horizon_thresh_min) / (weak_n - 1)) * (weak_k - 1));

            for(int samp = 0; samp < gradient_ns; samp++){
                if(is_weak_horizon[samp] || is_outlier[samp]){
                    for(int line = 0; line < border_opt[samp]; line++){
                        if(gradient_magnitude.get(line, samp) > weak_t){
                            weak_border_tmp[samp] = line;
                            break;
                        }
                    }
                }
            }

            double weak_sky_total = 0.0, weak_ground_total = 0.0;
            double weak_value;
            int weak_sky_nonzero_counter = 0, weak_ground_nonzero_counter = 0;

            for(int samp = 0; samp < gradient_ns; samp++){
                if(is_weak_horizon[samp] || is_outlier[samp]){
                    for(int line = 0; line < border_opt[samp]; line++){
                        weak_value = gradient_magnitude.get(line, samp);
                        if(line <= weak_border_tmp[samp]){
                            //weak sky region
                            weak_sky_total += weak_value;
                            weak_sky_nonzero_counter++;              
                        } else {
                            //weak ground region
                            weak_ground_total += weak_value;
                            weak_ground_nonzero_counter++;
                        }
                    }
                }
            }

            double weak_sky_avg = 0.0, weak_ground_avg = 0.0, weak_Jn;
            if(weak_sky_nonzero_counter != 0 && weak_ground_nonzero_counter != 0){
                weak_sky_avg = weak_sky_total / weak_sky_nonzero_counter;
                weak_ground_avg = weak_ground_total / weak_ground_nonzero_counter;
                weak_Jn = weak_ground_avg - weak_sky_avg;

                sprintf(msg, "%-15d%-20d%-20.5f%-20.5f%-15.5f", weak_k, weak_t, weak_sky_avg, weak_ground_avg, weak_Jn);
                zvmessage(msg, "");

                if(weak_Jn > weak_Jn_max){
                    weak_Jn_max = weak_Jn;

                    for(int samp = 0; samp < gradient_ns; samp++){
                        weak_border_opt[samp] = weak_border_tmp[samp];
                    }
                }
            }
        }

        //merge border_opt and weak_border_opt
        for(int samp = 0; samp < gradient_ns; samp++){
            if(is_weak_horizon[samp] || is_outlier[samp]){
                border_opt[samp] = weak_border_opt[samp];
            }
        }

        iteration_counter++;
    }
    zvmessage("", "");

    //remove outliers and columns contain no sky region
    zvmessage("*** Removing outliers and image columns without sky region ***", "");

    double border_avg_thresh1; //This means the border avg is ridiculously small.
    double border_avg_thresh2; //This means the border avg is okay. If borders smaller than this value, then whether
                               //the borders are good or bad will be determined by absdiff values.
    double border_avg_absdiff_thresh1; //This means the border absolute difference is okay. If borders absdiff is bigger than
                                           //this value, then whether the borders are good or bad will be determined by avg values.
    double border_avg_absdiff_thresh2; //This means the border absolute difference is ridiculously big.
    int border_partition_factor;

    zvparmd("BAR_THRESH", &border_avg_thresh1, &count, &def, 1, 0);  //BAA_THRESH stands for border average accept threshold value.
    zvparmd("BAA_THRESH", &border_avg_thresh2, &count, &def, 1, 0);  //BAR_THRESH stands for border average reject threshold value.
    zvparmd("BAADA_THRESH", &border_avg_absdiff_thresh1, &count, &def, 1, 0); //BAADA_THRESH stands for border average absolute difference accept threshold value.
    zvparmd("BAADR_THRESH", &border_avg_absdiff_thresh2, &count, &def, 1, 0); //BAADR_THRESH stands for border average absolute difference reject threshold value.
    zvparmd("BP_FACTOR", &border_partition_factor, &count, &def, 1, 0);

    sprintf(msg, "Border average accept threshold: %f", border_avg_thresh2);
    zvmessage(msg, "");

    sprintf(msg, "Border average reject threshold: %f", border_avg_thresh1);
    zvmessage(msg, "");

    sprintf(msg, "Border average absolute difference accept threshold: %f", border_avg_absdiff_thresh1);
    zvmessage(msg, "");

    sprintf(msg, "Border average absolute difference reject threshold: %f", border_avg_absdiff_thresh2);
    zvmessage(msg, "");

    sprintf(msg, "Border partition factor: %d", border_partition_factor);
    zvmessage(msg, "");

    int sub_width = (int)round(gradient_ns / border_partition_factor);
    for(int i = 0; i < border_partition_factor; i++){
        if(i == border_partition_factor - 1){
            border_cleanup(border_opt, i * sub_width, gradient_ns, border_avg_thresh1, border_avg_thresh2, border_avg_absdiff_thresh1, border_avg_absdiff_thresh2);
        } else {
            border_cleanup(border_opt, i * sub_width, (i + 1) * sub_width, border_avg_thresh1, border_avg_thresh2, border_avg_absdiff_thresh1, border_avg_absdiff_thresh2);
        }
    }

    //Output the horizon borders
    ofstream horizon_file(out_filename);
    int border_final[inp_ns];

    if(horizon_file.is_open()){
        int total_top_offset = trim_top + smooth_trim_offset + cut_offset;
        int total_left_offset = trim_left + smooth_trim_offset + cut_offset;
        int total_right_offset = trim_right + smooth_trim_offset + cut_offset;

        for(int samp = 0; samp < inp_ns; samp++){
            if(samp <= total_left_offset || samp >= inp_ns - total_right_offset - 1){
                border_final[samp] = -1;
            } else { 
                if(border_opt[samp - total_left_offset] == -1){
                    border_final[samp] = -1;
                } else {
                    border_final[samp] = border_opt[samp - total_left_offset] + total_top_offset;
                }
            }
        }
 
        for(int samp = 0; samp < inp_ns; samp++){
            if(samp == inp_ns - 1){
                horizon_file << border_final[samp];
            } else {
                horizon_file << border_final[samp] << ",";
            }
        }

        horizon_file.close();
    }

    //if QL_IMG is defined, then output the horizon overlay on top of the original image
    char out_qlimage[256];
    int out_qlimage_unit;
    
    zvp("QL_IMG", out_qlimage, &count);

    if(count == 1){
        for(int samp = 0; samp < gradient_ns; samp++){
            if(border_opt[samp] != -1){
                inp_img.set(border_opt[samp] + trim_top + smooth_trim_offset + cut_offset, samp + trim_left + smooth_trim_offset + cut_offset, 10000.0);
            }
        }

        zvunit(&out_qlimage_unit, "QL_IMG", 1, "u_name", out_qlimage, NULL);
        zvopen(out_qlimage_unit, "op", "write", "u_nl", inp_nl, "u_ns", inp_ns, "u_nb", 1, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

        for(int line = 0; line < inp_nl; line++){
            zvwrit(out_qlimage_unit, inp_img.linePtr(line), "BAND", 1, "LINE", line + 1, NULL);
        }

        zvclose(out_qlimage_unit, NULL);
    } 
}

//trim the input image to remove the borders
SimpleImage<double> trim_image(SimpleImage<double> inp_img, int top, int bot, int left, int right){
    SimpleImage<double> trimed_image;
    int trimed_nl = inp_img.getNL() - top - bot;
    int trimed_ns = inp_img.getNS() - left - right;

    trimed_image.alloc(trimed_nl, trimed_ns);

    for(int line = 0; line < trimed_nl; line++){
        for(int samp = 0; samp < trimed_ns; samp++){
            trimed_image.set(line, samp, inp_img.get(line + top, samp + left));
        }
    }

    return trimed_image;
}

double conv2D(SimpleImage<double> &image, int kernel[3][3], int line, int samp){
    double result = 0;    
    int kLine = 3, kSamp = 3;

    for(int m = 0; m < kLine; m++){
        int mm = kLine - 1 - m;
        for(int n = 0; n < kSamp; n++){
            int nn = kSamp - 1 - n;
            int ii = line + (m - (int)floor(kLine / 2));
            int jj = samp + (n - (int)floor(kSamp / 2));

            if(ii >= 0 && ii < image.getNL() && jj >= 0 && jj < image.getNS()){
                result += image.get(ii, jj) * kernel[mm][nn];
            }
        }
    }

    return result;
}

//calculate the magitude of horizontal and vertical gradients
double magnitude(double gradient_x, double gradient_y){
    return sqrt(gradient_x * gradient_x + gradient_y * gradient_y);
}

//calculate the direction of horizontal and vertical gradients
//results are in degrees
double direction(double gradient_x, double gradient_y){
    if(gradient_x == 0.0){
        return 0.0;
    }

    return atan2(gradient_y, gradient_x) * 180 / PI;
}

//filter the given image using filter size and filter_type defined in PDF
SimpleImage<double> filter(SimpleImage<double> &image, int filter_width, int filter_height, FilterType filter_type){
    SimpleImage<double> result_img;
    double window[filter_width * filter_height];
    int edge_x = (int)floor(filter_width / 2);
    int edge_y = (int)floor(filter_height / 2);
    int nl = image.getNL();
    int ns = image.getNS();
    int counter;
    int ret_index = 0;
    char msg[256];

    result_img.alloc(nl, ns);
    
    if(filter_type == MIN){
        sprintf(msg, "*** Filtering the INP image with %d * %d MIN kernel ***", filter_width, filter_height);
        ret_index = 0;
    } else if (filter_type == MAX){
        sprintf(msg, "*** Filtering the INP image with %d * %d MAX kernel ***", filter_width, filter_height); 
        ret_index = filter_width * filter_height - 1;
    } else {
        sprintf(msg, "*** Filtering the INP image with %d * %d MEDIAN kernel ***", filter_width, filter_height);
        ret_index = (filter_width * filter_height - 1) / 2;
    }
    zvmessage(msg, "");

    for(int line = edge_y; line < nl - edge_y; line++){
        if(line % 100 == 0){
            sprintf(msg, "Line %d", line);
            zvmessage(msg, "");
        }
  
        for(int samp = edge_x; samp < ns - edge_x; samp++){
            counter = 0;
            
            for(int y = 0; y < filter_height; y++){
                for(int x = 0; x < filter_width; x++){
                    window[counter] = image.get(line + y - edge_y, samp + x - edge_x);
                    counter++;
                }
            }

            //bubble sort window array 
            for(int i = 0; i < filter_width * filter_height - 1; i++){
                for(int j = 0; j < filter_width * filter_height - i - 1; ++j){
                    if(window[j] > window[j + 1]){
                        int temp = window[j];
                        window[j] = window[j + 1];
                        window[j + 1] = temp;
                    } 
                }
            }

            result_img.set(line, samp, window[ret_index]);
        }
    }
    zvmessage("", "");

    return result_img;
}

//Compute the determinant of a matrix with n * n dimension.
double determinant(double **matrix, int n){
    double det = 0;
    double **m = NULL;
    
    if(n < 1){
        zvmessage("matrix size cannot be smaller than 1.", "");
        zabend();
    } else if(n == 1){
        det = matrix[0][0];
    } else if(n == 2){
        det = matrix[0][0] * matrix[1][1] - matrix[1][0] * matrix[0][1];
    } else {
        det = 0;
        for(int j1 = 0; j1 < n; j1++){
            m = (double **)malloc((n - 1) * sizeof(double *));

            for(int i = 0; i < n - 1; i++){
                m[i] = (double *)malloc((n - 1) * sizeof(double));
            }

            for(int i = 1; i < n; i++){
                int j2 = 0;

                for(int j = 0; j < n; j++){
                    if(j == j1){
                        continue;
                    }

                    m[i - 1][j2] = matrix[i][j];
                    j2++;
                }
            }

            det += pow(-1.0, 1.0 + j1 + 1.0) * matrix[0][j1] * determinant(m, n - 1);

            for(int i = 0; i < n - 1; i++){
                free(m[i]);
            }

            free(m);
        }
    }

    return det;
}

//Calculate eigenvalues and eigenvectors of a real symmetric matrix, 
//using Rutishauser's modfications of the classical Jacobi rotation 
//method with threshold pivoting
//Parameter: 
//1. n, input, the size of the matrix.
//2. matrix, input, the matrix that we use to compute eigenvalues and eigenvectors.
//3. it_max, input, the maximum number of iterations.
//4. eigen_vectors, output, the matrix of eigenvectors.
//5. eigen_values, output, the eigenvalues in descending order.
//6. it_num, output, the total number of iterations.
//7. rot_num, output, the total number of rotations.
void eigen(int n, double matrix[], int it_max, double eigen_vectors[], double eigen_values[], int &it_num, int &rot_num){
    double c, g, gapq, h, s, t, tau, term, termp, termq, theta, thresh, w;
    double *bw, *zw;
    int m;

    identity_matrix(n, eigen_vectors);
    diagonal_vector(n, matrix, eigen_values);

    bw = new double[n];
    zw = new double[n];
 
    for(int i = 0; i < n; i++){
        bw[i] = eigen_values[i];
        zw[i] = 0.0;
    }

    it_num = 0;
    rot_num = 0;

    while(it_num < it_max){
        it_num += 1;
        thresh = 0.0;
    
        for(int j = 0; j < n; j++){
            for(int i = 0; i < j; i++){
                thresh = thresh + matrix[i + j * n] * matrix[i + j * n];
            }
        }

        thresh = sqrt(thresh) / (double)(4 * n);

        if(thresh == 0.0){
            break;
        }

        for(int p = 0; p < n; p++){
            for(int q = p + 1; q < n; q++){
                gapq = 10.0 * fabs(matrix[p + q * n]);
                termp = gapq + fabs(eigen_values[p]);
                termq = gapq + fabs(eigen_values[q]);

                if(4 < it_num && termp == fabs(eigen_values[p]) && termq == fabs(eigen_values[q])){
                    matrix[p + q * n] = 0.0;
                } else if (thresh <= fabs(matrix[p + q * n])){
                    h = eigen_values[q] - eigen_values[p];
                    term = fabs(h) + gapq;
                
                    if(term == fabs(h)){
                        t = matrix[p + q * n] / h;
                    } else {
                        theta = 0.5 * h / matrix[p + q * n];
                        t = 1.0 / (fabs(theta) + sqrt(1.0 + theta * theta));
            
                        if(theta < 0.0){
                            t = -t;
                        }
                    }

                    c = 1.0 / sqrt(1.0 + t * t);
                    s = t * c;
                    tau = s / (1.0 + c);
                    h = t * matrix[p + q * n];

                    zw[p] = zw[p] - h;
                    zw[q] = zw[q] + h;
                    eigen_values[p] = eigen_values[p] - h;
                    eigen_values[q] = eigen_values[q] + h;
                    matrix[p + q * n] = 0.0;

                    for(int j = 0; j < p; j++){
                        g = matrix[j + p * n];
                        h = matrix[j + q * n];
                        matrix[j + p * n] = g - s * (h + g * tau);
                        matrix[j + q * n] = h + s * (g - h * tau);
                    }

                    for(int j = p + 1; j < q; j++){
                        g = matrix[p + j * n];
                        h = matrix[j + q * n];
                        matrix[p + j * n] = g - s * (h + g * tau);
                        matrix[j + q * n] = h + s * (g - h * tau);
                    }

                    for(int j = q + 1; j < n; j++){
                        g = matrix[p + j * n];
                        h = matrix[q + j * n];
                        matrix[p + j * n] = g - s * (h + g * tau);
                        matrix[q + j * n] = h + s * (g - h * tau);
                    }

                    for(int j = 0; j < n; j++){
                        g = eigen_vectors[j + p * n];
                        h = eigen_vectors[j + q * n];
                        eigen_vectors[j + p * n] = g - s * (h + g * tau);
                        eigen_vectors[j + q * n] = h + s * (g - h * tau);
                    }

                    rot_num += 1;
                }
            }
        }

        for(int i = 0; i < n; i++){
            bw[i] = bw[i] + zw[i];
            eigen_values[i] = bw[i];
            zw[i] = 0.0;
        }
    }

    for(int j = 0; j < n; j++){
        for(int i = 0; i < j; i++){
            matrix[i + j * n] = matrix[j + i * n];
        }
    }

    //Ascending sort the eigen_values and eigen_vectors.
    for(int k = 0; k < n - 1; k++){
        m = k;

        for(int l = k + 1; l < n; l++){
            if(eigen_values[l] < eigen_values[m]){
                m = l;
            }
        }

        if(m != k){
            t = eigen_values[m];
            eigen_values[m] = eigen_values[k];
            eigen_values[k] = t;

            for(int i = 0; i < n; i++){
                w = eigen_vectors[i + m * n];
                eigen_vectors[i + m * n] = eigen_vectors[i + k * n];
                eigen_vectors[i + k * n] = w;
            }
        }
    }

    delete [] bw;
    delete [] zw;
}

//Set the matrix to the identity
//Parameter:
//1. n, input, the size of the matrix.
//2. matrix, output, the n by n identity matrix.
void identity_matrix(int n, double matrix[]){
    int k = 0;

    for(int j = 0; j < n; j++){
        for(int i = 0; i < n; i++){
            if(i == j){
                matrix[k] = 1.0;
            } else {
                matrix[k] = 0.0;
            }
            
            k += 1;
        }
    }
}

//Get the value of the diagonal of the matrix
//Parameter:
//1. n, input, the size of the matrix.
//2. matrix, input, the matrix we use to get the diagonal vertor.
//3. vector, output, the diagonal vector of the matrix.
void diagonal_vector(int n, double matrix[], double vector[]){
    for(int i = 0; i < n; i++){
        vector[i] = matrix[i + i * n];
    }
}

double** compute_sigma(int n, double diff){
    double **sigma;
    sigma = new double *[n];

    for(int i = 0; i < n; i++){
        sigma[i] = new double[n];

        for(int j = 0; j < n; j++){
            sigma[i][j] = diff * diff;
        }
    }   

    return sigma;
}

double** compute_sigma_sum(int n, double **sigma, double **sigma_sum){
    for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++){
            sigma_sum[i][j] = sigma_sum[i][j] + sigma[i][j];
        }
    }

    return sigma_sum;
}

double** compute_sigma_avg(int n, double **sigma_sum, int nonzero_count){
    double **sigma_avg;
    sigma_avg = new double *[n];

    for(int i = 0; i < n; i++){
        sigma_avg[i] = new double[n];

        for(int j = 0; j < n; j++){
            sigma_avg[i][j] = sigma_sum[i][j] / nonzero_count;
        }
    }

    return sigma_avg;
}

bool border_cleanup(int border[], int start_index, int end_index, double avg_thresh1, double avg_thresh2, double absdiff_thresh1, double absdiff_thresh2){
    char msg[256];

    //master switch to prevent possible infinite loop
    if(start_index >= end_index){
        zvmessage("Horizon border clean up process malfunction. Clean up process is skipped.", "");
        return false;
    }

    //compute average value of the border within [start_index, end_index)
    double border_total = 0.0, border_avg = 0.0;
    
    for(int samp = start_index; samp < end_index; samp++){
        border_total += border[samp];
    }
    border_avg = border_total / (end_index - start_index);

    //compute average value of absolute difference of the border within [start_index, end_index)
    double border_total_absdiff = 0.0, border_avg_absdiff = 0.0;
    
    for(int samp = start_index + 1; samp < end_index; samp++){
        border_total_absdiff = abs(border[samp] - border[samp - 1]) + border_total_absdiff;
    }
    border_avg_absdiff = border_total_absdiff / (end_index - start_index - 1);

    if(border_avg <= avg_thresh1 || 
       (border_avg <= avg_thresh2 && border_avg_absdiff >= absdiff_thresh1) ||
       border_avg_absdiff >= absdiff_thresh2){
        //borders within [start_index, end_index) contains no sky.
        for(int samp = start_index; samp < end_index; samp++){
            border[samp] = -1;
        }
        sprintf(msg, "-Horizon borders from index %d to %d are classified as:\nOutliers\nBorder average: %f\nBorder absolute difference average: %f", start_index, end_index, border_avg, border_avg_absdiff);        
        zvmessage(msg, "");      

        return true;
    } else if (border_avg > avg_thresh2 && border_avg_absdiff < absdiff_thresh1) {
        //borders within [start_index, end_index) are all good.
        sprintf(msg, "-Horizon borders from index %d to %d are classified as:\nTrue horizon\nBorder average: %f\nBorder absolute difference average: %f", start_index, end_index, border_avg, border_avg_absdiff);
        zvmessage(msg, "");

        return true;
    } else {
        //equally divide border into two, and recursively clean them up.
        if(end_index == start_index + 2){
            for(int samp = start_index; samp < end_index; samp++){
                border[samp] = -1;
            }
 
            sprintf(msg, "-Horizon borders from index %d to %d are classified as:\nOutliers\nBorder average: %f\nBorder absolute difference average: %f", start_index, end_index, border_avg, border_avg_absdiff);
            zvmessage(msg, ""); 

            return true;
        } else {
            int mid_index = floor((end_index - start_index) / 2) + start_index;

            return border_cleanup(border, start_index, mid_index, avg_thresh1, avg_thresh2, absdiff_thresh1, absdiff_thresh2) &&
                   border_cleanup(border, mid_index, end_index, avg_thresh1, avg_thresh2, absdiff_thresh1, absdiff_thresh2);
        }
    }
}   
