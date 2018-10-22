#include "vicmain_c"
#include "SimpleImage.h"
#include "SBTV_L1G.h"
#include "Noise2D.h"
#include "BC_2D.h"
#include "common_routines.h"

#include <cmath>
#include <cfloat>
#include <math.h>

using namespace std;

#define PI 3.14159265

double realtime(void);

void main44(){
    zvmessage("DenoiseTV version 1.5\n", "");
   
    char msg[256]; 
    char inp_filename[256], out_filename[256];
    int status, count, def;
    int inp_unit, nl, ns, nb, out_unit;
    int band;
    int band_count;


    zvp("OUT", out_filename, &count);
    if(count == 0){
        zvmessage("Parameter OUT is undefined.", "");
        zabend();
    }

    zvunit(&inp_unit, "INP", 1, NULL);
    zvopen(inp_unit, "op", "read", "u_format", "doub", "open_act", "sa", NULL);
    zvget(inp_unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);

    // *********************************************************
    // MULTI-BAND CODE:  
    // *********************************************************
    // get parameter overrides if any
    band = 1;         // Band # to process
    zvp("BAND", &band, &count);    // band is band # to process
    band_count = 1;   // The number of bands to process (is either 1 or all (below) )

    if (count == 0) {
        // no input band specified; process all bands
        band_count = nb;   // The number of bands to process (is either 1 (above) or all )
        band = 1;          // First band # to process
        sprintf(msg, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
    } else {
        // check if input band number is greater than number of bands in input
        if (band > nb) {
            sprintf(msg, "Input band (%d) is greater than number of bands in input image. Band set to 1.",
                    band);
            zvmessage(msg, "");
            band = 1;   // Band # to process
        }
    }
    // *********************************************************

    SimpleImage<double> inp_img;
    inp_img.alloc(nl, ns);

    //cout << "band_count = " << band_count << endl;

    float mu, lambda, gamma;
    int iter;

    zvp("MU", &mu, &count);
    zvp("LAMBDA", &lambda, &count);
    zvp("GAMMA", &gamma, &count);
    zvp("ITER", &iter, &count);

    Grid2D grid;
    grid.m = nl;	grid.n = ns;		grid.w = 1;
    grid.dx = 1.0;	grid.dy = 1.0;
    grid.xMin = 0.0;	grid.xMax = (nl-1)*grid.dx;
    grid.yMin = 0.0;	grid.yMax = (ns-1)*grid.dy;


    zvunit(&out_unit, "OUT", 1, NULL);
    //zvopen(out_unit, "op", "write", "u_nl", nl, "u_ns", ns, "u_nb", 1, "open_act", "sa", "u_format", "half", "u_org", "bsq", "o_format", "half", NULL);
    zvopen(out_unit, "op", "write", "u_nl", nl, "u_ns", ns, "u_nb", band_count, "open_act", "sa", "u_format", "doub", "u_org", "bsq", "o_format", "doub", NULL);

    double timeStart, timeTaken;

    timeStart = realtime();

    for( int b = 0; b < band_count; b++ )
    {

        cout << "b = " << b << ", band = " << band << endl << endl;

        for(int line = 0; line < nl; line++){
            zvread(inp_unit, inp_img.linePtr(line), "LINE", line + 1, "BAND", b+band, NULL);
        }

        DoubleArray2D img(nl,ns);
        for(int line = 0; line < nl; line++){
            for(int samp = 0; samp < ns; samp++){
                img(line, samp) = inp_img.get(line,samp);
            }
        }

        findMaxMin( img );
 
        DoubleArray2D f = img;				// noisy image
    
        //SimpleImage<short int> f_img;
        SimpleImage<double> f_img;
        f_img.alloc(nl, ns);

        for(int line = 0; line < nl; line++){
            for(int samp = 0; samp < ns; samp++){
                //int f_value = f(line, samp);
                double f_value = f(line, samp);
                f_img.set(line, samp, f_value);
            }
        }

        DoubleArray2D u = f;				// denoised image

        SBTV_L1G SB;

        SB.mu = mu;
        SB.lambda = lambda;
        SB.gamma = gamma;
        SB.nIter = iter;

        SB.epsilon = 1e-8;

        //
        //*******************************************************
        //  Output Parameters and Initial Conditions
        //*******************************************************
        //

        sprintf(msg, "\nM = %d, N = %d", grid.m, grid.n);
        zvmessage(msg, "");
        sprintf(msg, "dx = %f, dy = %f", grid.dx, grid.dy);
        zvmessage(msg, "");
        sprintf(msg, "Number of iterations: %d\n", SB.nIter);
        zvmessage(msg, "");
        sprintf(msg, "mu = %f, lambda = %f, gamma = %f\n", SB.mu, SB.lambda, SB.gamma);
        zvmessage(msg, "");

        SB.outputParameters( grid );
    
        SB.calculateEnergy( u, f, grid );
        SB.rmse.push_back( calculateRMSE( img, u) );
        SB.snr.push_back( calculateSNR( img, u) );


        zvmessage("Perofrm TV-L1 denoising.", "");

        SB.TV_L1G( u, f, img, grid );		


        //SimpleImage<short int> u_img;
        SimpleImage<double> u_img;
        u_img.alloc(nl, ns);

        for(int line = 0; line < nl; line++){
            for(int samp = 0; samp < ns; samp++){
                //int u_value = u(line, samp);
                double u_value = u(line, samp);
                u_img.set(line, samp, u_value);
            }
        }
    
        //Write out image
        for(int line = 0; line < nl; line++){
            zvwrit(out_unit, u_img.linePtr(line), "band", b+1, "line", line + 1, NULL);
        }
    } // end of the band loop

    timeTaken = realtime() - timeStart;
    sprintf(msg, "Filtering time  : %f milliseconds.  (timeb.h)", timeTaken);
    zvmessage(msg, "");
    sprintf(msg, "Filtering time  : %f seconds.      (timeb.h)", timeTaken/1000);
    zvmessage(msg, "");

    zvclose(inp_unit, NULL);
    zvclose(out_unit, NULL);

    zvmessage("Write output.", "");
}


