

/*
 * interpolate input y's to spacing of outx's
 */
  /*
     inx and iny contain original points" x"s and y"s. outx contains the
     x"s for which the interpolated y"s are to be found.  read the original
      y"s and compute the interpolations
  */

void interpolate (inx,iny,inpts,outx,outy,outpts)
 int inpts, outpts;
 float inx[], iny[], outx[], outy[];
{
   int  i;
   float  rmin = 0.001;
   float *offsetinx, *offsetiny, *offsetyderiv, *yderiv;
  
   yderiv = (float *)get_space(inpts * sizeof(*yderiv));
  
   /*
      spline needs unit offset vectors to work
   */
   offsetinx = inx - 1;
   offsetiny = iny - 1;
   offsetyderiv = yderiv - 1;
  
   /*
      get the y derivatives for the input array
   */
   nr_spline(offsetinx,offsetiny,inpts,0.0,0.0,offsetyderiv);

   for (i = 0; i < outpts; i++) {
     if (offsetinx[1]<=outx[i] && outx[i]<=offsetinx[inpts]) {
        nr_splint (offsetinx,offsetiny,offsetyderiv,inpts,outx[i],&outy[i]);
        outy[i] = (outy[i] < rmin) ? 0.0 : outy[i];
     }
     else
        outy[i] = 0.0;
   }
}

