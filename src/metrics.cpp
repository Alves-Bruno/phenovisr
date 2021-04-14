#include "metrics.h"

int is_black (rgb RGB) {
  return !(RGB.r + RGB.g + RGB.b);
}

rgb get_rgb_for_pixel(int pixel, image_t *image) {
  unsigned char *iimage = image->image;
  unsigned char r = iimage[pixel];
  unsigned char g = iimage[pixel + 1];
  unsigned char b = iimage[pixel + 2];

  rgb RGB = {(double)r / 255, (double)g / 255, (double)b / 255};
  return RGB;
}

hsv get_HSV_for_pixel(int pixel, image_t *image) {
  rgb RGB = get_rgb_for_pixel(pixel, image);
  // if (is_black(RGB)) {
  //   return {-1, -1, -1};
  // }

  hsv HSV = rgb2hsv(RGB);
  return HSV;
}

int get_gcc_bin_for_pixel(int pixel, image_t *image) {
  rgb RGB = get_rgb_for_pixel(pixel, image);
  int gccBin;
  if (is_black(RGB)) {
    // This is done to address division by zero. Since I need to consider
    // black pixels, I will assume that for whatever value in which 
    // R, G and B are the same (including a black pixel 0, 0, 0), the Gcc will
    // be 1/3. The limit of x/3x when x approaches 0 is 1/3.
    RGB = {1, 1, 1};
  }
  gccBin = floor(get_gcc_value(RGB) * 100);
  gccBin = (gccBin >= 100) ? 99 : gccBin;
  return gccBin;
}

double get_gcc_value(rgb RGB) {
  return (RGB.r + RGB.g + RGB.b) != 0
    ? RGB.g / (RGB.r + RGB.g + RGB.b)
    : 0;
}

double get_rcc_value(rgb RGB) {
  return (RGB.r + RGB.g + RGB.b) != 0
    ? RGB.r / (RGB.r + RGB.g + RGB.b)
    : 0;
}

double get_bcc_value(rgb RGB) {
  return (RGB.r + RGB.g + RGB.b) != 0
    ? RGB.b / (RGB.r + RGB.g + RGB.b)
    : 0;
}

double get_exg_value(rgb RGB) {
  return (2.0 * RGB.g - (RGB.r + RGB.b));
}


double rgb_to_linear(double colorChannel){

  if( colorChannel <= 0.04045){
    return colorChannel / 19.920;
  } else {
    return pow( (( colorChannel + 0.055 ) / 1.055), 2.4 );
  }

}

double get_L_star_value(rgb RGB){

  // Convert gamma encoded RGB to a linear value:
  rgb l_RGB = {0.00, 0.00, 0.00};
  l_RGB.r = rgb_to_linear(RGB.r);
  l_RGB.g = rgb_to_linear(RGB.g);
  l_RGB.b = rgb_to_linear(RGB.b);

  // Luminace ( Y )
  double Y = ( (0.2126*l_RGB.r) + (0.7152*l_RGB.g) + (0.0722*l_RGB.b));

  // L_star (Perceived Lightness)
  if ( Y <= (216.0000/24389.0000) ) {       // The CIE standard states 0.008856 but 216/24389 is the intent for 0.008856451679036
    return Y * (24389.0000/27.0000);  // The CIE standard states 903.3, but 24389/27 is the intent, making 903.296296296296296
  } else {
    return pow(Y,(1.0000/3.0000)) * (116.0000 - 16.0000);
  }
  
}