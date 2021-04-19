#ifndef __METRICS_EXTRACTION_H
#define __METRICS_EXTRACTION_H

#include <algorithm> 
#include <vector>
#include "jpeg_image.h"
#include "metrics.h"
#include "rgb2hsv.h"

typedef struct phenology_metrics {
  int consideredPixels;  // The amount of pixels considered in the computation (a.k.a non black pixels)
  int *hsv_h;             // The histogram of the H component
  double *SMean;          // The mean S value for each bin of H
  double *VMean;          // The mean V value for each bin of H
  double *SMode;          // The mode S value for each bin of H
  double *VMode;          // The mode V value for each bin of H
  
  int *Gcc;               // The histogram for the Gcc
  rgb *GccMeanColor;      // The mean RGB colors for the Gcc histogram
} phenology_metrics_t;

typedef struct moving_window_metrics {

  std::vector<double> *Gcc;
  std::vector<double> *Rcc;
  std::vector<double> *Bcc;
  std::vector<double> *Exg;
  std::vector<double> *Lstar;

} moving_window_metrics_t;

phenology_metrics_t *calculate_image_metrics(image_t *image, std::vector<int> unmaskedPixels);
double get_mean_gcc_for_image(image_t *image);
std::vector<double> get_mean_all_metrics_for_image(image_t *image);
void get_metrics_for_image(image_t *img, moving_window_metrics_t &values);
void get_metrics_moving_window(image_t *img1, image_t *img2, image_t *img3, moving_window_metrics_t &values);
double get_L_star_for_image(image_t *image);

#endif