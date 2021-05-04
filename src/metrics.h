#ifndef __METRICS_H
#define __METRICS_H
#include "jpeg_image.h"
#include "rgb2hsv.h"

typedef struct hex {

unsigned char r;
unsigned char g;
unsigned char b;

} HEX;

int is_black (rgb RGB);
double get_gcc_value(rgb RGB);
double get_rcc_value(rgb RGB);
double get_bcc_value(rgb RGB);
double get_exg_value(rgb RGB);
double get_L_star_value(rgb RGB);

rgb get_rgb_for_pixel(int pixel, image_t *image);
rgb get_rgb_for_pixel_256(int pixel, image_t *image);
void set_rgb_for_pixel(double r, double g, double b, int pixel, image_t *image);
HEX get_hex_for_pixel(int pixel, image_t *image);
hsv get_HSV_for_pixel(int pixel, image_t *image);
int get_gcc_bin_for_pixel(int pixel, image_t *image);

#endif
