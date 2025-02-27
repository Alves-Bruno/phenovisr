#ifndef __JPEG_IMAGE_H
#define __JPEG_IMAGE_H
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <jpeglib.h>
#include <chrono>

typedef struct image {
  unsigned char *image;
  int width;
  int height;
  unsigned long size;
  int channels;
}image_t;

void write_jpeg_image(image_t *image, const char *image_path);
image_t *load_jpeg_image (const char *filename);
image_t *load_jpeg_image_with_time(
				   const char *filename, double *decode_time);
int apply_mask (image_t *image, image_t *mask);
std::vector<int> get_unmasked_pixels(image_t *mask);
std::vector<int> get_all_pixels(image_t *image);
std::vector<bool> read_mask_bits (image_t *mask);

#endif
