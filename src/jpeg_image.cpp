#include "jpeg_image.h"

void write_jpeg_image(image_t *image, const char *image_path){

  /* This struct contains the JPEG compression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   * It is possible to have several such structures, representing multiple
   * compression/decompression processes, in existence at once.  We refer
   * to any one struct (and its associated working data) as a "JPEG object".
   */
  struct jpeg_compress_struct cinfo;
  /* This struct represents a JPEG error handler.  It is declared separately
   * because applications often want to supply a specialized error handler
   * (see the second half of this file for an example).  But here we just
   * take the easy way out and use the standard error handler, which will
   * print a message on stderr and call exit() if compression fails.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct jpeg_error_mgr jerr;
  /* More stuff */
  FILE * outfile;		/* target file */
  JSAMPROW row_pointer[1];	/* pointer to JSAMPLE row[s] */
  int row_stride;		/* physical row width in image buffer */

  /* Step 1: allocate and initialize JPEG compression object */

  /* We have to set up the error handler first, in case the initialization
   * step fails.  (Unlikely, but it could happen if you are out of memory.)
   * This routine fills in the contents of struct jerr, and returns jerr's
   * address which we place into the link field in cinfo.
   */
  cinfo.err = jpeg_std_error(&jerr);
  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  /* Note: steps 2 and 3 can be done in either order. */

  /* Here we use the library-supplied code to send compressed data to a
   * stdio stream.  You can also write your own code to do something else.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to write binary files.
   */
  if ((outfile = fopen(image_path, "wb")) == NULL) {
    fprintf(stderr, "can't open %s\n", image_path);
    exit(1);
  }
  jpeg_stdio_dest(&cinfo, outfile);

  /* Step 3: set parameters for compression */

  /* First we supply a description of the input image.
   * Four fields of the cinfo struct must be filled in:
   */
  cinfo.image_width = image->width; 	/* image width and height, in pixels */
  cinfo.image_height = image->height;
  cinfo.input_components = image->channels;		/* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB; 	/* colorspace of input image */
  /* Now use the library's routine to set default compression parameters.
   * (You must set at least cinfo.in_color_space before calling this,
   * since the defaults depend on the source color space.)
   */
  jpeg_set_defaults(&cinfo);
  /* Now you can set any non-default parameters you wish to.
   * Here we just illustrate the use of quality (quantization table) scaling:
   */
  jpeg_set_quality(&cinfo, 100, TRUE /* limit to baseline-JPEG values */);

  /* Step 4: Start compressor */

  /* TRUE ensures that we will write a complete interchange-JPEG file.
   * Pass TRUE unless you are very sure of what you're doing.
   */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */

  /* Here we use the library's state variable cinfo.next_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   * To keep things simple, we pass one scanline per call; you can pass
   * more if you wish, though.
   */
  row_stride = image->width * 3;	/* JSAMPLEs per row in image_buffer */

  while (cinfo.next_scanline < cinfo.image_height) {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could pass
     * more than one scanline at a time if that's more convenient.
     */
    row_pointer[0] = & image->image[cinfo.next_scanline * row_stride];
    (void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
  }

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);
  /* After finish_compress, we can close the output file. */
  fclose(outfile);

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_compress(&cinfo);

  /* And we're done! */

}


image_t *load_jpeg_image (const char *filename)
{
  image_t *image;        // data for the image
  
  struct jpeg_decompress_struct info; //for our jpeg info
  struct jpeg_error_mgr err;          //the error handler
  FILE *file = fopen(filename, "rb");  //open the file
  if(!file) {
     return NULL;
  }
  info.err = jpeg_std_error(&err);
  jpeg_create_decompress(&info);   //fills info structure

  jpeg_stdio_src(&info, file);    
  jpeg_read_header(&info, TRUE);   // read jpeg file header
  jpeg_start_decompress(&info);    // decompress the file

  image = (image_t*)malloc(sizeof(image_t));
  image->width = info.output_width;
  image->height = info.output_height;
  image->channels = info.num_components;

  // std::cout << "Filename: " << filename << std::endl;
  // std::cout << "Width: " << image->width << std::endl;
  // std::cout << "Height: " << image->height << std::endl;
  // std::cout << "Channels: " << image->channels << std::endl;

  image->size = image->width * image->height * image->channels;
  image->image = (unsigned char *)malloc(image->size);
  
  while (info.output_scanline < info.output_height){
    unsigned char *rowptr = &image->image[info.output_scanline * image->width * image->channels];
    jpeg_read_scanlines(&info, &rowptr, 1);
  }
  jpeg_finish_decompress(&info);   //finish decompressing
  jpeg_destroy_decompress(&info);
  fclose(file);
  return image; /* should be freed */
}

int apply_mask (image_t *image, image_t *mask)
{
  if (image->width != mask->width || image->height != image->height) {
    printf ("Image and Mask have different dimensions. Error.\n");
    return -1;
  }

  if (image->channels != mask->channels){
    printf ("Image and Mask have different number of channels. Error.\n");
    return -1;
  }

  int total_after_mask = 0;
  for (int i = 0; i < mask->size; i = i+3) {
    unsigned char r = mask->image[i+0];
    unsigned char g = mask->image[i+1];
    unsigned char b = mask->image[i+2];
    if (!(r == 255 && g == 255 && b == 255)){
      image->image[i+0] = 0;
      image->image[i+1] = 0;
      image->image[i+2] = 0;
    }else{
      total_after_mask++;
    }
  }

  return total_after_mask;
}

std::vector<bool> read_mask_bits (image_t *mask)
{
  std::vector<bool> mask_bool_vec(mask->size, false);

  for (int i = 0; i < mask->size; i = i+3) {
    unsigned char r = mask->image[i+0];
    unsigned char g = mask->image[i+1];
    unsigned char b = mask->image[i+2];
    if (r > 128 && g > 128 && b > 128){
      mask_bool_vec[i+0] = true;
      mask_bool_vec[i+1] = true;
      mask_bool_vec[i+2] = true;
    }
  }
  return mask_bool_vec;
}

std::vector<int> get_unmasked_pixels (image_t *mask) {
  std::vector<int> unmaskedPixels;
  for(int i=0; i < mask->size; i += 3) {
    unsigned char r = mask->image[i];
    unsigned char g = mask->image[i+1];
    unsigned char b = mask->image[i+2];

    if(r == 255 && g == 255 && b == 255) {
      unmaskedPixels.push_back(i);
    }
  }
  return unmaskedPixels;
}

std::vector<int> get_all_pixels (image_t *image) {
  std::vector<int> unmaskedPixels;
  for(int i = 0; i < image->size; i += 3) {
    unmaskedPixels.push_back(i);
  };
  return unmaskedPixels;
}