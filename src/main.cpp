#include <Rcpp.h>
#include <jpeglib.h>
#include <sys/time.h>
#include <math.h>
#include <string>
#include "jpeg_image.h"
#include "metrics_extraction.h"
#include "ColorSpace.h"
// #include <boost/math/statistics/univariate_statistics.hpp>
#include <boost/math/tools/univariate_statistics.hpp>
#include <boost/math/tools/bivariate_statistics.hpp>

#include <cstdio>
#include <iostream>

using namespace Rcpp;

static image_t *global_mask = NULL;
static image_t **globalMasks = NULL;
std::vector<bool> global_mask_bits;

// [[Rcpp::export]]
DataFrame phenovis_apply_mask_for_paper(
  StringVector images)
{

  CharacterVector columnNames;
  NumericMatrix matrix(images.size(), 1);

  // names is a vector to keep image names
  std::vector<std::string> names;

  // adj_names is a vector to keep the new image names
  std::vector<std::string> adj_names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // For every pixel...
      for (int p = 0; p < image->size; p += 3) {

        rgb RGB = get_rgb_for_pixel_256(p, image);
        if (global_mask_bits[p]) {

          set_rgb_for_pixel(RGB.r, RGB.g, RGB.b, p, image);
        } else{
          set_rgb_for_pixel(255,255,255, p, image);
        }
      }  

      std::string image_path = std::string(images(i));
      std::string to_find = ".jpg";
      std::string to_replace = "_masked.jpg";
      image_path.replace(image_path.find(to_find),to_find.length(),to_replace);

      // Write the new image:
      write_jpeg_image(image, image_path.c_str());

      // Push back the old image names
      names.push_back(std::string(images(i)));

      // Push back the new image names 
      adj_names.push_back(image_path);

      //Free the image data
      free(image->image);
      free(image);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.insert(ret.begin(), adj_names);
  columnNames.push_front("Picture.Path.adj");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);

}

// [[Rcpp::export]]
DataFrame phenovis_adjust_rgb(
  StringVector images,
  NumericVector r_diff,
  NumericVector g_diff,
  NumericVector b_diff)
{

  CharacterVector columnNames;
  columnNames.push_back("r_mean");
  columnNames.push_back("g_mean");
  columnNames.push_back("b_mean");
  columnNames.push_back("orig_r_mean");
  columnNames.push_back("orig_g_mean");
  columnNames.push_back("orig_b_mean");

  NumericMatrix matrix(images.size(), 6);

  // names is a vector to keep image names
  std::vector<std::string> names;

  // adj_names is a vector to keep the new image names
  std::vector<std::string> adj_names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // Vector to save adjusted pixels
    std::vector<rgb> image_adj;

    // Vector to save orig pixels
    std::vector<rgb> image_orig;

    // For every pixel...
      for (int p = 0; p < image->size; p += 3) {

        rgb RGB = get_rgb_for_pixel_256(p, image);
        if (global_mask_bits[p]) {

          rgb adj_rgb = {0,0,0};
          adj_rgb.r = RGB.r + r_diff[i];
          adj_rgb.g = RGB.g + g_diff[i];
          adj_rgb.b = RGB.b + b_diff[i];

          // condition ? expression-true : expression-false
          // Check values less than 0
          adj_rgb.r < 0 ? adj_rgb.r = 0 : adj_rgb.r = adj_rgb.r;
          adj_rgb.g < 0 ? adj_rgb.g = 0 : adj_rgb.g = adj_rgb.g;
          adj_rgb.b < 0 ? adj_rgb.b = 0 : adj_rgb.b = adj_rgb.b;
          // Check values greater than 255
          adj_rgb.r > 255 ? adj_rgb.r = 255 : adj_rgb.r = adj_rgb.r;
          adj_rgb.g > 255 ? adj_rgb.g = 255 : adj_rgb.g = adj_rgb.g;
          adj_rgb.b > 255 ? adj_rgb.b = 255 : adj_rgb.b = adj_rgb.b;

          // Save pixel
          image_adj.push_back(adj_rgb);
          // Save orig pixel
          image_orig.push_back(RGB);

          set_rgb_for_pixel(adj_rgb.r, adj_rgb.g, adj_rgb.b, p, image);

        }
      }  

      std::string image_path = std::string(images(i));
      std::string to_find = ".jpg";
      std::string to_replace = "_adjusted.jpg";
      image_path.replace(image_path.find(to_find),to_find.length(),to_replace);

      // Write the new image:
      write_jpeg_image(image, image_path.c_str());

      // Push back the old image names
      names.push_back(std::string(images(i)));

      // Push back the new image names 
      adj_names.push_back(image_path);

      // Calculate stats
      double r_sum = 0, g_sum = 0, b_sum = 0;
      int i_rgb_pixels = 0;
      for(auto rgb_pixel : image_adj){
        r_sum += rgb_pixel.r;
        g_sum += rgb_pixel.g;
        b_sum += rgb_pixel.b;
        i_rgb_pixels += 1;
      }
      double r_avg = r_sum / i_rgb_pixels;
      double g_avg = g_sum / i_rgb_pixels;
      double b_avg = b_sum / i_rgb_pixels;

      // Calculate stats
      double orig_r_sum = 0, orig_g_sum = 0, orig_b_sum = 0;
      int orig_i_rgb_pixels = 0;
      for(auto orig_rgb_pixel : image_orig){
        orig_r_sum += orig_rgb_pixel.r;
        orig_g_sum += orig_rgb_pixel.g;
        orig_b_sum += orig_rgb_pixel.b;
        orig_i_rgb_pixels += 1;
      }
      double orig_r_avg = orig_r_sum / orig_i_rgb_pixels;
      double orig_g_avg = orig_g_sum / orig_i_rgb_pixels;
      double orig_b_avg = orig_b_sum / orig_i_rgb_pixels;
      
      NumericVector row;
      row.push_back(r_avg);
      row.push_back(g_avg);
      row.push_back(b_avg);
      row.push_back(orig_r_avg);
      row.push_back(orig_g_avg);
      row.push_back(orig_b_avg);

      matrix.row(row_number) = row;
      row_number++;

      //Free the image data
      free(image->image);
      free(image);

  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.insert(ret.begin(), adj_names);
  columnNames.push_front("Picture.Path.adj");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);

}


// [[Rcpp::export]]
DataFrame phenovis_adjust_lab(
  StringVector images,
  NumericVector l_diff,
  NumericVector a_diff,
  NumericVector b_diff)
{

  CharacterVector columnNames;
  columnNames.push_back("L_mean");
  columnNames.push_back("A_mean");
  columnNames.push_back("B_mean");

  NumericMatrix matrix(images.size(), 3);

  // names is a vector to keep image names
  std::vector<std::string> names;

  // adj_names is a vector to keep the new image names
  std::vector<std::string> adj_names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // Vector to save LAB pixels
    std::vector<ColorSpace::Lab> image_in_lab;

    // For every pixel...
      for (int p = 0; p < image->size; p += 3) {

        rgb RGB = get_rgb_for_pixel_256(p, image);
        if (global_mask_bits[p]) {

          // Go to LAB from RGB pixels
          ColorSpace::Rgb rgb_(RGB.r, RGB.g, RGB.b);
          ColorSpace::Lab lab;
          rgb_.To<ColorSpace::Lab>(&lab);
          
          lab.l = lab.l + l_diff[i];
          lab.a = lab.a + a_diff[i];
          lab.b = lab.b + b_diff[i];

          // Check values less than 0
          lab.l < 0 ? lab.l = 0 : lab.l = lab.l;
          lab.a < -100 ? lab.a = -100 : lab.a = lab.a;
          lab.b < -100 ? lab.b = -100 : lab.b = lab.b;
          // Check values greater than 255
          lab.l > 100 ? lab.l = 100 : lab.l = lab.l;
          lab.a > 100 ? lab.a = 100 : lab.a = lab.a;
          lab.b > 100 ? lab.b = 100 : lab.b = lab.b;

          // Save LAB pixel
          image_in_lab.push_back(lab);

          // Get back to RGB
          ColorSpace::Lab adjusted_lab(lab.l, lab.a, lab.b);
         ColorSpace::Rgb adjusted_rgb;
         adjusted_lab.To<ColorSpace::Rgb>(&adjusted_rgb);

          set_rgb_for_pixel(adjusted_rgb.r, adjusted_rgb.g, adjusted_rgb.b, p, image);

        }
      }  

      std::string image_path = std::string(images(i));
      std::string to_find = ".jpg";
      std::string to_replace = "_adjusted.jpg";
      image_path.replace(image_path.find(to_find),to_find.length(),to_replace);

      // Write the new image:
      write_jpeg_image(image, image_path.c_str());

      // Push back the old image names
      names.push_back(std::string(images(i)));

      // Push back the new image names 
      adj_names.push_back(image_path);

      // Calculate stats
      double L_sum = 0, A_sum = 0, B_sum = 0;
      int i_lab_pixels = 0;
      for(auto lab_pixel : image_in_lab){
        L_sum += lab_pixel.l;
        A_sum += lab_pixel.a;
        B_sum += lab_pixel.b;
        i_lab_pixels += 1;
      }
      double L_avg = L_sum / i_lab_pixels;
      double A_avg = A_sum / i_lab_pixels;
      double B_avg = B_sum / i_lab_pixels;
      
      NumericVector row;
      row.push_back(L_avg);
      row.push_back(A_avg);
      row.push_back(B_avg);

      matrix.row(row_number) = row;
      row_number++;

      //Free the image data
      free(image->image);
      free(image);

  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.insert(ret.begin(), adj_names);
  columnNames.push_front("Picture.Path.adj");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);

}

// // [[Rcpp::export]]
// DataFrame phenovis_adjust_lab(
//   StringVector images,
//   double l_diff,
//   double a_diff,
//   double b_diff)
// {

//   CharacterVector columnNames;
//   columnNames.push_back("L_mean");
//   columnNames.push_back("A_mean");
//   columnNames.push_back("B_mean");
  
//   NumericMatrix matrix(images.size(), 3);

//   // names is a vector to keep image names
//   std::vector<std::string> names;
  
//   int i_img, row_number = 0;
//   for (i_img = 0; i_img < images.size(); i_img++) {
    
//     // Load the image and apply mask
//     image_t *image = load_jpeg_image(std::string(images(i_img)).c_str());
//     int considered_pixels = image->width * image->height;
//     if (global_mask) {
//       considered_pixels = apply_mask(image, global_mask);
//     }

//     // Vector to save LAB pixels
//     std::vector<ColorSpace::Lab> image_in_lab;
    
//     // For every pixel...
//     for (int i = 0; i < image->size; i += 3) {
//     // for (int i = 0; i < 30; i += 3) {  

//       rgb RGB = get_rgb_for_pixel_256(i, image);
//       if (global_mask_bits[i]) {

//         // Go to LAB from RGB pixels
//         ColorSpace::Rgb rgb_(RGB.r, RGB.g, RGB.b);
//         ColorSpace::Lab lab;
//         rgb_.To<ColorSpace::Lab>(&lab);

//         lab.l = lab.l - l_diff;
//         lab.a = lab.a - a_diff;
//         lab.b = lab.b - b_diff;

//         // Adjust limits 
//         if(lab.l > 100){
//           lab.l = 100;
//         } 
//         if(lab.l < 0){
//           lab.l = 0;
//         }
//         if(lab.a > 100){
//           lab.a = 100;
//         } 
//         if(lab.a < -100){
//           lab.a = -100;
//         }
//         if(lab.b > 100){
//           lab.b = 100;
//         } 
//         if(lab.b < -100){
//           lab.b = -100;
//         }

//         // Save LAB pixel
//         image_in_lab.push_back(lab);

//         // Get back to RGB
//         ColorSpace::Lab adjusted_lab(lab.l, lab.a, lab.b);
//         ColorSpace::Rgb adjusted_rgb;
//         adjusted_lab.To<ColorSpace::Rgb>(&adjusted_rgb);

//         set_rgb_for_pixel(adjusted_rgb.r, adjusted_rgb.g, adjusted_rgb.b, i, image);
//         // set_rgb_for_pixel(rgb_.r, 0, 0, i, image);
      
//       }
//     }  

//     std::string image_path = std::string(images(i_img));
//     std::string to_find = ".jpg";
//     std::string to_replace = "_adjusted.jpg";
//     image_path.replace(image_path.find(to_find),to_find.length(),to_replace);

//     // Write the new image:
//     write_jpeg_image(image, image_path.c_str());

//     //Free the image data
//     free(image->image);
//     free(image);

//     double L_sum = 0, A_sum = 0, B_sum = 0;
//     int i_lab_pixels = 0;
//     for(auto lab_pixel : image_in_lab){
//       L_sum += lab_pixel.l;
//       A_sum += lab_pixel.a;
//       B_sum += lab_pixel.b;
//       i_lab_pixels += 1;
//     }
//     double L_avg = L_sum / i_lab_pixels;
//     double A_avg = A_sum / i_lab_pixels;
//     double B_avg = B_sum / i_lab_pixels;

//     NumericVector row;
//     row.push_back(L_avg);
//     row.push_back(A_avg);
//     row.push_back(B_avg);

//     matrix.row(row_number) = row;
//     row_number++;

//   }

//   // Create the resulting data frame
//   DataFrame ret(matrix);
//   ret.insert(ret.begin(), names);
//   columnNames.push_front("Picture.Path");
//   ret.attr("names") = columnNames;
//   Function asDF("as.data.frame");
//   return asDF(ret);

// }


// [[Rcpp::export]]
DataFrame phenovis_lab_stats(StringVector images)
{

  CharacterVector columnNames;
  columnNames.push_back("L_mean");
  columnNames.push_back("A_mean");
  columnNames.push_back("B_mean");
  columnNames.push_back("Gcc_our");
  columnNames.push_back("Gcc_Bruna");
  columnNames.push_back("r_mean");
  columnNames.push_back("g_mean");
  columnNames.push_back("b_mean");

  NumericMatrix matrix(images.size(), 8);

  // names is a vector to keep image names
  std::vector<std::string> names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

      double count_pixels = 0;
      double L_sum = 0;
      double A_sum = 0;
      double B_sum = 0;
      double r_sum = 0;
      double g_sum = 0;
      double b_sum = 0;
      double Gcc_our_sum = 0;
      
    // For every pixel...
      for (int p = 0; p < image->size; p += 3) {
      // for (int p = 0; p < 30; p += 3) {  

        rgb RGB = get_rgb_for_pixel_256(p, image);
        if (global_mask_bits[p]) {

          ColorSpace::Rgb rgb_(RGB.r, RGB.g, RGB.b);
          ColorSpace::Lab lab;
          rgb_.To<ColorSpace::Lab>(&lab);

          L_sum += lab.l;
          A_sum += lab.a;
          B_sum += lab.b;
          r_sum += RGB.r;
          g_sum += RGB.g;
          b_sum += RGB.b;
          Gcc_our_sum += get_gcc_value(RGB);
          count_pixels += 1;

          // test outputed image with mask 
          //unsigned char r_255 = 255;
          //image->image[p] = r_255;
          //image->image[p+1] = 0;
          //image->image[p+2] = 0;
        }
      }  
      // test outputed image with mask  
      //write_jpeg_image(image, std::string(images(i)).c_str());

      double L_mean = L_sum / count_pixels;
      double A_mean = A_sum / count_pixels;
      double B_mean = B_sum / count_pixels;
      double Gcc_our = Gcc_our_sum / count_pixels;
      double total_avg_gcc = (r_sum / count_pixels) + (g_sum / count_pixels) + (b_sum / count_pixels);
      double gcc_mean_Bruna = (g_sum / count_pixels) / total_avg_gcc;

      // Push back the image names
      names.push_back(std::string(images(i)));

      NumericVector row;
      row.push_back(L_mean);
      row.push_back(A_mean);
      row.push_back(B_mean);
      row.push_back(Gcc_our);
      row.push_back(gcc_mean_Bruna);
      row.push_back(r_sum / count_pixels);
      row.push_back(g_sum / count_pixels);
      row.push_back(b_sum / count_pixels);

      matrix.row(row_number) = row;
      row_number++;

      //Free the image data
      free(image->image);
      free(image);

  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);

}

// [[Rcpp::export]]
std::string phenovis_adjust_lstar(std::string image_path, double l_star_mean, double l_star_std_dev, double factor)
{

  // Load the image and apply mask
  image_t *image = load_jpeg_image(image_path.c_str());
  int considered_pixels = image->width * image->height;
  if (global_mask) {
    considered_pixels = apply_mask(image, global_mask);
  }

  // For every pixel...
  for (int i = 0; i < image->size; i += 3) {
  // for (int i = 0; i < 30; i += 3) {  

    rgb RGB = get_rgb_for_pixel_256(i, image);
    if (!is_black(RGB)) {

      ColorSpace::Rgb rgb_(RGB.r, RGB.g, RGB.b);
      ColorSpace::Lab lab;
      rgb_.To<ColorSpace::Lab>(&lab);

      // Correct with a factor
      double l_star = lab.l;
      // If factor is negative:
      if(factor < 0){
        // Filter to pixels with high level of lightness
        if(l_star > (l_star_mean + l_star_std_dev) )
          l_star = l_star + factor;
      }
      // If factor is positive:
      else if(factor > 0){
        // Filter to pixels with low level of lightness
        if(l_star < (l_star_mean - l_star_std_dev) )
          l_star = l_star + factor;
      }

      // if( abs(l_star - l_star_mean) > l_star_std_dev){
      //   //double correction_factor = abs(l_star - l_star_std_dev);
      //   if(l_star > l_star_mean){
      //     l_star = l_star - factor;
      //   } else if(l_star < l_star_mean){
      //     l_star = l_star + factor;
      //   }
      // }

      ColorSpace::Lab adjusted_lab(l_star, lab.a, lab.b);

      ColorSpace::Rgb adjusted_rgb;
      adjusted_lab.To<ColorSpace::Rgb>(&adjusted_rgb);

      set_rgb_for_pixel(adjusted_rgb.r, adjusted_rgb.g, adjusted_rgb.b, i, image);
      // set_rgb_for_pixel(rgb_.r, 0, 0, i, image);
    
    }
  }  

  std::string to_find = ".jpg";
  std::string to_replace = "_adjusted.jpg";
  image_path.replace(image_path.find(to_find),to_find.length(),to_replace);

  // Write the new image:
  write_jpeg_image(image, image_path.c_str());

  //Free the image data
  free(image->image);
  free(image);

  return image_path;

}
// std::vector<std::string> phenovis_get_pixel_values(StringVector images) {

//   // names is a vector to keep image names
//   std::vector<std::string> names;

//   int i, row_number = 0;
//   for (i = 0; i < images.size(); i++) {
  
//     // Load the image and apply mask
//     image_t *image = load_jpeg_image(std::string(images(i)).c_str());
//     int considered_pixels = image->width * image->height;
//     if (global_mask) {
//       considered_pixels = apply_mask(image, global_mask);
//     }

//     std::vector<std::string> pixel_values;
//     // For every pixel...
//     for (int i = 0; i < image->size; i += 3) {

//       rgb RGB = get_rgb_for_pixel(i, image);
//       if (!is_black(RGB)) {
//         HEX hex = get_hex_for_pixel(i, image);
//         char hex_representation[7];
//         sprintf(hex_representation, "%02X%02X%02X", hex.r, hex.g, hex.b);
//         std::string pixel_color_s((char*) hex_representation);
//         pixel_values.push_back(pixel_color_s); 
//       }
//     }

//     return pixel_values;    
//   }

// }

// [[Rcpp::export]]
void phenovis_read_mask(std::string maskname)
{
  if (global_mask){
    free(global_mask->image);
    free(global_mask);
    global_mask = NULL;
  }
  global_mask = load_jpeg_image(maskname.c_str());
  global_mask_bits = read_mask_bits(global_mask);
}

// [[Rcpp::export]]
void phenovis_read_masks(StringVector maskNames) {
  globalMasks = (image_t**) malloc(sizeof(image_t*) * maskNames.size());
  int i;
  for (i=0; i<maskNames.size(); i++) {
    globalMasks[i] = load_jpeg_image(std::string(maskNames(i)).c_str());
    std::cout << "Loaded mask " << std::string(maskNames(i)) << " with index " << i << std::endl;
  }
  std::cout << "Done" << std::endl;
}

// [[Rcpp::export]]
DataFrame phenovis_get_mean_L_star(StringVector images) {
  CharacterVector columnNames;
  columnNames.push_back("Width");
  columnNames.push_back("Height");
  columnNames.push_back("Unmasked_Pixels");
  columnNames.push_back("L_Star");


  NumericMatrix matrix(images.size(), 4);

  // names is a vector to keep image names
  std::vector<std::string> names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // Calculate the L_star
    double L_star_mean = get_L_star_for_image(image);

    // Push back the image name (to align to this row)
    names.push_back(std::string(images(i)));
    NumericVector row;
    row.push_back(image->width);
    row.push_back(image->height);
    row.push_back(considered_pixels);
    row.push_back(L_star_mean);

    matrix.row(row_number) = row;
    row_number++;

    //Free the image data
    free(image->image);
    free(image);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}


// [[Rcpp::export]]
DataFrame phenovis_get_mean_gcc(StringVector images) {
  CharacterVector columnNames;
  columnNames.push_back("Width");
  columnNames.push_back("Height");
  columnNames.push_back("Unmasked_Pixels");
  columnNames.push_back("Mean_Gcc");


  NumericMatrix matrix(images.size(), 4);

  // names is a vector to keep image names
  std::vector<std::string> names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // Calculate the mean GCC
    double mean_gcc = get_mean_gcc_for_image(image);

    // Push back the image name (to aligh to this row)
    names.push_back(std::string(images(i)));
    NumericVector row;
    row.push_back(image->width);
    row.push_back(image->height);
    row.push_back(considered_pixels);
    row.push_back(mean_gcc);

    matrix.row(row_number) = row;
    row_number++;

    // write_jpeg_image(image, std::string(images(i)).c_str());

    //Free the image data
    free(image->image);
    free(image);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}

// Vec needs to be sorted
std::vector<double> calc_nth_percentile(std::vector<double> &vec, std::vector<int> nth){

  std::vector<double> percentiles;
  
  for(int i = 0; i < nth.size(); i++){
    int index = (nth[i] / 100.00) * vec.size();
    percentiles.push_back( vec[index] );
  }

  return percentiles;
}

double calc_p_90(std::vector<double> &vec){
  // Sort values 
  std::sort(vec.begin(), vec.end());

  int index_p90 = 0.9 * vec.size(); 
  return vec[index_p90];
}


double calc_mean(std::vector<double> &vec){

  double sum = 0;
  for(int i; i < vec.size(); i++){
    sum += vec[i];
  }
  return sum / vec.size();
}

// [[Rcpp::export]]
DataFrame phenovis_get_moving_window(StringVector images) {

  CharacterVector columnNames;
  columnNames.push_back("P90_Gcc");
  columnNames.push_back("P90_Rcc");
  columnNames.push_back("P90_Bcc");
  columnNames.push_back("P90_Exg");
  columnNames.push_back("P90_Lstar");
  columnNames.push_back("Mean_Gcc");
  columnNames.push_back("Mean_Rcc");
  columnNames.push_back("Mean_Bcc");
  columnNames.push_back("Mean_Exg");
  columnNames.push_back("Mean_Lstar");

  NumericMatrix matrix(1, 10);

  if(images.size() != 3){
    printf("Error. Invalid number of images, must be 3: previous, current, next.");
    return DataFrame::create();
  }
 
  // image_t *images[3];
  std::vector<image_t*> images_vec;
 
  for (int i = 0; i < 3; i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    images_vec.push_back(image);
  }

  // Calculate the GCC, RCC, BCC, EXG values for pixel
  std::vector<double> GCC, RCC, BCC, EXG, LSTAR;
  moving_window_metrics_t metrics = {&GCC, &RCC, &BCC, &EXG, &LSTAR};
  get_metrics_moving_window(images_vec[0], images_vec[1], images_vec[2], metrics);

  NumericVector row;

  // Calculate p90
  row.push_back(calc_p_90(GCC));
  row.push_back(calc_p_90(RCC));
  row.push_back(calc_p_90(BCC));
  row.push_back(calc_p_90(EXG));
  row.push_back(calc_p_90(LSTAR));
  
  // Calculate mean
  row.push_back(calc_mean(GCC));
  row.push_back(calc_mean(RCC));
  row.push_back(calc_mean(BCC));
  row.push_back(calc_mean(EXG));
  row.push_back(calc_mean(LSTAR));

  matrix.row(0) = row;

  //Free the image data
  for (int i = 0; i < 3; i++) {
    free(images_vec[i]->image);
    free(images_vec[i]);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), std::string(images(1)) );
  columnNames.push_front("Picture.Path");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}

// [[Rcpp::export]]
DataFrame phenovis_get_mean_all_metrics(StringVector images) {
  CharacterVector columnNames;
  columnNames.push_back("Width");
  columnNames.push_back("Height");
  columnNames.push_back("Unmasked_Pixels");
  columnNames.push_back("Mean_Gcc");
  columnNames.push_back("Mean_Rcc");
  columnNames.push_back("Mean_Bcc");
  columnNames.push_back("Mean_Exg");
  

  NumericMatrix matrix(images.size(), 7);

  // names is a vector to keep image names
  std::vector<std::string> names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // Calculate the mean GCC, RCC, BCC, EXG
    std::vector<double> mean_values = get_mean_all_metrics_for_image(image);

    // Push back the image name (to aligh to this row)
    names.push_back(std::string(images(i)));
    NumericVector row;
    row.push_back(image->width);
    row.push_back(image->height);
    row.push_back(considered_pixels);
    row.push_back(mean_values[0]); // GCC
    row.push_back(mean_values[1]); // RCC
    row.push_back(mean_values[2]); // BCC
    row.push_back(mean_values[3]); // EXG

    matrix.row(row_number) = row;
    row_number++;

    //Free the image data
    free(image->image);
    free(image);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}

// [[Rcpp::export]]
DataFrame phenovis_get_boost_stats(StringVector images) {

  std::vector<int> p_to_calc = {10, 20, 25, 30, 40, 50, 60, 70, 75, 80, 90};
  
  std::vector<std::string> metrics = {"Gcc", "Rcc", "Bcc", "ExG", "L_star"};
  std::vector<std::string> statistics = {"mean", "median", "std_dev", "mad", "max", "min"};
  
  for(auto percent : p_to_calc){
    statistics.push_back( "p" + std::to_string(percent) );
  }

  CharacterVector columnNames;
  for(auto metric_name : metrics){
    for(auto stat_name : statistics){
      columnNames.push_back(metric_name + "_" + stat_name);
    }
  }
  columnNames.push_back("corr_L_star_Gcc");
  columnNames.push_back("corr_L_star_Rcc");
  columnNames.push_back("corr_L_star_Bcc");
  columnNames.push_back("corr_L_star_Exg");
  //columnNames.push_back("corr_L_star_L_star");

  
  NumericMatrix matrix(images.size(), 4 + (metrics.size() * statistics.size()) );

  // names is a vector to keep image names
  std::vector<std::string> names;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {

    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());
    int considered_pixels = image->width * image->height;
    if (global_mask) {
      considered_pixels = apply_mask(image, global_mask);
    }

    // Push back the image name (to aligh to this row)
    names.push_back(std::string(images(i)));

    // Calculate the GCC, RCC, BCC, EXG values for pixel
    std::vector<double>* GCC = new std::vector<double>;
    std::vector<double>* RCC = new std::vector<double>;
    std::vector<double>* BCC = new std::vector<double>;
    std::vector<double>* EXG = new std::vector<double>;
    std::vector<double>* LSTAR = new std::vector<double>;

    moving_window_metrics_t metrics_ = {GCC, RCC, BCC, EXG, LSTAR};
    get_metrics_for_image(image, metrics_);
    
    std::vector<double> percentiles;
    NumericVector row;

    // GCC metrics
    row.push_back( boost::math::tools::mean( *GCC ));
    row.push_back( boost::math::tools::median( *GCC ));
    row.push_back( sqrt( boost::math::tools::sample_variance( *GCC ) ) );
    row.push_back( boost::math::tools::median_absolute_deviation( *GCC ));
    std::sort( GCC->begin(), GCC->end() );
    percentiles = calc_nth_percentile(*GCC, p_to_calc);
    row.push_back( *(GCC->end() - 1) ); // Max
    row.push_back( *(GCC->begin()) ); // Min
    for(auto p_cur : percentiles){
      row.push_back( p_cur );
    }  
    
    // RCC metrics
    row.push_back( boost::math::tools::mean(RCC->begin(), RCC->end()) );
    row.push_back( boost::math::tools::median(RCC->begin(), RCC->end()) );
    row.push_back( sqrt(boost::math::tools::sample_variance(RCC->begin(), RCC->end())) );
    row.push_back( boost::math::tools::median_absolute_deviation(RCC->begin(), RCC->end()) );
    std::sort( RCC->begin(), RCC->end() );
    percentiles = calc_nth_percentile(*RCC, p_to_calc);
    row.push_back( *(RCC->end()-1) ); // Max
    row.push_back( *(RCC->begin()) ); // Min
    for(auto p_cur : percentiles){
      row.push_back( p_cur );
    }

    // BCC metrics
    row.push_back( boost::math::tools::mean(BCC->begin(), BCC->end()) );
    row.push_back( boost::math::tools::median(BCC->begin(), BCC->end()) );
    row.push_back( sqrt(boost::math::tools::sample_variance(BCC->begin(), BCC->end())) );
    row.push_back( boost::math::tools::median_absolute_deviation(BCC->begin(), BCC->end()) );
    std::sort( BCC->begin(), BCC->end() );
    percentiles = calc_nth_percentile(*BCC, p_to_calc);
    row.push_back( *(BCC->end()-1) ); // Max
    row.push_back( *(BCC->begin()) ); // Min
    for(auto p_cur : percentiles){
      row.push_back( p_cur );
    }  

    // EXG metrics
    row.push_back( boost::math::tools::mean(EXG->begin(), EXG->end()) );
    row.push_back( boost::math::tools::median(EXG->begin(), EXG->end()) );
    row.push_back( sqrt(boost::math::tools::sample_variance(EXG->begin(), EXG->end())) );
    row.push_back( boost::math::tools::median_absolute_deviation(EXG->begin(), EXG->end()) );
    std::sort( EXG->begin(), EXG->end() );
    percentiles = calc_nth_percentile(*EXG, p_to_calc);
    row.push_back( *(EXG->end()-1) ); // Max
    row.push_back( *(EXG->begin()) ); // Min
    for(auto p_cur : percentiles){
      row.push_back( p_cur );
    }

    // LSTAR metrics
    row.push_back( boost::math::tools::mean(LSTAR->begin(), LSTAR->end()) );
    row.push_back( boost::math::tools::median(LSTAR->begin(), LSTAR->end()) );
    row.push_back( sqrt(boost::math::tools::sample_variance(LSTAR->begin(), LSTAR->end())) );
    row.push_back( boost::math::tools::median_absolute_deviation(LSTAR->begin(), LSTAR->end()) );
    std::sort( LSTAR->begin(), LSTAR->end() );
    percentiles = calc_nth_percentile(*LSTAR, p_to_calc);
    //row.push_back( LSTAR[LSTAR.size() - 1] ); // Max
    //row.push_back( LSTAR[0] ); // Min
    row.push_back( *(LSTAR->end()-1) ); // Max
    row.push_back( *(LSTAR->begin()) ); // Min

    for(auto p_cur : percentiles){
      row.push_back( p_cur );
    }

    // Correlation between metrics
    row.push_back(boost::math::tools::correlation_coefficient(*LSTAR, *GCC));
    row.push_back(boost::math::tools::correlation_coefficient(*LSTAR, *RCC));
    row.push_back(boost::math::tools::correlation_coefficient(*LSTAR, *BCC));
    row.push_back(boost::math::tools::correlation_coefficient(*LSTAR, *EXG));
    //row.push_back(boost::math::tools::correlation_coefficient(*LSTAR, *LSTAR));

    matrix.row(row_number) = row;
    row_number++;

    //Free the image data
    free(image->image);
    free(image);

    delete GCC;
    delete RCC;
    delete BCC;
    delete EXG;
    delete LSTAR;

  }
 
  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");
  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}

// [[Rcpp::export]]
DataFrame phenovis_get_metrics(StringVector images) {
  CharacterVector columnNames;
  columnNames.push_back("Considered_Pixels");
  columnNames.push_back("HSV_Bin");
  columnNames.push_back("HSV_H");
  columnNames.push_back("HSV_SMean");
  columnNames.push_back("HSV_VMean");
  columnNames.push_back("HSV_SMode");
  columnNames.push_back("HSV_VMode");
  columnNames.push_back("Gcc_Bin");
  columnNames.push_back("Gcc_Value");
  columnNames.push_back("Gcc_Mean_R");
  columnNames.push_back("Gcc_Mean_G");
  columnNames.push_back("Gcc_Mean_B");

  int rows = (images.size() * (360 + 100));
  NumericMatrix matrix(rows, 12);

  // names is a string vector to keep image names
  std::vector<std::string> names;
  std::vector<std::string> metricNames;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());

    std::vector<int> unmaskedPixels = (!!global_mask) 
      ? get_unmasked_pixels(global_mask) 
      : get_all_pixels(image);

    // Calculate the HSV_H metric
    phenology_metrics_t *phenology_metrics;
    phenology_metrics = calculate_image_metrics(image, unmaskedPixels);

    // Push HSV_H metrics into the matrix
    for (int j=0; j < 360; j++) {
      // Push back the image and metric names
      names.push_back(std::string(images(i)));
      metricNames.push_back("HSV");

      NumericVector row;
      row.push_back(phenology_metrics->consideredPixels);
      row.push_back(j);
      row.push_back(phenology_metrics->hsv_h[j]);
      row.push_back(phenology_metrics->SMean[j]);
      row.push_back(phenology_metrics->VMean[j]);
      row.push_back(phenology_metrics->SMode[j]);
      row.push_back(phenology_metrics->VMode[j]);

      // Empty values to fill in Gcc gap
      // TODO Test if R will assume NA if I ommit these lines. If not, how can I push NA values?
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);

      matrix.row(row_number) = row;
      row_number++;
    }

    // Push Gcc metrics into the matrix
    for (int j=0; j<100; j++) {
      // Push back the image and metric names
      names.push_back(std::string(images(i)));
      metricNames.push_back("Gcc");

      NumericVector row;
      // Empty stuff to fill in the HSV values
      row.push_back(phenology_metrics->consideredPixels);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);

      row.push_back(j);
      row.push_back(phenology_metrics->Gcc[j]);
      row.push_back(phenology_metrics->GccMeanColor[j].r);
      row.push_back(phenology_metrics->GccMeanColor[j].g);
      row.push_back(phenology_metrics->GccMeanColor[j].b);

      matrix.row(row_number) = row;
      row_number++;
    }

    // Free the calculated metrics
    free(phenology_metrics->hsv_h);
    free(phenology_metrics->SMean);
    free(phenology_metrics->VMean);
    free(phenology_metrics->SMode);
    free(phenology_metrics->VMode);
    free(phenology_metrics->Gcc);
    free(phenology_metrics->GccMeanColor);
    free(phenology_metrics);

    //Free the image data
    free(image->image);
    free(image);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);
  
  //Add names and metric names to the beginning
  ret.insert(ret.begin(), metricNames);
  columnNames.push_front("Metric_Type");
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");

  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}

// [[Rcpp::export]]
DataFrame phenovis_multimask_get_metrics(StringVector images, IntegerVector maskIndexes) {
  CharacterVector columnNames;
  columnNames.push_back("Considered_Pixels");
  columnNames.push_back("HSV_Bin");
  columnNames.push_back("HSV_H");
  columnNames.push_back("HSV_SMean");
  columnNames.push_back("HSV_VMean");
  columnNames.push_back("HSV_SMode");
  columnNames.push_back("HSV_VMode");
  columnNames.push_back("Gcc_Bin");
  columnNames.push_back("Gcc_Value");
  columnNames.push_back("Gcc_Mean_R");
  columnNames.push_back("Gcc_Mean_G");
  columnNames.push_back("Gcc_Mean_B");

  int rows = (images.size() * (360 + 100));
  NumericMatrix matrix(rows, 12);

  // names is a string vector to keep image names
  std::vector<std::string> names;
  std::vector<std::string> metricNames;

  int i, row_number = 0;
  for (i = 0; i < images.size(); i++) {
    // Load the image and apply mask
    image_t *image = load_jpeg_image(std::string(images(i)).c_str());

    std::vector<int> unmaskedPixels = (!!globalMasks)
      ? get_unmasked_pixels(globalMasks[maskIndexes(i)])
      : get_all_pixels(image);

    // Calculate the HSV_H metric
    phenology_metrics_t *phenology_metrics;
    phenology_metrics = calculate_image_metrics(image, unmaskedPixels);

    // Push HSV_H metrics into the matrix
    for (int j = 0; j < 360; j++) {
      // Push back the image and metric names
      names.push_back(std::string(images(i)));
      metricNames.push_back("HSV");

      NumericVector row;
      row.push_back(phenology_metrics->consideredPixels);
      row.push_back(j);
      row.push_back(phenology_metrics->hsv_h[j]);
      row.push_back(phenology_metrics->SMean[j]);
      row.push_back(phenology_metrics->VMean[j]);
      row.push_back(phenology_metrics->SMode[j]);
      row.push_back(phenology_metrics->VMode[j]);

      // Empty values to fill in Gcc gap
      // TODO Test if R will assume NA if I ommit these lines. If not, how can I push NA values?
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);

      matrix.row(row_number) = row;
      row_number++;
    }

    // Push Gcc metrics into the matrix
    for (int j = 0; j < 100; j++) {
      // Push back the image and metric names
      names.push_back(std::string(images(i)));
      metricNames.push_back("Gcc");

      NumericVector row;
      // Empty stuff to fill in the HSV values
      row.push_back(phenology_metrics->consideredPixels);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);
      row.push_back(-1);

      row.push_back(j);
      row.push_back(phenology_metrics->Gcc[j]);
      row.push_back(phenology_metrics->GccMeanColor[j].r);
      row.push_back(phenology_metrics->GccMeanColor[j].g);
      row.push_back(phenology_metrics->GccMeanColor[j].b);

      matrix.row(row_number) = row;
      row_number++;
    }

    // Free the calculated metrics
    free(phenology_metrics->hsv_h);
    free(phenology_metrics->SMean);
    free(phenology_metrics->VMean);
    free(phenology_metrics->SMode);
    free(phenology_metrics->VMode);
    free(phenology_metrics->Gcc);
    free(phenology_metrics->GccMeanColor);
    free(phenology_metrics);

    //Free the image data
    free(image->image);
    free(image);
  }

  // Create the resulting data frame
  DataFrame ret(matrix);

  //Add names and metric names to the beginning
  ret.insert(ret.begin(), metricNames);
  columnNames.push_front("Metric_Type");
  ret.insert(ret.begin(), names);
  columnNames.push_front("Picture.Path");

  ret.attr("names") = columnNames;
  Function asDF("as.data.frame");
  return asDF(ret);
}
