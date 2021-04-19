#include <Rcpp.h>
#include <jpeglib.h>
#include <sys/time.h>
#include <math.h>
#include <string>
#include "jpeg_image.h"
#include "metrics_extraction.h"
// #include <boost/math/statistics/univariate_statistics.hpp>
#include <boost/math/tools/univariate_statistics.hpp>

using namespace Rcpp;

static image_t *global_mask = NULL;
static image_t **globalMasks = NULL;

// [[Rcpp::export]]
void phenovis_read_mask(std::string maskname)
{
  if (global_mask){
    free(global_mask->image);
    free(global_mask);
    global_mask = NULL;
  }
  global_mask = load_jpeg_image(maskname.c_str());
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
DataFrame phenovis_get_boost_stats(std::string in_image) {

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
  
  NumericMatrix matrix(1, metrics.size() * statistics.size() );
 
  // Load the image and apply mask
  image_t *image = load_jpeg_image(in_image.c_str());
  int considered_pixels = image->width * image->height;
  if (global_mask) {
    considered_pixels = apply_mask(image, global_mask);
  }

  // Calculate the GCC, RCC, BCC, EXG values for pixel
  std::vector<double>* GCC = new std::vector<double>;
  std::vector<double>* RCC = new std::vector<double>;
  std::vector<double>* BCC = new std::vector<double>;
  std::vector<double>* EXG = new std::vector<double>;
  std::vector<double>* LSTAR = new std::vector<double>;

  moving_window_metrics_t metrics_ = {GCC, RCC, BCC, EXG, LSTAR};
  get_metrics_for_image(image, metrics_);

  // double mu = boost::math::tools::mean(v);
  // double m = boost::math::tools::median(v.begin(), v.end());
  // double mad = boost::math::tools::median_absolute_deviation(v);
  // double std_deviation = sqrt( boost::math::tools::sample_variance(v) );
  // std::vector<double> percentiles = calc_nth_percentile(v, p_to_calc);

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

  matrix.row(0) = row;

  //Free the image data
  free(image->image);
  free(image);

  // Create the resulting data frame
  DataFrame ret(matrix);
  ret.insert(ret.begin(), std::string(in_image) );
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
