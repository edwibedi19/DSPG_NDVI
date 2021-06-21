# IMPORTS
import math
import numpy as np
from PIL import Image
import json
from pprint import pprint
from datetime import datetime
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
import gzip
import pickle

# DETAILS

__author__ = "Atticus Rex"
__copyright__ = "Copyright (C) 2021 Atticus Rex"
__license__ = "Public Domain"
__version__ = "2.0"

# FIELDS

Image.MAX_IMAGE_PIXELS = 1e9

# Width of the section that we're inputting
width = 20

# This is where you enter the file names for the metadata files
y1_filename = "LC08_L1TP_017034_20140524_20200911_02_T1_MTL"
y2_filename = "LC08_L1TP_017034_20160614_20200906_02_T1_MTL"

# This is where you enter the displacement (in pixels) from one year to the next
# Essentially the distance you need to move the year 2 image (down = positive vert, right = positive horizontal)
vert_disp = 0
horiz_disp = 19

# This is something weird that happens with the numpy.load() function, these lines fix it
np_load_old = np.load
np.load = lambda *a,**k: np_load_old(*a, allow_pickle=True, **k)

# FUNCTIONS

# Reads the .json file with all the details
def get_file_details(details_path):
    with open(details_path) as infile:
        details = json.load(infile)
    return details

# Converts a .TIF file to a numpy array
def tif_to_array(image_path):
    im = Image.open(image_path)
    imarray = np.array(im)
    return imarray

# Converts the file details dictionary into a dictionary of directories to make accessing each .TIF file much easier. 
def get_directory_names(file_details):
    data_directory = {}
    directory_name = ""
    data_directory["Date"] = datetime.strptime(file_details['LANDSAT_METADATA_FILE']['IMAGE_ATTRIBUTES']['DATE_ACQUIRED'], '%Y-%m-%d')
    data_directory["Band 1"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_1']
    data_directory["Band 2"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_2']
    data_directory["Band 3"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_3']
    data_directory["Band 4"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_4']
    data_directory["Band 5"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_5']
    data_directory["Band 6"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_6']
    data_directory["Band 7"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_7']
    data_directory["Band 8"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_8']
    data_directory["Band 9"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_9']
    data_directory["Band 10"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_10']
    data_directory["Band 11"] = directory_name + file_details['LANDSAT_METADATA_FILE']['PRODUCT_CONTENTS']['FILE_NAME_BAND_11']
    

    return data_directory

# This creates a mask to filter out all of the dead areas in the photographs (Use Band 4 for best results)
def create_mask(image_array1, image_array2):
    mask1 = np.zeros(np.shape(image_array1))
    mask2 = np.zeros(np.shape(image_array2))
    mask1[image_array1 != 0] = 1
    mask1[mask1 != 1] = 0
    mask2[image_array2 != 0] = 1
    mask2[mask2 != 1] = 0

    mask1[mask2 == 0] = 0

    return np.array(mask1)

# This is going to break up a raw Landsat image into specific subsets
def create_subsets(image, mask, cloud_mask, water_mask, square_width):
    # This checks individual rectangles to see if they are in the mask or not
    def check_square(row, column, width):
        cloud_threshold = 17500 # This is the threshold to reject the presence of a cloud in a scene
        water_threshold = 15000 # This threshold rejects the presence of a body of water in a scene
        for i in range(row, row + width + 1):
            # Checks whether or not the scene is in the mask
            try:
                if (np.amin(im[i][column:column + width + 1] == 0)):
                    return False
            except:
                return False
            # Checks Cloud Cover
            if (np.amax(cloud_mask[i][column:column + width + 1] > cloud_threshold)):
                return False
            
            if (np.mean(water_mask[i][column:column + width + 1] < water_threshold)):
                return False
                
        return True
                
    # This just renames the parameters to make the code less wordy
    im = image
    w = square_width

    # Finds the midpoint of the image
    mid_r = int(len(image) / 2)
    mid_c = int(len(image[0]) / 2)

    # Creates the list of possible subsets
    subsets = []

    for i in range(int((len(im) - w - 1) / w)):
        row = i * w
        for j in range(int((len(im) - w - 1) / w)):
            col = j * w
            if check_square(row, col, w):
                subsets.append([row, col])
    
    return subsets

# Compress images more accurately
def compress_image(image, compression_scale):
    row_segs = int(len(image) / compression_scale)
    col_segs = int(len(image[0]) / compression_scale)

    new_img = np.zeros((row_segs, col_segs))

    for i in range(row_segs):
        for j in range(col_segs):
            row = i * compression_scale
            col = j * compression_scale
            
            new_img[i][j] = np.mean(image[row:row + compression_scale, col:col+compression_scale])
    
    new_img = np.flip(new_img, 0)

    return new_img

# This plots the particular data as a cmesh numpy plot
def plot_cmap(image, figure=1):
    image = np.array(image)
    plt.figure(num=figure, figsize=(12, 10))
    plt.pcolormesh(image, cmap='nipy_spectral')
    cbar = plt.colorbar()
    plt.show()

# This function crops a pixel image for zooming and viewing more specific datasets
def crop_image(image, width_range, height_range):
    w_min = width_range[0]
    w_max = width_range[1]
    h_min = height_range[0]
    h_max = height_range[1]

    new_img = []

    for i in range(len(image)):
        if (i <= h_max) and (i >= h_min):
            temp_list = []
            for j in range(len(image[i])):
                if (j <= w_max) and (j >= w_min):
                    temp_list.append(image[i][j])

            new_img.append(temp_list)
    
    return new_img

# This function displays the subsets on the graph of a particular band
def disp_subsets(image, subsets, width):
    image_mask = np.zeros(np.shape(image))
    for subset in subsets:
        row = subset[0]
        col = subset[1]
        for i in range(width):
            for j in range(width):
                image_mask[row][col] = -1
    image[image_mask == 0] = 0
    return image
    
# This function compares an input and output subsets and returns the list of subsets that they have in common
def filter_subsets(subset1, subset2):
    new_subsets = []

    for i in range(len(subset1)):
        if subset1[i] in subset2:
            new_subsets.append(subset1[i])
    
    return new_subsets

# This function gets the numpy array for the data for bands 1-11 for a given year
def get_band_data(year, paths, bands=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], Bottom=None):
    band_data = {}
    for i in bands:
        band_data["Band %d" % i] = tif_to_array("Data/Year%d/" % (year) + paths["Band %d" % i])
        if year == 2:
            band_data["Band %d" % i] = correct_for_distortion(Bottom["Band %d" % i], band_data["Band %d" % i], vert_disp, horiz_disp)
        print("Year %d, Band %d data acquired. . . " % (year, i))
    
    return band_data

# This returns a numpy array with the NDVI calculated
def get_ndvi(band_4, band_5):
    sum_arr = band_4 + band_5
    diff_arr = band_5 - band_4
    
    # This filters out divide by zero errors of the mask
    sum_arr[sum_arr == 0] = 1
    diff_arr[sum_arr == 0] = 0

    ndvi = diff_arr / sum_arr 

    # This filters out abnormally high values from water
    ndvi[ndvi > 1] = 1

    return ndvi
    
# This returns a numpy array with the EVI calculated
def get_evi(band_2, band_4, band_5, band_6):
    numerator = band_5 - band_4
    denominator = (band_5 + 6 * band_4 - 7.5 * band_2 + 1)
    denominator[denominator == 0] = 1
    numerator[denominator == 0] = 0

    evi = 2.5 * numerator / denominator + 1# NOTE: for some reason, the values on the website don't add 1, but you definitely do have to add 1 to make the calculations work

    evi[evi > 1] = 1
    evi[evi < -1] = -1

    return evi

# This returns a numpy array with the NDWI calculated
def get_ndwi(band_5, band_6):
    numerator = band_5 - band_6
    denominator = band_5 + band_6
    denominator[denominator == 0] = 1
    numerator[denominator == 0] = 0

    ndwi = numerator / denominator * 2.5

    ndwi[ndwi > 1] = 1
    ndwi[ndwi < 0] = 0

    return ndwi

# This corrects for any distortion encountered between the two photographs
def correct_for_distortion(bottom_img, top_img, vert_disp, horizontal_disp):
    if vert_disp < 0:
        vert_disp = abs(vert_disp)
        for i in range(vert_disp):
            top_img = np.delete(top_img, 0, axis=0)
            top_img = np.append(top_img, np.zeros((1, len(top_img[0]))), axis=0)
    elif vert_disp > 0:
        for i in range(vert_disp):
            top_img = np.delete(top_img, len(top_img) - 1, axis=0)
            top_img = np.insert(top_img, 0, 0, axis=0)
    elif horizontal_disp < 0:
        for i in range(abs(horizontal_disp)):
            top_img = np.delete(top_img, 0, axis=1)
            top_img = np.append(top_img, np.zeros((len(top_img), 1)), axis=1)
    elif horizontal_disp > 0:
        for i in range(abs(horizontal_disp)):
            top_img = np.delete(top_img, len(top_img[0]) - 1, axis=1)
            top_img = np.insert(top_img, 0, 0, axis=1)
    
    return top_img

# This sets up band data and aligns the two images
def set_up_bands(y1_filename, y2_filename):
    # Specifies which bands we are looking for 
    bands = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    #bands = [1, 2, 4, 5, 6]

    # This gets the file details for each year
    year1_details = get_file_details("Data/Year1/" + y1_filename + ".json")
    year2_details = get_file_details("Data/Year2/" + y2_filename + ".json")
    print("File Details Acquired")

    # This converts the file details into a more digestible dictionary format
    y1_paths = get_directory_names(year1_details)
    y2_paths = get_directory_names(year2_details)
    print("Paths Created")

    # This creates a numpy array from the .tif files for each of the photos (Band 1)
    band_data_1 = get_band_data(1, y1_paths, bands=bands)
    band_data_2 = get_band_data(2, y2_paths, bands=bands, Bottom=band_data_1)
    print("\n*********************\nAll Band Data Acquired!\n*********************\n")

    return [band_data_1, band_data_2]

# This compiles subsets of possible NN input data based upon a sepecified 
def compile_subsets(band_data_1, band_data_2, mask):
    # Now we have to create the different subsets for the data
    subsets_1 = create_subsets(band_data_1["Band 1"], mask, band_data_1["Band 4"], band_data_1["Band 5"], width)
    print("Subset 1 Collected! (Length: %d)" % (len(subsets_1)))

    subsets_2 = create_subsets(band_data_2["Band 1"], mask, band_data_2["Band 4"], band_data_2["Band 5"], width)
    print("Subset 2 Collected! (Length: %d)" % (len(subsets_2)))
    print("Individual Subsets Created\n")

    # Now we have to compare the subsets to each other and see how many overlap
    combined_subsets = filter_subsets(subsets_1, subsets_2)
    print("Combined Subsets Created (Length: %d)\n" % (len(combined_subsets)))
    return combined_subsets

# this will be responsible for creating conditioned inputs to the NN
def create_nn_inputs(band_data_1, indicators, compiled_subsets):
    # This is the list of inputs
    input_list = np.zeros((len(compiled_subsets), width * width, 10))
    # Sets up the class to scale the data into a 0-1 range
    scaler = MinMaxScaler()

    # Loops through each subset to condition
    for index in range(len(compiled_subsets)):
        subset = compiled_subsets[index]
        # Loops through each set of band data
        for i in range(1, 11):
            # Has to correct for excluding band 8 (Different size)
            if i > 7:
                # Copies the band data from the original photograph and saves it at the correct position in the input array.
                input_list[index,:, i - 1] = np.ravel(band_data_1["Band %d" % (i + 1)][subset[0]:subset[0] + width, subset[1]:subset[1] + width])
            else:
                input_list[index,:, i - 1] = np.ravel(band_data_1["Band %d" % (i + 1)][subset[0]:subset[0] + width, subset[1]:subset[1] + width])
    

    # This scales the data into a 0-1 range
    for i in range(0, 10):
        scaler.fit(input_list[:,:,i])
        input_list[:,:,i] = scaler.transform(input_list[:,:,i])


    return input_list

# This creates the desired outputs for the neural network
def create_outputs(compiled_subsets, indicators):
    output_list = np.zeros((len(compiled_subsets), 1, 3))

    for i in range(len(compiled_subsets)):
        subset = compiled_subsets[i]

        # This finds the mean of each of the three indicators within each subset
        output_list[i, 0, 0] = np.mean(indicators[0][subset[0]:subset[0] + width, subset[1]:subset[1] + width])
        output_list[i, 0, 1] = np.mean(indicators[1][subset[0]:subset[0] + width, subset[1]:subset[1] + width])
        output_list[i, 0, 2] = np.mean(indicators[2][subset[0]:subset[0] + width, subset[1]:subset[1] + width])
    
    return output_list

# This saves the conditioned inputs to a .npz (binary) file 
def save_data(file_name, data):
    np.savez_compressed(file_name + '.npz', data)

# This loads the conditioned inputs from a .npz file
def load_data(file_name):
    dict_data = np.load(file_name + '.npz')
    data = dict_data['arr_0']
    return np.array(data)

# SCRIPT - This is the main functionality of the program

def main():
    # This gets the band data for each year
    [band_data_1, band_data_2] = set_up_bands(y1_filename, y2_filename)

    # Now we have to create a mask for the dead spots
    mask = create_mask(band_data_1["Band 1"], band_data_2["Band 1"])

    # This creates a combined subset list of everything we have
    combined_subsets = compile_subsets(band_data_1, band_data_2, mask)

    # This gets the indicator data for the second year (which is what we are targetting)
    ndvi = get_ndvi(band_data_2["Band 4"], band_data_2["Band 5"])
    evi = get_evi(band_data_2["Band 2"], band_data_2["Band 4"], band_data_2["Band 5"], band_data_2["Band 6"])
    ndwi = get_ndwi(band_data_2["Band 5"], band_data_2["Band 6"])

    indicators = [ndvi, evi, ndwi]

    # This creates the inputs and corresponding outputs for the NN
    input_list = create_nn_inputs(band_data_1, indicators, combined_subsets)
    output_list = create_outputs(combined_subsets, indicators)

    # This saves the inputs and outputs to a binary compressed file
    save_data('inputs', input_list)
    save_data('outputs', output_list)

    # This isn't really necessary but loads the file to make sure they work properly
    load_data('inputs')
    load_data('outputs')

#main()