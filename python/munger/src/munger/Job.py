import multiprocessing
import os
import numpy
import sys
from numpy.core.numeric import ones_like
import tqdm
import pandas as pd
from tqdm.contrib.concurrent import process_map


# define a function that takes a list of file paths and runs the proived function on each, 6 at a time, in parallel
def run_parallel(file_paths, function):
    result = process_map(function, file_paths, max_workers=12)
    # if result is a list of lists, flatten it
    if isinstance(result[0], list):
        result = [item for sublist in result for item in sublist]
    numpy.savez("/Users/timpierson/arity/fauveal-subspace/python/munger/data/preprocessed/clustered.npz", *result)
    return None

# define a function that gets all file paths in a directory
def get_file_paths(directory):
    file_paths = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_paths.append(os.path.join(root, file))
    return file_paths

# a function that adds a scalar index to each row of a 2d numpy array
def add_index(data, scalar):
    ones = numpy.ones_like(data[:, 0]).reshape(data.shape[0], -1)
    data_prime = numpy.hstack([ones * scalar, data])
    df = pd.DataFrame(data_prime, columns=["index", "r", "g", "b", "size"])
    return df.groupby(["index", "r", "g", "b"]).sum().reset_index().to_numpy()


#define a function that loads an npz file and slices each array in the list
def load_npz_labels_and_sizes(file_path):

    data = numpy.load(file_path, allow_pickle=True)

    # print(list(data.keys()))
    # print([data[key].shape for key in data.keys()])
    # [print(data[key]) for key in data.keys()]
    enumeration  = [(file_path, key, i) for i,key in enumerate(data.keys())]
    results = process_map(go, enumeration, max_workers=4, chunksize=16)
    return numpy.vstack(results)

def go(idata):

        (file_path, key, i) = idata
        data = numpy.load(file_path, allow_pickle=True)
        data_prime = data[key]
        return take_labels_and_sizes(add_index(data_prime, i))

def take_labels_and_sizes(data):
    return remove_duplicate_rows(data[:, :5])

# a function that removes duplicate rows from a 2d numpy array
def remove_duplicate_rows(data):
    return numpy.unique(data, axis=0)

# a function that writes a numpy array to a csv file
def write_to_csv(data, file_path):
    numpy.savetxt(file_path, data, delimiter=",")
    return None

def convert_to_color_label_csv(file_path):
    data = load_npz_labels_and_sizes(file_path)
    write_to_csv(data, "/Users/timpierson/arity/fauveal-subspace/python/munger/data/preprocessed/color_label.csv")
    return None

if __name__ == "__main__":
    convert_to_color_label_csv("/Users/timpierson/arity/fauveal-subspace/python/munger/data/preprocessed/clustered.npz")
