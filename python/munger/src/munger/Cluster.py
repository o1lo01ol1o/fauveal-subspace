

from sklearn.metrics.pairwise import pairwise_distances
import cv2
import umap
import hdbscan
import numpy as numpy
import hdbscan
import pandas as pd
from PIL import Image
from  sklearn.cluster import KMeans
from scipy.spatial import distance
from functools import reduce
from itertools import product, repeat, tee
import logging

# define global logging configuration
logging.basicConfig(level=logging.WARN)

DO_LOG= True

# take a list of labels, the kmeans object, the UMAP object, and replace the list of labels with the inverse UMAP value
def kmeansLabelToInverseUMAPValue(labelArray, kmeans, umapper):
    inverse_centers = umapper.inverse_transform(kmeans.cluster_centers_)
    inverseValue = numpy.apply_along_axis(lambda clabel: inverse_centers[clabel], 0, labelArray.reshape(-1))
    return inverseValue.reshape((labelArray.shape[0], -1))


#define a function that gets all the unique values along a given axis
def colorSegments(array, axis=0, debugSave=False):
    zeros = numpy.zeros_like(array)
    ones = numpy.ones_like(array)
    values = numpy.unique(array.reshape(-1,3), axis=axis)
    segments = []
    sizes = []
    labels = []
    for value in values:
        p1, m1 = plusMinus(value)
        segment = cv2.inRange(array, m1, p1)
        size = numpy.where(array==value, ones, zeros).sum()
        if debugSave:
            saveImage("./testSegment.jpg", segment)
        segments.append(segment)
        sizes.append(size)
        labels.append(value)
    return (segments, sizes, labels)


# Define a functtion that takes a filepath and opens an image using Pillow
def openImage(filepath, size=(128, 128)):
    # open the image using Pillow
    img = Image.open(filepath)
    img.thumbnail(size)
    return img

# define a function that takes a 3-tuple of ints and returns two 3-tupes of plus 1 and minus 1
def plusMinus(t):
    return (t[0] + 1, t[1] + 1, t[2] + 1), (t[0] - 1, t[1] - 1, t[2] - 1)

# define a function that converts from a Pillow image to an opencv image
def convertPILtoCV(img):
    # convert the image to a numpy array
    img = numpy.array(img)
    # convert the numpy array to an opencv image
    img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
    # return the opencv image
    return img

def cluster(d, n=2, k=15):
    d = numpy.asarray(d)
    d_prime = d.reshape((-1,3))
    mapper = umap.UMAP(densmap=False, random_state=42, n_components=n)
    clusterable_embedding = mapper.fit_transform(d_prime)
    kmeans = KMeans(n_clusters=k, random_state=42).fit(clusterable_embedding)
    labels = kmeans.predict(clusterable_embedding)
    labels = kmeansLabelToInverseUMAPValue(labels, kmeans, mapper)

    # hdbs = hdbscan.HDBSCAN(
    #     min_samples=4,
    #     min_cluster_size=20,
    #     )
    # labels = hdbs.fit_predict(clusterable_embedding.reshape(-1,n))
    # probs = hdbs.probabilities_
    # df = pd.DataFrame(numpy.concatenate([labels, d_prime, probs, ]), columns= ["label", "colors", "probs"])
    # return numpy.concatenate(df.groupby("label").idxmax(axis="probs")["colors"].tolist())
    return labels.reshape(d.shape)


def segment(img):
    # img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
    # # Convert to grayscale
    # img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    # img = cv2.convertScaleAbs(img)
    # Apply thresholding
    ret, img = cv2.threshold(img,10,255,cv2.THRESH_OTSU)
    # Apply morphology
    # kernel = numpy.ones((3, 3), numpy.uint8)
    # img = cv2.morphologyEx(img, cv2.MORPH_OPEN, kernel)
    saveImage("./testThreashold.jpg", img)

    # Apply contour detection
    contours, hierarchy = cv2.findContours(img, cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)
    # Create a list of clusters
    clusters = []
    areas = []
    # Iterate through each contour
    for contour in contours:
        # if the type of contour is an array print the shape and dtype
        if isinstance(contour, numpy.ndarray):
            logShapeAndDtype(contour)
        # else print the type of contour
        else:
            logType(contour)
        # Append the contour to the list of clusters
        clusters.append(contour)
        areas.append(cv2.contourArea(contour))
    # Return the list of clusters
    return (drawContours(contours, img.copy()), (clusters, map(getCentroid, clusters)), areas)

# if the log flag is set, log the shape and dtype of an array using the logging module
def logShapeAndDtype(array, log=DO_LOG):
    if log:
        logging.info("Shape: {}".format(array.shape))
        logging.info("Dtype: {}".format(array.dtype))

# if the log flag is set, log the type of a using the logging module
def logType(a, log=DO_LOG):
    if log:
        logging.info("Type: {}".format(type(a)))


def getInteriorIntensity(c, img):

    # Create a mask image that contains the contour filled in
    cimg = numpy.zeros_like(img)
    cv2.drawContours(cimg, c, 0, (255,255,255), -1)

    # Access the image pixels and create a 1D numpy array then add to list
    pts = numpy.where(cimg == 255)
    return img[pts[0], pts[1]]

# take a list of contours from opencv and return an image with the contours drawn on it
def drawContours(contours, img):
    # draw all the contours
    cv2.drawContours(cv2.cvtColor(img.copy(), cv2.COLOR_GRAY2RGB), contours, 0,(94,234,255),-1)
    # return the image
    return img

# define a function that finds the closest pair of points in two arrays
def closestPair(a, b, n=2):
    a = a.squeeze()
    b = b.squeeze()
    # if a and b are not 2d arrays reshape them to 2d arrays
    if len(a.shape) == 1:
        a = a.reshape(-1, n)
    if len(b.shape) == 1:
        b = b.reshape(-1, n)
    dist_matrix = distance.cdist(a, b, 'euclidean')
    return dist_matrix.min()


# gets the centroid of a cluster
def getCentroid(cluster):
    # get the moments of the cluster
    moments = cv2.moments(cluster)
    try:
        # get the centroid of the cluster
        centroid = (int(moments['m10'] / moments['m00']), int(moments['m01'] / moments['m00']))
    except ZeroDivisionError:
        centroid = (-1, -1)
    # return the centroid
    return centroid

# filters a list for only the tuples that don't have the same value
def filterTuples(tuples):
    return filter(lambda t: not sameArray(t[0], t[1]), tuples)

# check if two numpy arrays are the same
def sameArray(a, b):
    if a.shape == b.shape:
        return (a == b).all()
    else: return False

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return zip(a, b)


class Segmentation:
    """A class that takes a 3-tuple of the original image, the contoured image and the segmented contours, a list of contours, a list of contour labels, a dictionary of labels, and a list of centroids
and initializes getters an setters for the class.
for k in KMeans, there will be k clusteredImages, k colors, and k*k contours and (k*k)-k edges with distances between nearest contour points (no self edges)
    """
    def __init__(self, img, clusteredImage, contouredImgs, contours, labels, centroids, sizes):
        self.img = img
        self.clusteredImage = clusteredImage
        self.contouredImgs = contouredImgs
        self.contours = contours
        self.labels = labels
        self.centroids = centroids
        self.sizes = sizes
    # get number of contours
    def getNumContours(self):
        return len(self.contours)
    def getNumLabels(self):
        return len(self.labels)
    #get number of sizes
    def getNumSizes(self):
        return len(self.sizes)

    def centroidEdgesMatrix(self):
        assert (len(self.centroids) == len(self.sizes))
        assert (len(self.centroids) == len(self.labels))
        centroids = numpy.concatenate([numpy.array([c[0],c[1]]) for c in self.centroids], axis=0).reshape(-1,2)
        distMat = pairwise_distances(centroids)
        labelArray = numpy.vstack([numpy.array([c[0],c[1], c[2]]).reshape(3) for c in self.labels])
        x = numpy.concatenate([labelArray, numpy.array(self.sizes).reshape(-1,1) , distMat], axis=1).reshape(distMat.shape[0], -1)
        sortedIdxs = numpy.argsort(x[:, :2], axis=0)
        return x[sortedIdxs]

    def labelsAndSizes(self):
        assert (len(self.centroids) == len(self.sizes))
        assert (len(self.centroids) == len(self.labels))
        labelArray = numpy.vstack([numpy.array([c[0],c[1], c[2]]).reshape(3) for c in self.labels])
        x = numpy.concatenate([labelArray, numpy.array(self.sizes).reshape(-1,1)], axis=1).reshape(len(self.sizes), -1)
        return x




def flatten(t):
    return [item for sublist in t for item in sublist]

def piplineK(filepath, ks, size=(128, 128)):
    results = []
    for k in ks:
        try:
            results.append(pipline(filepath, k, size).labelsAndSizes())
        except Exception as e:
            print(e)
            pass
    return results


def pipline(filepath, k=15, size=(128, 128)):
    """takes a file path, clusters via UMAP and Kmeans, outputs a Segmentation object

    Args:
        filepath (filepath): [the file path to the image]
        size (tuple, optional): [the size of the downscaled image]. Defaults to (128, 128).

    Returns:
        Segmentation : [The Segmentation object]
    """
    img = openImage(filepath, size=size)
    clusters = cluster(img, k=k)
    clusteredImage = cv2.cvtColor(clusters, cv2.COLOR_RGB2BGR)
    (segments, _sizes, labels) = colorSegments(clusters)
    countourImgs = []
    contours = []
    centroids_prime = []
    contours_areas =[]
    contourLabels = []
    for seg, lab in zip(segments, labels):
        (countourImg, (contours_prime, centroid_prime), contours_areas_prime) = segment(seg)
        countourImgs.append(countourImg)
        contours.append(contours_prime)
        centroids_prime.append(centroid_prime)
        contours_areas.append(contours_areas_prime)
        contourLabels.append(repeat(lab,(len(contours_areas_prime))))
    centroids = flatten(centroids_prime)
    contours_areas= flatten(contours_areas)
    contours =flatten(contours)
    contourLabels = flatten(contourLabels)
    # contourIdx = list(map(lambda x: x[0], enumerate(contours)))
    # cartesianContours = product(contours, contours)
    # cartesianContourIdxs = product(contourIdx, contourIdx)
    # contourIdxs = list(filter(filterTuples,  cartesianContourIdxs))
    # contours = list(filter(filterTuples, cartesianContours))
    # dicts = list(map (lambda x: { x[1] : closestPair(numpy.concatenate(x[0][0], axis=0), numpy.concatenate(x[0][1], axis=0))}, zip(contours, contourIdxs)))
    # distances = reduce(lambda d, d_prime : union(d, d_prime), dicts)
    return Segmentation(img, clusteredImage, countourImgs, contours, contourLabels, centroids, contours_areas)

#a function that takes a filepath and an opencv image and saves it to the filepath
def saveImage(filepath, img):
    cv2.imwrite(filepath, img)


# a function that assserts that an array is 2d
def assert2d(array):
    assert len(array.shape) == 2

# take the union of two dictionaries
def union(d1, d2):
    # assert that d1 and d2 are dictionaries
    assert isinstance(d1, dict)
    assert isinstance(d2, dict)
    return {**d1, **d2}


if __name__ == '__main__':
    # Test the function
    # clusters = cluster(fp)
    fp = "/Users/timpierson/arity/fauveal-subspace/python/munger/notebooks/images/woman-in-white-1915.jpg"
    segmentation = pipline(fp)
    print (segmentation.labelsAndSizes().shape)
    print (segmentation.getNumLabels())
