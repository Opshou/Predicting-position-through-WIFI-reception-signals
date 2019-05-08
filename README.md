# Predicting position through WIFI signals
## Supervised learning project (K-nearest neighbors)

We have information on WIFI signals received by different phones and users inside of 3 different buildings. Our goal is to find a model that will predict the position of a user just by looking at the signals he/ she is getting.

#### Data (Matrix 19.938 x 520):
The data consists of 19.938 observations of 520 signals coming from 520 different WIFI routers. By analyzing the strength received at every point in the building, a model is built by which we predict the position of a device by the signals received.

#### Specific scope:
Through K-nearest neighbors show that higher accuracy in training set does not provide good results in validation set. By increasing number of neighbors we find better results in the validation set.

#### Results: WIFI allocation.pdf
More information on the results in the PDF.

#### Our Data:
The data comes from the UC Irvine Machine Learning Repository dataset: Collected by researchers in the University Jaume I in Valencia. Data repository: http://archive.ics.uci.edu/ml/datasets/UJIIndoorLocD

#### Information on the dataset - how it was collected:
https://s3.amazonaws.com/gbstool/courses/614/docs/UJIIndoorLoc%20-%20A%20New%20Multi-building%20and%20Multi-floor%20Database%20for%20WLAN%20Fingerprint-based%20Indoor%20Localization%20Problems.pdf?AWSAccessKeyId=AKIAJBIZLMJQ2O6DKIAA&Expires=1554022800&Signature=7kcgwdd4azsZQgc2Su8%2FTATkhuI%3D

