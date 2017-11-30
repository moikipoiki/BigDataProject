import numpy as np


data_link = "data/winequality-white.csv"

def load_Data():
    f = open(data_link)

    # skip header
    f.readline()

    attributes = [  "fixed_acidity",
                    "volatile_acidity",
                    "citric_acid",
                    "residual_sugar",
                    "chlorides",
                    "free_sulfur_dioxide",
                    "total_sulfur_dioxide",
                    "density",
                    "pH",
                    "sulphates",
                    "alcohol",
                    "quality"]

    data = np.loadtxt(f,unpack=True,delimiter=';')

    return [attributes,data]

def analyze_attribute(name,data):
    print("\nStatistical Information on: %s"%(name))
    print("\tMean: %f" % (np.mean(data)))
    print("\tVariance: %f" % (np.var(data)))


if __name__ == '__main__':
    data = load_Data()
    print(data[0][0])
    print(data[1][0])

    analyze_attribute(data[0][0],data[1][0])