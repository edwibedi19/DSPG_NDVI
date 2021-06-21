# IMPORTS 
import numpy as np
from keras.models import Sequential
from keras.layers import Dense
import keras
import matplotlib.pyplot as plt
from keras.optimizers import SGD

# DETAILS

__author__ = "Atticus Rex"
__copyright__ = "Copyright (C) 2021 Atticus Rex"
__license__ = "Public Domain"
__version__ = "1.0"

# FIELDS

# This has to modify the old np.load function (I have no idea how this works, stole this from stack overflow)
np_load_old = np.load

np.load = lambda *a,**k: np_load_old(*a, allow_pickle=True, **k)

# This is the font for the graphs
font = {'fontname' :  'Times New Roman'}

# FUNCTIONS

# This loads the conditioned inputs from a .npz file
def load_data(file_name):
    dict_data = np.load(file_name + '.npz')
    data = dict_data['arr_0']
    return np.array(data)

def reset_weights(model):
    import keras.backend as K
    session = K.get_session()
    for layer in model.layers: 
        if hasattr(layer, 'kernel_initializer'): 
            layer.kernel.initializer.run(session=session)
        if hasattr(layer, 'bias_initializer'):
            layer.bias.initializer.run(session=session)  

# SCRIPT

input_list = load_data('inputs')

X = np.zeros((len(input_list), 4000))

for i in range(len(input_list)):
    X[i] = np.ravel(input_list[i])

output_list = load_data('outputs')

Y = np.zeros((len(output_list), 3))

for i in range(len(output_list)):
    Y[i] = output_list[i, 0, :]

model = Sequential()
model.add(keras.Input(shape=(None, None, 4000)))
model.add(Dense(256, activation='relu'))
model.add(Dense(128, activation='relu'))
model.add(Dense(64, activation='relu'))
model.add(Dense(3))

opt = SGD(lr=1e-6, momentum=0.9, decay=0.01)
model.compile(loss='mean_squared_error', optimizer='adam', metrics=['accuracy'])

history = model.fit(X, Y, epochs=20, batch_size=300)

accuracy = round(history.history['accuracy'][len(history.history['accuracy']) - 1] * 100, 2)

plt.title("Prediction Error vs. Epoch of Neural Network Prediction\n Final Accuracy: %f percent" % (accuracy), font)
plt.xlabel("Number of Epochs", font)
plt.ylabel("Error (%)")
plt.plot(history.history['loss'], color='red')
plt.grid(color='gray')
plt.show()
