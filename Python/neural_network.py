#%% Load libs
from pyexpat.errors import XML_ERROR_NOT_STANDALONE
from re import X
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tensorflow as tf
import feature_engineering as fe

#%% Check TF version
print(f"Tensorflow version: {tf.__version__}")

#%% Load data
df = pd.read_csv("..//Dados//ECOR3.SA.csv")
pd.set_option('display.max_columns', None)

df = df.drop(columns=['ref.date','ticker','Unnamed: 0'])
df = df.fillna(0)
#%% Test Feature Engineering
# class_fe = fe.FeatureEngineeringFactory(df)
# dados_fe, cols_fe = class_fe.cria_feature_engineering(['derivada', 'momentos_estatisticos', 'integral'], [['price.close'],['price.high'],['price.low']], janela_momentos=5, janela_integral=5)
# df = df.merge(dados_fe, left_index=True, right_index=True)
# df = df.fillna(0)
# %% Separing train and test
#80% for training and 20% for testing
percent_train = int(0.8 * df.shape[0])

df_train = df.iloc[:percent_train,:]
df_test = df.iloc[percent_train:,:]

#O procedimento abaixo já tira 'price.close'  do df:
train_labels = df_train.pop('price.close')
test_labels = df_test.pop('price.close')



#%% Normalize data - 2
def norm(x):
  return (x - df_train.mean()) / df_train.std()

df_train_norm = norm(df_train)
df_test_norm = norm(df_test)



#%% Building model
model = tf.keras.Sequential([
        tf.keras.layers.Dense(df_train_norm.shape[1]),
        tf.keras.layers.Dense(4,activation='relu'),
        tf.keras.layers.Dense(1)
])
optimizer = tf.keras.optimizers.RMSprop(0.001)
model.compile(optimizer=optimizer, loss="mse", metrics=['mae',"mse"])
# %% Fiting the model
history = model.fit(df_train_norm, train_labels, epochs=100)
model.metrics_names
# %% Predict
y_pred = model.predict(df_test_norm).flatten()

# %% Evaluation Regression
from sklearn.metrics import mean_squared_error, mean_absolute_error
from sklearn.metrics import r2_score
aux = test_labels.astype('float32').to_numpy()
print(f"MSE: {mean_squared_error(aux, y_pred)}")
print(f"MAE: {mean_absolute_error(aux, y_pred)}")
print(f'R2: {r2_score(aux, y_pred)}')

# %% Plotling the curve
import plotly.express as px
df = pd.DataFrame()
aux = list(test_labels)

valores = aux.append(list(y_pred))
df['valor'] = test_labels
df['predicoes'] = y_pred
fig = px.line(df)
fig.show()
#%%
