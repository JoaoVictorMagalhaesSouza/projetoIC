#%% Load libs
from pyexpat.errors import XML_ERROR_NOT_STANDALONE
from re import X
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tensorflow as tf
import feature_engineering as fe
import models as ml

#%% Check TF version
print(f"Tensorflow version: {tf.__version__}")

#%% Load data
df = pd.read_csv("..//Dados//LAME4.SA.csv")
pd.set_option('display.max_columns', None)

df = df.drop(columns=['ref.date','ticker','Unnamed: 0'])
df = df.fillna(0)
#%% Test Feature Engineering
class_fe = fe.FeatureEngineeringFactory(df)
dados_fe, cols_fe = class_fe.cria_feature_engineering(['derivada', 'momentos_estatisticos', 'integral'], [['price.close'],['price.high'],['price.low']], janela_momentos=5, janela_integral=5)
df = df.merge(dados_fe, left_index=True, right_index=True)
df = df.fillna(0)
# %% Separing train and test
#80% for training and 20% for testing
percent_train = int(0.8 * df.shape[0])

df_train = df.iloc[:percent_train,:]
df_test = df.iloc[percent_train:,:]

#O procedimento abaixo já tira 'price.close'  do df:
y_train = df_train.pop('price.close')
y_test = df_test.pop('price.close')



#%% Normalize data - 2
def norm(x):
  return (x - df_train.mean()) / df_train.std()

X_train = norm(df_train)
x_test = norm(df_test)



#%% Building MLP
model = ml.MLP(X_train,x_test,y_train,y_test)
model.fit()
model.predict()
model.evaluate()
model.plotCurve()

# %% Building LSTM
model = ml.LSTM(X_train,x_test,y_train,y_test)
model.fit()
model.predict()
model.evaluate()
model.plotCurve()

# %% Building CatBoost
model = ml.CatBoost(X_train,x_test,y_train,y_test)
model.fit()
model.predict()
model.evaluate()
model.plotCurve()




