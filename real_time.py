#%% Load libs
from pyexpat.errors import XML_ERROR_NOT_STANDALONE
from re import X
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tensorflow as tf
#Importando funções internas:
import os, sys
from feature_engineering import *
from data_prep import *
from models import *
import os.path

def predita(x_test, nome_ativo):
    # Load data
    df = pd.read_csv(f"{nome_ativo}.csv")
    pd.set_option('display.max_columns', None)

    df = df.drop(columns=['ref.date','ticker','Unnamed: 0', 'ret.adjusted.prices',	'ret.closing.prices', 'price.adjusted'])
    df = df.fillna(0)
    #  Separing train and test
    X_train, y_train, = aplica_pipeline(df, nome_ativo)
    # Montar x_test
    
    

    #
    model = MLP_real_time(X_train,x_test,y_train)
    model.fit()
    return model.predict()
# 

#%% Teste
def main():
    preco_abertura = 14.20
    preco_mais_alto = 14.50
    preco_mais_baixo = 14.20
    #Pensar em colocar o preço de fechamento do último dia
    #Preço de agora se os dados forem do dia atual.
    #Default: dados do dia anterior mas é possível alterar para os dados do dia atual.
    volume = 42230000

    nome_ativo = 'B3SA3.SA'
    x_test = np.array([preco_abertura,preco_mais_alto,preco_mais_baixo,volume]).reshape(1,-1)
    x_test = pd.DataFrame(x_test, columns=['price.open','price.high','price.low','volume'])
    x_test = normaliza_dados(x_test, nome_ativo)


    return predita(x_test, nome_ativo)[0]   


predicao = main()
    
# %%
"""
    Próximos passos:
        - Tentar relizar a integração;
        - Ler o artigo sobre o Dashboard (SIGMAE);
            - Insights como escrever o nosso;

"""