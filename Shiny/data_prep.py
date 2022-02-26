import pandas as pd
import numpy as np

def limpa_dados(df: pd.DataFrame):
    pass


def normaliza_dados(x: pd.DataFrame, nome_ativo: str):
    #Med e DP
    df = pd.read_csv(f"{nome_ativo}.csv")
    df = df.drop(columns=['ref.date','ticker','Unnamed: 0','price.close', 'ret.adjusted.prices',	'ret.closing.prices', 'price.adjusted'])
    df = df.fillna(0)
    return (x - df.mean()) / df.std()

    #Min e Max:
    # return (df - df.min()) / (df.max() - df.min())

def separa_dados(df: pd.DataFrame, percent_train: float = 1):
    percent_train = int(percent_train * df.shape[0])
    X_train = df.iloc[:percent_train,:]
    y_train = X_train.pop('price.close')

    return X_train, y_train


def aplica_pipeline(df: pd.DataFrame, nome_ativo: str):
    X_train, y_train = separa_dados(df)
    X_train = normaliza_dados(X_train, nome_ativo)
    return X_train, y_train