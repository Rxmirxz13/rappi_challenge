# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import os
import json
import numpy as np

#definimos nuestro directorio de trabajo 
os.chdir('D:/Emiliano/Rappi/raw')

#importamos base de datos en formato csv

df_rappi = pd.read_csv('ds_challenge_data.csv') 

#vemos que hay una variable que es de tipo json. La transformaremos a una variable
#normal 

json_cols = ['dispositivo']

for column in json_cols:

	c_load = df_rappi[column].apply(json.loads)
	c_list = list(c_load)
	c_dat = json.dumps(c_list)

	df_rappi = df_rappi.join(pd.read_json(c_dat))
	df_rappi = df_rappi.drop(column , axis=1)
    
#comenzamos el análisis exploratorio de los datos
#primero echamos un vistazo a los datos 

df_rappi.head()

#existen variables categóricas que no están declaradas como tal. Convirtámoslas.
#1 para mujer 0 para hombre
df_rappi['d_genero']= np.where(df_rappi['genero']== 'M',1,0)

#obtenemos algunos estadísticos descriptivos de nuestra base

df_rappi['genero'].describe()
df_rappi['genero'].describe()
df_rappi['genero'].describe()
