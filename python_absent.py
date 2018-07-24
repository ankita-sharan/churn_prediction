
# coding: utf-8

# In[169]:


import os


# In[170]:


import pandas as pd


# In[171]:


import numpy as np


# In[172]:


import matplotlib.pyplot as plt


# In[173]:


import sklearn


# In[174]:


import seaborn as sns


# In[175]:


from scipy import stats


# In[176]:


import random


# In[177]:


import statsmodels.api as sm


# In[178]:


from scipy.stats import pearsonr


# In[179]:


from sklearn import preprocessing


# In[180]:


from statsmodels.formula.api import ols


# In[181]:


from sklearn import linear_model


# In[182]:


from sklearn.ensemble import RandomForestRegressor


# In[183]:


from sklearn.model_selection import train_test_split


# In[184]:


from math import sqrt


# In[185]:


from sklearn import metrics


# In[186]:


from numpy import cov


# In[187]:


from sklearn.metrics import mean_squared_error


# In[188]:


from sklearn.tree import DecisionTreeRegressor


# In[189]:


pd.set_option('display.float_format', lambda x:'%f'%x)


# In[190]:


os.chdir("C:/Users/kumar shubham/Desktop")


# In[191]:


os.getcwd()


# In[192]:


absent = pd.read_excel("Absenteeism_at_work_Project.xls")


# In[193]:


absent.shape


# In[194]:


absent['Absenteeism time in hours'].value_counts()


# In[195]:


absent.dtypes


# In[196]:


# Missing Value Analysis


# In[197]:


missing_val = pd.DataFrame(absent.isnull().sum())


# In[198]:


missing_val = missing_val.reset_index()


# In[199]:


missing_val = missing_val.rename(columns = {'index':'variables',0:'Missing_Percentage'})


# In[200]:


missing_val['Missing_Percentage'] = (missing_val['Missing_Percentage']/len(absent))*100


# In[201]:


missing_val


# In[202]:


# Imputing missing values with help of mean and median


# In[203]:


absent['Reason for absence'] = absent['Reason for absence'].fillna(absent['Reason for absence'].median())


# In[204]:


absent['Month of absence'] = absent['Month of absence'].fillna(absent['Month of absence'].median())


# In[205]:


absent['Transportation expense'] = absent['Transportation expense'].fillna(absent['Transportation expense'].median())


# In[206]:


absent['Distance from Residence to Work'] = absent['Distance from Residence to Work'].fillna(absent['Distance from Residence to Work'].median())


# In[207]:


absent['Service time'] = absent['Service time'].fillna(absent['Service time'].median())


# In[208]:


absent['Age'] = absent['Age'].fillna(absent['Age'].median())


# In[209]:


absent['Work load Average/day '] = absent['Work load Average/day '].fillna(absent['Work load Average/day '].median())


# In[210]:


absent['Hit target'] = absent['Hit target'].fillna(absent['Hit target'].median())


# In[211]:


absent['Disciplinary failure'] = absent['Disciplinary failure'].fillna(absent['Disciplinary failure'].median())


# In[212]:


absent['Education'] = absent['Education'].fillna(absent['Education'].median())


# In[213]:


absent['Social drinker'] = absent['Social drinker'].fillna(absent['Social drinker'].median())


# In[214]:


absent['Social smoker'] = absent['Social smoker'].fillna(absent['Social smoker'].median())


# In[215]:


absent['Son'] = absent['Son'].fillna(absent['Son'].median())


# In[216]:


absent['Pet'] = absent['Pet'].fillna(absent['Pet'].median())


# In[217]:


absent['Height'] = absent['Height'].fillna(absent['Height'].median())


# In[218]:


absent['Weight'] = absent['Weight'].fillna(absent['Weight'].median())


# In[219]:


absent['Body mass index'] = absent['Body mass index'].fillna(absent['Body mass index'].mean())


# In[220]:


absent['Absenteeism time in hours'] = absent['Absenteeism time in hours'].fillna(absent['Absenteeism time in hours'].median())


# In[221]:


absent.isnull().sum()


# In[222]:


data = absent.copy()


# In[223]:


absent['ID'] = absent['ID'].astype('category')
absent['Reason for absence'] = absent['Reason for absence'].astype('category')
absent['Month of absence'] = absent['Month of absence'].astype('category')
absent['Day of the week'] = absent['Day of the week'].astype('category')
absent['Seasons'] = absent['Seasons'].astype('category')
absent['Disciplinary failure'] = absent['Disciplinary failure'].astype('category')
absent['Education'] = absent['Education'].astype('category')
absent['Social drinker'] = absent['Social drinker'].astype('category')
absent['Social smoker'] = absent['Social smoker'].astype('category')


# In[224]:


#numeric = absent[['Transportation expense', 'Distance from Residence to Work', 'Service time', 'Age', 'Work load Average/day ', 'Hit target',
    # 'Son', 'Pet', 'Weight', 'Height', 'Body mass index', 'Absenteeism time in hours']]


# In[225]:


#numeric.shape


# In[226]:


#factor = absent[['ID', 'Reason for absence', 'Month of absence', 'Day of the week','Seasons', 'Disciplinary failure', 'Education', 'Social drinker',
 #      'Social smoker']]


# In[227]:


#factor.shape


# In[228]:


# outlier analysis
get_ipython().run_line_magic('matplotlib', 'inline')
plt.boxplot(absent['Transportation expense'])


# In[229]:


plt.boxplot(absent['Distance from Residence to Work'])


# In[230]:


plt.boxplot(absent['Service time'])


# In[231]:


plt.boxplot(absent['Age'])


# In[232]:


plt.boxplot(absent['Work load Average/day '])


# In[233]:


plt.boxplot(absent['Hit target'])


# In[234]:


plt.boxplot(absent['Son'])


# In[235]:


plt.boxplot(absent['Pet'])


# In[236]:


plt.boxplot(absent['Weight'])


# In[237]:


plt.boxplot(absent['Height'])


# In[238]:


plt.boxplot(absent['Body mass index'])


# In[239]:


plt.boxplot(absent['Absenteeism time in hours'])


# In[240]:


for i in absent :
    print(i)
    q75,q25 = np.percentile(absent.loc[:,i],[75,25])
    iqr = q75 - q25
    min = q25 - (iqr*1.5)
    max = q75 + (iqr*1.5)
    
    print(min)
    print(max)
    
    


# In[241]:


# calculating minimum and maximum values 


# In[242]:


q75,q25 = np.percentile(absent['Transportation expense'],[75,25])


# In[243]:


iqr = q75 - q25


# In[244]:


minimum = q25 - (iqr*1.5)
maximum = q75 + (iqr*1.5)
print(minimum)
print(maximum)


# In[245]:


absent.loc[absent['Transportation expense']< minimum,:'Transportation expense'] = np.nan
absent.loc[absent['Transportation expense']> maximum,:'Transportation expense'] = np.nan


# In[246]:


q75,q25 = np.percentile(absent['Age'],[75,25])


# In[247]:


iqr = q75 - q25


# In[248]:


minimum2 = q25 - (iqr*1.5)
maximum2 = q75 + (iqr*1.5)
print(minimum2)
print(maximum2)


# In[249]:


absent.loc[absent['Age']< minimum2,:'Age'] = np.nan
absent.loc[absent['Age']> maximum2,:'Age'] = np.nan


# In[250]:


q75,q25 = np.percentile(absent['Service time'],[75,25])


# In[251]:


iqr = q75 - q25


# In[252]:


minimum = q25 - (iqr*1.5)
maximum = q75 + (iqr*1.5)
print(minimum)
print(maximum)


# In[253]:


absent.loc[absent['Service time']< minimum,:'Service time'] = np.nan
absent.loc[absent['Service time']> maximum,:'Service time'] = np.nan


# In[254]:


q75,q25 = np.percentile(absent['Work load Average/day '],[75,25])


# In[255]:


iqr = q75 - q25


# In[256]:


minimum3 = q25 - (iqr*1.5)
maximum3 = q75 + (iqr*1.5)
print(minimum3)
print(maximum3)


# In[257]:


absent.loc[absent['Work load Average/day ']< minimum3,:'Work load Average/day '] = np.nan
absent.loc[absent['Work load Average/day ']> maximum3,:'Work load Average/day '] = np.nan


# In[258]:


q75,q25 = np.percentile(absent['Hit target'],[75,25])


# In[259]:


iqr = q75 - q25


# In[260]:


minimum4 = q25 - (iqr*1.5)
maximum4 = q75 + (iqr*1.5)
print(minimum4)
print(maximum4)


# In[261]:


absent.loc[absent['Hit target']< minimum4,:'Hit target'] = np.nan
absent.loc[absent['Hit target']> maximum4,:'Hit target'] = np.nan


# In[262]:


q75,q25 = np.percentile(absent['Pet'],[75,25])


# In[263]:


iqr = q75 - q25


# In[264]:


minimum6 = q25 - (iqr*1.5)
maximum6 = q75 + (iqr*1.5)
print(minimum6)
print(maximum6)


# In[265]:


absent.loc[absent['Pet']< minimum6,:'Pet'] = np.nan
absent.loc[absent['Pet']> maximum6,:'Pet'] = np.nan


# In[266]:


q75,q25 = np.percentile(absent['Height'],[75,25])


# In[267]:


iqr = q75 - q25


# In[268]:


minimum8 = q25 - (iqr*1.5)
maximum8 = q75 + (iqr*1.5)
print(minimum8)
print(maximum8)


# In[269]:


absent.loc[absent['Height']< minimum8,:'Height'] = np.nan
absent.loc[absent['Height']> maximum8,:'Height'] = np.nan


# In[270]:


# imputing outliers values with median


# In[271]:


absent['Transportation expense'] = absent['Transportation expense'].fillna(absent['Transportation expense'].median())
absent['Age'] = absent['Age'].fillna(absent['Age'].median())
absent['Work load Average/day '] = absent['Work load Average/day '].fillna(absent['Work load Average/day '].median())
absent['Hit target'] = absent['Hit target'].fillna(absent['Hit target'].median())
absent['Service time'] = absent['Service time'].fillna(absent['Service time'].median())
absent['Pet'] = absent['Pet'].fillna(absent['Pet'].median())
absent['Height'] = absent['Height'].fillna(absent['Height'].median())
absent['Absenteeism time in hours'] = absent['Absenteeism time in hours'].fillna(absent['Absenteeism time in hours'].median())


# In[272]:


# Copying data in new object "data"


# In[273]:


absent['ID'] = data['ID']
absent['Reason for absence'] = data['Reason for absence']
absent['Month of absence'] = data['Month of absence']
absent['Day of the week'] = data['Day of the week']
absent['Seasons'] = data['Seasons']
absent['Distance from Residence to Work'] = data['Distance from Residence to Work']
absent['Disciplinary failure'] = data['Disciplinary failure']
absent['Education'] = data['Education']
absent['Son'] = data['Son']
absent['Social drinker'] = data['Social drinker']
absent['Social smoker'] = data['Social smoker']
absent['Weight'] = data['Weight']
absent['Body mass index'] = data ['Body mass index']


# In[274]:


# checking missing values after outlier analysis


# In[275]:


missval = pd.DataFrame(absent.isnull().sum())


# In[276]:


missval


# In[277]:


# Converting data in proper data types


# In[278]:


absent['ID'] = absent['ID'].astype('category')
absent['Reason for absence'] = absent['Reason for absence'].astype('category')
absent['Month of absence'] = absent['Month of absence'].astype('category')
absent['Day of the week'] = absent['Day of the week'].astype('category')
absent['Seasons'] = absent['Seasons'].astype('category')
absent['Disciplinary failure'] = absent['Disciplinary failure'].astype('category')
absent['Education'] = absent['Education'].astype('category')
absent['Social drinker'] = absent['Social drinker'].astype('category')
absent['Social smoker'] = absent['Social smoker'].astype('category')


# In[279]:


# feature selection
numeric_c = absent[['Transportation expense', 'Distance from Residence to Work', 'Service time', 'Age', 'Work load Average/day ', 'Hit target',
     'Son', 'Pet', 'Weight', 'Height', 'Body mass index','Absenteeism time in hours']]


# In[280]:


# Feature selection
corr = numeric_c.corr()


# In[281]:


f,ax = plt.subplots(figsize = (10,8))
sns.heatmap(corr,mask = np.zeros_like(corr,dtype = np.object),cmap = sns.diverging_palette(220,10,as_cmap = True),square = True, ax=ax,annot = True)


# In[282]:


# anova for categorical variable
factor = absent[['ID', 'Reason for absence', 'Month of absence', 'Day of the week','Seasons', 'Disciplinary failure', 'Education', 'Social drinker',
       'Social smoker',]]


# In[283]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Reason for absence"]))


# In[284]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Month of absence"]))


# In[285]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Day of the week"]))


# In[286]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Seasons"]))


# In[287]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Disciplinary failure"]))


# In[288]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Education"]))


# In[289]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Social drinker"]))


# In[290]:


print(stats.f_oneway(absent["Absenteeism time in hours"],absent["Social smoker"]))


# In[291]:


data = absent.copy()


# In[292]:


absent = absent.drop(['ID','Seasons','Education','Height','Hit target','Pet','Body mass index','Disciplinary failure','Age','Social smoker','Social drinker','Son'],axis = 1)


# In[293]:


absent.shape


# In[294]:


# DAta normalisation
#Normality check
absent['Transportation expense'].hist(bins = 20)


# In[295]:


absent['Distance from Residence to Work'].hist(bins = 20)


# In[296]:


absent['Service time'].hist(bins = 20)


# In[297]:


absent[ 'Work load Average/day '].hist(bins = 20)


# In[298]:


absent['Weight'].hist(bins = 20)


# In[299]:


# Data Normalisation
from sklearn.preprocessing import normalize
normalized_absent = preprocessing.normalize(absent)


# In[300]:


absent.dtypes


# In[301]:


# ML Algorithm
## dividing data into train and test
train,test = train_test_split(absent,test_size= 0.2)


# In[302]:


# Decision Tree Regression
random.seed(123)
fit = DecisionTreeRegressor(max_depth = 2).fit(train.iloc[:,0:8],train.iloc[:,8])


# In[303]:


predictions_dt = fit.predict(test.iloc[:,0:8])


# In[304]:


mse_dt = (mean_squared_error(test.iloc[:,8], predictions_dt))
print(mse_dt)


# In[305]:


rmse_dt = sqrt(mean_squared_error(test.iloc[:,8],predictions_dt))
print(rmse_dt)


# In[306]:


# Random forest
# n = 100
random.seed(123)
rfregressor100 = RandomForestRegressor(n_estimators = 100, random_state = 0)
rfregressor100.fit(train.iloc[:,0:8],train.iloc[:,8])


# In[307]:


predictions_rf100 = rfregressor100.predict(test.iloc[:,0:8])


# In[308]:


mse_rf100 = (mean_squared_error(test.iloc[:,8], predictions_rf100))
print(mse_rf100)


# In[309]:


rmse_rf100 = sqrt(mean_squared_error(test.iloc[:,8],predictions_rf100))
print(rmse_rf100)


# In[310]:


# Random forest for n = 200
random.seed(123)
rfregressor200 = RandomForestRegressor(n_estimators = 200, random_state = 0)
rfregressor200.fit(train.iloc[:,0:8],train.iloc[:,8])


# In[311]:


predictions_rf200 = rfregressor200.predict(test.iloc[:,0:8])


# In[312]:


mse_rf200 = (mean_squared_error(test.iloc[:,8], predictions_rf200))
print(mse_rf200)


# In[313]:


rmse_rf200 = sqrt(mean_squared_error(test.iloc[:,8],predictions_rf200))
print(rmse_rf200)


# In[314]:


# Random forest for n = 300

rfregressor300 = RandomForestRegressor(n_estimators = 300, random_state = 0)
rfregressor300.fit(train.iloc[:,0:8],train.iloc[:,8])


# In[315]:


predictions_rf300 = rfregressor300.predict(test.iloc[:,0:8])


# In[316]:


mse_rf300 = (mean_squared_error(test.iloc[:,8], predictions_rf300))
print(mse_rf300)


# In[317]:


rmse_rf300 = sqrt(mean_squared_error(test.iloc[:,8],predictions_rf300))
print(rmse_rf300)


# In[318]:


# Random forest for n = 500

rfregressor500 = RandomForestRegressor(n_estimators = 500, random_state = 0)
rfregressor500.fit(train.iloc[:,0:8],train.iloc[:,8])


# In[319]:


predictions_rf500 = rfregressor500.predict(test.iloc[:,0:8])


# In[320]:


mse_rf500 = (mean_squared_error(test.iloc[:,8], predictions_rf500))
print(mse_rf500)


# In[321]:


rmse_rf500 = sqrt(mean_squared_error(test.iloc[:,8],predictions_rf500))
print(rmse_rf500)


# In[322]:


# Linear regression 

absent['Reason for absence'] = absent['Reason for absence'].astype('float')
absent['Day of the week'] = absent['Day of the week'].astype('float')
absent['Month of absence'] = absent['Month of absence'].astype('float')


# In[323]:


train1,test1 = train_test_split(absent,test_size = 0.2)


# In[324]:


line_regression = sm.OLS(train1.iloc[:,8],train1.iloc[:,0:8]).fit()


# In[325]:


line_regression.summary()


# In[326]:


predictions_lr = line_regression.predict(test1.iloc[:,0:8])


# In[327]:


mse_lr = (mean_squared_error(test.iloc[:,8], predictions_lr))
print(mse_lr)


# In[328]:


rmse_linear = sqrt(mean_squared_error(test1.iloc[:,8],predictions_lr))
print(rmse_linear)


# In[329]:


## LOSS per month
data.shape


# In[330]:


loss = data[['Month of absence','Service time','Work load Average/day ','Absenteeism time in hours']]


# In[331]:


# Work loss = (Workload*Absenteeism time)/Service time

loss["loss_month"] = (loss['Work load Average/day ']*loss['Absenteeism time in hours'])/loss['Service time']


# In[332]:


loss.shape
loss.head(5)


# In[333]:


loss["loss_month"] = np.round(loss["loss_month"]).astype('int64')


# In[334]:


No_absent = loss[loss['Month of absence'] == 0]['loss_month'].sum()
January = loss[loss['Month of absence'] == 1]['loss_month'].sum()
February = loss[loss['Month of absence'] == 2]['loss_month'].sum()
March = loss[loss['Month of absence'] == 3]['loss_month'].sum()
April = loss[loss['Month of absence'] == 4]['loss_month'].sum()
May = loss[loss['Month of absence'] == 5]['loss_month'].sum()
June = loss[loss['Month of absence'] == 6]['loss_month'].sum()
July = loss[loss['Month of absence'] == 7]['loss_month'].sum()
August = loss[loss['Month of absence'] == 8]['loss_month'].sum()
September = loss[loss['Month of absence'] == 9]['loss_month'].sum()
October = loss[loss['Month of absence'] == 10]['loss_month'].sum()
November = loss[loss['Month of absence'] == 11]['loss_month'].sum()
December = loss[loss['Month of absence'] == 12]['loss_month'].sum()


# In[335]:


loss.head(5)


# In[336]:


data1 = {'No Absent': No_absent, 'Janaury': January,'Febraury': February,'March': March,
       'April': April, 'May': May,'June': June,'July': July,
       'August': August,'September': September,'October': October,'November': November,
       'December': December}


# In[337]:


workloss = pd.DataFrame.from_dict(data1,orient = 'index')


# In[338]:


workloss.rename(index = str, columns={0:"Workload loss pr month"})

