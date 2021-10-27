# **DL_RS_GEE: Deep Learning with Remote Sensing imagery from Google Earth Engine with R language.**

This repo provide code and datasets for training in image segmentation with Deep Learning with Remote Sensing imagery. Experiment is an exercise on rice crops in southern Brazil based on Sentinel-2 data.

<BR>

*Material was used at:*

> Ricardo Dalagnol, Fabien Wagner. (15/09/2021). **Deep learning for Remote Sensing imagery**. *Mini-course WORCAP 2021 (http://www.inpe.br/worcap/2021/)*. National Institute for Space Research - INPE. (<a href=https://github.com/ricds/DL_RS_GEE/raw/main/Minicourse_DeepLearning_v1_WORCAP.pdf>Presentation Link</a>). (<a href=https://youtu.be/foRhRg6VaCQ>Video pt-br</a>).

*Next events:*
> 28/10/2021 - **Introduction to Google Earth Engine with R language.** *IEEE GRSS-ISPRS 2021.* http://grss-isprs.udesc.br/ <BR>
> 04/11/2021 - **Deep Learning for Remote Sensing images with R language.** *IEEE GRSS-ISPRS 2021.* http://grss-isprs.udesc.br/

<BR>

*Example of the output:*

| **U-Net (Deep Learning)**   | **Random Forests** |
| ------------- | ------------- |
| <img src="https://user-images.githubusercontent.com/9935501/133295141-52b349cc-1c4f-4306-af32-9c326bfb1b00.png" width="450" />  | <img src="https://user-images.githubusercontent.com/9935501/133295929-9cb84dd0-2d35-48cb-ba05-067b00ad6bec.png" width="450" />  |

<BR><BR>

# *Codes in R language:*
  
**1) Acquiring imagery from Google Earth Engine directly within R**
<BR>RGEE pre-installation - please take a look and follow this: *rgee_install_packages.R* (<a href=https://youtu.be/1-k6wNL2hlo>Video showing the installation in English</a>)
<BR>R code: *rgee_basics.R*
<BR>R code: *rgee_data_acquisition.R*
  
>In the rgee_basics, we see the basics of Google Earth Engine (GEE) in R, thus RGEE<BR>
>In the rgee_data_acquisition, we have a nice script to acquire Sentinel-2 data for a Machine Learning & Deep Learning experiment<BR>
  
<BR>
  
**2) Applying deep learning to satellite imagery to map rice crops in the local GPU**
<BR>Packages pre-installation - please Run this: *deep_learning_install_packages.R* 
<BR>R code: *deep_learning_crop.R*

>i) Start with a raw satellite image<BR>
>ii) Overlay it with samples and crop the data into patches<BR>
>iii) Train a DL model, and use the DL model to predict the class for all image. Can be done in local PC with GPU or Google Colab<BR>
>iv) Combine the prediction (multiple patches) into a single mosaic<BR>
>v) Assess map accuracy quantitatively and qualitatively (visually)<BR>
>vi) Compare results with a previously produced Random Forests map<BR>

<BR>

**3) Deep Learning with remote sensing data for image segmentation: example of rice crop mapping using Sentinel-2 images**
<BR>Google Colab Jupyter notebook R code: *DL_UNet_CropExample.ipynb* - *link below for Google Colab*

<a href="https://colab.research.google.com/github/ricds/DL_RS_GEE/blob/main/DL_UNet_CropExample.ipynb">
  <img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/>
</a>
  
>i) Train a deep learning model based on previously prepared cropped patches<BR>
>ii) Use the model to predict the class for all image patches

<BR>

# *Dataset:*
  
**1) This dataset contains all the data for the experiment to run the code #2 in the PC.**
  
> https://zenodo.org/record/5504554/files/DL_Unet_CropExample_dataset.rar

Ricardo Dalagnol. (2021). Dataset for "Deep Learning with remote sensing data for image segmentation: example of rice crop mapping using Sentinel-2 images" (Version v1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.5504554

<BR>
  
# *Contact*
Ricardo Dal'Agnol da Silva<br>
National Institute for Space Research (INPE)<br>
Earth Observation and Geoinformatics Division (DIOTG)<br>
São José dos Campos-SP, Brazil<br>
e-mails: ricds@hotmail.com ; ricardo.silva@inpe.br<br>
phone: +55 12 98208-5089<br>
https://ricds.wordpress.com/<br>
Follow me on Twitter <a href=https://twitter.com/RicardoDalagnol>@RicardoDalagnol</a> :) <br>
