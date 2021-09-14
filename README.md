# **DL_RS_GEE: Deep Learning with Remote Sensing imagery from Google Earth Engine.**

Authors: Ricardo Dalagnol, Fabien Wagner


# *Codes in R language:*

**1) Applying deep learning to satellite imagery to map rice crops in the local GPU**
<BR>R code: *deep_learning_crop.R*

>i) Start with a raw satellite image<BR>
>ii) Overlay it with samples and crop the data into patches<BR>
>iii) Train a DL model, and use the DL model to predict the class for all image. Can be done in local PC with GPU or Google Colab<BR>
>iv) Combine the prediction (multiple patches) into a single mosaic<BR>
>v) Assess map accuracy quantitatively and qualitatively (visually)<BR>
>vi) Compare results with a previously produced Random Forests map<BR>

<BR>

**2) Deep Learning with remote sensing data for image segmentation: example of rice crop mapping using Sentinel-2 images**
<BR>Google Colab Jupyter notebook R code: *DL_UNet_CropExample.ipynb* - *link below for Google Colab*

<a href="https://colab.research.google.com/github/ricds/DL_RS_GEE/blob/main/DL_UNet_CropExample.ipynb">
  <img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/>
</a>
  
>i) Train a deep learning model based on previously prepared cropped patches<BR>
>ii) Use the model to predict the class for all image patches

<BR>

# *Dataset:*
  
**1) This dataset contains all the data for the experiment to run the code #1 in the PC.**
  
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
