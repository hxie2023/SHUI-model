import numpy as np
import xarray as xr
import netCDF4 as nc
from pyproj import CRS,Proj
from osgeo import gdal,osr
from osgeo import gdalconst
import rioxarray
import os
import glob
from datetime import datetime

###将gpm数据(nc)裁剪至研究区
cwd=os.getcwd()
folder_path=os.path.join(cwd,'dat','GPMdata')
file_pattern='*.nc4'
file_list=glob.glob(os.path.join(folder_path,file_pattern))
time_format = '3B-DAY.MS.MRG.3IMERG.'+'%Y%m%d'+'-S000000-E235959.V07.nc4'  # 文件名中的时间格式
sorted_file_list = sorted(file_list, key=lambda x: datetime.strptime(os.path.basename(x), time_format))

nc_auxi = os.path.join(cwd,'dat','GPMdata','auxi.nc')

j = 0

file_prefix = "prec"
file_suffix = ".nc"

for file in sorted_file_list:
    print(file)
    j = j + 1
    gpm_dataset = xr.open_dataset(file)
    prec_data = gpm_dataset['precipitation'] #按需选择变量
    data=prec_data.loc[:,118.9:119.8,31.1:31.8]#通过限定研究区经纬度范围裁剪数据
    #研究区经纬度范围实际为119.0：119.8，31.1：31.7，裁剪范围+0.1度

    data.to_netcdf(nc_auxi)          #裁剪后的数据存入nc文件
    # time_data = gpm_dataset['time']  #将time变量放入最后生成的nc文件中
    # print(time_data.values)
    auxi_data = nc.Dataset(nc_auxi)
    lat_value = auxi_data.variables['lat'][:]
    lon_value = auxi_data.variables['lon'][:]
    band = np.asarray(auxi_data.variables['precipitation']).astype(float)
    #影像四角坐标
    lonmin, latmax, lonmax, latmin = [lon_value.min(), lat_value.max(), 
                                      lon_value.max(), lon_value.min()]
    auxi_data.close()
    #计算分辨率
    num_lat = len(lat_value)
    num_lon = len(lon_value)
    lat_res = (latmax-latmin)/(float(num_lat)-1)
    lon_res = (lonmax-lonmin)/(float(num_lon)-1) 

    for i in range(band.shape[0]):
        arr1 = band[i,:,:]
        driver = gdal.GetDriverByName('GTiff')
        out_tif_path = os.path.join(cwd,'dat','GPMdata','auxi.tif')
        out_tif = driver.Create(out_tif_path, num_lon, num_lat, 1, gdal.GDT_Float32) 
        geotransform = (lonmin, lon_res, 0.0, latmax, 0.0, -lat_res)
        out_tif.SetGeoTransform(geotransform)

        #设置tif的地理坐标‘wgs84’
        prj = osr.SpatialReference()
        prj.ImportFromEPSG(4326)
        out_tif.SetProjection(prj.ExportToWkt())          
        # 设置异常值   
        arr1[arr1[:, :]> 1000000] = -9999
        if arr1.ndim==2:     
            a = arr1[:,:]   
        else:                
            a = arr1[0,:,:]    
        reversed_arr = a.T    
        out_tif.GetRasterBand(1).SetNoDataValue(-9999) #set nodata value
        out_tif.GetRasterBand(1).WriteArray(reversed_arr) # write tif 
        out_tif.FlushCache() #save
        del out_tif #close tif


    ##改变tif文件分辨率为30*30
    tif_file = os.path.join(cwd,'dat','GPMdata','auxi.tif')
    tif2_file = os.path.join(cwd,'dat','GPMdata','auxi2.tif')
    reference_tif = os.path.join(cwd,'dat','reference.tif')
    #获取输出影像信息
    inputrasfile = gdal.Open(tif_file, gdal.GA_ReadOnly)
    inputProj = inputrasfile.GetProjection()
    # 获取参考影像信息
    referencefile = gdal.Open(reference_tif, gdal.GA_ReadOnly)
    referencefileProj = referencefile.GetProjection()
    referencefileTrans = referencefile.GetGeoTransform()
    x = referencefile.RasterXSize
    y = referencefile.RasterYSize
    
    #设置重采样需保留的变量，此处为inputfile中的‘precipitation’
    bandreferencefile = inputrasfile.GetRasterBand(1)
    nbands=inputrasfile.RasterCount


    # 创建重采样输出文件（设置投影及六参数）
    driver = gdal.GetDriverByName('GTiff')
    output = driver.Create(tif2_file, x, y,nbands, bandreferencefile.DataType)
    output.SetGeoTransform(referencefileTrans)
    output.SetProjection(referencefileProj)
    options = gdal.WarpOptions(srcSRS=inputProj, dstSRS=referencefileProj,
                               resampleAlg=gdalconst.GRA_Bilinear,
                               srcNodata=-9999,dstNodata=-9999)
    gdal.Warp(output,tif_file,options=options) # 输出重采样后的tif

    #将重采样后的tif数据写入到新建的prec.nc文件中
    input_tif = tif2_file
    output_nc = os.path.join(cwd,'dat','GPMdata',f"{file_prefix}_{j}{file_suffix}")
    
    xds = rioxarray.open_rasterio(input_tif) #将tif数据读取为dataArray形式
    t = [j]
    ds = xr.Dataset(
                    {'precipitation':(['time','lat','lon'],xds.values)},
                     coords={'time':('time',t),
                             'lat': ('lat', xds.y.values),
                             'lon': ('lon', xds.x.values),})
    ds.to_netcdf(output_nc, mode='w') # 输出

# 合并多个nc文件成一个降雨nc文件
os.remove(nc_auxi)
file_pattern='*.nc'
folder_path=os.path.join(cwd,'dat','GPMdata')
file_list=sorted(glob.glob(os.path.join(folder_path,file_pattern)))
datasets=[xr.open_dataset(file) for file in file_list]
prec_dataset=xr.concat(datasets,dim='time')
prec_dataset.to_netcdf(os.path.join(cwd,'dat','prec.nc'))
for dataset in datasets:
    dataset.close()
