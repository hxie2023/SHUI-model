from osgeo import gdal
from datetime import datetime
import xarray as xr
import numpy as np
import pandas as pd
import netCDF4 as nc
from osgeo import gdal,osr
from zipfile import ZipFile
import pandas as pd

def ncTOtif(infilename,variables,outfilename,frequency,strftime,timelen,outtifname,outtifpath):
##get lat&lon
    dataset = gdal.Open(r"/home/hx2/test-data/20230919_zhonghe/out/landuse_model.tif")
    adfGeoTransform = dataset.GetGeoTransform()

    nXsize = dataset.RasterXSize#
    nYsize = dataset.RasterYSize
    # print(adfGeoTransform[0])#
    # print(adfGeoTransform[3])
    row1 = []#
    col1 = []
    for i in range(nYsize):
        row = []
        col = []
        for j in range(nXsize):
            px = adfGeoTransform[0]+j*adfGeoTransform[1]+i*adfGeoTransform[2]#lon
            py = adfGeoTransform[3]+j*adfGeoTransform[4]+i*adfGeoTransform[5]#lat
            col.append(px)
            row.append(py)
        row1.append(row)
        col1.append(col)   
    p1 = np.array(row1)
    p2 = np.array(col1)#
    lat = p1[:,0]
    lon = p2[0,:]#

    nc_file =  r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'
    ds = xr.open_dataset(nc_file+infilename+'.nc')
    # print(ds.variables['runoff'][1:1])

    monthly_data=ds.resample(time=frequency).sum(skipna=False)
    month_outfile = r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'+outfilename+'nolat.nc'
    monthly_data.to_netcdf(month_outfile)
    month_sum = xr.open_dataset(month_outfile)

     
    date_start = pd.to_datetime('2014/4/30')
    start_range= pd.date_range('2014-5-01', periods=timelen,freq=frequency)
    end_range= pd.date_range('2014-5-01', periods=timelen,freq=frequency)
    new = []
    time = []
    for i in range(timelen):
        start_date = start_range[i]
        end_date = end_range[i]
        month_sum = xr.open_dataset(month_outfile)

        da1 =month_sum[variables].sel(time=slice(start_date, end_date))
        da2 = xr.DataArray.to_numpy(da1)
        da3 = da2.reshape(2063,2570) #
        da3[np.isnan(da3)] = -9999 

        
        outfile = r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'+outfilename+'_'+'%i.nc'%(i+1)
        f_w = nc._netCDF4.Dataset(outfile,'w',format = 'NETCDF4')#

        date_start_end = end_date - date_start
        time = np.array([date_start_end.days])#
        
        f_w.createDimension('time',1)
        f_w.createDimension('lat',len(lat))
        f_w.createDimension('lon',len(lon))  

        
        times = f_w.createVariable('time',np.int8,dimensions='time')   
        latitudes = f_w.createVariable('lat',np.float32,dimensions='lat')  
        longitudes = f_w.createVariable('lon',np.float32,dimensions='lon')
        runoff = f_w.createVariable( variables, np.float32,dimensions= ('time','lat','lon'))
        
        
        times.units = "days since 2014-4-30"
        latitudes.units = "degrees north"
        longitudes.units = "degrees east"
        runoff.missing_value = -9999.0 #

        
        f_w.variables["time"][:] = time
        f_w.variables["lat"][:] = lat
        f_w.variables["lon"][:] = lon
        f_w.variables[variables][:] = da3
        
        f_w.close()
    
    m_open = []
    for i in range(timelen):
        b = 'm_open' + str(i+1)
        m_open.append(b)
    
    for i in range(timelen):
        openfile = r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'+outfilename+'_'+'%i.nc'%(i+1)
        m_open[i] = xr.open_dataset(openfile)
    #
    runoff = xr.concat(m_open,dim='time')
    runoff.to_netcdf(r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'+outfilename+'.nc')


    open = []
    for i in range(timelen):
        start_range= pd.date_range('2020/01/01', periods=timelen,freq=frequency)
        c =start_range.strftime(strftime)
        open.append(c)

    p_data = nc.Dataset(r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'+outfilename+'.nc')
    # print(p_data)  # 
    Lat = p_data.variables['lat'][:]
    Lon = p_data.variables['lon'][:]
    time = p_data.variables['time']
    times = nc.num2date(time[:],time.units) #
    band = np.asarray(p_data.variables[variables]).astype(float) 
    #
    LonMin,LatMax,LonMax,LatMin = [Lon.min(),Lat.max(),Lon.max(),Lat.min()] 
    #
    N_Lat = len(Lat) 
    if Lon.ndim==1 :
        N_Lon = len(Lon)   #
    else:
        N_Lon = len(Lon[0])
    Lon_Res = (LonMax - LonMin) /(float(N_Lon)-1)
    Lat_Res = (LatMax - LatMin) / (float(N_Lat)-1)

    for i in range(band.shape[0]):
        arr1 = band[i,:,:]                   #
    # print(arr1.ndim)
    # print(band.shape[0])
        driver = gdal.GetDriverByName('GTiff')#
        out_tif_name = r'/home/hx2/test-data/SHUI_v1.1_20230922/out/'
        out_tif = driver.Create(out_tif_name+outtifname+c[i]+'.tif',N_Lon,N_Lat,1,gdal.GDT_Float32) 

        geotransform = (LonMin, Lon_Res, 0, LatMax, 0, -Lat_Res)
        out_tif.SetGeoTransform(geotransform)
        
        #
        dataset = gdal.Open(r'/home/hx2/test-data/20230909_zhonghe/out/a.tif')# 
        prosrs = osr.SpatialReference() #
        prosrs.ImportFromWkt(dataset.GetProjection())
        # geosrs = prosrs.CloneGeogCS()
        out_tif.SetProjection(prosrs.ExportToWkt())            

        #    
        arr1[arr1[:, :]> 1000000] = -9999

        #
        if arr1.ndim==2:     #
            a = arr1[:,:]   
        else:                #
            a = arr1[0,:,:]    

        # reversed_arr = a[::-1]    # 
        reversed_arr = a
        out_tif.GetRasterBand(1).SetNoDataValue(-9999) #set nodata value
        out_tif.GetRasterBand(1).WriteArray(reversed_arr) # write tif 
        out_tif.FlushCache() #save
        del out_tif #close tif

# ncTOtif('tn_cell_nc','tn_cell_nc','tn_month','1M','%Y-%m',36,'tn','tif-month')
ncTOtif('out_land','runoff_surf','runoff_surf_month','1M','%Y-%m',1,'runoff_surf','tif-month')

# ncTOtif('tn_cell_nc','tn_cell_nc','tn_day','1d','%Y-%m-%d',1096,'tn','tif-day')
# ncTOtif('tp_cell_nc','tp_cell_nc','tp_day','1d','%Y-%m-%d',30,'runoff','tif-day')