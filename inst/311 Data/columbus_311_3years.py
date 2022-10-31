import requests
import json
import copy
import pandas as pd
import geopandas as gpd
from datetime import date

# =================== #
# ---- Last year ---- 
# =================== #

params = {
    "where": "1=1",      # Download all records.  Do not filter by attribute.
    "outFields": "*",
    "f":"geojson"        # Download in GeoJSON format (other formats are probably not useful)
}    

baseUrl = "http://maps2.columbus.gov/arcgis/rest/services/Applications/ServiceRequests/MapServer/1/query"

url = "https://maps2.columbus.gov/arcgis/rest/services/Applications/ServiceRequests/MapServer/1/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"

r = requests.get(url=url, params=params)
data = r.json()

file_dir = "/fs/ess/PDE0001/311/Columbus/API_result/"
file_name = "columbus311_3years.geojson"
file = file_dir + file_name


with open(file, "w") as f:
    f.write(json.dumps(data, indent=4))
    
print("Data stored at: " + file)
