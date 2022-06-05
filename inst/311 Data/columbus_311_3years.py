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
    "outFields": "STATUS, SHAPE, OBJECTID, STATUS_DATE, DEPARTMENT_NAME, DIVISION_NAME, SECTION_NAME, DATAHUB_ID, CASE_ID, REPORTED_DATE, REQUEST_CATEGORY, REQUEST_SUBCATEGORY, REQUEST_TYPE, TEAM_NAME, STREET, CITY, ZIP, COLUMBUSCOMMUNITY, AREACOMMISSION, COUNCILDISTRICT, C1PRIORITYAREA, ZIPCODE, LOCATION_ID, LATITUDE, LONGITUDE, REQUEST_MODIFIED",    # Retrieve a subset of the attributes
    "f":"geojson"        # Download in GeoJSON format (other formats are probably not useful)
}    

baseUrl = "http://maps2.columbus.gov/arcgis/rest/services/Applications/ServiceRequests/MapServer/1/query"

r = requests.get(url=baseUrl, params=params)
data = r.json()

file_dir = "/fs/ess/PDE0001/311/Columbus/API_result/"
file_name = "columbus311_3years.geojson"
file = file_dir + file_name


with open(file, "w") as f:
    f.write(json.dumps(data, indent=4))
    
print("Data stored at: " + file)
