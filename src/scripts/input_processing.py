'''
Script Name: Hazard Model 2.0 Input Processing
Author: Nicholas Hollingshead, Cornell University
Description: Prepares data from the CWD Data Warehouse for the
             Hazard Model 2.0 R script.
Inputs:
  params.json
  risk_weights.json
  seasonal_migration.json
  sub_administrative_areas.ndJson
  risk_factors/ (optional)
    agricultural_practices.json
    antler_shed_collection.json
    baiting_stations.json
    captive_cervid_facilities.json
    dumping_carcasses.json
    feedgrounds.json
    free_ranging_cervid_dispersal.ndJson
    guzzlers.json
    incinerators.json
    landfill_disposal_imported_carcasses.json
    local_practices.json
    mineral_licks.json
    rendering_facilities.json
    roadkill_collection.json
    taxidermists.json
    venison_food_banks.json
    venison_processors.json
    waterbodies.json
    wildlife_rehabilitation_facilities.json
Outputs:
  DataTotals.csv
  Distance.csv
  Movement.csv
  Subadmin.csv
  Weights.csv
  info.html
  execution_log.log
'''

#############
# Environment
import sys
import os
from pathlib import Path
import json
import pathlib
import csv
import logging
import datetime
import psycopg2
from shapely.geometry import shape, mapping, MultiPoint, Point
from shapely.ops import unary_union
from shapely import STRtree, shortest_line
from pyproj import Geod

######################
# Database Credentials

pg_user = "geodata_user"
pg_password = "4vzGxZ9UDYbdxNT97yfJn56T"
pg_host = "geo.cwd-data.org"
pg_port = '5432'
pg_database = 'geodata'

##################
# FILE PATHS
data_path = Path('/data')
parameters_file = data_path / 'params.json'
risk_weights_file = data_path / 'risk_weights.json'
seasonal_migration_file_path = data_path / 'seasonal_migration.json'
subadmins_file_path = data_path / 'sub_administrative_area.ndJson'
home_range_file_path = data_path / 'cervid_home_range_size.ndJson'
model_metadata_log_file = data_path / 'attachments' / 'info.html'
logging_path = data_path / 'attachments' / 'execution_log.log'
attachments_json_path = data_path / 'attachments.json'

###################
# FUNCTIONS

def model_log(line='', filename=model_metadata_log_file):
  """
  Writes a single line to the user-facing log file with a trailing break tag.

  Args:
    line (str): Text to append to the log.
    filename (Path): Destination file path.
  """
  with open(filename, 'a') as f:
    f.write(line + '<br>' + '\n')

def model_log_html(line='', html_element="p", filename=model_metadata_log_file):
  """
  Wraps text in an HTML element and appends it to the user-facing log.

  Args:
    line (str): Content to log.
    html_element (str): HTML tag name (e.g., "p", "h4").
    filename (Path): Destination file path.
  """
  with open(filename, 'a') as f:
    f.write(f"<{html_element}>{line}</{html_element}>" + '\n')

def json_stringify(data, indent=3):
  """Custom formats a nested dictionary into a string with spaces for indentation.

  Args:
    data: The nested dictionary.
    indent: The number of spaces for indentation.

  Returns:
    A formatted string.
  """

  def format_helper(data, level):
    lines = []
    for key, value in data.items():
      if isinstance(value, dict):
        lines.append(f"{(' ' * level)}{key}:")
        lines.extend(format_helper(value, level + indent))
      else:
        lines.append(f"{(' ' * level)}{key}: {value}")
    return lines

  return '\n'.join(format_helper(data, indent))

def json_stringify_html(data, indent=3):
  """Custom formats a nested dictionary into a string with spaces for indentation and html breaks.

  Args:
    data: The nested dictionary.
    indent: The number of spaces for indentation.

  Returns:
    A formatted string.
  """

  def format_helper(data, level):
    lines = []
    for key, value in data.items():
      if isinstance(value, dict):
        lines.append(f"{(' ' * level)}{key}:<br>")
        lines.extend(format_helper(value, level + indent))
      else:
        lines.append(f"{(' ' * level)}{key}: {value}<br>")
    return lines

  return '\n'.join(format_helper(data, indent))

def dict_to_html_list(data, list_type='unordered'):
  """
  Converts a Python dictionary to an HTML string representing a list.

  Args:
    data: The input dictionary.
    list_type: 'unordered' (default) or 'ordered' to specify the list type.

  Returns:
    An HTML string representing the dictionary.
  """

  def _dict_to_html_helper(data):
    """Recursive helper function to handle nested dictionaries."""
    html_str = ""
    if list_type == 'unordered':
      html_str += "<ul>"
    elif list_type == 'ordered':
      html_str += "<ol>"
    else:
      raise ValueError("Invalid list_type. Use 'unordered' or 'ordered'.")

    for key, value in data.items():
      html_str += f"<li>{key}: "
      if isinstance(value, dict):
        html_str += _dict_to_html_helper(value)
      elif isinstance(value, list):
        html_str += "<ul>"
        for item in value:
          html_str += f"<li>{item}</li>"
        html_str += "</ul>"
      else:
        html_str += f"{value}"
      html_str += "</li>"

    if list_type == 'unordered':
      html_str += "</ul>"
    elif list_type == 'ordered':
      html_str += "</ol>"

    return html_str

  return _dict_to_html_helper(data)

def rename_key(dict_, old_key, new_key):
  """Renames a key in a dictionary.

  Args:
    dict_: The dictionary to modify.
    old_key: The key to be renamed.
    new_key: The new key name.
  """

  if old_key in dict_:
    dict_[new_key] = dict_.pop(old_key)

def add_item_to_json_file_list(file_path, new_item):
  """
  Adds a new item to the list within a JSON file.

  Args:
    file_path: Path to the JSON file.
    new_item: The item to be added to the list.

  Raises:
    FileNotFoundError: If the specified file does not exist.
    json.JSONDecodeError: If the file content is not valid JSON.
  """

  try:
    with open(file_path, 'r') as f:
      data = json.load(f)

    if isinstance(data, list):
      data.append(new_item)
    else:
      raise ValueError("The JSON file does not contain a list.")

    with open(file_path, 'w') as f:
      json.dump(data, f, indent=2)

  except FileNotFoundError:
    print(f"Error: File '{file_path}' not found.")
    raise
  except json.JSONDecodeError:
    print(f"Error: Invalid JSON in '{file_path}'.")
    raise
  except ValueError as e:
    print(f"Error: {e}")
    raise

######################
# SETUP FILE STRUCTURE

# Ensure the directory for logs and HTML feedback exists.
os.makedirs(os.path.dirname(pathlib.Path(model_metadata_log_file)), exist_ok=True)

# Initialize attachments.json as an empty list.
# This file acts as a manifest for all files the platform should recognize as outputs.
with open(attachments_json_path, 'w', newline='') as f:
  writer = json.dump(list(), f)

# Register the raw technical log for developer debugging.
attachment = {
  "filename": "execution_log.log",
  "content_type": "text/plain",
  "role": "downloadable"
  }
add_item_to_json_file_list(attachments_json_path, attachment)

# Register the HTML log for front-end user feedback.
attachment = {
  "filename": "info.html",
  "content_type": "text/html",
  "role": "feedback"}
add_item_to_json_file_list(attachments_json_path, attachment)

###############
# SETUP LOGGING

# Create log file including any parent folders (if they don't already exist)
os.makedirs(os.path.dirname(pathlib.Path(logging_path)), exist_ok=True)

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path,
                    filemode = 'w', # a is append, w is overbite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(type, value, traceback):
  logging.error(f"{type} error has occurred with value: {value}. Traceback: {traceback}")
sys.excepthook = handle_uncaught_exception

## Initialize the HTML feedback log

# Clear model log file contents if necessary.
open(pathlib.Path(model_metadata_log_file), 'w').close()
model_log_html("Model Execution Summary", "h3")
model_log_html("Model: Hazard Risk Model 2")
model_log_html('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT', "p")
model_log_html('Input Processing Started', 'h4')
logging.info("Model: Hazard Risk Model 2")
logging.info('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT')
logging.info("This log records data for debugging purposes in the case of a model execution error.")

######
# MAIN

################
# Subadmin Areas

# Get subadmin areas ndjson
all_subadmin_areas = []
with open(subadmins_file_path, 'r', encoding='utf-8') as f:
  for line in f:
    if line.strip():
      all_subadmin_areas.append(json.loads(line))
logging.info(f"Loaded {len(all_subadmin_areas)} sub-administrative areas from geographic data.")
subadmin_id_to_code = {area['_id']: area.get('code') for area in all_subadmin_areas}
model_log_html("Base geographic sub-administrative area data loaded.", "p")

###############################################################################
# MODEL PARAMETERS
###############################################################################

logging.info("Loading model parameters...")

try:
  # Load the master configuration for the current model run.
  with open(pathlib.Path(parameters_file), 'r') as f:
    params = json.load(f)
    logging.info("Parameters json file loaded successfully")
except:
  # The model cannot be executed without a params file. Exit with an error immediately.
  logging.error("params.json File does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("Parameters (params.json) file not found. Execution halted.", "p")
  sys.exit(1)

# Get provider admin area
provider_admin_area = params['_provider']['_administrative_area']['administrative_area']
# Remove Provider parameter, which is not used in this model and is nested
model_log_html(f"Provider geographic focus: {provider_admin_area}", "p")
del(params['_provider'])

##############
# Subadmin.csv
# Fields: SubAdminID, FullName, Name

logging.info(f"Identifying sub-administrative units for modeling...")

subadmin_list = []

for _subadmin_id in params['_sub_administrative_areas']:
  subadmin_dict = {}
  subadmin_dict['SubAdminID'] = _subadmin_id

  # Match ID to the full record to extract human-readable names
  for subadmin_area in all_subadmin_areas:
    if subadmin_area.get('_id') == _subadmin_id:
      subadmin_dict['FullName'] = subadmin_area.get('full_name')
      continue
  subadmin_list.append(subadmin_dict)

model_log_html(f"Preparing model data for {len(subadmin_list)} sub-administrative units.", "p")

###############################################################################
# MIGRATION CORRIDORS
###############################################################################
# Pre-process migration corridors to determine how they influence proximity to
# infected areas.

logging.info("Processing seasonal migration corridors...")
geod = Geod(ellps="WGS84")
corridor_geoms = []

if seasonal_migration_file_path.exists():
  try:
    with open(seasonal_migration_file_path, 'r') as f:
      seasonal_migration_areas = json.load(f)
      logging.info("Seasonal Migration json file loaded successfully")
    # Create a list of Shapely Objects
    corridor_geoms = [shape(c["boundary"]["geometry"]) for c in seasonal_migration_areas]

  except Exception as e:
    logging.error(f"Seasonal Migration json file is invalid: {e}")
    model_log_html("ERROR", "h4")
    model_log_html("Seasonal Migration file is invalid", "p")
    sys.exit(1)
else:
  model_log_html("INFO", "h4")
  model_log_html("Seasonal Migration not included in the model.", "p")

# Calculate max migration distance per subadmin area
# 1. Merge touching seasonal_migration.json objects to form continuous corridors.
# 2. Find the "diameter" (longest axis) of the merged corridor using a convex hull.
# 3. Save as kilometers for use in the hazard calculations.

max_migration_distances = {}
# Create a mapping for subadmin geometries for faster lookup
subadmin_geom_map = {area['_id']: shape(area["boundary"]["geometry"]) for area in all_subadmin_areas}

for sa_id, sa_geom in subadmin_geom_map.items():
  # Find corridors that touch/intersect this subadmin
  touching = [c for c in corridor_geoms if c.intersects(sa_geom)]

  if not touching:
    max_migration_distances[sa_id] = 0
    continue

  # Dissolve touching corridors into a single geometry
  merged = unary_union(touching)
  hull = merged.convex_hull

  dist_km = 0
  # Calculate the longest possible movement within the corridor using the hull vertices
  if not hull.is_empty:
    if hull.geom_type == 'Point':
      dist_km = 0
    elif hull.geom_type == 'LineString':
      # For a line, the longest distance is between endpoints
      p1, p2 = hull.coords[0], hull.coords[-1]
      _, _, dist = geod.inv(p1[0], p1[1], p2[0], p2[1])
      dist_km = dist / 1000
    elif hull.geom_type == 'Polygon':
      # The diameter of a convex hull is the max distance between any two vertices
      coords = list(hull.exterior.coords)
      max_d = 0
      for i in range(len(coords)):
        for j in range(i + 1, len(coords)):
          _, _, d = geod.inv(coords[i][0], coords[i][1], coords[j][0], coords[j][1])
          if d > max_d:
            max_d = d
        dist_km = max_d / 1000

  max_migration_distances[sa_id] = round(dist_km, 2)

count_with_migration = sum(1 for d in max_migration_distances.values() if d > 0)
logging.info(f"Calculated migration distances. {count_with_migration} areas intersected corridors.")
if corridor_geoms:
  model_log_html(f"Seasonal migration data processed for {count_with_migration} areas.", "p")

###############################################################################
# WEIGHTS
###############################################################################
# Weights - required

try:
  with open(risk_weights_file, 'r') as f:
    risk_weights_json = json.load(f)
    logging.info("Risk weights json file loaded successfully")
except Exception as e:
  # The model cannot be executed without a risk weights file. Exit with an error immediately.
  logging.error(f"risk_weights.json file does not exist or is invalid: {e}")
  model_log_html("ERROR", "h4")
  model_log_html("Risk weights (risk_weights.json) file not found. Execution halted.", "p")
  sys.exit(1)
model_log_html("Risk weights successfully loaded.", "p")

# Risk factor names vary between the JSON storage and R script requirements.
# This map standardizes the keys for CSV output.
risk_factor_name_to_csv_column = {
  "Mineral licks": "MineralLicks",
  "Water bodies": "WaterBodies",
  "Feedgrounds": "Feedgrounds",
  "Guzzlers": "Guzzlers",
  "Baiting stations": "BaitingStations",
  "Agriculture practices": "AgriculturePractices",
  "Captive cervid facilities": "CaptiveCervidFacilities",
  "Wildlife rehabilitation facilities": "RehabilitationFacilities",
  "Taxidermists": "Taxidermists",
  "Venison processors": "Processors",
  "Rendering facilities": "RenderingFacilities",
  "Incinerators": "Incinerators",
  "Venison food banks": "FoodBanks",
  "Landfill disposal of imported carcasses": "Landfill",
  "Dumping imported carcasses into environment": "Dumping",
  "Antler shed collection": "Sheds",
  "Roadkill collection": "Roadkill",
  "Local practices": "LocalPractices",
  "Free-ranging cervid dispersal": "FreeRangingCervidDispersal",
  "Seasonal migration": "SeasonalMigration"
}

weights_csv_data = []
for human_readable_name, csv_factor_name in risk_factor_name_to_csv_column.items():
  row = {
    'RiskFactor': csv_factor_name,
    'UserSelection': 0, # Default to inactive; updated below based on input files.
    'MeanWeightNear': risk_weights_json.get('closer', {}).get(human_readable_name, {}).get('mean'),
    'SDWeightNear': risk_weights_json.get('closer', {}).get(human_readable_name, {}).get('std_deviation'),
    'MeanWeightFar': risk_weights_json.get('farther', {}).get(human_readable_name, {}).get('mean'),
    'SDWeightFar': risk_weights_json.get('farther', {}).get(human_readable_name, {}).get('std_deviation')
  }
  weights_csv_data.append(row)

###############################################################################
# DATA TOTALS (RISK FACTORS)
###############################################################################

risk_factor_file_paths = {
  "AgriculturePractices": data_path / "risk_factors" / "agricultural_practices.json",
  "Sheds": data_path / "risk_factors" / "antler_shed_collection.json",
  "BaitingStations": data_path / "risk_factors" / "baiting_stations.json",
  "CaptiveCervidFacilities": data_path / "risk_factors" / "captive_cervid_facilities.json",
  "Feedgrounds": data_path / "risk_factors" / "feedgrounds.json",
  "Dumping": data_path / "risk_factors" / "dumping_carcasses.json",
  "Guzzlers": data_path / "risk_factors" / "guzzlers.json",
  "Incinerators": data_path / "risk_factors" / "incinerators.json",
  "Landfill": data_path / "risk_factors" / "landfill_disposal_imported_carcasses.json",
  "LocalPractices": data_path / "risk_factors" / "local_practices.json",
  "MineralLicks": data_path / "risk_factors" / "mineral_licks.json",
  "RenderingFacilities": data_path / "risk_factors" / "rendering_facilities.json",
  "Roadkill": data_path / "risk_factors" / "roadkill_collection.json",
  "Taxidermists": data_path / "risk_factors" / "taxidermists.json",
  "FoodBanks": data_path / "risk_factors" / "venison_food_banks.json",
  "Processors": data_path / "risk_factors" / "venison_processors.json",
  "RehabilitationFacilities": data_path / "risk_factors" / "wildlife_rehabilitation_facilities.json"
}

# Make a copy of the subadmin_list for a base
import copy
Risk_factor_list = copy.deepcopy(subadmin_list)

active_risk_factors = [] # list for logging

for risk_factor, risk_factor_file_path in risk_factor_file_paths.items():
  # Get model parameters file
  if Path.exists(risk_factor_file_path):
    # If the file exists, then the user chose to use the factor.
    active_risk_factors.append(risk_factor) # Add to list of used factors for logging
    try: # Open the file and get the values
      with open(pathlib.Path(risk_factor_file_path), 'r') as f:
        risk_scores = json.load(f)
        logging.info(f"{risk_factor} json file loaded successfully")
        for sa in Risk_factor_list:
          sa[risk_factor] = risk_scores.get("data", {}).get(sa['SubAdminID'])
    except:
      # If file is present, but cannot be processed, report an error and exit.
      logging.error(f"{risk_factor}.json File cannot be processed.")
      model_log_html("ERROR", "h4")
      model_log_html(f"{risk_factor} file provided, but could not be processed. Execution halted.", "p")
      sys.exit(1)
    # Mark the weight as active in the Weights table
    for rf in weights_csv_data:
      if rf['RiskFactor'] == risk_factor: # found the right risk factor
        rf['UserSelection'] = 1
        continue

# Risk Factor: Waterbodies
if params.get('water_bodies'):
    active_risk_factors.append("WaterBodies")
    for rf in weights_csv_data:
        if rf['RiskFactor'] == "WaterBodies":
            rf['UserSelection'] = 1

# Waterbodies - Fetch water edge length (km) from PostgreSQL
if params.get('risk_factors', {}).get('water_bodies'):
  logging.info("Integrating waterbodies data from PostgreSQL...")
  try:
    conn = psycopg2.connect(
      user=pg_user,
      password=pg_password,
      host=pg_host,
      port=pg_port,
      database=pg_database)
    cur = conn.cursor()
    cur.execute('SELECT "SubadminAreaCode", "total_stream_km" FROM "admin_areas"."subadmin_areas_stream_lengths"')
    rows = cur.fetchall()
    logging.info(f"Fetched {len(rows)} rows from database.")
    stream_data = {row[0]: float(row[1]) if row[1] else 0.0 for row in rows}
    cur.close()
    conn.close()

    for sa in Risk_factor_list:
      sa_code = subadmin_id_to_code.get(sa['SubAdminID'])
      sa['WaterBodies'] = stream_data.get(sa_code, 0.0)
    model_log_html("Waterbodies integrated from database.", "p")
  except Exception as e:
    logging.error(f"Failed to fetch waterbodies data from PostgreSQL: {e}")
    model_log_html("WARNING: Could not connect to database for waterbodies. Defaulting to 0.", "p")
else:
  logging.info("Waterbodies risk factor not included.")

# Process Seasonal Migration as a Risk Factor
if params.get('_seasonal_migration'): # User has selected to include this parameter
  active_risk_factors.append("SeasonalMigration") # add the factor to the list for logging
  # Update weights_csv_data to reflect user selection
  for rf in weights_csv_data:
      if rf['RiskFactor'] == "SeasonalMigration":
          rf['UserSelection'] = 1

for sa in Risk_factor_list:
  sa['SeasonalMigration'] = max_migration_distances.get(sa['SubAdminID']) # see calculation above

# Risk Factor: Free Ranging Cervid Dispersal
# Definition the maximum distance that any cervid species within a subadmin
# will travel. The provided file is an ndjson because the user can select
# one or more demography documents (datasets).
logging.info("Processing free-ranging cervid dispersal data...")

FreeRangingCervidDispersal_file_path = data_path / "risk_factors" / "free_ranging_cervid_dispersal.ndJson"

# Get free ranging cervid dispersal
dispersal_demographies_data = []
if FreeRangingCervidDispersal_file_path.exists():
  active_risk_factors.append("FreeRangingCervidDispersal")
  for rf in weights_csv_data:
    if rf['RiskFactor'] == "FreeRangingCervidDispersal":
      rf['UserSelection'] = 1

  with open(FreeRangingCervidDispersal_file_path, 'r', encoding='utf-8') as f:
    for line in f:
      if line.strip():
        # Each line is parsed individually
        dispersal_demographies_data.append(json.loads(line))

  # Determine the max dispersal distance per subadmin area
  max_dispersal_distance = {}
  for dispersal_demography in dispersal_demographies_data:
    for sa, dist in dispersal_demography['data'].items():
      if sa in max_dispersal_distance:
        # Subadmin area already represented in data.
        # Check if "new" distance is greater. If so take it.
        if dist > max_dispersal_distance[sa]:
          max_dispersal_distance[sa] = dist
      else:
        max_dispersal_distance[sa] = dist

  model_log_html("Cervid dispersal ranges calculated.", "p")
else:
  max_dispersal_distance = {}

for sa in Risk_factor_list:
  sa['FreeRangingCervidDispersal'] = max_dispersal_distance.get(sa['SubAdminID'])

if active_risk_factors:
  model_log_html(f"Risk factors included: {', '.join(active_risk_factors)}", "p")

###############################################################################
# DATA CLEANING (FILL ZEROS)
###############################################################################
# Sub-admin areas None where they should have zero. Currently, they may have None
# because zero is implied by the lack of data

# Identify registry-based risk factors that should default to zero if missing (vs unknown/Landscape)
rf_params = params.get('risk_factors', {})
registry_factors = {
  'Taxidermists': rf_params.get('taxidermists'),
  'Processors': rf_params.get('venison_processors'),
  'RehabilitationFacilities': rf_params.get('wildlife_rehabilitation_facilities', {}).get("rehabilitators"),
  'Feedgrounds': rf_params.get("feedgrounds", {}).get("congregation"),
  'RenderingFacilities': rf_params.get("rendering_facilities", {}).get("disposal"),
  'Incinerators': rf_params.get("incinerators", {}).get("disposal"),
  'Landfill': rf_params.get("landfill_disposal_imported_carcasses", {}).get("disposal"),
  'CaptiveCervidFacilities': rf_params.get("captive_cervid_facilities")
}

# Factors that are active in parameters but might have missing data in specific sub-admin areas
factors_to_zero = [k for k, v in registry_factors.items() if v]

for sa in Risk_factor_list:
  for factor in factors_to_zero:
  # Safely check for None without raising a KeyError
    if sa.get(factor) is None:
      sa[factor] = 0

###############################################################################
# DISTANCE CALCULATIONS (PROXIMITY ANALYSIS)
###############################################################################
# Calculate the distance from the edge of the given sub-administrative area to
# the centroid of the nearest positive sub-administrative area. If migration
# corridors are used, then union the migration corridor withe the current sub-
# administrative area to determine the shortest possible path to the nearest
# positive sub-administrative area centroid.

logging.info("Calculating spatial distances to nearest positive areas...")
logging.info("Fetching positive sub-administrative area centroids from PostgreSQL...")
positive_location_geoms = []

# Proximity analysis requires known positive locations from the central DB
try:
  conn = psycopg2.connect(
    user=pg_user,
    password=pg_password,
    host=pg_host,
    port=pg_port,
    database=pg_database)
  cur = conn.cursor()
  # Fetch centroids for positive areas
  cur.execute('SELECT "latitude", "longitude" FROM "admin_areas"."subadmin_areas_centroids_CWD_positive_only"')
  rows = cur.fetchall()
  cur.close()
  conn.close()

  for row in rows:
    if row[0] is not None and row[1] is not None:
      # Note: Point takes (longitude, latitude) as (x, y)
      positive_location_geoms.append(Point(float(row[1]), float(row[0])))

  if not positive_location_geoms:
    logging.warning("No positive locations found in database. Distance calculations may be invalid.")
    model_log_html("WARNING: No CWD-positive locations found in database.", "p")
  else:
    logging.info(f"Successfully loaded {len(positive_location_geoms)} positive locations.")
    model_log_html(f"Loaded {len(positive_location_geoms)} positive locations from database.", "p")
except Exception as e:
  logging.error(f"Failed to fetch positive locations from PostgreSQL: {e}")
  model_log_html("ERROR: Could not fetch positive locations from database.", "p")

positive_location_geoms_all = MultiPoint(positive_location_geoms)

# Organize subadmin geometries for distance calculations
subadmin_geoms = []
for area in all_subadmin_areas:
  subadmin_geoms.append(
    {
      '_id': area['_id'],
      'full_name': area['full_name'],
      'geom': shape(area["boundary"]["geometry"])
    }
  )
logging.info(f"Using {len(positive_location_geoms)} positive locations for distance analysis.")

# STRtree spatial indices are used to optimize distance queries, reducing
# complexity from O(n^2) to roughly O(n log n).
corridor_tree = STRtree(corridor_geoms)
positive_tree = STRtree(positive_location_geoms)

# Empty dictionary for holding distance measurements for each subadmin area
distances = []

for subadmin in subadmin_geoms:
  area_geom = subadmin['geom']

  # Base Case: Distance from the edge of the subadmin area to the nearest positive centroid.
  nearest_positive_index = positive_tree.query_nearest(area_geom)[0]
  nearest_positive = positive_location_geoms[nearest_positive_index]

  line_base = shortest_line(area_geom, nearest_positive)
  base_meters = geod.geometry_length(line_base)
  base_dist_km = round(base_meters / 1000, 2)

  # Scenario: If a migration corridor touches the area, animals can travel
  # "through" the corridor. We check if any part of the corridor is closer
  # to a positive location than the area itself.
  overlapping_indices = corridor_tree.query(area_geom)

  touching_corridors = [
    corridor_geoms[corr] for corr in overlapping_indices
    if corridor_geoms[corr].intersects(area_geom)
  ]

  # Calculate distance considering corridors
  if touching_corridors:
    corridor_distances = []
    for corridor in touching_corridors:
      nearest_c_index = positive_tree.query_nearest(corridor)[0]
      nearest_positive = positive_location_geoms[nearest_c_index]

      # Measure shortest path from the corridor to a known positive
      line_corridor = shortest_line(corridor, nearest_positive)
      corridor_meters = geod.geometry_length(line_corridor)
      corridor_distances.append(round(corridor_meters/1000, 2))

    # Get the minimum fo any distance from any positive county to any corridor
    min_corridor_km = min(corridor_distances)

    with_corridor_dist_km = min(base_dist_km, min_corridor_km)

  else: # Use the base distance
    with_corridor_dist_km = base_dist_km

  # Store distances
  subadmin_record = {
    "SubAdminID": subadmin['_id'],
    "FullName":subadmin['full_name'],
    "DistanceToNearestPositive": base_dist_km,
    "DistanceAdjustedWithMigration": with_corridor_dist_km
  }
  distances.append(subadmin_record)

model_log_html("Proximity analysis to CWD-positive areas complete.", "p")

###############################################################################
# CERVID MOVEMENT (HOME RANGES)
###############################################################################

logging.info("Processing cervid home range data...")
# Get home range areas ndjson
home_ranges_data = []
with open(home_range_file_path, 'r', encoding='utf-8') as f:
  for line in f:
    if line.strip():
      # Each line is parsed individually
      home_ranges_data.append(json.loads(line))

max_home_range = {}
for home_range in home_ranges_data:
  for sa, area in home_range['data'].items():
    if sa in max_home_range:
      if area > max_home_range[sa]:
        max_home_range[sa] = area
    else:
      max_home_range[sa] = area

# Create Movement dictionaries
movement_list = copy.deepcopy(subadmin_list)

for sa in movement_list:
  sa['AverageMovement'] = max_home_range.get(sa.get('SubAdminID'))

model_log_html("Cervid home range data processed.", "p")


###############################################################################
# TRANSITION PROBABILITIES
###############################################################################

logging.info("Retrieving Transition Probabilities from PostgreSQL...")
try:
  conn = psycopg2.connect(
    user=pg_user,
    password=pg_password,
    host=pg_host,
    port=pg_port,
    database=pg_database)
  cur = conn.cursor()
  # Select specific columns to match the required CSV structure for the R script
  cur.execute('SELECT "year", "county", "positive", "dist_1", "closest_bin3", "transition_prob", "pos_prob" FROM "admin_areas"."subadmin_areas_cwd_transition_probability"')
  columns = [desc[0] for desc in cur.description]
  rows = cur.fetchall()
  logging.info(f"Fetched {len(rows)} rows from database.")
  transition_prob_rows = [dict(zip(columns, row)) for row in rows]
  cur.close()
  conn.close()
except Exception as e:
  logging.error(f"Failed to fetch Transition Probabilities from PostgreSQL: {e}")
  model_log_html("ERROR", "h4")
  model_log_html("Transition Probabilities data could not be retrieved from PostgreSQL. Execution halted.", "p")
  sys.exit(1)

###############################################################################
# FILE EXPORT (CSV)
###############################################################################
logging.info("Writing model input CSV files...")

def replace_none_with_na(data_list):
  """Replaces None values with 'NA' in a list of dictionaries."""
  for item in data_list:
    for key in item:
      if item[key] is None:
        item[key] = "NA"
  return data_list

# Write to Subadmin.csv
with open(data_path / "Subadmin.csv", 'w', newline='') as f:
  writer = csv.DictWriter(
    f,
    fieldnames=["SubAdminID","FullName"],
    extrasaction='ignore')
  writer.writeheader()
  writer.writerows(subadmin_list)

# Write to Weights.csv
with open(data_path / "Weights.csv", 'w', newline='') as f:
  fieldnames = ["RiskFactor", "UserSelection", "MeanWeightNear", "MeanWeightFar", "SDWeightNear", "SDWeightFar"]
  writer = csv.DictWriter(
    f,
    fieldnames=fieldnames,
    restval="NA"
    )
  writer.writeheader()
  writer.writerows(replace_none_with_na(weights_csv_data))

# Write to DataTotals.csv
with open(data_path / "DataTotals.csv", 'w', newline='') as f:
  fieldnames = [
    "SubAdminID",
    "FullName",
    "FreeRangingCervidDispersal",
    "MineralLicks",
    "WaterBodies",
    "SeasonalMigration",
    "Feedgrounds",
    "Guzzlers",
    "BaitingStations",
    "AgriculturePractices",
    "CaptiveCervidFacilities",
    "RehabilitationFacilities",
    "Taxidermists",
    "Processors",
    "RenderingFacilities",
    "Incinerators",
    "FoodBanks",
    "Landfill",
    "Dumping",
    "Sheds",
    "Roadkill",
    "LocalPractices"
    ]

  writer = csv.DictWriter(
    f,
    fieldnames=fieldnames,
    restval="NA"
    )
  writer.writeheader()
  writer.writerows(replace_none_with_na(Risk_factor_list))

# Write Distance.csv
with open(data_path / "Distance.csv", 'w', newline='') as f:
  fieldnames = ["SubAdminID", "FullName", "DistanceToNearestPositive", "DistanceAdjustedWithMigration"]
  writer = csv.DictWriter(
    f,
    fieldnames=fieldnames,
    restval="NA"
    )
  writer.writeheader()
  writer.writerows(replace_none_with_na(distances))

# Write to Movement.csv
with open(data_path / "AverageMovement.csv", 'w', newline='') as f:
  writer = csv.DictWriter(
    f,
    # quoting=csv.QUOTE_NONNUMERIC,
    fieldnames=["SubAdminID","FullName", "AverageMovement"],
    extrasaction='ignore',
    restval="NA")
  writer.writeheader()
  writer.writerows(replace_none_with_na(movement_list))

# Write to CWD_Transition_Probability.csv
with open(data_path / "CWD_Transition_Probability.csv", 'w', newline='') as f:
  fieldnames = ["year", "county", "positive", "dist_1", "closest_bin3", "transition_prob", "pos_prob"]
  writer = csv.DictWriter(
    f,
    fieldnames=fieldnames,
    extrasaction='ignore',
    restval="NA")
  writer.writeheader()
  writer.writerows(replace_none_with_na(transition_prob_rows))

model_log_html("Model input files (CSVs) generated successfully.", "p")

######################
# Export GeoJSON Files (Optional)
# Exports processed geometries for visualization in GIS software. The resulting
# data are not used by the CWD Data Warehouse or the model.

# logging.info("Exporting diagnostic GeoJSON files...")
# def export_to_geojson(features, output_path):
#     geojson = {
#         "type": "FeatureCollection",
#         "features": features
#     }
#     with open(output_path, 'w') as f:
#         json.dump(geojson, f)
#     logging.info(f"Exported GeoJSON to {output_path}")

# # 1. Positive Locations
# positive_features = []
# for i, geom in enumerate(positive_location_geoms):
#     positive_features.append({
#         "type": "Feature",
#         "properties": {"id": i, "label": "Positive Location"},
#         "geometry": mapping(geom)
#     })
# export_to_geojson(positive_features, data_path / "positive_locations.geojson")

# # 2. Migration Corridors
# migration_features = []
# for i, geom in enumerate(corridor_geoms):
#     migration_features.append({
#         "type": "Feature",
#         "properties": {"id": i, "label": "Migration Corridor"},
#         "geometry": mapping(geom)
#     })
# export_to_geojson(migration_features, data_path / "migration_corridors.geojson")

# # 3. Subadmin Areas
# subadmin_features = []
# for sa in subadmin_geoms:
#     subadmin_features.append({
#         "type": "Feature",
#         "properties": {"SubAdminID": sa['_id'], "FullName": sa['full_name']},
#         "geometry": mapping(sa['geom'])
#     })
# export_to_geojson(subadmin_features, data_path / "subadmin_areas.geojson")

# model_log_html("Input processing complete. Handing off to hazard model.", "p")