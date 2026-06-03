'''
Script Name: Hazard Model 2 Output Processing
Author: Nicholas Hollingshead, Cornell University
Description: Converts output files from the Hazard Model 2
             to file formats appropriate for the CWD Data Warehouse.
Inputs: 
  OutputHazards.csv
Outputs: 
  output.json
  attachments.json
  info.html
  execution_log.log 

Date Created: 2024-08-19
Date Modified: 2024-08-20
Version: 1.0
'''

from pathlib import Path
import json
import csv
import os
import sys
import logging

data_path = Path('/data')
model_metadata_log_file = data_path / "attachments" / "info.html"
attachments_json_path = data_path / "attachments.json"
logging_path = data_path / "attachments" / "execution_log.log"
model_output_path = data_path / "attachments" / "OutputHazards.csv"
weights_csv_path = data_path / "Weights.csv"

###################
# Functions

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

def safely_convert_float(val):
    """Attempts to convert a value to float; returns None if it fails."""
    try:
        return float(val)
    except (ValueError, TypeError):
        # Handles empty strings, 'N/A', or unexpected None types gracefully
        return None
      
################
# LOGGING CONFIG

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'a', # a is append, w is overwrite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(type, value, traceback):
  logging.error(f"{type} error has occurred with value: {value}. Traceback: {traceback}")
sys.excepthook = handle_uncaught_exception

###############################################################################
# CONVERT MODEL OUTPUTS
###############################################################################

logging.info("Starting model output post-processing...")
model_log_html("Model Exports", "h3")

# Verify that the model output file exists before proceeding
if not model_output_path.exists():
    logging.error(f"Hazard model output file not found: {model_output_path}")
    model_log_html("Hazard model output was not found. Post-processing aborted.", "p")
    sys.exit(1)

try:
    model_output_dict_list = []
    with open(model_output_path, 'r') as f:
        csv_rdr = csv.DictReader(f)
        model_output_dict_list = list(csv_rdr)

    if not model_output_dict_list:
        logging.warning("Hazard model output file is empty.")
        model_log_html("Model output contains no results to export.", "p")
        sys.exit(0)

    # Identify inactive risk factors from Weights.csv to filter output fields
    inactive_factors = []
    if weights_csv_path.exists():
        with open(weights_csv_path, 'r') as f:
            for row in csv.DictReader(f):
                if row.get('UserSelection') == '0':
                    inactive_factors.append(row.get('RiskFactor'))

    # Map inactive factors to column names (elasticity 'e_' and weight elasticity 'ew_')
    cols_to_exclude = []
    for factor in inactive_factors:
        cols_to_exclude.extend([f"e_{factor}", f"ew_{factor}"])

    # Set excluded columns to None in the data records
    for row in model_output_dict_list:
        for col in cols_to_exclude:
            if col in row:
                row[col] = None

    # Identify numeric keys automatically, excluding protected metadata
    protected_keys = {"FullName", "SubAdminID"}
    float_keys = [k for k in model_output_dict_list[0].keys() if k not in protected_keys]

    # Perform data type conversion and rounding (to 4 decimal places)
    for each_row in model_output_dict_list:
        for each_key in float_keys:
            val = safely_convert_float(each_row[each_key])
            each_row[each_key] = round(val, 4) if val is not None else None

    # Write to JSON file for platform integration
    model_output_json_path = data_path / "attachments" / "output.json"
    with open(model_output_json_path, 'w', newline='') as f:
        json.dump(model_output_dict_list, f, indent=3)
        
    # Add output.json to the attachments list manifest
    attachment = {"filename": "output.json", "content_type": "application/json", "role": "primary"}
    add_item_to_json_file_list(attachments_json_path, attachment)
    
    logging.info("Post-processing complete. output.json created.")
    model_log_html("Model exports successfully created.", "p")

except Exception as e:
    logging.exception("An error occurred during output processing.")
    model_log_html("An internal error occurred during file export.", "p")
    sys.exit(1)
