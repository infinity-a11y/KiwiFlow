import sys
import unidec
import re

# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
input_file = sys.argv[1]
engine.raw_process(input_file)

# Open converted txt file
txt_file = re.sub(r"\.raw$", "_rawdata.txt", input_file)
engine.open_file(txt_file)

# Process and deconvolve the data
engine.process_data()
engine.run_unidec()
engine.pick_peaks()
