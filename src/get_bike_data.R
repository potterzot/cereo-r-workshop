#####
# Get the bike data from the workshop folder and save it in the data folder

download.file(
  url = "https://drive.google.com/file/d/1kfmFupf81DtW9cMU1VhoUfkjBtfoyvKW/view?usp=sharing",
  destfile = "data/daily_bike_data.csv"
)
