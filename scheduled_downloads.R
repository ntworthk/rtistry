frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
download.file(
  "https://docs.google.com/spreadsheets/d/1T9FPgB5FhqqS8wuWbGcGY-l9M7QZDIHY8-BORKXgo5o/export?gid=915433531&format=csv",
  file.path(PATH, "data/distance_to_go.csv")
)
download.file(
  "https://docs.google.com/spreadsheets/d/1bOpX7ynvyUR4cuTmenj8ZQ-9UHv_ekYdovGUhP-YCuo/export?gid=395516908&format=csv",
  file.path(PATH, "data/wedding_summary.csv")
)
download.file(
  "https://docs.google.com/spreadsheets/d/1T9FPgB5FhqqS8wuWbGcGY-l9M7QZDIHY8-BORKXgo5o/export?gid=1956137224&format=csv",
  file.path(PATH, "data/running.csv")
)
download.file(
  "https://docs.google.com/spreadsheets/d/1T9FPgB5FhqqS8wuWbGcGY-l9M7QZDIHY8-BORKXgo5o/export?gid=888983572&format=csv",
  file.path(PATH, "data/cycling.csv")
)
