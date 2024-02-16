set datafile separator ','
set xdata time
set timefmt "%m/%d/$y"
set format x "%m/%d"
set key autotitle columnheader
set title "{/:Bold Selected T-Bill Trends}"
set title font ",16"
set key font ",12"
set key bottom right box w 3

plot for [i=2:*] 'savedData.csv' using 1:i with points, for [i=2:*] 'lsq.csv' using 1:i with lines 
#plot for [i=2:*] 'lsq.csv' using 1:i with lines  
