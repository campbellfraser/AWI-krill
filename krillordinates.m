% krillordinates
krill_coordinates = readtable("krillordinates.csv");
latLim = [-35,-65];
lonLim = [-25,-63];
geoplot(krill_coordinates,"Lat","Lon","LineStyle","none","Marker",'*')
hold on