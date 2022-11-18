# perp-dashboard
R Shiny Dashboard for visualizing the worst pipeline incidents in a given month

## R Files:
- perp_table cleans the incident and mileage data and creates the table for the dashboard
- offshorefinder is a function built to classify incidents which lacked a clear location 
- server and ui are the main pieces of the dashboard
- helpfuns is a set of helper functions for a variety of tasks in the dashboard 

## www:
- css file for styling certain elements 
- pipeline safety trust branding files

## data 
- incident and mileage data from PHMSA 
- Census TIGER-derived shapefiles 
- all_inc is the cleaned data 
