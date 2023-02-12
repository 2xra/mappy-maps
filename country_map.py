import plotly.express as px

# Define the average temperature for each country
country_temperatures = {
    "Argentina": "included",
    "Australia": "included",
    "Austria": "included",
    "Bangladesh": "included",
    "Belgium": "included",
    "Brazil": "included",
    "Canada": "included",
    "Chile": "included",
    "Colombia": "included",
    "Ivory Coast": "included",
    "Germany": "included",
    "Denmark": "included",
    "Egypt": "included",
    "Spain": "included",
    "Finland": "included",
    "France": "included",
    "United Kingdom": "included",
    "Greece": "included",
    "Hong Kong": "included", #not on map
    "Indonesia": "included",
    "India": "included",
    "Israel": "included",
    "Italy": "included",
    "Jamaica": "included",
    "Jordan": "included",
    "Japan": "included",
    "Korea": "included",
    "Luxembourg": "included",
    "Mexico": "included",
    "Malaysia": "included",
    "Morocco": "included",
    "Nigeria": "included",
    "Netherlands": "included",
    "Norway": "included",
    "New Zealand": "included",
    "Pakistan": "included",
    "Peru": "included",
    "Philippines": "included",
    "Portugal": "included",
    "Singapore": "included", #not on map
    "Sweden": "included",
    "Thailand": "included",
    "Turkey": "included",
    "Taiwan": "included",
    "USA": "included",
    "Venezuela": "included",
    "Zimbabwe": "included",
    
}

# Create a map of the average temperature in each country
fig = px.choropleth(
    locations=list(country_temperatures.keys()),
    locationmode="country names",
    color=list(country_temperatures.values()),
    title="Whole sample countries",
    
)

# Show the map
fig.show()