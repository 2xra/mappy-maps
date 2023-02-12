import plotly.express as px

# Define the average temperature for each country
country_temperatures = {
    "Australia": "included",
    "Argentina": "not",
    "Austria": "included",
    "Bangladesh": "included",
    "Belgium": "included",
    "Brazil": "not",
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
    "Israel": "not",
    "Italy": "included",
    "Jamaica": "included",
    "Jordan": "not",
    "Japan": "included",
    "Korea": "included",
    "Luxembourg": "not",
    "Mexico": "not",
    "Malaysia": "included",
    "Morocco": "included",
    "Nigeria": "included",
    "Netherlands": "included",
    "Norway": "included",
    "New Zealand": "included",
    "Pakistan": "not",
    "Peru": "not",
    "Philippines": "not",
    "Portugal": "included",
    "Singapore": "included", #not on map
    "Sweden": "included",
    "Thailand": "not",
    "Turkey": "included",
    "Taiwan": "not",
    "USA": "included",
    "Venezuela": "not",
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