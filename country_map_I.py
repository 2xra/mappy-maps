import plotly.express as px

# Define the average temperature for each country
country_temperatures = {
    "Argentina": "included",
    "Australia": "included",
    "Austria": "not",
    "Bangladesh": "not",
    "Belgium": "not",
    "Brazil": "included",
    "Canada": "not",
    "Chile": "included",
    "Colombia": "included",
    "Ivory Coast": "not",
    "Germany": "not",
    "Denmark": "not",
    "Egypt": "not",
    "Spain": "not",
    "Finland": "not",
    "France": "not",
    "United Kingdom": "included",
    "Greece": "included",
    "Hong Kong": "included", #not on map
    "Indonesia": "included",
    "India": "included",
    "Israel": "not",
    "Italy": "included",
    "Jamaica": "not",
    "Jordan": "included",
    "Japan": "included",
    "Korea": "included",
    "Luxembourg": "included",
    "Mexico": "included",
    "Malaysia": "included",
    "Morocco": "not",
    "Nigeria": "included",
    "Netherlands": "not",
    "Norway": "not",
    "New Zealand": "not",
    "Pakistan": "included",
    "Peru": "included",
    "Philippines": "included",
    "Portugal": "included",
    "Singapore": "not", #not on map
    "Sweden": "not",
    "Thailand": "included",
    "Turkey": "not",
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