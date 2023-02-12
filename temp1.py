import plotly.express as px

# Define the average temperature for each state
state_temperatures = {
    "AL": 729,
    "AK": 159,
    "AZ": 691,
    "AR": 563,
    "CA": -503,
    "CO": 469,
    "CT": 441,
    "DE": 125,
    "FL": 1513,
    "GA": 298,
    "HI": 156,
    "ID": 129,
    "IL": 989,
    "IN": 1839,
    "IA": 296,
    "KS": 369,
    "KY": 1524,
    "LA": 681,
    "ME": 437,
    "MD": 1123,
    "MA": 1155,
    "MI": 1167,
    "MN": 496,
    "MS": 482,
    "MO": 931,
    "MT": 166,
    "NE": 206,
    "NV": 79,
    "NH": 429,
    "NJ": 1004,
    "NM": 394,
    "NY": 38,
    "NC": 1330,
    "ND": 128,
    "OH": 4730,
    "OK": 438,
    "OR": 72,
    "PA": 3179,
    "RI": 84,
    "SC": 453,
    "SD": 200,
    "TN": 1257,
    "TX": 228,
    "UT": 151,
    "VT": 171,
    "VA": 890,
    "WA": 434,
    "WV": 1023,
    "WI": 806,
    "WY": -7
}

# Create a map of the average temperature in each state
fig = px.choropleth(
    locations=list(state_temperatures.keys()),
    locationmode="USA-states",
    color=list(state_temperatures.values()),
    color_continuous_scale="gray",
    title="Estimated excess deaths from increasing midlife mortality, 2010–2017",
    scope="usa",
    labels={'color':'Excess deaths'}
)

# Show the map
fig.show()

fig.savefig('fuckAI.jpg')