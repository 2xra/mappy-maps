import plotly.express as px

# Define the average temperature for each state
state_temperatures = {
    "AL": 75,
    "AK": 32,
    "AZ": 60,
    "AR": 68,
    "CA": 60,
    "CO": 50,
    "CT": 55,
    "DE": 65,
    "FL": 75,
    "GA": 70,
    "HI": 75,
    "ID": 50,
    "IL": 60,
    "IN": 60,
    "IA": 55,
    "KS": 65,
    "KY": 65,
    "LA": 75,
    "ME": 40,
    "MD": 65,
    "MA": 55,
    "MI": 50,
    "MN": 45,
    "MS": 70,
    "MO": 65,
    "MT": 45,
    "NE": 60,
    "NV": 60,
    "NH": 45,
    "NJ": 65,
    "NM": 60,
    "NY": 55,
    "NC": 70,
    "ND": 45,
    "OH": 60,
    "OK": 65,
    "OR": 55,
    "PA": 60,
    "RI": 55,
    "SC": 75,
    "SD": 50,
    "TN": 68,
    "TX": 75,
    "UT": 50,
    "VT": 45,
    "VA": 65,
    "WA": 55,
    "WV": 60,
    "WI": 45,
    "WY": 45
}

# Create a map of the average temperature in each state
fig = px.choropleth(
    locations=list(state_temperatures.keys()),
    locationmode="USA-states",
    color=list(state_temperatures.values()),
    color_discrete_sequence=["black", "white"],
    title="Average Temperature by State",
    scope="usa",
    labels={'color':'Average Temperature'}
)

# Show the map
fig.show()