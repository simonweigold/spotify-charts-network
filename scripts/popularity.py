import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import pandas as pd

# Replace 'YOUR_CLIENT_ID' and 'YOUR_CLIENT_SECRET' with your own Spotify API credentials
client_id = 'YOUR_CLIENT_ID'
client_secret = 'YOUR_CLIENT_SECRET'


# Read artist names from CSV file
artist_data = pd.read_csv('data/artist_names.csv')

# Check the format of the CSV file
print(artist_data.head())

# Initialize Spotipy with your credentials
client_credentials_manager = SpotifyClientCredentials(client_id=client_id, client_secret=client_secret)
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

# Function to get the total streams for an artist
def get_artist_streams(artist_name):
    results = sp.search(q=artist_name, type='artist', limit=1)
    if results['artists']['items']:
        streams = results['artists']['items'][0]['popularity']
        return streams
    return "Unknown"

# Create a new DataFrame to store the results
result_data = pd.DataFrame(columns=['Artist', 'Streams'])

# Iterate through the list of artists and add the total streams to the result DataFrame
for artist in artist_data['artist']:
    streams = get_artist_streams(artist)
    result_data = result_data.append({'Artist': artist, 'Streams': streams}, ignore_index=True)

# Save the result DataFrame to a new CSV file
result_data.to_csv('data/artist_popularity.csv', index=False)

# Check the format of the result CSV file
print(result_data.head())
