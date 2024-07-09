Bitcoin Powerlaw

A simple application which calculates the current and future theoretical Bitcoin price base on the Bitcoin powerlaw, with theoretical future investment returns should the powerlaw remain accurate.

You'll need:
- Docker or podman installed
- A (free) api key from CoinGecko: https://rapidapi.com/coingecko/api/coingecko
- Add API_KEY=<your_api_key> to .env file in root directory.
- In root directory run (replace podman with docker if you are using docker)
-   `podman build . -t powerlaw`
-   `podman run -p 3000:3000 powerlaw`
-   In browser navigate to localhost:3000 
