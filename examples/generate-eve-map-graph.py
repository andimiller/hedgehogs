#! /usr/bin/env nix-shell
#! nix-shell -i python -p python3 
import sqlite3
import json

# Generates some JSON dumps from an EVE Online Static Database in sqlite format
# The data is used under the EVE Online Third Party Developer License

if __name__ == "__main__":
    con = sqlite3.connect("sqlite-latest.sqlite")
    cur = con.cursor()
    with open("systems.json", "w") as fh:
        for (systemId, name) in cur.execute("select solarSystemId, solarSystemName from mapSolarSystems"):
            fh.write(json.dumps({"id": systemId, "data": name})+"\n")
    with open("gates.json", "w") as fh:
        for (fromId, toId) in cur.execute("select fromSolarSystemID, toSolarSystemID from mapSolarSystemJumps"):
            fh.write(json.dumps({"from": fromId, "to": toId, "weight": 1})+"\n")
