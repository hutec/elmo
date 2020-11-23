"""This module contains the Flask App."""

import time
import json
import os

from flask import request
from flask import Flask
from flask import redirect
from flask import jsonify
from flask import json
import requests

import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

import secrets
import swagger_client
from strava import activity_to_dict
from strava import refresh_user
from models import db, User, Route, get_and_store_routes

RESPONSE_TYPE = "code"
SCOPE = "read_all,activity:read_all,activity:read,profile:read_all"


def create_app():
    app = Flask(__name__)
    app.config["SQLALCHEMY_DATABASE_URI"] = "sqlite:///elmo.db"
    app.config["SQLALCHEMY_TRACK_MODIFICATIONS"] = False
    db.init_app(app)
    return app


app = create_app()


@app.route("/users")
def list_users():
    """List all users."""
    users = User.query.all()
    users = [{"id": str(u.id), "name": u.firstname} for u in users]
    response = jsonify(users)
    response.headers.add("Access-Control-Allow-Origin", "*")
    return response


@app.route("/<user_id>/routes")
def list_routes(user_id):
    """List all routes of a user."""
    user_id = int(user_id)
    routes = (
        Route.query.filter_by(user_id=user_id).order_by(Route.start_date.desc()).all()
    )
    routes = list(map(lambda r: r.to_json(), routes))
    response = jsonify(routes)
    response.headers.add("Access-Control-Allow-Origin", "*")
    return response


@app.route("/<user_id>/heatmap")
def heatmap(user_id):
    user_id = int(user_id)
    routes = Route.query.filter_by(user_id=user_id).all()
    routes = list(map(lambda r: r.to_json(), routes))

    min_lat = request.args.get("minlat", type=float)
    max_lat = request.args.get("maxlat", type=float)
    min_lon = request.args.get("minlon", type=float)
    max_lon = request.args.get("maxlon", type=float)
    lat_range = [min_lat, max_lat]
    lon_range = [min_lon, max_lon]

    all_coords = []
    for r in routes:
        all_coords.extend(r["route"])

    all_coords = np.array(all_coords)

    res = np.histogram2d(
        all_coords[:, 0], all_coords[:, 1], range=[lat_range, lon_range], bins=50
    )

    heat = res[0].T

    heat = (heat != 0) * 0.5

    # heat = np.log(heat)
    # heat[np.isneginf(heat)] = 0
    # heat = heat/heat.max()

    upper = np.meshgrid(res[1][:-1], res[2][:-1])
    lower = np.meshgrid(res[1][1:], res[2][1:])

    mesh = np.stack([*lower, *upper, heat], axis=-1)
    mesh = mesh.reshape(-1, 5)

    response = jsonify(mesh.tolist())
    response.headers.add("Access-Control-Allow-Origin", "*")
    return response


@app.route("/start")
def authenticate():
    """Starts the authentication process."""
    url = f"https://www.strava.com/oauth/authorize?client_id={secrets.STRAVA_CLIENT_ID}&response_type=code&redirect_uri={secrets.REDIRECT_URI}&scope={SCOPE}"
    return redirect(url)


@app.route("/user_token_exchange")
def user_token_exchange():
    """Receive the user code and query Strava to get the final access token."""
    user_code = request.args.get("code")
    scopes = request.args.get("scope")

    r = requests.post(
        "https://www.strava.com/api/v3/oauth/token",
        data={
            "client_id": secrets.STRAVA_CLIENT_ID,
            "client_secret": secrets.STRAVA_CLIENT_SECRET,
            "code": user_code,
            "grant_type": "authorization_code",
        },
    )
    user = User.from_json(r.json())
    db.session.add(user)
    db.session.commit()
    get_and_store_routes(user)

    return f"Welcome {user}"


if __name__ == "__main__":
    app.run(debug=True)
