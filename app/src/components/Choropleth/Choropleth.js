// "use client";

import "leaflet/dist/leaflet.css";
import { GeoJSON, MapContainer, TileLayer, ZoomControl } from "react-leaflet";

import "./Choropleth.css";

// import localAuthorities from "../../../../data/hh_size_data.json";
import areas from "../../data/spatial_data.json";

import { useEffect, useState } from "react";
// import { noConflict } from "leaflet";

export default function Choropleth({ local_authority, changeLocalAuthority }) {
  // Initial state for selector
  const [isSelect, setIsSelect] = useState({
    name: local_authority["name"],
    link: local_authority["link"],
  });

  const [isHighlight, setIsHighlight] = useState({
    name: null,
    link: null,
  });

  // When initial state updates, then update parent state
  useEffect(() => {
    changeLocalAuthority({
      name: isSelect["name"],
      link: isSelect["link"],
    });

    // console.log("Is select within useEffect");
    // console.log(isSelect);
  }, [isSelect, changeLocalAuthority]);

  const map_box_token =
    "pk.eyJ1IjoibC1nb3JtYW4iLCJhIjoiY20wbW0yYnU5MDRqbjJrcXNyNWcycWQ5ayJ9.mqWPPtqV8lvGzHefYst7XA";

  const url = `https://api.mapbox.com/styles/v1/mapbox/light-v11/tiles/{z}/{x}/{y}?access_token=${map_box_token}`;
  const attribution =
    '<a href="https://www.mapbox.com/about/maps/">Mapbox</a> &copy <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a><strong><a href="https://labs.mapbox.com/contribute/" target="_blank"><br/>Improve this map</a></strong>';

  const position = [53, -6];

  // Some basic styles
  const styleHighlight = {
    weight: 5,
    color: "darkgrey",
    dashArray: "",
    fillOpacity: 0.7,
  };

  const styleSelected = {
    weight: 5,
    color: "darkgrey",
    dashArray: "",
    fillOpacity: 0.7,
    fillColor: "black",
  };

  const styleNormal = {
    weight: 1,
    opacity: 1,
    color: "darkgrey",
    dashArray: "3",
    fillOpacity: 0.7,
  };

  // A closure function for updating styles
  const styleClosure = (isSelect, isHighlight) => {
    return (feature) => {
      if (feature["properties"]["link"] === isSelect["link"]) {
        return styleSelected;
      }

      if (feature["properties"]["link"] === isHighlight["link"]) {
        return styleHighlight;
      }

      return {
        fillColor: getColor(feature.properties["4 people in household"]), // Adjust based on your GeoJSON properties
        weight: 1,
        opacity: 1,
        color: "darkgrey",
        dashArray: "",
        fillOpacity: 0.7,
      };
    };
  };

  //   Function to get color based on density or any other property
  const getColor = (density) => {
    return density > 30000
      ? "#800026"
      : density > 20000
      ? "#BD0026"
      : density > 10000
      ? "#E31A1C"
      : density > 5000
      ? "#FC4E2A"
      : density > 2500
      ? "#FD8D3C"
      : density > 1000
      ? "#FEB24C"
      : density > 10
      ? "#FED976"
      : "#FFEDA0";
  };

  const onEachFeatureClosure = (setIsSelect, setIsHighlight) => {
    return (feature, layer) => {
      layer.on({
        mouseover: (e) => highlightFeature(e, feature, setIsHighlight),
        mouseout: (e) => resetHighlight(e, feature, setIsHighlight),
        click: (e) => clickFeature(e, feature, setIsSelect),
      });
    };
  };

  const highlightFeature = (e, feature, setIsHighlight) => {
    const layer = e.target;

    setIsHighlight({
      name: feature["properties"]["LAD23NM"],
      link: feature["properties"]["link"],
    });

    layer.setStyle(styleHighlight);
    layer.bringToFront();
  };

  const resetHighlight = (e, feature, setIsHighlight) => {
    const layer = e.target;

    layer.setStyle(styleNormal);

    layer.bringToFront();

    setIsHighlight({
      name: null,
      link: null,
    });

    // layer.bringToFront();
  };

  const clickFeature = (e, feature, setIsSelect) => {
    const layer = e.target;

    setIsSelect({
      name: feature["properties"]["LAD23NM"],
      link: feature["properties"]["link"],
    });

    layer.setStyle(styleSelected);

    layer.bringToFront();
  };

  return (
    <MapContainer center={position} zoom={7}>
      <TileLayer url={url} attribution={attribution} />
      <GeoJSON
        data={areas}
        style={styleClosure(isSelect, isHighlight)}
        onEachFeature={onEachFeatureClosure(setIsSelect, setIsHighlight)}
      />
      <ZoomControl position="topright" />
    </MapContainer>
  );
}
