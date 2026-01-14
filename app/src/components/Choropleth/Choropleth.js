import { useEffect, useState } from "react";
import { 
  GeoJSON, 
  MapContainer, 
  TileLayer, 
  ZoomControl 
} from "react-leaflet";
import { 
  SPATIAL_DATA_URL, 
  CHORO_DATA_URL, 
  MAP_TILES_URL,
  ATTRIBUTION
} from "../../config";
import {
  getColorFromValue,
  styleHighlight,
  styleNormal
} from "../utils/ChoroplethUtils"

// CSS imports
import "leaflet/dist/leaflet.css";
import "./Choropleth.css";


export default function Choropleth({ 
  local_authority, 
  changeLocalAuthority,
  school_selection,
  group_selection,
  year_selection,
  setChoroInfo
}) {

  // Initial state for selector
  const [isSelect, setIsSelect] = useState({
    name: local_authority["name"],
    link: local_authority["link"],
  });

  const [areas, setAreaData] = useState(null)
  const [loading, setLoading] = useState(true)
  const [valueCache, setValueCache] = useState(null)
  const [choro_info, setChoroInfoLocal] = useState(null);

  useEffect(() => {
    const fetchData = async () => {
      try {
        console.log('Attempting to fetch:', SPATIAL_DATA_URL);
        const response = await fetch(SPATIAL_DATA_URL)
        console.log('Response status:', response.status);
        
        const text = await response.text();
        const data = JSON.parse(text);
        setAreaData(data)

        const response_choro = await fetch(CHORO_DATA_URL)
        const text_choro = await response_choro.text();
        const data_choro = JSON.parse(text_choro);
        
        const choro_info_dict = {}
        data_choro.forEach(element => {
          const key = `${element["link"]}_${element["year"]}_${element["group"]}_${element["school"]}`
          
          choro_info_dict[key] = {
            'colour': element['colour'],
            'value': element['value'],        
            'break_label': element['break_label']
          }
        })

        console.log('Choropleth data loaded (first 100):', 
          Object.entries(choro_info_dict).slice(0, 100)
        );
        setChoroInfoLocal(choro_info_dict);
        setChoroInfo(choro_info_dict); 
        setLoading(false);
      } catch (error) {
        console.error('Error fetching JSON:', error)
      }
    }

    fetchData()
  }, [setChoroInfo])

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
  }, [isSelect, changeLocalAuthority]);

  const url = MAP_TILES_URL;
  const attribution = ATTRIBUTION;
  const position = [53, -6];

  useEffect(() => {
    if (choro_info) {
      const currentKeys = Object.keys(choro_info).filter(k => 
        k.includes(`_${year_selection}_${group_selection}_${school_selection}`)
      );
      
      const values = currentKeys
        .map(key => choro_info[key].value)
        .filter(v => v !== null && v !== undefined);
      
      setValueCache(values);
      
      console.log('Value distribution:', {
        min: Math.min(...values),
        max: Math.max(...values),
        median: values.sort((a, b) => a - b)[Math.floor(values.length / 2)],
        count: values.length
      });
    }
  }, [choro_info, school_selection, group_selection, year_selection]);

  // A closure function for updating styles
  const styleClosure = (isSelect, isHighlight, school_selection, group_selection, year_selection) => {
    return (feature) => {
      const key = `${feature["properties"]["link"]}_${year_selection}_${group_selection}_${school_selection}`;
      
      let fill_colour = "grey";
      if (choro_info[key] && valueCache) {
        const value = choro_info[key].value;
        fill_colour = getColorFromValue(value, valueCache);
      }

      // Selected: thick border but keep the color
      if (feature["properties"]["link"] === isSelect["link"]) {
        return {
          fillColor: fill_colour,
          weight: 5,
          color: "gray", 
          dashArray: "",
          fillOpacity: 0.7,
        };
      }

      // Highlighted: medium border
      if (feature["properties"]["link"] === isHighlight["link"]) {
        return {
          fillColor: fill_colour,
          weight: 3,
          color: "darkgrey",
          dashArray: "",
          fillOpacity: 0.7,
        };
      }

      // Normal
      return {
        fillColor: fill_colour,
        weight: 1,
        opacity: 1,
        color: "darkgrey",
        dashArray: "3",
        fillOpacity: 0.7,
      };
    };
  };

  // Add this after choro_info is set in the useEffect
  useEffect(() => {
    if (choro_info) {
      // Log color distribution for current selection
      const currentKeys = Object.keys(choro_info).filter(k => 
        k.includes(`_${year_selection}_${group_selection}_${school_selection}`)
      );
      
      const colorCounts = {};
      currentKeys.forEach(key => {
        const color = choro_info[key].colour;
        colorCounts[color] = (colorCounts[color] || 0) + 1;
      });
      
      console.log('Color distribution for current selection:', {
        school: school_selection,
        group: group_selection,
        year: year_selection,
        total_areas: currentKeys.length,
        color_counts: colorCounts,
        unique_colors: Object.keys(colorCounts).length
      });
    }
  }, [choro_info, school_selection, group_selection, year_selection]);

  useEffect(() => {
    if (choro_info) {
      const currentKeys = Object.keys(choro_info).filter(k => 
        k.includes(`_${year_selection}_${group_selection}_${school_selection}`)
      );
      
      const colorInfo = {};
      currentKeys.forEach(key => {
        const color = choro_info[key].colour;
        const value = choro_info[key].value;
        
        if (!colorInfo[color]) {
          colorInfo[color] = {
            count: 0,
            values: [],
            break_labels: new Set()
          };
        }
        colorInfo[color].count++;
        if (value !== undefined) colorInfo[color].values.push(value);
        if (choro_info[key].break_label) {
          colorInfo[color].break_labels.add(choro_info[key].break_label);
        }
      });
      
      // Calculate min/max for each color
      Object.keys(colorInfo).forEach(color => {
        const values = colorInfo[color].values;
        colorInfo[color].min = Math.min(...values);
        colorInfo[color].max = Math.max(...values);
        colorInfo[color].break_labels = Array.from(colorInfo[color].break_labels);
        delete colorInfo[color].values; // Remove to keep output clean
      });
      
      console.log('Color scheme analysis:', {
        school: school_selection,
        group: group_selection,
        year: year_selection,
        total_areas: currentKeys.length,
        colors: colorInfo
      });
    }
  }, [choro_info, school_selection, group_selection, year_selection]);

  const onEachFeatureClosure = (setIsSelect, setIsHighlight, school_selection, group_selection, year_selection, choro_info) => {
    return (feature, layer) => {
      const key = `${feature["properties"]["link"]}_${year_selection}_${group_selection}_${school_selection}`;
      const data = choro_info && choro_info[key] ? choro_info[key] : null;
      
      const tooltipContent = `
        <strong>${feature["properties"]["LAD24NM"]}</strong><br/>
        ${data ? `
          Segregation Index: ${data.value ? data.value.toFixed(3) : 'N/A'}<br/>
          Range: ${data.break_label || 'N/A'}
        ` : 'No data available'}
      `;
      
      layer.bindTooltip(tooltipContent, {
        permanent: false,
        sticky: true
      });
      
      layer.on({
        mouseover: (e) => highlightFeature(e, feature, setIsHighlight, styleHighlight),
        mouseout: (e) => resetHighlight(e, feature, setIsHighlight, styleNormal),
        click: (e) => clickFeature(e, feature, setIsSelect),
      });
    };
  };

  const highlightFeature = (e, feature, setIsHighlight, styleHighlight) => {
    const layer = e.target;

    setIsHighlight({
      name: feature["properties"]["LAD24NM"],
      link: feature["properties"]["link"],
    });

    layer.setStyle(styleHighlight);
    layer.bringToFront();
  };

  const resetHighlight = (e, feature, setIsHighlight, styleNormal) => {
    const layer = e.target;

    layer.setStyle(styleNormal);
    layer.bringToFront();

    setIsHighlight({
      name: null,
      link: null,
    });
  };
  
  const clickFeature = (e, feature, setIsSelect) => {
    const layer = e.target;

    setIsSelect({
      name: feature["properties"]["LAD24NM"],
      link: feature["properties"]["link"],
    });

    // Get the current fill color and preserve it
    const currentStyle = layer.options;
    layer.setStyle({
      weight: 5,
      color: "darkgrey",
      dashArray: "",
      fillOpacity: 0.7,
      fillColor: currentStyle.fillColor  // Keep the existing color
    });

    layer.bringToFront();
  };

  
  if (loading) {
      return <p>Loading...</p>
    }
  if (areas) {
    return(
      <MapContainer center={position} zoom={7}>
        <TileLayer url={url} attribution={attribution} />
          <GeoJSON
            data={areas}
            style={styleClosure(isSelect, isHighlight, school_selection, group_selection, year_selection)}
            onEachFeature={onEachFeatureClosure(setIsSelect, setIsHighlight, school_selection, group_selection, year_selection, choro_info)}
          />
        <ZoomControl position="topright" />
      </MapContainer>
    );
  }
}
