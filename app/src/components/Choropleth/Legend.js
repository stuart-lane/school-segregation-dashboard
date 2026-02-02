import React from "react";
import "./Legend.css";

export default function Legend({ 
    values 
}) {
  if (!values || values.length === 0) {
    return null;
  }

  const sortedValues = [...values].sort((a, b) => a - b);
  const n = sortedValues.length;

  const colors = [
    "#fde724", "#b5de2b", "#6ece58", "#35b779",
    "#1f9e89", "#26828e", "#31688e", "#3e4989",
    "#482878", "#440154"
  ];

  // Calculate the actual threshold values for each bin
  const quantiles = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0];
  const thresholds = quantiles.map(q => {
    const idx = Math.min(Math.floor(q * n), n - 1);
    return sortedValues[idx];
  });

  const legendItems = [];
  // Build legend from highest to lowest (reverse order)
  for (let i = colors.length - 1; i >= 0; i--) {
    const minVal = thresholds[i];
    const maxVal = thresholds[i + 1];

    legendItems.push({
      color: colors[i],
      label: `${minVal.toFixed(3)} - ${maxVal.toFixed(3)}`
    });
  }

  return (
    <div className="map-legend">
      <h4>Segregation Index</h4>
      {legendItems.map((item, index) => (
        <div key={index} className="legend-item">
          <span
            className="legend-color"
            style={{ backgroundColor: item.color }}
          />
          <span className="legend-label">{item.label}</span>
        </div>
      ))}
    </div>
  );
}