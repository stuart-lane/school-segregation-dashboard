export const getColorFromValue = (value, allValues) => {
  if (value === null || value === undefined) return "#808080";
  
  const sortedValues = [...allValues].sort((a, b) => a - b);
  const n = sortedValues.length;
  
  const quantiles = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0];
  let bin = 0;
  for (let i = 0; i < quantiles.length - 1; i++) {
    const threshold = sortedValues[Math.floor(quantiles[i + 1] * n)];
    if (value <= threshold) {
      bin = i;
      break;
    }
  }
  
  // Viridis color palette (10 colors)
  const colors = [
    "#440154", "#482878", "#3e4989", "#31688e",
    "#26828e", "#1f9e89", "#35b779", "#6ece58",
    "#b5de2b", "#fde724"
  ];
  
  return colors[bin];
};

export const styleHighlight = {
	weight: 5,
	color: "darkgrey",
	dashArray: "",
	fillOpacity: 0.7,
};

export const styleNormal = {
	weight: 1,
	opacity: 1,
	color: "darkgrey",
	dashArray: "3",
	fillOpacity: 0.7,
};
