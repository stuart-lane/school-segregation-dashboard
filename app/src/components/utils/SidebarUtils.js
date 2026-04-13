/*

import { BASE_URL } from "../../config";

// SCHOOL AND SEGREGATION DICTIONARIES ==================================================
export const school_values = [
  {name: "Primary", value: "primary"},
  {name: "Secondary", value: "secondary"},
];

export const grouping_values = [
	{name: "Free School Meals", value: "fsm"},
	{name: "Race", value: "race"},
];

*/

/*

// HELPER FUNCTIONS =====================================================================
export function GetImage({ url, local_authority }) {
  if (local_authority.name === "Select local authority by clicking on the map") {
    return <></>;
  }
  if (url === null) {
    return (
      <div>
        <p>Image not available</p>
      </div>
    );
  } else {
    // Check if it's a PDF
    if (url.endsWith('.pdf')) {
      return (
        <embed 
          className="plot-image" 
          src={url} 
          type="application/pdf" 
          width="100%" 
          height="600px"
        />
      );
    }
    return <img className="plot-image" alt="segregation-plot" src={url}></img>;
  }
}

export function constructUrl({ 
  plot_info, 
  local_authority, 
  school_selection,
  group_selection,
  year_selection 
}) {
  const plotType = school_selection + "_" + group_selection;

  console.log("=== constructUrl Debug ===");
  console.log("Looking for:", {
    link: local_authority["link"],
    year: year_selection.toString(),
    plotType: plotType
  });

  // Let's see what we're searching through
  console.log("Sample plot_info entries:", plot_info.slice(0, 3));

  const plot_info_to_use = plot_info.find(
    (element) => {
      const linkMatch = element["link"] === local_authority["link"];
      const yearMatch = element["year"] === year_selection.toString();
      const typeMatch = element["plot_type_store"] === plotType;
      
      // Log first match attempt to see what's failing
      if (element["link"] === local_authority["link"]) {
        console.log("Found matching link:", {
          element: element,
          linkMatch,
          yearMatch,
          typeMatch,
          elementYear: element["year"],
          searchYear: year_selection.toString()
        });
      }
      
      return linkMatch && yearMatch && typeMatch;
    }
  );

  console.log("Found plot_info:", plot_info_to_use);

  if (plot_info_to_use) {
    const complete_url = BASE_URL + plot_info_to_use["plot_path"];
    console.log("Complete URL:", complete_url);
    return complete_url;
  } else {
    console.log("No matching plot found!");
    return undefined;
  }
}

*/

/*
// HELPER FUNCTIONS =====================================================================
export function GetImage({ url, local_authority }) {
  if (local_authority.name === "Select local authority by clicking on the map") {
    return <></>;
  }
  if (url === null) {
    return (
      <div>
        <p>Image not available</p>
      </div>
    );
  } else {
    // Check if it's a PDF
    if (url.endsWith('.pdf')) {
      return (
        <embed 
          className="plot-image" 
          src={url}
          type="application/pdf" 
          width="100%" 
          height="600px"
        />
      );
    }
    return <img className="plot-image" alt="segregation-plot" src={url}></img>;
  }
}

export function constructUrl({ 
  local_authority, 
  school_selection,
  group_selection,
  year_selection 
}) {
  // Don't construct URL if no local authority is selected
  if (!local_authority.link) {
    return null;
  }

  // Convert year format: 2009 -> "2009_10"
  const year_store = `${year_selection}_${(parseInt(year_selection) + 1).toString().slice(-2)}`;
  
  // Construct plot type: "primary_fsm"
  const plot_type_store = `${school_selection}_${group_selection}`;
  
  // Construct the full path following the pattern:
  // /2_figures_new/{year_store}/{plot_type_store}/{area}_{plot_type_store}_{year_store}.png
  const plot_path = `/2_figures_new_pdf/${year_store}/${plot_type_store}/${local_authority.link}_${plot_type_store}_${year_store}.pdf`;
  
  const complete_url = BASE_URL + plot_path;
  
  console.log("Constructed URL:", complete_url);
  
  return complete_url;
}

*/


/*

// HELPER FUNCTIONS =====================================================================
export function GetImage({ url, local_authority }) {
  if (local_authority.name === "Select local authority by clicking on the map") {
    return <></>;
  }
  if (url === null) {
    return (
      <div>
        <p>Image not available</p>
      </div>
    );
  } else {
    // SVG and regular images work the same way with <img>
    return <img className="plot-image" alt="segregation-plot" src={url}></img>;
  }
}

export function constructUrl({ 
  local_authority, 
  school_selection,
  group_selection,
  year_selection 
}) {
  // Don't construct URL if no local authority is selected
  if (!local_authority.link) {
    return null;
  }

  // Convert year format: 2009 -> "2009_10"
  const year_store = `${year_selection}_${(parseInt(year_selection) + 1).toString().slice(-2)}`;
  
  // Construct plot type: "primary_fsm"
  const plot_type_store = `${school_selection}_${group_selection}`;
  
  // Construct the full path for SVG files
  // If your SVG folder is named differently, update '2_figures_new_svg' accordingly
  const plot_path = `/2_figures_new/${year_store}/${plot_type_store}/${local_authority.link}_${plot_type_store}_${year_store}.png`;
  
  const complete_url = BASE_URL + plot_path;
  
  console.log("Constructed URL:", complete_url);
  
  return complete_url;
}

*/

import { BASE_URL } from "../../config";

// SCHOOL AND SEGREGATION DICTIONARIES ==================================================
export const school_values = [
  { name: "Primary", value: "primary" },
  { name: "Secondary", value: "secondary" },
];

export const grouping_values = [
  { name: "Free School Meals", value: "fsm" },
  { name: "Race", value: "race" },
];

// HELPER FUNCTIONS =====================================================================

export function GetImage({ url, local_authority }) {
  if (
    local_authority.name === "Select local authority by clicking on the map"
  ) {
    return <></>;
  }

  if (!url) {
    return (
      <div>
        <p>Image not available</p>
      </div>
    );
  }

  return (
    <img
      className="plot-image"
      alt="segregation-plot"
      src={url}
      key={url}
      onError={(e) => {
        console.error("Image failed to load:", url);
        e.target.style.display = "none";
      }}
      onLoad={(e) => {
        e.target.style.display = "";
      }}
    />
  );
}

export function constructUrl({
  local_authority,
  school_selection,
  group_selection,
  year_selection,
}) {
  if (!local_authority || !local_authority.link) {
    return null;
  }

  // Convert year format: "2009" -> "2009_10"
  const year_store = `${year_selection}_${(parseInt(year_selection) + 1)
    .toString()
    .slice(-2)}`;

  // Construct plot type e.g. "primary_fsm"
  const plot_type_store = `${school_selection}_${group_selection}`;

  // Use process.env.PUBLIC_URL as the base — this handles both local dev
  // and GitHub Pages deployment correctly regardless of the homepage setting
  const base = process.env.PUBLIC_URL || "";

  const url = `${base}/2_figures_new/${year_store}/${plot_type_store}/${local_authority.link}_${plot_type_store}_${year_store}.png`;

  console.log("Constructed URL:", url);

  return url;
}

export function constructTimeSeriesUrl({ local_authority }) {
  if (!local_authority || !local_authority.name || 
      local_authority.name === "Select local authority by clicking on the map") {
    return null;
  }
  const base = process.env.PUBLIC_URL || "";
  const name = local_authority.name.replace(/ /g, "_");
  return `${base}/time_series/${name}.png`;
}