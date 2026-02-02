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