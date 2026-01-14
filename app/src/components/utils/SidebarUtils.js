import { BASE_URL } from "../../config";

// SCHOOL AND SEGREGATION DICTIONARIESa
export const school_values = [
  {name: "Primary", value: "primary"},
  {name: "Secondary", value: "secondary"},
];

export const grouping_values = [
	{name: "Free School Meals", value: "fsm"},
	{name: "Race", value: "race"},
];

// HELPER FUNCTIONS
export function GetImage({
  url,
  local_authority 

}) {
  if (
    local_authority.name === "Select local authority by clicking on the map"
  ) {
    return <></>;
  }
  if (url === null) {
    return (
      <div>
        <p>Image not available</p>
      </div>
    );
  } else {
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

  const plot_info_to_use = plot_info.find(
    (element) =>
      (element["link"] === local_authority["link"]) &
      (element["year"] === year_selection.toString()) &
      (element["plot_type_store"] === plotType)
  );

  // return(undefined)
  if (plot_info_to_use) {
    
    const complete_url = BASE_URL + plot_info_to_use["plot_path"];

    return complete_url;
  } else {
    return undefined;
  }
}