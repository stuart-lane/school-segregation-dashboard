import React, { useEffect, useState } from "react";

import Form from "react-bootstrap/Form";

import "./Sidebar.css";

// import localAuthorities from "../../../../data/hh_size_data.json";

// import filters from "../../data/filters.json";
// import plot_info from "../../data/plot_information.json";

function GetImage({
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

function constructUrl({ plot_info, local_authority, 
  school_selection,
  group_selection,
  year_selection }) {
  // console.log(plot_info);
  const plotType = school_selection + "_" + group_selection;

  // console.log(local_authority);
  // console.log(year);
  // console.log(plotType);
  // find item in plot_information.json which
  // has the same year and plot type as args
  const plot_info_to_use = plot_info.find(
    (element) =>
      (element["link"] === local_authority["link"]) &
      (element["year"] === year_selection.toString()) &
      (element["plot_type_store"] === plotType)
  );

  // console.log(plot_info_to_use);

  // return(undefined)
  if (plot_info_to_use) {
    const base_url =
      "https://raw.githubusercontent.com/JGIBristol/school-segregation-dashboard/refs/heads/main/segDataPrep";
    const complete_url = base_url + plot_info_to_use["plot_path"];

    return complete_url;
  } else {
    return undefined;
  }

  // );

  // //https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511//2_figures/2009_10/primary_fsm/dudley_primary_fsm_2009_10.png
  // const plot_path = "https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511/"+ plot_info_to_use["plot_path"];
}

export default function Sidebar({
  local_authority,
  school_selection,
  setSchoolSelection,
  group_selection,
  setGroupSelection,
  year_selection,
  setYearSelection,
}) {
  // console.log(localAuthorities['features'][0])
  // const area_to_look_at = localAuthorities.features.find((element) => element.properties["LAD21CD"] === local_authority['code'])
  // console.log(area_to_look_at)

  const school_values = [
    {
      name: "Primary",
      value: "primary",
    },
    {
      name: "Secondary",
      value: "secondary",
    },
  ];

  const grouping_values = [
    {
      name: "Free School Meals",
      value: "fsm",
    },
    {
      name: "Race",
      value: "race",
    },
  ];

  const [plotUrl, setPlotURL] = useState(null);
  // const [year, setYear] = useState(2011);
  // const [plotType, setPlotType] = useState("primary_fsm");

  const [filters, setFilters] = useState(null);
  const [plot_info, setPlotInfo] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response_plot_info = await fetch(
          "https://raw.githubusercontent.com/JGIBristol/school-segregation-dashboard/refs/heads/main/segDataPrep/outputs/plot_information.json"
        );
        const response_filter = await fetch(
          "https://raw.githubusercontent.com/JGIBristol/school-segregation-dashboard/refs/heads/main/segDataPrep/outputs/filters.json"
        );

        const plot_info_data = await response_plot_info.json();
        const response_filter_data = await response_filter.json();

        setFilters(response_filter_data);
        setPlotInfo(plot_info_data);
        setLoading(false);
      } catch (error) {
        console.error("Error fetching JSON:", error);
      }
    };

    fetchData();
  }, []);

  useEffect(() => {
    if (plot_info) {
      const plot_info_to_use = constructUrl({
        plot_info,
        local_authority,
        school_selection,
        group_selection,
        year_selection      
      });

      if (plot_info_to_use) {
        // const plot_path = "https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511/"+ plot_info_to_use["plot_path"];
        setPlotURL(plot_info_to_use);
        // console.log(plot_path);
      } else {
        setPlotURL(null);
      }
    }
  }, [plot_info, local_authority, school_selection, group_selection, year_selection]);

  if (loading) {
    return (
      <div className="sidebar-content">
        <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>
        <p>Loading...</p>
      </div>
    );
  }

  if (!loading) {
    return (
      <div className="sidebar-content">
        <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>

        <Form className="selector-form">
          <Form.Group className="form-group-custom" controlId="formPlotSelect">
            <Form.Label className="form-text">
              Select a what school you want to plot for:
            </Form.Label>

            <Form.Select
              size="sm"
              onChange={(e) => {
                setSchoolSelection(e.target.value);
              }}
            >
              {school_values.map((group) => {
                return (
                  <option key={group.value} value={group.value}>
                    {group.name}
                  </option>
                );
              })}
            </Form.Select>
            </Form.Group>

            <Form.Group className="form-group-custom" controlId="formPlotSelect">
            <Form.Label className="form-text">
              Select a what grouping you want to plot for:
            </Form.Label>

            <Form.Select
              size="sm"
              onChange={(e) => {
                setGroupSelection(e.target.value);
              }}
            >
              {grouping_values.map((group) => {
                return (
                  <option key={group.value} value={group.value}>
                    {group.name}
                  </option>
                );
              })}
            </Form.Select>
            </Form.Group>
            {/* <Form.Select
              size="sm"
              onChange={(e) => {
                setPlotType(e.target.value);
              }}
            >
              {filters["plot_type"].map((plot_type) => {
                return (
                  <option key={plot_type} value={plot_type}>
                    {plot_type}
                  </option>
                );
              })}
            </Form.Select> */}

          <Form.Group className="form-group-custom" controlId="formYearSelect">
            <Form.Label className="form-text">
              Select a year to plot for:
            </Form.Label>
            <Form.Select
              size="sm"
              onChange={(e) => {
                setYearSelection(e.target.value);
              }}
            >
              {filters["year"].map((year) => {
                return (
                  <option key={year} value={year}>
                    {year}
                  </option>
                );
              })}
            </Form.Select>
          </Form.Group>
        </Form>

        {/* Select a year to plot for:<br></br>
        <select
        
            onChange={(e) => {
            console.log(e.target.value);

            setYear(e.target.value);
            }}>
                {filters['year'].map((year) => {
                    return <option key={year} value={year}>{year}</option>;
                })}
            </select>
            <br></br>
        Select plot type: <br></br>
        <select
            onChange={(e) => {
            console.log(e.target.value);
            setPlotType(e.target.value);
            }}>
                {filters['plot_type'].map((plot_type) => {
                    return <option key={plot_type} value={plot_type}>{plot_type}</option>;
                })}
            </select> */}

        <p>
          <br></br>
          {local_authority["name"] ===
          "Select local authority by clicking on the map" ? (
            local_authority["name"]
          ) : (
            <p>
              Area Selected: <strong>{local_authority["name"]}</strong>
            </p>
          )}
        </p>

        <GetImage url={plotUrl} local_authority={local_authority}></GetImage>
      </div>
    );
  }
}
