import { 
  useEffect, 
  useState 
} from "react";
import { 
  PLOT_INFO, 
  FILTERS 
} from "../../config";
import { 
  school_values, 
  grouping_values, 
  GetImage, 
  constructUrl
} from "../utils/SidebarUtils";
import Form from "react-bootstrap/Form";

import "./Sidebar.css";

export default function Sidebar({
  local_authority,
  school_selection,
  setSchoolSelection,
  group_selection,
  setGroupSelection,
  year_selection,
  setYearSelection,
  choro_info
}) {

  const [plotUrl, setPlotURL] = useState(null);
  const [filters, setFilters] = useState(null);
  const [plot_info, setPlotInfo] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response_plot_info = await fetch(
            PLOT_INFO
        );
        const response_filter = await fetch(
          FILTERS
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
        setPlotURL(plot_info_to_use);
      } else {
        setPlotURL(null);
      }
    }
  }, [plot_info, local_authority, school_selection, group_selection, year_selection]);

  const getSegregationData = () => {
    if (!choro_info || local_authority.name === "Select local authority by clicking on the map") {
      return null;
    }
    
    const key = `${local_authority.link}_${year_selection}_${group_selection}_${school_selection}`;
    return choro_info[key] || null;
  };

  const segData = getSegregationData();

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
          <Form.Group className="form-group-custom" controlId="formYearSelect">
            <Form.Label className="form-text">
              Select a year to plot for:
            </Form.Label>
            <Form.Select
              size="sm"
              value={year_selection}
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
        <p>
          <br></br>
          {local_authority["name"] === "Select local authority by clicking on the map" ? (
            local_authority["name"]
          ) : (
            <>
              <p className="selected-area-info">
                Area selected: <strong>{local_authority["name"]}</strong>
              </p>
              {segData && (
                <p>
                  Segregation index: <strong>{segData.value ? segData.value.toFixed(3) : 'N/A'}</strong><br/>
                  Colour bracket: <strong>{segData.break_label || 'N/A'}</strong>
                </p>
              )}
            </>
          )}
        </p>

        <GetImage url={plotUrl} local_authority={local_authority}></GetImage>
      </div>
    );
  }
}
