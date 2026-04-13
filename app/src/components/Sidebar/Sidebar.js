/*

import { useEffect, useState } from "react";
import { PLOT_INFO, FILTERS } from "../../config";
import { school_values, grouping_values, GetImage, constructUrl } from "../utils/SidebarUtils";
import Form from "react-bootstrap/Form";
import { ToggleButtonGroup, ToggleButton, Button } from 'react-bootstrap';

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

  console.log("Plot URL:", plotUrl);

  if (loading) {
    return (
      <div className="sidebar-content">
        <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>
        <p>Loading...</p>
      </div>
    );
  }

  // Get min and max years for slider
  const years = filters["year"] || [];
  const minYear = years.length > 0 ? Math.min(...years.map(y => parseInt(y))) : 2020;
  const maxYear = years.length > 0 ? Math.max(...years.map(y => parseInt(y))) : 2024;
  const currentYear = parseInt(year_selection);

  const handleYearDecrement = () => {
    if (currentYear > minYear) {
      setYearSelection((currentYear - 1).toString());
    }
  };

  const handleYearIncrement = () => {
    if (currentYear < maxYear) {
      setYearSelection((currentYear + 1).toString());
    }
  };

  if (!loading) {
    return (
      <div className="sidebar-content">
        <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>

        <Form className="selector-form">
          <Form.Group className="form-group-custom">
            <Form.Label className="form-text">
              School type:
            </Form.Label>
            <ToggleButtonGroup 
              type="radio" 
              name="school" 
              value={school_selection}
              onChange={setSchoolSelection}
              className="custom-toggle-group"
            >
              {school_values.map((group) => (
                <ToggleButton 
                  key={group.value} 
                  id={`tbg-school-${group.value}`} 
                  value={group.value}
                  variant="outline-primary"
                  className="custom-toggle-button"
                >
                  {group.name}
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
          </Form.Group>

          <Form.Group className="form-group-custom">
            <Form.Label className="form-text">
              Grouping type:
            </Form.Label>
            <ToggleButtonGroup 
              type="radio" 
              name="grouping" 
              value={group_selection}
              onChange={setGroupSelection}
              className="custom-toggle-group"
            >
              {grouping_values.map((group) => (
                <ToggleButton 
                  key={group.value} 
                  id={`tbg-group-${group.value}`} 
                  value={group.value}
                  variant="outline-primary"
                  className="custom-toggle-button"
                >
                  {group.name}
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
          </Form.Group>

          <Form.Group className="form-group-custom">
            <Form.Label className="form-text">
              Year: <strong>{year_selection}</strong>
            </Form.Label>
            <div className="slider-container">
              <Button 
                variant="link" 
                className="slider-arrow-button"
                onClick={handleYearDecrement}
                disabled={currentYear <= minYear}
              >
                ‹
              </Button>
              <Form.Range
                min={minYear}
                max={maxYear}
                value={currentYear}
                onChange={(e) => setYearSelection(e.target.value)}
                className="custom-slider"
              />
              <Button 
                variant="link" 
                className="slider-arrow-button"
                onClick={handleYearIncrement}
                disabled={currentYear >= maxYear}
              >
                ›
              </Button>
            </div>
            <div className="slider-labels">
              <span>{minYear}</span>
              <span>{maxYear}</span>
            </div>
          </Form.Group>
        </Form>
        
        <div>
          <br />
          {local_authority["name"] === "Select local authority by clicking on the map" ? (
            <p>{local_authority["name"]}</p>
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
        </div>

        <GetImage url={plotUrl} local_authority={local_authority}></GetImage>
      </div>
    );
  }
}

*/



/*

import { useEffect, useState } from "react";
import { FILTERS } from "../../config";
import { school_values, grouping_values, GetImage, constructUrl } from "../utils/SidebarUtils";
import Form from "react-bootstrap/Form";
import { ToggleButtonGroup, ToggleButton, Button } from 'react-bootstrap';

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
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response_filter = await fetch(FILTERS);
        const response_filter_data = await response_filter.json();

        setFilters(response_filter_data);
        setLoading(false);
      } catch (error) {
        console.error("Error fetching JSON:", error);
      }
    };

    fetchData();
  }, []);

  useEffect(() => {
    // Construct URL directly from state - no need for plot_info JSON!
    const plotUrl = constructUrl({
      local_authority,
      school_selection,
      group_selection,
      year_selection      
    });

    setPlotURL(plotUrl);
  }, [local_authority, school_selection, group_selection, year_selection]);

  const getSegregationData = () => {
    if (!choro_info || local_authority.name === "Select local authority by clicking on the map") {
      return null;
    }
    
    const key = `${local_authority.link}_${year_selection}_${group_selection}_${school_selection}`;
    return choro_info[key] || null;
  };

  const segData = getSegregationData();

  console.log("Plot URL:", plotUrl);

  if (loading) {
    return (
      <div className="sidebar-content">
        <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>
        <p>Loading...</p>
      </div>
    );
  }

  // Get min and max years for slider
  const years = filters["year"] || [];
  const minYear = years.length > 0 ? Math.min(...years.map(y => parseInt(y))) : 2020;
  const maxYear = years.length > 0 ? Math.max(...years.map(y => parseInt(y))) : 2024;
  const currentYear = parseInt(year_selection);

  const handleYearDecrement = () => {
    if (currentYear > minYear) {
      setYearSelection((currentYear - 1).toString());
    }
  };

  const handleYearIncrement = () => {
    if (currentYear < maxYear) {
      setYearSelection((currentYear + 1).toString());
    }
  };

  if (!loading) {
    return (
      <div className="sidebar-content">
        <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>

        <Form className="selector-form">
          <Form.Group className="form-group-custom">
            <Form.Label className="form-text">
              School type:
            </Form.Label>
            <ToggleButtonGroup 
              type="radio" 
              name="school" 
              value={school_selection}
              onChange={setSchoolSelection}
              className="custom-toggle-group"
            >
              {school_values.map((group) => (
                <ToggleButton 
                  key={group.value} 
                  id={`tbg-school-${group.value}`} 
                  value={group.value}
                  variant="outline-primary"
                  className="custom-toggle-button"
                >
                  {group.name}
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
          </Form.Group>

          <Form.Group className="form-group-custom">
            <Form.Label className="form-text">
              Grouping type:
            </Form.Label>
            <ToggleButtonGroup 
              type="radio" 
              name="grouping" 
              value={group_selection}
              onChange={setGroupSelection}
              className="custom-toggle-group"
            >
              {grouping_values.map((group) => (
                <ToggleButton 
                  key={group.value} 
                  id={`tbg-group-${group.value}`} 
                  value={group.value}
                  variant="outline-primary"
                  className="custom-toggle-button"
                >
                  {group.name}
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
          </Form.Group>

          <Form.Group className="form-group-custom">
            <Form.Label className="form-text">
              Year: <strong>{year_selection}</strong>
            </Form.Label>
            <div className="slider-container">
              <Button 
                variant="link" 
                className="slider-arrow-button"
                onClick={handleYearDecrement}
                disabled={currentYear <= minYear}
              >
                ‹
              </Button>
              <Form.Range
                min={minYear}
                max={maxYear}
                value={currentYear}
                onChange={(e) => setYearSelection(e.target.value)}
                className="custom-slider"
              />
              <Button 
                variant="link" 
                className="slider-arrow-button"
                onClick={handleYearIncrement}
                disabled={currentYear >= maxYear}
              >
                ›
              </Button>
            </div>
            <div className="slider-labels">
              <span>{minYear}</span>
              <span>{maxYear}</span>
            </div>
          </Form.Group>
        </Form>
        
        <div>
          <br />
          {local_authority["name"] === "Select local authority by clicking on the map" ? (
            <p>{local_authority["name"]}</p>
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
        </div>

        <GetImage url={plotUrl} local_authority={local_authority}></GetImage>
      </div>
    );
  }
}


*/


import { useEffect, useState } from "react";
import { FILTERS } from "../../config";
import {
  school_values,
  grouping_values,
  GetImage,
  constructUrl,
  constructTimeSeriesUrl,
} from "../utils/SidebarUtils";
import Form from "react-bootstrap/Form";
import { ToggleButtonGroup, ToggleButton, Button } from "react-bootstrap";

import "./Sidebar.css";

export default function Sidebar({
  local_authority,
  school_selection,
  setSchoolSelection,
  group_selection,
  setGroupSelection,
  year_selection,
  setYearSelection,
  choro_info,
}) {
  const [plotUrl, setPlotURL] = useState(null);
  const [timeSeriesUrl, setTimeSeriesUrl] = useState(null);
  const [filters, setFilters] = useState(null);
  const [loading, setLoading] = useState(true);

  // Fetch filter data on mount
  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await fetch(FILTERS);
        const data = await response.json();
        setFilters(data);
        setLoading(false);
      } catch (error) {
        console.error("Error fetching filters:", error);
      }
    };
    fetchData();
  }, []);

  // Rebuild plot URL whenever relevant state changes
  useEffect(() => {
    const url = constructUrl({
      local_authority,
      school_selection,
      group_selection,
      year_selection,
    });
    setPlotURL(url);
  }, [local_authority, school_selection, group_selection, year_selection]);

  // Rebuild time series URL whenever local authority changes
  useEffect(() => {
    const url = constructTimeSeriesUrl({ local_authority });
    setTimeSeriesUrl(url);
  }, [local_authority]);

  const getSegregationData = () => {
    if (
      !choro_info ||
      local_authority.name === "Select local authority by clicking on the map"
    ) {
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

  const years = filters["year"] || [];
  const minYear =
    years.length > 0 ? Math.min(...years.map((y) => parseInt(y))) : 2020;
  const maxYear =
    years.length > 0 ? Math.max(...years.map((y) => parseInt(y))) : 2024;
  const currentYear = parseInt(year_selection);

  const handleYearDecrement = () => {
    if (currentYear > minYear) setYearSelection((currentYear - 1).toString());
  };

  const handleYearIncrement = () => {
    if (currentYear < maxYear) setYearSelection((currentYear + 1).toString());
  };

  return (
    <div className="sidebar-content">
      <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>

      <Form className="selector-form">
        {/* School type */}
        <Form.Group className="form-group-custom">
          <Form.Label className="form-text">School type:</Form.Label>
          <ToggleButtonGroup
            type="radio"
            name="school"
            value={school_selection}
            onChange={setSchoolSelection}
            className="custom-toggle-group"
          >
            {school_values.map((group) => (
              <ToggleButton
                key={group.value}
                id={`tbg-school-${group.value}`}
                value={group.value}
                variant="outline-primary"
                className="custom-toggle-button"
              >
                {group.name}
              </ToggleButton>
            ))}
          </ToggleButtonGroup>
        </Form.Group>

        {/* Grouping type */}
        <Form.Group className="form-group-custom">
          <Form.Label className="form-text">Grouping type:</Form.Label>
          <ToggleButtonGroup
            type="radio"
            name="grouping"
            value={group_selection}
            onChange={setGroupSelection}
            className="custom-toggle-group"
          >
            {grouping_values.map((group) => (
              <ToggleButton
                key={group.value}
                id={`tbg-group-${group.value}`}
                value={group.value}
                variant="outline-primary"
                className="custom-toggle-button"
              >
                {group.name}
              </ToggleButton>
            ))}
          </ToggleButtonGroup>
        </Form.Group>

        {/* Year slider */}
        <Form.Group className="form-group-custom">
          <Form.Label className="form-text">
            Year: <strong>{year_selection}</strong>
          </Form.Label>
          <div className="slider-container">
            <Button
              variant="link"
              className="slider-arrow-button"
              onClick={handleYearDecrement}
              disabled={currentYear <= minYear}
            >
              ‹
            </Button>
            <Form.Range
              min={minYear}
              max={maxYear}
              value={currentYear}
              onChange={(e) => setYearSelection(e.target.value)}
              className="custom-slider"
            />
            <Button
              variant="link"
              className="slider-arrow-button"
              onClick={handleYearIncrement}
              disabled={currentYear >= maxYear}
            >
              ›
            </Button>
          </div>
          <div className="slider-labels">
            <span>{minYear}</span>
            <span>{maxYear}</span>
          </div>
        </Form.Group>
      </Form>

      {/* Local authority info */}
      <div>
        <br />
        {local_authority.name ===
        "Select local authority by clicking on the map" ? (
          <p>{local_authority.name}</p>
        ) : (
          <>
            <p className="selected-area-info">
              Area selected: <strong>{local_authority.name}</strong>
            </p>
            {segData && (
              <p>
                Segregation index:{" "}
                <strong>
                  {segData.value ? segData.value.toFixed(3) : "N/A"}
                </strong>
                <br />
                Colour bracket:{" "}
                <strong>{segData.break_label || "N/A"}</strong>
              </p>
            )}
          </>
        )}
      </div>

      {/* Segregation plot */}
      <GetImage url={plotUrl} local_authority={local_authority} />

      {/* Time series plot */}
      <GetImage url={timeSeriesUrl} local_authority={local_authority} />
    </div>
  );
}