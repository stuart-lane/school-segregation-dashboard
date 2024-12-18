// 'use client'

import React, { useCallback, useState } from "react";
import Choropleth from "../Choropleth/";
import Sidebar from "../Sidebar/";

import "./MainPage.css";

export default function MainPage() {
  const [local_authority, setLocalAuthority] = useState({
    name: "Select local authority by clicking on the map",
    link: false,
  });

  const [school_selection, setSchoolSelection] = useState("primary");
  const [group_selection, setGroupSelection] = useState("fsm");
  const [year_selection, setYearSelection] = useState(2015);

  // Use callback to prevent infinite loop
  const changeLocalAuthority = useCallback((value) => {
    setLocalAuthority(value);
  }, []);

  return (
    <div className="main-container">
      {/* {local_authority} */}
      <div className="custom-sidebar">
        <Sidebar
          local_authority={local_authority}
          school_selection={school_selection}
          setSchoolSelection={setSchoolSelection}
          group_selection={group_selection}
          setGroupSelection={setGroupSelection}
          year_selection={year_selection}
          setYearSelection={setYearSelection}
        />
      </div>

      <div className="leaflet-container">
        <Choropleth
          local_authority={local_authority}
          changeLocalAuthority={changeLocalAuthority}
          school_selection={school_selection}
          setSchoolSelection={setSchoolSelection}
          group_selection={group_selection}
          setGroupSelection={setGroupSelection}
          year_selection={year_selection}
          setYearSelection={setYearSelection}
        />
      </div>
    </div>
  );
}
