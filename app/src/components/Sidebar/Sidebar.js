import React, { useEffect, useState } from "react";

import Form from 'react-bootstrap/Form';

import "./Sidebar.css";


// import localAuthorities from "../../../../data/hh_size_data.json";

import filters from "../../data/filters.json";
import plot_info from "../../data/plot_information.json";

function GetImage({ url, local_authority}) {


  if (local_authority.name === "Select local authority by clicking on the map"){
    return(<></>)
  }
  if (url === null) {
    return (
      <div>
        <p>Image not available</p>
      </div>
    );
  } else {
    return (
      <img className="plot-image"
      alt="segregation-plot"
        src={
          url
        }
      ></img>
    );
  }
}


function constructUrl({local_authority, year, plotType}){

    
    // console.log(plot_info);

    // console.log(local_authority);
    // console.log(year);
    // console.log(plotType);
    // find item in plot_information.json which
    // has the same year and plot type as args
    const plot_info_to_use = plot_info.find((element) => element["link"] === local_authority["link"] & element["year"] === year.toString() & element["plot_type_store"] === plotType);

    // console.log(plot_info_to_use);

    // return(undefined)
    if (plot_info_to_use){
    const base_url = "https://raw.githubusercontent.com/JGIBristol/school-segregation-dashboard/refs/heads/main/segDataPrep";
    const complete_url = base_url + plot_info_to_use["plot_path"];
    
    return(complete_url);
    }
    else{
        return(undefined);
    }

        
    // );



    // //https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511//2_figures/2009_10/primary_fsm/dudley_primary_fsm_2009_10.png
    // const plot_path = "https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511/"+ plot_info_to_use["plot_path"];

}

export default function Sidebar({ local_authority }) {
  // console.log(localAuthorities['features'][0])
  // const area_to_look_at = localAuthorities.features.find((element) => element.properties["LAD21CD"] === local_authority['code'])
  // console.log(area_to_look_at)

  const [plotUrl, setPlotURL] = useState(null);
  const [year, setYear] = useState(2011);
  const [plotType, setPlotType] = useState('primary_fsm');

//   useEffect(() => {
//     const plot_info_to_use = plot_info.find(
//       (element) => element["link"] === local_authority["link"]
//     );

//     if(plot_info_to_use){
//     const plot_path = "https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511/"+ plot_info_to_use["plot_path"];
//     setPlotURL(plot_path);
//     console.log(plot_path);
//     }
//   }, [local_authority]);


  useEffect(() => {
    
    const plot_info_to_use = constructUrl({local_authority, year, plotType});

    if(plot_info_to_use){
    // const plot_path = "https://raw.githubusercontent.com/l-gorman/temp-image-store/242e0fbaf7a25b8bed1351bfaae063357478e511/"+ plot_info_to_use["plot_path"];
    setPlotURL(plot_info_to_use);
    // console.log(plot_path);
    }else{
      setPlotURL(null);
    }
  }, [local_authority, year, plotType]);

  return (
    <div className="sidebar-content">
      <h1 className="sidebar-header">Spatial Analysis Dashboard Mockup</h1>

        <Form className="selector-form">
            <Form.Group className="form-group-custom" controlId="formPlotSelect">
                <Form.Label className="form-text">Select a what grouping you want to plot for:</Form.Label>
                <Form.Select  size="sm" onChange={(e) => {
                    setPlotType(e.target.value);
                }}>
                    {filters['plot_type'].map((plot_type) => {
                        return <option key={plot_type} value={plot_type}>{plot_type}</option>;
                    })}
                </Form.Select>
            </Form.Group>


            <Form.Group className="form-group-custom" controlId="formYearSelect">
                <Form.Label className="form-text" >Select a year to plot for:</Form.Label>
                <Form.Select  size="sm"  onChange={(e) => {
                    setYear(e.target.value);
                }}>
                    {filters['year'].map((year) => {
                        return <option key={year} value={year}>{year}</option>;
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

      <p><br></br>{
      (local_authority["name"]=== "Select local authority by clicking on the map") 
      ? local_authority["name"]
      :<p>Area Selected: <strong>{local_authority["name"]}</strong></p>}
      </p>

      <GetImage url={plotUrl} local_authority={local_authority}></GetImage>
    </div>
  );
}

