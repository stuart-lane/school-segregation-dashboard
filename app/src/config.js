// Use GitHub raw URLs for data files
const GITHUB_RAW_BASE = "https://raw.githubusercontent.com/stuart-lane/school-segregation-dashboard/refs/heads/main/segDataPrep/outputs";
// export const BASE_URL = "."

// Links
export const SPATIAL_DATA_URL = `${GITHUB_RAW_BASE}/spatial_data.geojson`;
export const CHORO_DATA_URL = `${GITHUB_RAW_BASE}/seg_indices.json`;
export const FILTERS = `${GITHUB_RAW_BASE}/filters.json`;
export const PLOT_INFO = `${GITHUB_RAW_BASE}/plot_information.json`;

// Map defaults
export const MAP_TILES_URL = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
export const ATTRIBUTION = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
export const MAP_POSITION = [53, -6] // Default coordinates

export const BASE_URL = "https://raw.githubusercontent.com/stuart-lane/school-segregation-dashboard/refs/heads/main/segDataPrep";