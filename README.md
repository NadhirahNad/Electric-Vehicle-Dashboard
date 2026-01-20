# Electric Vehicle Dashboard

An interactive **R Shiny Dashboard** for exploring **Electric Vehicle (EV) datasets**, analyzing price, efficiency, and performance metrics across brands, continents, and powertrains.

---

## Features

- **Filter and explore EV data** by brand, price, and rapid charge availability
- **Price analysis** by continent and body style
- **Performance analysis** with powertrain and range
- **Efficiency analysis** by plug type
- **Interactive visualizations**:
  - Bar plots
  - Histograms
  - Boxplots
  - Scatter plots
  - Pie charts

---

## Dashboard Overview

### Page 1: Basic Description Dataset
![Page 1](DashboardPic\EV1.png)

### Page 2: Price Analysis by Continent
![Page 2](DashboardPic\EV2.png)

### Page 3: Performance Analysis with Power Train
![Page 3](DashboardPic\EV3.png)

### Page 4: Efficiency Analysis with Plug Type
![Page 4](DashboardPic\EV4.png)

> Add your own screenshots by saving them in the `images/` folder.

---

## Getting Started

### Prerequisites

- R >= 4.5.2  
- R packages:

```r
install.packages(c(
  "shiny","shinydashboard","shinyWidgets","ggplot2",
  "gridExtra","magrittr","dplyr","reshape2","RColorBrewer"
))
