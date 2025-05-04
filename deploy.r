# deploy.R
# A simple script to deploy your Shiny app to shinyapps.io

# 1) Load rsconnect
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

# 2) Set your shinyapps.io account info
#    (Replace these placeholders with your actual info)
rsconnect::setAccountInfo(
  name='kiriillb',
  token='85CEA0FF9C9FFFA96F635F846B2B9C52',
  secret='tz34CUU0F61dZXI0vjz8XZt29vxR3rV3AhkbQEze'
)

# 3) Deploy the app
#    (Adjust appDir if app.R is in a different folder)
rsconnect::deployApp(
  appDir   = ".",
  appName  = "ASXETFdashboard",   # choose any name you like
  forceUpdate = TRUE
)

# After successful deployment, you'll see a URL in the console.
# Anyone can access your app at that URL.
