#' env_vars
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

included.cities <-
  c("100 Mile House", "Nakusp", "Castlegar","Chase","Clearwater", "Trail",
    "Lillooet","Merritt", "Kamloops","Sparwood", "Oliver", "Keremeos")
not.included <- c(
  "Ashcroft", "Cranbrook", "Creston", "Fernie", "Golden", "Grand Forks",
  "Invermere", "Kelowna", "Nelson", "Penticton", "Princeton",
  "Revelstoke", "Salmon Arm", "Sparwood", "Vernon", "Williams Lake")

dynamicText <- function(){
  googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1Wkqz6LQQUP34q9Gq2YVWzIU9v9Hz77AK_TF80IEJw0s/edit?gid=0#gid=0')
}

# Google Drive Folder ID (replace with your actual folder ID)
DRIVE_FOLDER_ID <- "19te_XAcl6w6PmlWMhgb3ZyzfODsuCU9X"

custom_colors <-
  c(
    "#1f77b4", # blue
    "#ff7f0e", # orange
    "#2ca02c", # green
    "#d62728", # red
    "#17becf"  # cyan
  )
# c("#055899", "#9DB7CF", "#F8FCFD", "#FDFDFD", "#87B4DB")

facilities.in.model <- c("100 Mile District Hospital", "Arrow Lakes Hospital", "Boundary Hospital", 
                         "Cariboo Memorial Hosp & Hlth", "Castlegar & District Community", 
                         "Chase & District Health Centre", "Creston Valley Hospital", 
                         "Dr Helmcken Memorial Hospital", "East Kootenay Reg Hospital", 
                         "Elk Valley Hospital", "Golden & District Hospital", "Invermere & District Hospital", 
                         "Kelowna General Hospital", "Kootenay Boundary Reg Hospital", 
                         "Kootenay Lake Hospital", "Lillooet Hospital & Health Cen", "Nicola Valley Hospital & HC", 
                         "Penticton Regional Hospital", "Princeton General Hospital", 
                         "Queen Victoria Hospital", "Shuswap Lake Hospital", "Slocan Community Health Centre", 
                         "South Okanagan General Hosp.", "South Similkameen Health Centr", 
                         "Sparwood Health Care Centre", "Vernon Jubilee Hospital", "Victorian Com. Health Kaslo"
)
