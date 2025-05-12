library(shiny)
library(data.table)
library(DT)
library(shinyWidgets)
library(readxl)
library(writexl)

# File paths for Excel files (adjust as needed)
inventory_file <- "inventory.xlsx"
transactions_file <- "transactions.xlsx"
users_file <- "users.xlsx"

# Helper functions to read/write Excel files as data.tables
read_excel_dt <- function(file_path) {
  if (!file.exists(file_path)) {
    # Return empty data.tables with expected columns
    if (file_path == inventory_file) {
      return(data.table(ItemID = character(), ItemName = character(), Quantity = integer(), Date = as.Date(character())))
    } else if (file_path == transactions_file) {
      return(data.table(TransactionID = character(), Date = as.Date(character()), Type = character(),
                        ItemID = character(), ItemName = character(), Quantity = integer(),
                        Status = character(), User = character()))
    } else if (file_path == users_file) {
      return(data.table(username = character(), password = character(), role = character(), email = character()))
    }
  }
  df <- read_excel(file_path)
  setDT(df)
}

write_excel_dt <- function(dt, file_path) {
  writexl::write_xlsx(dt, path = file_path)
}

# UI login/signup page
login_signup_page <- fluidPage(
  titlePanel("Login or Sign Up"),
  radioButtons("auth_mode", "Choose Action:", choices = c("Sign In", "Sign Up")),
  textInput("user", "Username"),
  passwordInput("pass", "Password"),
  conditionalPanel(
    condition = "input.auth_mode == 'Sign Up'",
    tagList(
      selectInput("new_role", "Select Role", choices = c("receive", "dispatch", "manager")),
      textInput("email", "Email")
    )
  ),
  actionButton("auth_btn", "Submit"),
  textOutput("auth_status")
)

# Main UI based on role
main_ui <- function(role) {
  switch(role,
         receive = fluidPage(
           titlePanel("Receive Items"),
           actionButton("logout_btn", "Logout", class = "btn-danger"),
           textInput("recv_id", "Item ID"),
           textInput("recv_name", "Item Description"),
           numericInput("recv_qty", "Quantity Received", value = 1, min = 1),
           dateInput("recv_date", "Date Received", value = Sys.Date()),
           actionButton("recv_btn", "Receive Item"),
           br(), br(),
           DTOutput("inventory_table")
         ),
         
         dispatch = fluidPage(
           titlePanel("Request Dispatch"),
           actionButton("logout_btn", "Logout", class = "btn-danger"),
           selectInput("dispatch_item", "Select Item", choices = NULL),
           numericInput("dispatch_qty", "Quantity to Dispatch", value = 1, min = 1),
           actionButton("request_dispatch", "Request Dispatch"),
           h3("Pending Dispatch Requests"),
           DTOutput("pending_dispatch"),
           h3("Approved Dispatches"),
           DTOutput("approved_dispatch"),
           h3("Available Inventory"),
           DTOutput("dispatcher_inventory")
         ),
         
         manager = fluidPage(
           titlePanel("Manager Approval"),
           actionButton("logout_btn", "Logout", class = "btn-danger"),
           DTOutput("approve_table"),
           actionButton("approve_btn", "Approve Selected"),
           actionButton("reject_btn", "Reject Selected"),
           h3("Inventory Balance"),
           DTOutput("manager_inventory"),
           h3("Transaction Log"),
           DTOutput("log_table")
         )
  )
}

server <- function(input, output, session) {
  user_role <- reactiveVal(NULL)
  current_user <- reactiveVal(NULL)
  
  inv_data <- reactiveVal(read_excel_dt(inventory_file))
  tx_log <- reactiveVal(read_excel_dt(transactions_file))
  users_data <- reactiveVal(read_excel_dt(users_file))
  
  observeEvent(input$auth_btn, {
    users <- users_data()
    if (input$auth_mode == "Sign In") {
      user <- users[username == input$user & password == input$pass]
      if (nrow(user) == 1) {
        user_role(user$role)
        current_user(user$username)
        output$auth_status <- renderText("")
      } else {
        output$auth_status <- renderText("Invalid credentials")
      }
    } else if (input$auth_mode == "Sign Up") {
      if (input$user %in% users$username) {
        output$auth_status <- renderText("Username already exists")
      } else {
        new_user <- data.table(username = input$user, password = input$pass, role = input$new_role, email = input$email)
        users <- rbind(users, new_user)
        users_data(users)
        write_excel_dt(users, users_file)
        user_role(input$new_role)
        current_user(input$user)
        output$auth_status <- renderText("")
      }
    }
  })
  
  observeEvent(input$logout_btn, {
    user_role(NULL)
    current_user(NULL)
    updateTextInput(session, "user", value = "")
    updateTextInput(session, "pass", value = "")
  })
  
  update_inventory_and_tx <- function(new_inv, new_tx) {
    inv_data(new_inv)
    tx_log(new_tx)
    write_excel_dt(new_inv, inventory_file)
    write_excel_dt(new_tx, transactions_file)
  }
  
  pending_dispatches <- reactive({
    tx_log()[Type == "Dispatch" & Status == "Pending"]
  })
  
  output$root_ui <- renderUI({
    if (is.null(user_role())) login_signup_page else main_ui(user_role())
  })
  
  observe({
    inv <- inv_data()
    if (!is.null(inv) && nrow(inv) > 0) {
      updateSelectInput(session, "dispatch_item",
                        choices = setNames(inv[Quantity > 0, ItemID], paste(inv[Quantity > 0, ItemID], "-", inv[Quantity > 0, ItemName]))
      )
    } else {
      updateSelectInput(session, "dispatch_item", choices = character(0))
    }
  })
  
  observeEvent(input$recv_btn, {
    inv <- inv_data()
    tx <- tx_log()
    tx_id <- paste0("TX", as.integer(Sys.time()))
    
    if (input$recv_id %in% inv$ItemID) {
      inv[ItemID == input$recv_id, `:=`(
        Quantity = Quantity + input$recv_qty,
        Date = input$recv_date,
        ItemName = input$recv_name
      )]
    } else {
      inv <- rbind(inv, data.table(
        ItemID = input$recv_id,
        ItemName = input$recv_name,
        Quantity = input$recv_qty,
        Date = input$recv_date
      ))
    }
    
    tx <- rbind(tx, data.table(
      TransactionID = tx_id,
      Date = input$recv_date,
      Type = "Receive",
      ItemID = input$recv_id,
      ItemName = input$recv_name,
      Quantity = input$recv_qty,
      Status = "Approved",
      User = current_user()
    ))
    
    update_inventory_and_tx(inv, tx)
  })
  
  output$inventory_table <- renderDT({
    datatable(inv_data()[, .(ItemID, ItemName, Quantity, Date)], options = list(pageLength = 5))
  })
  
  output$dispatcher_inventory <- renderDT({
    datatable(inv_data()[, .(ItemID, ItemName, Quantity, Date)], options = list(pageLength = 5))
  })
  
  output$approved_dispatch <- renderDT({
    datatable(tx_log()[Type == "Dispatch" & Status == "Approved", .(ItemID, ItemName, Quantity, Date, User)], options = list(pageLength = 5))
  })
  
  observeEvent(input$request_dispatch, {
    inv <- inv_data()
    tx <- tx_log()
    available <- inv[ItemID == input$dispatch_item, Quantity]
    item_name <- inv[ItemID == input$dispatch_item, ItemName]
    if (length(item_name) == 0) {
      showNotification("Item not found in inventory", type = "error")
      return()
    }
    req(length(available) == 1 && input$dispatch_qty <= available)
    
    tx_id <- paste0("TX", as.integer(Sys.time()))
    tx <- rbind(tx, data.table(TransactionID = tx_id, Date = Sys.Date(), Type = "Dispatch",
                               ItemID = input$dispatch_item, ItemName = item_name,
                               Quantity = input$dispatch_qty,
                               Status = "Pending", User = current_user()))
    update_inventory_and_tx(inv, tx)
  })
  
  output$pending_dispatch <- renderDT({
    datatable(pending_dispatches()[, .(ItemID, ItemName, Quantity, Date, User)], options = list(pageLength = 5))
  })
  
  output$approve_table <- renderDT({
    datatable(pending_dispatches()[, .(ItemID, ItemName, Quantity, Date, User)], selection = "multiple")
  })
  
  observeEvent(input$approve_btn, {
    selected <- input$approve_table_rows_selected
    if (length(selected) > 0) {
      tx <- tx_log()
      selected_tx <- pending_dispatches()[selected]
      inv <- inv_data()
      
      for (row in 1:nrow(selected_tx)) {
        item <- selected_tx[row]
        stock <- inv[ItemID == item$ItemID, Quantity]
        if (length(stock) == 1 && stock >= item$Quantity) {
          inv[ItemID == item$ItemID, Quantity := Quantity - item$Quantity]
          tx[TransactionID == item$TransactionID, Status := "Approved"]
        } else {
          tx[TransactionID == item$TransactionID, Status := "Rejected"]
        }
      }
      update_inventory_and_tx(inv, tx)
    }
  })
  
  observeEvent(input$reject_btn, {
    selected <- input$approve_table_rows_selected
    if (length(selected) > 0) {
      tx <- tx_log()
      selected_tx <- pending_dispatches()[selected]
      for (row in 1:nrow(selected_tx)) {
        item <- selected_tx[row]
        tx[TransactionID == item$TransactionID, Status := "Rejected"]
      }
      update_inventory_and_tx(inv_data(), tx)
    }
  })
  
  output$log_table <- renderDT({
    datatable(tx_log(), options = list(pageLength = 10))
  })
  
  output$manager_inventory <- renderDT({
    datatable(inv_data()[, .(ItemID, ItemName, Quantity, Date)], options = list(pageLength = 5))
  })
}

shinyApp(
  ui = uiOutput("root_ui"),
  server = server
)