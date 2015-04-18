library(stringr)

refine_decs <- function(x)
{
  if (length(str_extract_all(x, "[[:alpha:]]")[[1]]) == 0)
  {
    regexp <- "[[:digit:]]+|[[:punct:]]+"
    x <- paste(str_extract_all(x, regexp)[[1]], collapse = "", sep = "")
    if (str_sub(x, 1, 1) == "0")
    {
      wholes <- 0
      regexp <- "[[:digit:]]+"  
      x <- str_sub(x,3, length(x) - 2)
      d <- as.numeric(paste("0", as.character(x), sep = "."))
    } else 
    {
      dec <- str_sub(x, str_length(x)-2, str_length(x)-2)
      if ((dec == ",") | (dec == "."))
      {
        d <- as.numeric(str_sub(x, str_length(x)-1, str_length(x))) / 100
        regexp <- "[[:digit:]]+"  
        x <- paste(str_extract_all(x, regexp)[[1]], collapse = "", sep = "")
        wholes <- as.numeric(str_sub(x, 1, str_length(x)-2))
      } else { 
        
        dec <- str_sub(x, str_length(x)-1, str_length(x)-1)
        if ((dec == ",") | (dec == ".")) 
        {  
          d <- as.numeric(str_sub(x, str_length(x), str_length(x))) / 10
          regexp <- "[[:digit:]]+"  
          x <- paste(str_extract_all(x, regexp)[[1]], collapse = "", sep = "")
          wholes <- as.numeric(str_sub(x, 1, str_length(x)-1))
        } else
        {
          d <- 0
          regexp <- "[[:digit:]]+"  
          x <- paste(str_extract_all(x, regexp)[[1]], collapse = "", sep = "")
          wholes <- as.numeric(str_sub(x, 1, str_length(x)))
        }
      }
    }
    wholes + d
  } else
  {
    as.character(x)
  }
  
}

split_content <- function(decl_tab)
{
  family_member <- NULL
  transport_all <- NULL
  numeric_value <- NULL
  for (i in 1:length(decl_tab$content))
  {
    if (decl_tab$index[i] == 4)
    {
      family_member <- c(family_member, as.character(decl_tab$content[i]))
      numeric_value <- c(numeric_value, NA)
      transport_all <- c(transport_all, NA)
    } else 
    {
      family_member <- c(family_member, NA)
      if ((decl_tab$index[i] > 34) & (decl_tab$index[i] < 45) )
      {
        transport_all <- c(transport_all, as.character(decl_tab$content[i]))
        numeric_value <- c(numeric_value, NA)
      } else 
      {
        transport_all <- c(transport_all, NA)
        numeric_value <- c(numeric_value, refine_decs(as.character(decl_tab$content[i])))
      }
        
    }
  }
  decl_tab['content'] <- NULL
  cbind(decl_tab, cbind(family_member, numeric_value, transport_all))
}

refine_all <- function(x)
{
  #add_new_columns and remove extra column
  x['position'] <- NULL
  x['source_type'] <- "сайт ВРУ"
  x['unit_of_measures'] <- NA
  x['unit_of_measures_purchase'] <- NA
  x['unit_of_measures_rent'] <- NA
  x['chapter_num'] <- NA
  x['no_data'] <- NA
  x['source_link'] <- NA

  ids_df <- read.csv("fullname_ids.csv")
  x <- merge(ids_df, x, by = "person", all.y = TRUE)
  x['person'] <- NULL
  x <- add_indices(x)
  x <- split_content(x)
  x <- add_transport(x)
  x$transport_mark <- x$transport_all
  #correct names
  colnames(x)[which(names(x) == "declarer.family")] <- "declarer"
  colnames(x)[which(names(x) == "Sum1_property")] <- "purchase_cost"
  colnames(x)[which(names(x) == "Sum2_leasing")] <- "rent_cost"
  colnames(x)[which(names(x) == "Name_of_country")] <- "foreign_income_country"
  colnames(x)[which(names(x) == "Name_of_currency")] <- "foreign_income_currency"
  colnames(x)[which(names(x) == "Sum3_income_in_currency")] <- "foreign_currency_number"
  colnames(x)[which(names(x) == "additional_information")] <- "additional"
  colnames(x)[which(names(x) == "decl_section")] <- "chapter_name"
  #correct the order of columns
  x <- order_columns(x)
  x
}

add_transport <- function(x)
{
  x['transport_mark'] <- NA
  x['transport_model'] <- NA
  x['transport_year'] <- NA
  x['transport_description'] <- NA
  x
}

add_indices <- function(x)
{
  indices <- read.csv("new_indices.csv")
  x <- merge(x, indices, by = c("point_title", "declarer.family"))
  x['point_code'] <- NULL
  x
}

order_columns <- function(x)
{
  A <- read.csv("right_order.csv", stringsAsFactors = FALSE)
  right_names <- names(A)
  ret <- x["person_id"]
  for (i in 2:length(right_names))
  {
    ret <- cbind(ret, x[,right_names[i]])
  }
  names(ret) <- right_names
  ret[is.na(ret)] <- ""
  ret
}


