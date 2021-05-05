



output$new_test_item_select = renderDataTable({
  req(db, values$parms)
  items = get_items(db) %>%
    semi_join(get_items(values$parms), by='item_id') 
  add_column(items, 
             paste0('<input type="checkbox" value="',
                     htmlEscape(items$item_id,attribute=TRUE),'"></input>'),
              .before=1) %>%
    datatable(rownames=FALSE, escape=-1,selection='none',
              options=list(columnDefs = list(list(targets=0,type='html'))))
})

output$new_test_matrix = renderTable({
  req(db, values$new_test_items)
  
  items = get_items(db) %>%
    semi_join(get_items(values$parms), by='item_id') 
  
  if(ncol(items)>1)
  {
    nd = summarise_all(items, n_distinct)
    suitable = as.integer(nd)
    names(suitable) = names(nd)
    suitable = sort(suitable[suitable<0.5*nrow(items)])
    
    as.data.frame(table(items[,suitable]))
  }
  
})

