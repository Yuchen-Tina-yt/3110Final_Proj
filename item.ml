
type t = {item_name: string; item_value: float}

let make_item name value = 
  {item_name = name; item_value = value}

let get_item_name item =
  item.item_name

let get_item_value item =
  item.item_value