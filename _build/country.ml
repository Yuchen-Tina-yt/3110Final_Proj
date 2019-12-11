type t = {
  currency: string;
  exchange_rate: float;
  exchange_fee: float; 
}

let make_country crncy ex_rate ex_fee = {
  currency = crncy;
  exchange_rate = ex_rate;
  exchange_fee = ex_fee;
}

let currency_in country =
  country.currency

let exchange_amount_for_USD country usd_amount =
  usd_amount *. country.exchange_rate

let exchange_amount_to_USD country other_amount =
  other_amount /. country.exchange_rate

let exchange_fee_for country amount =
  amount *. country.exchange_fee