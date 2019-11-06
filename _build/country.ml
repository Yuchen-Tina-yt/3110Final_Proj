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

let exchange_amount_for country usd_amount =
  usd_amount *. country.exchange_rate

let exchange_fee_for country usd_amount =
  usd_amount *. country.exchange_fee