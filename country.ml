type t = {
  currency: string;
  exchange_rate: float;
  exchange_fee: float; 
}

let currency_in country =
  country.currency

let exchange_amount_for country usd_amount =
  usd_amount *. country.exchange_rate

let exchange_fee_for country usd_amount =
  usd_amount *. country.exchange_fee