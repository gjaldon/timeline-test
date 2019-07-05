# Timeline Take-Home Challenge

## Notes
- Free version of the wordltradingdata API limits the stocks symbols you can get historical data for to two at once. This causes
the app to fail getting data from the API when you allocate to more than two stocks.
- It will fail silently when submitting symbols that don't exist in the stock market. Need to input valid symbols.
- It will fail silently on week days that are not trading days such as holidays. It will also fail for future dates.


## How to Run

Requires `elm-live` and `elm` installed.

```bash
$ elm-live src/Main.elm --open -- --output=elm.js
```
