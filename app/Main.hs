module Main (main) where


import MyLib

main :: IO ()
main = do
  result <- runHydraClient "http://127.0.0.1:3001" $ 
    queryFunds "addr_test1qzl4ugvgu920q4lr86lydvfqstm42arezeyjm0wy8mujvm93dzr5pcgc37ym773c8w6q5uem08ts6qskrvc6mw6nvf4s4tu94z"
  case result of
    Left err -> print err
    Right funds -> print funds