module Main (main) where


import MyLib

main :: IO ()
main = do
  result <- runHydraClient "http://127.0.0.1:3001" $ 
    queryFunds "addr_test1qprg7rzdsm2ml82499utezhsl43sz8nr39sd2t8djefhd7xjgx8m3d6keddfp04fr59vw2k0curt6jtss6evepvj5yjsa8wwlv"
  case result of
    Left err -> print err
    Right funds -> print funds