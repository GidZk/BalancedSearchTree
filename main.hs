{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}



import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  contents <- getContents
  let src = words contents
  let tree = foldr (insert) (emptyTree) (src)

  let height'   = height tree
  let size'     = size tree

  let oph       = ceiling (logBase 2 (fromIntegral size'+1 ) -1 )  

  let ratio     = (fromIntegral height') / (fromIntegral oph)
  let prefix    = take 20 (inorder tree)


  putStrLn ("Size :"                    ++ show size')
  putStrLn ("height : "                 ++ show height')
  putStrLn ("Optimal height "           ++ show oph)
  putStrLn ("Height / Optimal Height "  ++ show (fromIntegral height' / fromIntegral oph)) 
  putStrLn ("CheckTree : "              ++ show (checkTree tree))
  putStrLn (" First 20 words : "        ++ (show prefix))

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase

{-Size: 12582
Height: 18
Optimal height: 13
Height / Optimal height: 1.3846153846153846
checkTree: True
First 20 words: A ABP AC AD ADLugha ADNa ADR ADSL ADWabantu ADwaarabu AIDS AL ALigombea AME ANC ANGALIA ARPA ARPANET ASP Abd

-}




--------------------------------------------------------------------------------
