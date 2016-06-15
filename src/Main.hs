import Data.List
import Graph.Parse
import Graph.SpanningTree
import Graph.DistTable

main :: IO ()
main = do
  putStr "\n\nForneça o caminho para o arquivo de entrada:\n"
  filePath <- getLine

  txt <- readFile filePath
  let dg = txtDigraph txt
      g  = digraphToGraph dg
      mst = spanningTree g

      table = distTable dg

  putStr "Arquivo Fornecido:\n"
  putStr txt

  putStr "\nDígrafo representado:\n"
  print dg

  putStr "\nPseudografo subjacente: \n"
  print g

  putStr "\nÁrvore Geradora Mínima:\n"
  print mst

  putStr "\nTabela de distâncias a partir do nodo 0:\n"
  print table
