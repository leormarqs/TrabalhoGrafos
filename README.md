                       	Universidade Federal do Rio Grande do Sul
                                Instituto de Informática
                INF05512 - Teoria dos Grafos e Análise Combinatória
                                  Trabalho Prático Final

                                Leonardo Marques Rodrigues
                                 Marcos Vinicios da Silva
                                 Valéria Soldera Girelli

Disponível em: http://github.com/leormarqs/TrabalhoGrafos

Este trabalho foi desenvolvido utilizando a linguagem de programação Haskell, em SO Linux,
utilizando a ferramenta Stack como gerenciador de projeto, portanto, deverá ser utilizado da
seguinte maneira:

  Instruções de Compilação / Execução:
	 
  Para compilar, em linha de comando:
  
        --$ stack build
        --$ stack install

        -- O executável será instalado em "~/.local/bin"
        -- (caso não seja instalado no diretório padrão, será informado no retorno do comando)

  Para executar, em linha de comando:

        --$ ~/.local/bin/TrabalhoGrafos
        --(caso tenha sido instalado em outro diretório, ajustar o caminho acima)

Será solicitado o caminho para o arquivo de entrada contendo o digrafo, após isso, o programa
calculará e exibira, o digrafo representado no arquivo fornecido, seu pseudografo subjacente,
a arvore geradora minima do pseudografo   subjacente e também a tabela de distancias de todos
os nodos partindo do nodo 0.
Os grafos presentes na pasta "./tests" são alguns casos de uso, os arquivos "./tests/g1.txt",
"./tests/g2.txt" e "./tests/g3.txt" respresentamos grafos mostrados na imagem "./tests/tests_g1_g2_g3.png"