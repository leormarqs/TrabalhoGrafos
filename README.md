                       	Universidade Federal do Rio Grande do Sul
                                Instituto de Informática
                INF05512 - Teoria dos Grafos e Análise Combinatória
                                  Trabalho Prático Final

                                Leonardo Marques Rodrigues
                                 Marcos Vinicios da Silva
                                 Valéria Soldera Girelli


Instruções de Compilação / Execução:

Este trabalho foi desenvolvido utilizando a linguagem de programação Haskell, em SO Linux,
utilizandoa ferramenta Stack como gerenciador de projeto, portanto, deverá ser utilizado da
seguinte maneira:

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