# Group T16_07

![](images/types.png)

- **City** = String --> nome de uma cidade
- **Path** = Lista de cidades --> sequência de cidades
- **Distance** = Int --> distância entre duas cidades
- **RoadMap** = Lista de tuplos --> mapa rodoviário, onde cada tuplo conecta duas cidades e inclui as distências entre elas

&nbsp;

![](images/1.png)

- Função que extrai uma lista de todas as cidades mencionadas no mapa rodoviário, removendo as duplicadas

&nbsp;

![](images/2.png)

- Significa que para cada tuplo no **roadmap**, ele retorna ambos os valores *city1* e *city2*

&nbsp;

![](images/3.png)

- **RoadMap** = Lista de tuplos --> Cada tuplo indica que duas cidades estão conectadas por uma estrada de uma certa distância
- **City** = String --> Cidades
- **Bool** = Bool --> Indica se as duas estão diretamente conectadas

&nbsp;

![](images/4.png)

- A função usa *any*, que verifica se algum elemento de uma lista satisfaz uma condição. Aqui queremos verificar se algum tuplo no **roadmap** corresponde à condição descrita pela função **matches**

&nbsp;

![](images/5.png)

- *x* e *y* são duas cidades conectadas e o terceiro valor *_* é a distância
- Retorna *True* se a *x* é a mesma que *city1* e *y* a mesma que *city2* ou vice-versa

&nbsp;

![](images/6.png)

- **RoadMap** = Lista de tuplos --> Cada tuplo indica que duas cidades estão conectadas por uma estrada e inclui a distância entre elas
- **City** --> As duas cidades para as quais queremos saber a distância
- **Maybe Distance** --> Pode retornar um valor de distância (*Just Distance*) se as cidades estiverem conectadas, ou *Nothing* se isso não se verificar

&nbsp;

![](images/7.png)

- **Data.List.find matches roadmap** --> Procura no roadmap um tuplo (x, y, d) onde *x* e *y* são as cidades e *d* é a distância entre elas. A função **find** retorna o *primeiro elemento* que faz o *match* ou *Nothing* se nenhuma tupla satisfizer a condição

&nbsp;

![](images/8.png)

- **RoadMap** = Lista de tuplos --> Cada tuplo indica que duas cidades estão conectadas uma com a outra e invlui a distância entre elas
- **City** = String --> Cidade

- A função retorna uma lista de pares *[(City, Distance)]*. Cada par representa uma cidade conectada diretamente à cidade fornecida e a distância entre elas.

&nbsp;

![](images/9.png)

- Percorre o **roadmap** à procura do tuplos onde *x* é a cidade **city** fornecida
- Para cada tuplo onde *x == city*, retorna o par *(y, d)* qe contém a cidade *y* diretamente conectada a **city** e a distância *d* entre elas

&nbsp;

![](images/10.png)

- **RoadMap** = Lista de tuplos --> indica as cidades conectadas e as distâncias entre elas
- **Path** = Caminho (lista de cidades) --> Representa a sequência de cidades pelas quais desejamos calcular a distância
- A função retorna um valor do tipo *Maybe Distance*. Se todas as cidades no caminho estão conectadas, retorna *Just Distance* que contém a distância total. Caso contrário, retorna *Nothing*

&nbsp;

![](images/11.png)
![](images/12.png)

- Se o *path* estiver vazio ([]) ou se tiver apenas uma cidade (por exemplo, ["A"]), a função retorna *Just 0*
- Um caminho vazio não tem uma distância associada, então o resultado é zero

&nbsp;

![](images/13.png)

- **distance roadmap city1 city2** --> verificar se as duas primeiras cidades estão diretamente conectadas e, se sim, qual a distância entre elas
- **Just d** --> Significa que as cidades estão conectadas e que *d* é a distância entre elas
- **Nothing** --> Significa que não há conexão