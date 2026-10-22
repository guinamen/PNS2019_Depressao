# Diagnóstico de depressão na PNS 2019 — pipeline completo

Modelo de lógica paraconsistente anotada (Eτ) para depressão na PNS 2019 e a
análise da desigualdade regional de diagnóstico que saiu dele. São as nove
rodadas de testes, na ordem em que foram feitas, mais um gerador de
relatório que monta o placar de hipóteses a partir dos números.

## Como rodar

No terminal, dentro desta pasta:

```bash
Rscript run_all.R
```

Ao terminar, abra `saida/relatorio_achado.html` no navegador.

Para refazer só o relatório, sem rodar os modelos de novo (por exemplo,
depois de ajustar o texto em `R/relatorio.R`):

```bash
Rscript run_all.R --so-relatorio
```

## Requisitos

- R 4.1 ou mais recente
- Pacotes: `install.packages(c("survey", "poLCA", "lavaan", "semTools"))`
- Microdados da PNS 2019 em `~/dados/pns2019/pns2019.rds`, com os códigos
  numéricos (não rotulados). O caminho está em `CFG$arquivo`, no topo de
  `R/01_modelo_etau.R`.

## Antes da primeira execução

Os códigos de variáveis ficam numa lista `CFG...` no topo de cada script.
Os marcados como `CONFERIR` ou `PLACEHOLDER` precisam bater com o dicionário
da PNS 2019. Os mais importantes:

| Script | Variável | Código usado |
|---|---|---|
| 01 | itens do PHQ-9 | N010 a N018 |
| 01 | diagnóstico de depressão | Q092 |
| 01 | medicação / psicoterapia atuais | Q09502 / Q09501 |
| 01 | peso, estrato, UPA | V00291, V0024, UPA_PNS |
| 01 | escolaridade / autoavaliação de saúde | VDD004A / N001 |
| 02 | idade / idade no diagnóstico | C008 / Q093 |
| 03 | última consulta por depressão | Q10101 |
| 04 | UF | V0001 |
| 05 | situação urbana/rural, renda, cor/raça | V0026, VDF004, C009 |
| 06 | tipo de área | V0031 |
| 07 | cadastro na ESF / plano de saúde | B001 / I00101 |
| 07b | visitas da equipe de SF | B003 |
| 08 | itens nucleares do PHQ-9 | N012, N016 |

Se uma variável não existir, o script para com uma mensagem dizendo qual.
A função `achar(dados, "texto")` (definida no script 01) procura variáveis
pelo nome ou rótulo.

## O que cada rodada faz

| Script | Pergunta |
|---|---|
| `01_modelo_etau.R` | Modelo Eτ com LCA (K = 2); testes iniciais T1 a T3; sensibilidade |
| `02_selecao_K.R` | Seleção de K; onde o modelo perde informação; acesso; tempo desde o diagnóstico |
| `03_episodico.R` | O conflito acompanha a atividade clínica recente? |
| `04_cobertura_uf.R` | Cobertura de diagnóstico entre sintomáticos, por UF |
| `05_composicao.R` | As diferenças entre UFs sobrevivem à composição individual? |
| `06_capital_interior.R` | Capital × interior dentro da mesma UF |
| `07_atencao_primaria.R` | O cadastro na ESF explica a vantagem do interior? |
| `07b_visitas.R` | As visitas da equipe de SF explicam? |
| `08_denominador.R` | Um denominador inflado na capital explica? |
| `09_invariancia.R` | Invariância do PHQ-9 entre regiões e robustez a três denominadores |
| `relatorio.R` | Lê os resultados e escreve o relatório |

As rodadas rodam numa única sessão de R, em sequência, porque cada uma usa
objetos das anteriores. Se uma falhar, as seguintes não rodam, e o
relatório sai parcial, indicando o que ficou indisponível.

## Como o relatório é montado

Cada script grava seus números numa lista `RES` (blocos "Registro para o
relatório", no fim de cada arquivo). `R/relatorio.R` lê essa lista, aplica o
critério de cada hipótese e escreve o texto. **Nenhum resultado ou veredito
está escrito à mão**: se uma nova execução der números diferentes, o placar e
o texto do achado principal mudam junto.

Os critérios de refutação estão no cabeçalho de cada script, escritos antes
de cada rodada ter sido executada pela primeira vez. Os poucos definidos
depois do resultado aparecem no placar como *post hoc*.

O relatório marca as afirmações como **Medido** (número desta execução),
**Inferência** (interpretação que vai além do número) ou **Construção**
(escolha metodológica própria).

## Saídas (pasta `saida/`)

- `relatorio_achado.html` — o relatório
- `resultados.rds` — todos os números registrados (`readRDS` para inspecionar)
- `NN_*.log` — a saída completa de cada rodada
- CSVs e PNGs de cada rodada

## Tempo de execução

Depende da máquina. As partes mais lentas são os modelos de classes
latentes (K de 2 a 5, mais um ajuste por nível de escolaridade) e, no
script 09, os ajustes da análise fatorial ordinal e o cálculo dos escores.
O console mostra o tempo de cada rodada.

## Observações

- O modelo de classes latentes usa reinícios aleatórios; a semente está
  fixada no script 01, mas versões diferentes do `poLCA` podem dar números
  ligeiramente diferentes.
- A parte de "médico da família" do script 07 é pulada (o código da
  pergunta não foi identificado); as visitas da equipe de SF (07b) ocupam
  esse lugar.
- No script 09, os índices robustos de ajuste dos modelos com restrições
  podem vir como `NA`, dependendo da versão do `lavaan`; o relatório usa
  então o RMSEA da diferença do teste de Satorra (2000).
