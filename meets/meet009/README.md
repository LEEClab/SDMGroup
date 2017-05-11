# SDMGroup

## Resumo das reuniões

### Dia 009 - 11/05/2017

### Resumo
- Apresentação do Ricardo Bovendorp

- "Mudanças climáticas e diversidade filogenética de pequenos mamíferos para a Mata Atlântica"

---

1. Ocorrências
- Pontos do SpeciesLink, GBIF e Banco de Dados (BD)

- Limpeza dos dados com script Caro-Pablo

---

2. Variáveis

- Limite/extensão: limite extendido da MA (Muylaert et al. *in press*)

- Climáticas: WorldClim v 1.4; presente, 2050 e 2070; rcp6.0 e rcp8.5; CSIRO (ACCESS1-0)

- Resolução: 2.5-arc-minuto (~ 5 km)

- Paisagem: SOS MA 2014 para corte dos *pixels*

---

3. Algoritmos

- *Ensemble* por média ponderada (TSS > 0.5)

- *Sensitivity-specificity sum maximization* para calibrar os ENMs

- *lowest presence threshold* para mapas de riqueza, apenas para os pontos do BD

