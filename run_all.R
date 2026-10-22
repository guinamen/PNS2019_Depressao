#!/usr/bin/env Rscript
# =====================================================================
# run_all.R — roda as nove rodadas em sequência e gera o relatório final
#
# Uso, no terminal, dentro desta pasta:
#   Rscript run_all.R                  roda tudo e gera o relatório
#   Rscript run_all.R --so-relatorio   refaz só o relatório a partir de
#                                      saida/resultados.rds (sem rodar modelos)
#
# Saídas em ./saida:
#   relatorio_achado.html   relatório final (abre no navegador)
#   resultados.rds          todos os números registrados pelas rodadas
#   NN_*.log                saída completa de cada rodada
#   *.csv, *.png            tabelas e figuras de cada rodada
#
# As rodadas dependem umas das outras (rodam na mesma sessão). Se uma
# falhar, as seguintes não rodam e o relatório sai parcial, indicando o
# que ficou indisponível.
# =====================================================================

.run_raiz <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
})
.run_saida <- file.path(.run_raiz, "saida")
dir.create(.run_saida, showWarnings = FALSE)
.run_args <- commandArgs(trailingOnly = TRUE)
.run_html <- file.path(.run_saida, "relatorio_achado.html")

# --- modo só relatório ------------------------------------------------
if ("--so-relatorio" %in% .run_args) {
  RES <- readRDS(file.path(.run_saida, "resultados.rds"))
  source(file.path(.run_raiz, "R", "relatorio.R"))
  gerar_relatorio(RES, .run_html)
  cat("Relatório:", .run_html, "\n")
  quit(save = "no")
}

# --- pacotes ----------------------------------------------------------
.run_pkgs <- c("survey", "poLCA", "lavaan", "semTools")
.run_falta <- .run_pkgs[!vapply(.run_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.run_falta))
  stop("Instale antes: install.packages(c(", paste0('"', .run_falta, '"', collapse = ", "), "))")

.run_scripts <- c(
  "01_modelo_etau.R",        # modelo Eτ com LCA (K = 2), testes T1–T3, sensibilidade
  "02_selecao_K.R",          # seleção de K, decomposição do T2, acesso, tempo de diagnóstico
  "03_episodico.R",          # conflito × última consulta (curso episódico)
  "04_cobertura_uf.R",       # cobertura de diagnóstico por UF
  "05_composicao.R",         # efeito de UF ajustado pela composição
  "06_capital_interior.R",   # capital × interior dentro da UF
  "07_atencao_primaria.R",   # cadastro na ESF e plano de saúde
  "07b_visitas.R",           # visitas da equipe de Saúde da Família
  "08_denominador.R",        # limites do denominador e perfil de sintomas
  "09_invariancia.R"         # invariância do PHQ-9 e robustez ao denominador
)

RES <- list(
  meta = list(inicio = Sys.time(), R = R.version.string,
              pacotes = vapply(.run_pkgs, function(p) as.character(packageVersion(p)), "")),
  log = list()
)
options(warn = 1)
setwd(.run_saida)

for (.run_s in .run_scripts) {
  .run_nome <- .run_s                      # cópia: os scripts podem redefinir variáveis comuns
  .run_log  <- file.path(.run_saida, sub("\\.R$", ".log", .run_nome))
  cat(format(Sys.time(), "%H:%M:%S"), " ", .run_nome, " ... ", sep = "")
  .run_t0  <- Sys.time()
  .run_con <- file(.run_log, open = "wt")
  sink(.run_con); sink(.run_con, type = "message")
  .run_ok <- tryCatch({
    source(file.path(.run_raiz, "R", .run_nome), local = globalenv(), echo = FALSE)
    TRUE
  }, error = function(e) { message("ERRO: ", conditionMessage(e)); FALSE })
  sink(type = "message"); sink(); close(.run_con)
  .run_dt <- as.numeric(difftime(Sys.time(), .run_t0, units = "mins"))
  RES$log[[.run_nome]] <- list(ok = .run_ok, minutos = .run_dt)
  cat(if (.run_ok) "ok" else "FALHOU", sprintf("(%.1f min)\n", .run_dt))
  if (!.run_ok) {
    cat("  Veja o erro no fim de", .run_log, "\n",
        " As rodadas seguintes dependem desta; o relatório sairá parcial.\n")
    break
  }
}

RES$meta$fim <- Sys.time()
saveRDS(RES, file.path(.run_saida, "resultados.rds"))

# O gerador é carregado só agora, para que nenhum script sobrescreva suas funções
source(file.path(.run_raiz, "R", "relatorio.R"))
gerar_relatorio(RES, .run_html)
cat("\nRelatório:", .run_html, "\n")
