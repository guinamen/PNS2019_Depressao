# =====================================================================
# Rodada 9 — Invariância de medida do PHQ-9 entre regiões
# Rodar na MESMA sessão, depois das rodadas 1 a 8
# (usa d, des2 com i1..i9/post_alvo/D/UF/macro/nne/idade/mulher/g/rural/
#  renda/raca, CFG4, covs$M3)
#
# Pacotes: install.packages(c("lavaan", "semTools"))
#
# Pergunta: a heterogeneidade de cobertura entre UFs (o achado mais
# robusto até aqui) sobrevive quando o denominador "sintomáticos" é
# definido por um escore que corrige o funcionamento diferencial do
# PHQ-9 entre regiões?
#
# Com ~90 mil casos, QUALQUER diferença entre grupos será
# estatisticamente significativa. A pergunta útil não é "há não
# invariância?" (há), e sim "ela muda a conclusão?". Por isso o teste
# tem duas partes:
#
# Parte A — Diagnóstico (CFA ordinal multigrupo, WLSMV)
#   Sequência de Wu & Estabrook (2016): configural -> limiares ->
#   limiares + cargas -> + interceptos. Critério de ajuste por variação
#   (Chen, 2007; cortes pensados para dados contínuos, usados aqui como
#   referência): delta CFI >= -0,010 e delta RMSEA <= +0,015 frente ao configural.
#   Se o modelo escalar falhar, busca gulosa de invariância parcial:
#   libera, um item por vez, cargas e limiares do item que mais melhora
#   o CFI, até atender o critério (no máximo 4 itens liberados, para
#   manter pelo menos 5 âncoras).
#
# Parte B — Impacto (o teste decisivo)
#   Três denominadores, todos com o MESMO tamanho nacional (corte único
#   nacional no escore que reproduz a prevalência de sintomáticos da
#   classificação original):
#     (0) LCA, posterior > 0,8         (o usado até aqui)
#     (1) escore do modelo escalar     (supõe invariância total)
#     (2) escore do modelo parcial     (corrige o funcionamento diferencial)
#   Para cada um: Wald de UF, dispersão de log(O/E) e o efeito de
#   Norte/Nordeste, todos ajustados pela composição M3.
#
# Critério (fixado antes de rodar), comparando (2) com (0):
#   O achado SOBREVIVE se  (a) Wald de UF com p < 0,001  E
#                          (b) dispersão de log(O/E) >= 50% da original  E
#                          (c) o efeito de N/NE mantém o sinal negativo
#                              com IC 95% inteiro abaixo de zero.
#   Caso contrário, a desigualdade entre UFs é, em parte relevante,
#   artefato de medida.
#
# Observações
#   - Grupos: macrorregião (padrão) ou UF (CFG9$grupo = "UF"; 27 grupos,
#     bem mais lento e com células esparsas nas categorias altas).
#   - Pesos: o modelo de medida é ajustado sem pesos, como o LCA; as
#     estimativas populacionais da Parte B usam o desenho completo.
#   - Tempo: cada ajuste WLSMV com ~90 mil casos leva de segundos a
#     poucos minutos; a busca parcial pode exigir até ~30 ajustes; o
#     cálculo dos escores (lavPredict) também pode levar alguns minutos.
# =====================================================================

suppressPackageStartupMessages({
  library(lavaan)
  library(semTools)
})

CFG9 <- list(
  grupo       = "macro",   # "macro" ou "UF"
  dcfi        = -0.010,
  drmsea      =  0.015,
  max_liberar = 4,
  saida_csv   = "invariancia_impacto_uf.csv"
)

itens <- paste0("i", 1:9)
v  <- des2$variables
ok <- complete.cases(v[, itens]) & !is.na(v[[CFG9$grupo]])
dd <- data.frame(v[ok, itens], grupo = as.character(v[[CFG9$grupo]][ok]))
idx_des <- which(ok)                       # linha de des2 correspondente a cada linha de dd
cat("Casos com os 9 itens completos:", nrow(dd), "\n")
print(table(dd$grupo))

mod <- paste("F =~", paste(itens, collapse = " + "))

ajustar <- function(igualdades = "", liberar = NULL) {
  measEq.syntax(configural.model = mod, data = dd, ordered = itens,
                parameterization = "delta", ID.fac = "std.lv",
                ID.cat = "Wu.Estabrook.2016", group = "grupo",
                group.equal = igualdades, group.partial = liberar,
                return.fit = TRUE, estimator = "WLSMV")
}
medidas <- function(fit) {
  fm <- fitMeasures(fit, c("cfi.scaled", "rmsea.scaled", "chisq.scaled", "df.scaled"))
  setNames(as.numeric(fm), c("CFI", "RMSEA", "chisq", "df"))
}
liberar_item <- function(j) c(paste0("F=~", j), paste0(j, "|t", 1:3), paste0(j, "~1"))

# ---------------------------------------------------------------------
# A. Sequência de invariância
# ---------------------------------------------------------------------
cat("\n=== A. Sequência de invariância (grupos:", CFG9$grupo, ") ===\n")
passos <- list(
  configural = "",
  limiares   = "thresholds",
  metrica    = c("thresholds", "loadings"),
  escalar    = c("thresholds", "loadings", "intercepts")
)
fits <- list()
for (nm in names(passos)) {
  cat("Ajustando", nm, "...\n")
  fits[[nm]] <- ajustar(passos[[nm]])
}
tab_inv <- t(sapply(fits, medidas))
tab_inv <- cbind(tab_inv, dCFI = tab_inv[, "CFI"] - tab_inv["configural", "CFI"],
                    dRMSEA = tab_inv[, "RMSEA"] - tab_inv["configural", "RMSEA"])
print(round(tab_inv, 4))
if (tab_inv["configural", "CFI"] < 0.95)
  cat("AVISO: ajuste configural fraco (CFI < 0,95) — a estrutura de um fator pode não valer em todos os grupos.\n")

atende <- function(fit) {
  m <- medidas(fit)
  (m["CFI"] - tab_inv["configural", "CFI"] >= CFG9$dcfi) &&
    (m["RMSEA"] - tab_inv["configural", "RMSEA"] <= CFG9$drmsea)
}
escalar_ok <- atende(fits$escalar)
cat("\nInvariância escalar pelo critério:", if (escalar_ok) "ATENDIDA" else "NÃO ATENDIDA", "\n")

# Busca gulosa de invariância parcial
livres <- character(0)
fit_parcial <- fits$escalar
if (!escalar_ok) {
  cat("\n=== Busca de invariância parcial ===\n")
  repeat {
    cand <- setdiff(itens, livres)
    cfis <- sapply(cand, function(j) {
      f <- ajustar(passos$escalar, unlist(lapply(c(livres, j), liberar_item)))
      medidas(f)["CFI"]
    })
    melhor <- names(which.max(cfis))
    livres <- c(livres, melhor)
    fit_parcial <- ajustar(passos$escalar, unlist(lapply(livres, liberar_item)))
    m <- medidas(fit_parcial)
    cat(sprintf("  liberado %s -> CFI %.4f (delta  %.4f) | RMSEA %.4f (delta  %.4f)\n", melhor,
                m["CFI"], m["CFI"] - tab_inv["configural", "CFI"],
                m["RMSEA"], m["RMSEA"] - tab_inv["configural", "RMSEA"]))
    if (atende(fit_parcial) || length(livres) >= CFG9$max_liberar) break
  }
  cat("Itens não invariantes:", livres,
      if (atende(fit_parcial)) "(critério atendido)" else "(limite de itens atingido sem atender o critério)", "\n")
  cat("Correspondência com os códigos da PNS:",
      paste0(livres, " = ", CFG$phq_itens[match(livres, itens)], collapse = ", "), "\n")
}

cat("\nMédias latentes por grupo (referência = primeiro grupo):\n")
medias <- function(fit) {
  pe <- parameterEstimates(fit)
  r <- pe[pe$lhs == "F" & pe$op == "~1", c("group", "est")]
  setNames(r$est, lavInspect(fit, "group.label")[r$group])
}
print(round(rbind(escalar = medias(fits$escalar), parcial = medias(fit_parcial)), 3))

# ---------------------------------------------------------------------
# B. Impacto sobre a cobertura
# ---------------------------------------------------------------------
cat("\n=== B. Impacto sobre a heterogeneidade entre UFs ===\n")
escores <- function(fit) {
  sc  <- lavPredict(fit)
  cid <- lavInspect(fit, "case.idx")
  out <- rep(NA_real_, nrow(dd))
  for (gi in seq_along(sc)) out[cid[[gi]]] <- sc[[gi]][, "F"]
  if (cor(out, rowSums(dd[, itens]), use = "complete.obs") < 0) out <- -out
  full <- rep(NA_real_, nrow(v)); full[idx_des] <- out
  full
}
cat("Calculando escores (pode demorar)...\n")
sc_esc <- escores(fits$escalar)
sc_par <- escores(fit_parcial)

w_all <- 1 / des2$prob
sint0 <- as.integer(v$post_alvo > CFG4$corte_post)
base_ok <- !is.na(sint0) & !is.na(sc_par)
p0 <- sum(w_all[base_ok] * sint0[base_ok]) / sum(w_all[base_ok])
cat(sprintf("Prevalência nacional de sintomáticos (original): %.4f\n", p0))

corte_nacional <- function(sc) {
  o <- order(sc[base_ok]); x <- sc[base_ok][o]; w <- w_all[base_ok][o]
  x[which(cumsum(w) / sum(w) >= 1 - p0)[1]]
}
sint1 <- as.integer(sc_esc >= corte_nacional(sc_esc))
sint2 <- as.integer(sc_par >= corte_nacional(sc_par))
sint1[!base_ok] <- NA; sint2[!base_ok] <- NA; sint0[!base_ok] <- NA
cat("Concordância com a classificação original (sem peso):\n")
cat(sprintf("  escalar: %.3f | parcial: %.3f\n",
            mean(sint1 == sint0, na.rm = TRUE), mean(sint2 == sint0, na.rm = TRUE)))

heterog <- function(ind) {
  des2$variables$sint_x <- ind
  s <- subset(des2, sint_x %in% 1 & !is.na(D) & !is.na(UF) & !is.na(idade) &
                    !is.na(mulher) & !is.na(g) & !is.na(rural) & !is.na(renda) & !is.na(raca))
  m_nac <- svyglm(as.formula(paste("D ~", covs$M3)), s, family = quasibinomial())
  f <- as.numeric(fitted(m_nac)); stopifnot(length(f) == nrow(s$variables))
  s$variables$D_esp <- f
  oe <- svyby(~D, ~UF, s, svyratio, denominator = ~D_esp)
  w  <- regTermTest(svyglm(as.formula(paste("D ~ UF +", covs$M3)), s, family = quasibinomial()), ~UF)
  mn <- svyglm(as.formula(paste("D ~ nne +", covs$M3)), s, family = quasibinomial())
  prev_nne <- svyby(~sint_x, ~nne, subset(des2, !is.na(sint_x)), svymean)
  list(n = nrow(s$variables), oe = coef(oe), F = as.numeric(w$Ftest), p = as.numeric(w$p),
       sd_log = sd(log(coef(oe))), nne = unname(coef(mn)["nne"]),
       nne_lo = unname(confint(mn)["nne", 1]), nne_hi = unname(confint(mn)["nne", 2]),
       prev_nne = coef(prev_nne))
}

res9 <- list(original = heterog(sint0), escalar = heterog(sint1), parcial = heterog(sint2))
resumo9 <- data.frame(
  denominador = names(res9),
  n_sint      = sapply(res9, `[[`, "n"),
  F_UF        = round(sapply(res9, `[[`, "F"), 2),
  p_UF        = signif(sapply(res9, `[[`, "p"), 3),
  sd_logOE    = round(sapply(res9, `[[`, "sd_log"), 3),
  pct_orig    = round(100 * sapply(res9, `[[`, "sd_log") / res9$original$sd_log, 1),
  logOR_NNE   = round(sapply(res9, `[[`, "nne"), 3),
  NNE_lo      = round(sapply(res9, `[[`, "nne_lo"), 3),
  NNE_hi      = round(sapply(res9, `[[`, "nne_hi"), 3),
  prev_resto  = round(sapply(res9, function(r) r$prev_nne[1]), 4),
  prev_NNE    = round(sapply(res9, function(r) r$prev_nne[2]), 4),
  row.names = NULL
)
print(resumo9, row.names = FALSE)

ok_a <- res9$parcial$p < 0.001
ok_b <- res9$parcial$sd_log >= 0.5 * res9$original$sd_log
ok_c <- res9$parcial$nne_hi < 0
cat(sprintf("\nCritério (parcial vs original): (a) %s | (b) %s | (c) %s  ->  %s\n",
            ok_a, ok_b, ok_c,
            if (ok_a && ok_b && ok_c)
              "O ACHADO SOBREVIVE: a desigualdade entre UFs não é artefato de medida"
            else "O ACHADO NÃO SOBREVIVE: parte relevante é artefato de medida"))

ufs <- names(res9$original$oe)
tab9 <- data.frame(UF = ufs,
                   OE_original = res9$original$oe[ufs],
                   OE_escalar  = res9$escalar$oe[ufs],
                   OE_parcial  = res9$parcial$oe[ufs], row.names = NULL)
tab9 <- tab9[order(-tab9$OE_parcial), ]
tp <- tab9; tp[2:4] <- round(tp[2:4], 3)
cat("\n=== O/E por UF com cada denominador ===\n")
print(tp, row.names = FALSE)
cat("\nSpearman original x parcial:",
    round(cor(tab9$OE_original, tab9$OE_parcial, method = "spearman"), 3), "\n")
write.csv(tab9, CFG9$saida_csv, row.names = FALSE)
cat("Arquivo:", CFG9$saida_csv, "\n")

# ---------------------------------------------------------------------
# C. Checagens finais
#   C1. Índices robustos de ajuste e teste de diferença próprio do WLSMV
#       (os índices escalonados da Parte A não são comparáveis entre
#        modelos aninhados). A coluna RMSEA do teste de diferença é o
#        RMSEA da diferença: o desajuste que cada bloco de restrições
#        acrescenta.
#   C2. Denominador sem nenhum modelo de medida: soma bruta do PHQ-9,
#       com o mesmo corte nacional por prevalência. Mesmo critério de B.
# ---------------------------------------------------------------------
cat("\n=== C1. Índices robustos e teste de diferença (Satorra, 2000) ===\n")
rob <- t(sapply(fits, function(f) tryCatch(
  fitMeasures(f, c("cfi.robust", "rmsea.robust")),
  error = function(e) c(cfi.robust = NA, rmsea.robust = NA))))
print(round(rob, 4))
lrt <- tryCatch(lavTestLRT(fits$configural, fits$limiares, fits$metrica, fits$escalar,
                           method = "satorra.2000"),
                error = function(e) { message("lavTestLRT falhou: ", conditionMessage(e)); NULL })
if (!is.null(lrt)) print(lrt)

cat("\n=== C2. Denominador pela soma bruta do PHQ-9 ===\n")
corte_soma <- corte_nacional(v$phq_soma)
sint3 <- as.integer(v$phq_soma >= corte_soma); sint3[!base_ok] <- NA
r_soma <- heterog(sint3)
soma_ok <- r_soma$p < 0.001 && r_soma$sd_log >= 0.5 * res9$original$sd_log && r_soma$nne_hi < 0
cat(sprintf("Corte nacional na soma: %s | sintomáticos: %d | F UF = %.2f (p = %.2g) | dispersão = %.1f%% | log-OR N/NE = %.3f [%.3f; %.3f] -> %s\n",
            corte_soma, r_soma$n, r_soma$F, r_soma$p,
            100 * r_soma$sd_log / res9$original$sd_log,
            r_soma$nne, r_soma$nne_lo, r_soma$nne_hi,
            if (soma_ok) "O ACHADO SOBREVIVE" else "O ACHADO NÃO SOBREVIVE"))

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r9 <- list(
  grupo       = CFG9$grupo,
  n_grupos    = tente(table(dd$grupo)),
  invariancia = tab_inv,
  escalar_ok  = escalar_ok,
  livres      = livres,
  medias      = tente(rbind(escalar = medias(fits$escalar), parcial = medias(fit_parcial))),
  robustos    = rob,
  lrt         = if (is.null(lrt)) NULL else tente(as.data.frame(lrt)),
  p0          = p0,
  concord     = c(escalar = mean(sint1 == sint0, na.rm = TRUE),
                  parcial = mean(sint2 == sint0, na.rm = TRUE)),
  resumo      = resumo9,
  tab         = tab9,
  ok          = c(a = ok_a, b = ok_b, c = ok_c),
  sobrevive   = ok_a && ok_b && ok_c,
  soma        = list(corte = corte_soma, n = r_soma$n, F = r_soma$F, p = r_soma$p,
                     pct = 100 * r_soma$sd_log / res9$original$sd_log,
                     nne = c(est = r_soma$nne, lo = r_soma$nne_lo, hi = r_soma$nne_hi),
                     oe = r_soma$oe, sobrevive = soma_ok)
)
