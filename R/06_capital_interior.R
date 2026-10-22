# =====================================================================
# Rodada 6 — Capital × interior dentro de cada UF
# Rodar na MESMA sessão, depois das rodadas 1 a 5
# (usa d, des2 com UF/rural/renda/raca etc., num(), CFG4, covs$M3)
#
# Comparar capital e interior DENTRO da mesma UF mantém constantes os
# fatores estaduais (desenvolvimento geral, cultura, versão regional do
# questionário). Serviços especializados concentram-se nas capitais.
#
# P1: a cobertura de diagnóstico é maior na capital que no interior da
#     mesma UF (ajustado pela composição de M3).
# P2: a diferença capital − interior é MAIOR no Norte/Nordeste.
#     A macrorregião é um critério geográfico, NÃO derivado da cobertura,
#     para evitar circularidade (classificar UFs por cobertura baixa
#     tornaria a diferença maior por construção).
#
# Critério (fixado antes de rodar), na escala log-odds:
#   corroborada   se IC 95% > 0
#   refutada      se IC 95% inteiro abaixo de log(1,25)  (efeito no máximo pequeno)
#   inconclusiva  nos demais casos
#
# V0031: 1 = capital | 2 = resto da RM | 3 = RIDE (excl. capital) | 4 = resto da UF
# =====================================================================

CFG6 <- list(area = "V0031", saida_csv = "cobertura_capital_interior_uf.csv")
if (!CFG6$area %in% names(d)) stop("Variável V0031 ausente")

v <- des2$variables
a <- num(d[[CFG6$area]])
v$area <- factor(ifelse(a == 1, "capital",
                 ifelse(a %in% 2:3, "metropolitana",
                 ifelse(a == 4, "interior", NA))),
                 levels = c("interior", "metropolitana", "capital"))
cod_uf  <- num(d[[CFG4$uf]])
v$macro <- factor(c(`1` = "Norte", `2` = "Nordeste", `3` = "Sudeste",
                    `4` = "Sul", `5` = "Centro-Oeste")[substr(as.character(cod_uf), 1, 1)],
                  levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))
v$nne <- as.integer(v$macro %in% c("Norte", "Nordeste"))
v$cap <- as.integer(v$area == "capital")
v$met <- as.integer(v$area == "metropolitana")
des2$variables <- v

dura6 <- subset(des2, !is.na(D) & !is.na(post_alvo) & post_alvo > CFG4$corte_post &
                      !is.na(UF) & !is.na(idade) & !is.na(mulher) & !is.na(g) &
                      !is.na(rural) & !is.na(renda) & !is.na(raca) & !is.na(area))
cat("Sintomáticos na amostra:", nrow(dura6$variables), "\n")
cat("\nCasos (sem peso) por macrorregião e tipo de área:\n")
print(table(dura6$variables$macro, dura6$variables$area))

cat("\n=== Cobertura (sem ajuste) por macrorregião e tipo de área ===\n")
print(svyby(~D, ~macro + area, dura6, svymean))

classificar <- function(est, lo, hi, limite = log(1.25)) {
  if (lo > 0) "CORROBORADA" else if (hi < limite) "REFUTADA" else "INCONCLUSIVA"
}
relatar <- function(m, termo, rotulo) {
  est <- coef(m)[termo]; ic <- confint(m)[termo, ]
  cat(sprintf("%s: log-OR = %.3f [%.3f; %.3f] | OR = %.2f [%.2f; %.2f] -> %s\n",
              rotulo, est, ic[1], ic[2], exp(est), exp(ic[1]), exp(ic[2]),
              classificar(est, ic[1], ic[2])))
}

# ---------------------------------------------------------------------
# P1 — capital × interior, efeitos fixos de UF + composição M3
# ---------------------------------------------------------------------
cat("\n=== P1: capital vs. interior (dentro da UF) ===\n")
m1 <- svyglm(as.formula(paste("D ~ cap + met + UF +", covs$M3)), dura6, family = quasibinomial())
relatar(m1, "cap", "Capital vs interior")
relatar(m1, "met", "Metropolitana vs interior")

# ---------------------------------------------------------------------
# P2 — a diferença capital − interior é maior no Norte/Nordeste?
#      (o efeito principal de N/NE é absorvido pelos efeitos fixos de UF)
# ---------------------------------------------------------------------
cat("\n=== P2: interação capital × Norte/Nordeste ===\n")
m2 <- svyglm(as.formula(paste("D ~ cap + met + I(cap * nne) + I(met * nne) + UF +", covs$M3)),
             dura6, family = quasibinomial())
relatar(m2, "cap", "Capital vs interior (Sul/Sudeste/Centro-Oeste)")
relatar(m2, "I(cap * nne)", "Diferença adicional no Norte/Nordeste")
cat("Teste conjunto das interações:\n")
print(regTermTest(m2, ~ I(cap * nne) + I(met * nne)))

# ---------------------------------------------------------------------
# Descritivo por UF: diferença capital − interior (sem ajuste)
# ---------------------------------------------------------------------
cat("\n=== Diferença capital − interior por UF (sem ajuste, descritivo) ===\n")
ci <- subset(dura6, area %in% c("capital", "interior"))
by_uf <- svyby(~D, ~UF + area, ci, svymean, keep.var = TRUE)
df_uf <- data.frame(UF = by_uf$UF, area = by_uf$area, cob = coef(by_uf), se = SE(by_uf))
w <- reshape(df_uf, idvar = "UF", timevar = "area", direction = "wide")
w <- w[complete.cases(w), ]
w$dif    <- w$cob.capital - w$cob.interior
w$dif_lo <- w$dif - 1.96 * sqrt(w$se.capital^2 + w$se.interior^2)
w$dif_hi <- w$dif + 1.96 * sqrt(w$se.capital^2 + w$se.interior^2)
w$macro  <- dura6$variables$macro[match(w$UF, dura6$variables$UF)]
w <- w[order(-w$dif), c("UF", "macro", "cob.capital", "cob.interior", "dif", "dif_lo", "dif_hi")]
wp <- w; wp[3:7] <- round(wp[3:7], 3)
print(wp, row.names = FALSE)
cat("\nUFs sem par capital/interior na amostra (ex.: DF):",
    paste(setdiff(levels(dura6$variables$UF), as.character(w$UF)), collapse = ", "), "\n")
write.csv(w, CFG6$saida_csv, row.names = FALSE)
cat("Arquivo:", CFG6$saida_csv, "\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r6 <- list(
  cap      = tente(ef(m1, "cap")),
  met      = tente(ef(m1, "met")),
  cap_ssco = tente(ef(m2, "cap")),
  inter    = tente(ef(m2, "I(cap * nne)")),
  p_inter  = tente(as.numeric(regTermTest(m2, ~ I(cap * nne) + I(met * nne))$p)),
  cob_area = tente(sb_df(svyby(~D, ~macro + area, dura6, svymean))),
  tab_uf   = w
)
