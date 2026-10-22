# =====================================================================
# Rodada 2 — rodar na MESMA sessão, logo depois de etau_depressao_pns.R
# (usa d, des, CFG, num(), ajustar_lca(), lik_fonte(), calcular_etau(), regiao())
#
#   A. Seleção de K e redefinição da classe-alvo
#   B. Decomposição da perda do T2 (LCA vs. mapeamento LR -> mu/lambda)
#   C. T3 refeito: posterior só pelos sintomas, com pesos e ajuste por idade
#   D. T1': conflito vs. tempo desde o diagnóstico (hipótese reformulada)
# =====================================================================

CFG2 <- list(
  idade      = "C008",   # idade do morador (CONFERIR)
  idade_diag = "Q09301",   # idade no primeiro diagnóstico de depressão (CONFERIR)
  Ks         = 2:5,
  corte_post = 0.8       # posterior mínima para classificar como classe-alvo (C)
)
falta <- setdiff(unlist(CFG2[c("idade", "idade_diag")]), names(d))
if (length(falta)) stop("Ajuste CFG2 — ausentes: ", paste(falta, collapse = ", "))

clip <- function(x) pmin(pmax(x, 1e-12), 1e12)

# ---------------------------------------------------------------------
# A. Seleção de K
#    Com ~90 mil casos o BIC tende a cair sempre; decida também pela
#    plausibilidade da classe-alvo (tamanho e PHQ-9 esperado >= 10).
# ---------------------------------------------------------------------
cat("=== A. Seleção de K ===\n")
ajs <- lapply(CFG2$Ks, function(K) ajustar_lca(d, K = K))
print(data.frame(K = CFG2$Ks, BIC = sapply(ajs, function(a) a$modelo$bic)))
for (i in seq_along(ajs)) {
  a <- ajs[[i]]
  cat("\nK =", CFG2$Ks[i], "\n")
  print(round(rbind(tamanho = a$modelo$P,
                    phq_esperado = a$escore_phq_esperado,
                    t(a$modelo$probs$T3)), 3))
}

K_esc <- CFG2$Ks[which.min(sapply(ajs, function(a) a$modelo$bic))]  # ou fixe à mão
aj2   <- ajs[[match(K_esc, CFG2$Ks)]]
alvo2 <- which(aj2$escore_phq_esperado >= 10)
if (!length(alvo2)) alvo2 <- which.max(aj2$escore_phq_esperado)
aj2$alvo <- alvo2
cat("\nK escolhido:", K_esc, "| classes-alvo:", alvo2,
    "| tamanho da classe-alvo:", round(sum(aj2$modelo$P[alvo2]), 3), "\n")

e2 <- calcular_etau(d, aj2)
des2 <- des
for (v in names(e2)) des2$variables[[paste0(v, "_2")]] <- e2[[v]]
des2$variables$regiao_2 <- regiao(e2$Gc, e2$Gct)
des2$variables$llr_phq  <- log(clip(e2$lr_phq))
des2$variables$llr_t3   <- log(clip(e2$lr_t3))
des2_ok <- subset(des2, !is.na(mu_2))

cat("\nRegiões com a nova classe-alvo:\n")
print(svymean(~regiao_2, des2_ok))
cat("\nConflito por T3 (nova classe-alvo):\n")
print(svyby(~confl_2, ~factor(T3), des2_ok, svymean))

# ---------------------------------------------------------------------
# B. Onde se perde informação no T2?
#    llr ~ phq_soma  -> a perda está no mapeamento LR -> (mu, lambda)
#    llr ~ mu_lambda -> a perda está na compressão do LCA
# ---------------------------------------------------------------------
cat("\n=== B. Decomposição do T2 (mesma amostra) ===\n")
des2_y <- subset(des2, !is.na(mu_2) & !is.na(y_ext))
mods2 <- list(
  phq_soma    = svyglm(y_ext ~ phq_soma,              des2_y, family = quasibinomial()),
  phq_mais_T3 = svyglm(y_ext ~ phq_soma + factor(T3), des2_y, family = quasibinomial()),
  llr         = svyglm(y_ext ~ llr_phq + llr_t3,      des2_y, family = quasibinomial()),
  mu_lambda   = svyglm(y_ext ~ mu_2 + lambda_2,       des2_y, family = quasibinomial()),
  Gc          = svyglm(y_ext ~ Gc_2,                  des2_y, family = quasibinomial())
)
print(t(sapply(mods2, function(m) AIC(m)[c("eff.p", "AIC")])))

# ---------------------------------------------------------------------
# C. T3 refeito
#    Classe-alvo definida SÓ pelos sintomas (sem usar T3), com item-
#    parâmetros comuns a todos os grupos (modelo global). Depois, entre
#    os classificados como alvo, a taxa de diagnóstico por escolaridade,
#    com desenho e ajuste por idade.
#    Refutação: regTermTest de factor(g) sem evidência e coeficientes planos.
# ---------------------------------------------------------------------
cat("\n=== C. Diagnóstico entre sintomáticos, por escolaridade ===\n")
m2   <- aj2$modelo
Lphq <- sweep(lik_fonte(m2, paste0("i", 1:9), d), 2, m2$P, "*")
des2$variables$post_alvo <- rowSums(Lphq[, alvo2, drop = FALSE]) / rowSums(Lphq)
des2$variables$idade     <- num(d[[CFG2$idade]])

des_c <- subset(des2, post_alvo > CFG2$corte_post & !is.na(D) & !is.na(g))
print(svyby(~D, ~factor(g), des_c, svymean))
mt3 <- svyglm(D ~ factor(g) + idade + I(idade^2), des_c, family = quasibinomial())
print(round(summary(mt3)$coefficients, 4))
print(regTermTest(mt3, ~factor(g)))

# ---------------------------------------------------------------------
# D. T1' — hipótese reformulada:
#    entre diagnosticados SEM tratamento, o conflito cresce com o tempo
#    desde o diagnóstico (diagnóstico antigo + sintomas atuais baixos).
#    Refutação: coeficiente de tempo_diag nulo ou negativo.
# ---------------------------------------------------------------------
cat("\n=== D. Conflito vs. tempo desde o diagnóstico ===\n")
idade_diag <- num(d[[CFG2$idade_diag]])
idade_diag[idade_diag < 0 | idade_diag > 110] <- NA   # códigos especiais
des2$variables$tempo_diag <- des2$variables$idade - idade_diag

tempo_res <- list()
for (t3 in 2:3) {
  des_t <- subset(des2, T3 == t3 & !is.na(tempo_diag) & tempo_diag >= 0 & !is.na(confl_2))
  cat("\nT3 =", t3, "\n")
  mt <- svyglm(confl_2 ~ tempo_diag + idade, des_t)
  print(round(summary(mt)$coefficients, 5))
  tempo_res[[as.character(t3)]] <- tente(c(ef(mt, "tempo_diag"),
                                           p = unname(summary(mt)$coefficients["tempo_diag", 4])))
}

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r2 <- list(
  bic      = tente(data.frame(K = CFG2$Ks, BIC = sapply(ajs, function(a) a$modelo$bic))),
  K_esc    = K_esc,
  alvo     = tente(c(tamanho = sum(aj2$modelo$P[alvo2]),
                     phq = max(aj2$escore_phq_esperado[alvo2]))),
  regioes  = tente(coef(svymean(~regiao_2, des2_ok))),
  confl_T3 = tente(sb_df(svyby(~confl_2, ~factor(T3), des2_ok, svymean))),
  T2_aic   = tente(t(sapply(mods2, function(m) AIC(m)[c("eff.p", "AIC")]))),
  acesso_D = tente(sb_df(svyby(~D, ~factor(g), des_c, svymean))),
  acesso_p = tente(as.numeric(regTermTest(mt3, ~factor(g))$p)),
  tempo    = tempo_res
)
