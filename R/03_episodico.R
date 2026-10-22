# =====================================================================
# Rodada 3 — Teste do curso episódico
# Rodar na MESMA sessão, depois de etau_depressao_pns.R e etau_rodada2.R
# (usa d, des2, num(), CFG2 e a variável confl_2 da classe-alvo com K = 5)
#
# Hipótese H_ep: entre diagnosticados, o conflito (diagnóstico de vida
# + poucos sintomas atuais) é MENOR em quem procurou atendimento por
# depressão nos últimos 12 meses, porque essas pessoas estariam em
# episódio ativo, e não entre episódios.
#
# Critério de refutação (fixado antes de rodar):
#   - coeficiente de atend12 >= 0, OU
#   - IC 95% inclui 0 e exclui efeitos menores que -0,03
#     (ou seja, o efeito, se existir, é desprezível frente à base ~0,4)
#   O teste é feito DENTRO de cada situação de tratamento (T3 = 2 e 3)
#   e ajustado por idade, sexo e escolaridade, para não confundir
#   "procurou atendimento" com "está em tratamento" ou com "tem acesso".
# =====================================================================

CFG3 <- list(
  ultimo_atend = "Q10101",  # última vez que recebeu atendimento médico por depressão
  sexo         = "C006"     # CONFERIR: 1 = homem, 2 = mulher
)
# Q10101: 1 = < 6 meses | 2 = 6 meses a < 1 ano | 3 = 1 a < 2 anos
#         4 = 2 a < 3 anos | 5 = 3 anos ou mais | 6 = nunca fez
#         9 = ignorado     | vazio = não aplicável
falta <- setdiff(unlist(CFG3), names(d))
if (length(falta)) stop("Ajuste CFG3 — ausentes: ", paste(falta, collapse = ", "))

ua <- num(d[[CFG3$ultimo_atend]])
ua[!(ua %in% 1:6)] <- NA                                   # 9 e não aplicável -> NA
des2$variables$ult_atend <- ua
des2$variables$atend12   <- ifelse(ua %in% 1:2, 1L, ifelse(ua %in% 3:6, 0L, NA_integer_))
des2$variables$mulher    <- as.integer(num(d[[CFG3$sexo]]) == 2)

cat("Conferência: última consulta x T3 (deve existir só para diagnosticados)\n")
print(table(T3 = d$T3, ult_atend = ua, useNA = "ifany"))

des_ep <- subset(des2, T3 %in% 2:3 & !is.na(atend12) & !is.na(confl_2) & !is.na(g))

# ---------------------------------------------------------------------
# 1. Descritivo: conflito por atendimento recente, dentro de cada T3
# ---------------------------------------------------------------------
cat("\n=== Conflito médio por atendimento nos últimos 12 meses ===\n")
print(svyby(~confl_2, ~factor(T3) + factor(atend12), des_ep, svymean))

# ---------------------------------------------------------------------
# 2. Teste principal, estratificado por T3
# ---------------------------------------------------------------------
avaliar <- function(m, termo = "atend12", limite = -0.03) {
  est <- coef(m)[termo]
  ic  <- confint(m)[termo, ]
  refutada <- est >= 0 || (ic[1] <= 0 && ic[2] >= 0 && ic[1] > limite)
  cat(sprintf("  efeito = %.4f  IC95%% = [%.4f, %.4f]  ->  %s\n",
              est, ic[1], ic[2],
              if (refutada) "H_ep REFUTADA neste estrato" else "H_ep sobreviveu neste estrato"))
}

r3_bin <- list(); r3_trend <- list()
for (t3 in 2:3) {
  cat("\n=== T3 =", t3, if (t3 == 2) "(diagnóstico sem tratamento)" else "(com tratamento)", "===\n")
  m <- svyglm(confl_2 ~ atend12 + idade + I(idade^2) + mulher + factor(g),
              subset(des_ep, T3 == t3))
  print(round(summary(m)$coefficients[1:5, ], 5))
  avaliar(m)
  r3_bin[[as.character(t3)]] <- tente(ef(m, "atend12"))
}

# ---------------------------------------------------------------------
# 3. Checagem do mecanismo (não é teste independente):
#    se H_ep vale, a diferença no conflito deve vir do PHQ-9, não do T3.
#    Compara a soma do PHQ-9 entre quem procurou e quem não procurou.
# ---------------------------------------------------------------------
cat("\n=== Mecanismo: PHQ-9 médio por atendimento recente ===\n")
print(svyby(~phq_soma, ~factor(T3) + factor(atend12), des_ep, svymean))

# ---------------------------------------------------------------------
# 4. Teste de dose-resposta (mais exigente que o binário)
#    Se H_ep vale, o conflito deve SUBIR monotonicamente com o tempo
#    desde a última consulta (categorias 1..5; "nunca fez" à parte).
#    Refutação: tendência linear <= 0, ou perfil claramente não monotônico.
# ---------------------------------------------------------------------
cat("\n=== Dose-resposta: conflito por tempo desde a última consulta ===\n")
print(svyby(~confl_2, ~factor(T3) + factor(ult_atend), des_ep, svymean))

for (t3 in 2:3) {
  cat("\nTendência linear (categorias 1..5), T3 =", t3, "\n")
  m <- svyglm(confl_2 ~ ult_atend + idade + I(idade^2) + mulher + factor(g),
              subset(des_ep, T3 == t3 & ult_atend %in% 1:5))
  est <- coef(m)["ult_atend"]; ic <- confint(m)["ult_atend", ]
  cat(sprintf("  inclinação por categoria = %.4f  IC95%% = [%.4f, %.4f]  ->  %s\n",
              est, ic[1], ic[2],
              if (ic[1] > 0) "compatível com H_ep" else "H_ep não sustentada (dose-resposta)"))
  r3_trend[[as.character(t3)]] <- tente(ef(m, "ult_atend"))
}

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r3 <- list(
  bin       = r3_bin,
  trend     = r3_trend,
  confl_ult = tente(sb_df(svyby(~confl_2, ~factor(T3) + factor(ult_atend), des_ep, svymean)))
)
