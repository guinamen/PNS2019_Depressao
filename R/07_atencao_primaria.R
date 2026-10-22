# =====================================================================
# Rodada 7 — Atenção primária explica "interior > capital"?
# Rodar na MESMA sessão, depois das rodadas 1 a 6
# (usa d, des2 com area/macro/cap/met/UF/..., num(), CFG4, covs$M3)
#
# H_ap: o diagnóstico entre sintomáticos chega pela atenção primária
#       (ESF / médico da família), que é mais presente no interior.
#
# Etapas
#   0. Pré-condição: a cobertura da ESF é MAIOR no interior que na capital?
#      (se não for, H_ap não pode explicar a vantagem do interior)
#   1. Rival do denominador: a prevalência de sintomáticos é maior na capital?
#   2. Efeitos de ESF, plano de saúde e médico da família sobre o diagnóstico
#   3. Mediação: a vantagem do interior e a heterogeneidade entre UFs
#      diminuem quando a atenção primária entra no modelo?
#
# Critério (fixado antes de rodar):
#   Efeito de uma variável (log-OR): CORROBORADO se IC 95% > 0;
#     REFUTADO se IC inteiro abaixo de log(1,25); INCONCLUSIVO caso contrário.
#   H_ap CORROBORADA se: pré-condição vale  E  efeito de ESF (ou médico da
#     família) corroborado  E  o log-OR de capital atenua >= 50%.
#   H_ap REFUTADA se: pré-condição falha  OU  efeito refutado  OU  atenuação < 20%.
#   Caso contrário, INCONCLUSIVA.
#
# Ressalva: em regressão logística, acrescentar covariáveis muda os
# coeficientes mesmo sem confusão (não colapsabilidade), em geral
# AFASTANDO-os de zero. A atenuação medida tende, portanto, a ser
# conservadora.
# =====================================================================

CFG7 <- list(
  esf    = "B001",   # domicílio cadastrado na unidade de saúde da família (CONFERIR)
  esf_sim = 1, esf_nao = 2,                    # 3 = não sabe -> NA
  plano  = "I00101", # tem plano de saúde médico (CONFERIR)
  plano_sim = 1, plano_nao = 2,
  medfam = "MEDFAM", # PLACEHOLDER: médico da família / de referência — ajuste o código
  medfam_sim = 1, medfam_nao = 2,
  saida_csv = "atencao_primaria_resumo.csv"
)
# Para localizar:  achar(d, "família");  achar(d, "médico");  achar(d, "plano")

rc <- function(x, sim, nao) ifelse(x %in% sim, 1L, ifelse(x %in% nao, 0L, NA_integer_))

obrig <- unlist(CFG7[c("esf", "plano")])
if (length(setdiff(obrig, names(d)))) stop("Ajuste CFG7 — ausentes: ",
                                            paste(setdiff(obrig, names(d)), collapse = ", "))
tem_medfam <- CFG7$medfam %in% names(d)
if (!tem_medfam) message("Médico da família (", CFG7$medfam,
                         ") não encontrado — essa parte será pulada. Ajuste CFG7$medfam.")

v <- des2$variables
v$esf   <- rc(num(d[[CFG7$esf]]),   CFG7$esf_sim,   CFG7$esf_nao)
v$plano <- rc(num(d[[CFG7$plano]]), CFG7$plano_sim, CFG7$plano_nao)
if (tem_medfam) v$medfam <- rc(num(d[[CFG7$medfam]]), CFG7$medfam_sim, CFG7$medfam_nao)
des2$variables <- v

classificar <- function(lo, hi, limite = log(1.25))
  if (lo > 0) "CORROBORADO" else if (hi < limite) "REFUTADO" else "INCONCLUSIVO"
relatar <- function(m, termo, rotulo) {
  est <- coef(m)[termo]; ic <- confint(m)[termo, ]
  cat(sprintf("  %-28s log-OR = %6.3f [%6.3f; %6.3f]  OR = %.2f [%.2f; %.2f]  -> %s\n",
              rotulo, est, ic[1], ic[2], exp(est), exp(ic[1]), exp(ic[2]),
              classificar(ic[1], ic[2])))
  invisible(c(est = unname(est), lo = unname(ic[1]), hi = unname(ic[2])))
}
sd_uf <- function(m) { b <- coef(m); sd(c(0, b[grepl("^UF", names(b))])) }
ajusta <- function(des, extra = "")
  svyglm(as.formula(paste("D ~ cap + met + UF +", covs$M3, extra)), des, family = quasibinomial())

sint <- function(extra_filtro = TRUE) subset(des2,
  !is.na(D) & !is.na(post_alvo) & post_alvo > CFG4$corte_post & !is.na(UF) &
  !is.na(idade) & !is.na(mulher) & !is.na(g) & !is.na(rural) & !is.na(renda) &
  !is.na(raca) & !is.na(area))

# ---------------------------------------------------------------------
# 0. Pré-condição
# ---------------------------------------------------------------------
cat("=== 0. Pré-condição: cobertura de ESF e plano entre sintomáticos ===\n")
pre <- subset(sint(), !is.na(esf) & !is.na(plano))
print(svyby(~esf + plano, ~macro + area, pre, svymean))
m_pre <- svyglm(esf ~ cap + met + UF, pre, family = quasibinomial())
b_pre  <- coef(m_pre)["cap"]; ic_pre <- confint(m_pre)["cap", ]
precond <- unname(ic_pre[2] < 0)     # ESF significativamente MENOR na capital
cat(sprintf("ESF na capital vs interior (mesma UF): OR = %.2f [%.2f; %.2f]\n",
            exp(b_pre), exp(ic_pre[1]), exp(ic_pre[2])))
cat("  Pré-condição (ESF menor na capital):", if (precond) "VALE" else "FALHA", "\n")

# ---------------------------------------------------------------------
# 1. Rival do denominador
# ---------------------------------------------------------------------
cat("\n=== 1. Prevalência de sintomáticos (posterior média) por área ===\n")
todos <- subset(des2, !is.na(post_alvo) & !is.na(area) & !is.na(UF))
print(svyby(~post_alvo, ~macro + area, todos, svymean))
m_den <- svyglm(post_alvo ~ cap + met + UF, todos)
b <- coef(m_den)["cap"]; ic <- confint(m_den)["cap", ]
cat(sprintf("  Capital − interior (mesma UF): %.4f [%.4f; %.4f]\n", b, ic[1], ic[2]))

# ---------------------------------------------------------------------
# 2 e 3. Efeitos e mediação — ESF e plano (amostra comum)
# ---------------------------------------------------------------------
cat("\n=== 2. Efeitos sobre o diagnóstico (sintomáticos) ===\n")
des7 <- pre
cat("Amostra comum:", nrow(des7$variables), "\n")
mA <- ajusta(des7)
mB <- ajusta(des7, "+ esf + plano")
cat("Modelo B (com ESF e plano):\n")
r_esf   <- relatar(mB, "esf",   "ESF (cadastro)")
r_plano <- relatar(mB, "plano", "plano de saúde")

cat("\n=== 3. Mediação ===\n")
capA <- coef(mA)["cap"]; capB <- coef(mB)["cap"]
aten <- 1 - capB / capA
cat(sprintf("  log-OR capital: A = %.3f  B = %.3f  -> atenuação = %.0f%%\n", capA, capB, 100 * aten))
cat(sprintf("  Dispersão dos efeitos de UF: A = %.3f  B = %.3f (%.0f%% de A)\n",
            sd_uf(mA), sd_uf(mB), 100 * sd_uf(mB) / sd_uf(mA)))
wA <- regTermTest(mA, ~UF); wB <- regTermTest(mB, ~UF)
cat(sprintf("  Wald UF: A F = %.2f  B F = %.2f\n", wA$Ftest, wB$Ftest))

decide <- function(precond, efeito, aten) {
  if (!precond || efeito == "REFUTADO" || aten < 0.2) "REFUTADA"
  else if (efeito == "CORROBORADO" && aten >= 0.5) "CORROBORADA"
  else "INCONCLUSIVA"
}
v_esf <- classificar(r_esf["lo"], r_esf["hi"])
cat("\n>> H_ap (via ESF):", decide(precond, v_esf, aten), "\n")

resumo <- data.frame(teste = c("ESF", "plano"), est = c(r_esf["est"], r_plano["est"]),
                     lo = c(r_esf["lo"], r_plano["lo"]), hi = c(r_esf["hi"], r_plano["hi"]),
                     capA = capA, capB = capB, atenuacao = aten)

# ---------------------------------------------------------------------
# Médico da família (amostra própria: pode ser perguntado só a um subgrupo)
# ---------------------------------------------------------------------
if (tem_medfam) {
  cat("\n=== Médico da família ===\n")
  cat("Ausentes entre sintomáticos:",
      round(mean(is.na(sint()$variables$medfam)), 3), "\n")
  des_mf <- subset(pre, !is.na(medfam))
  cat("Amostra:", nrow(des_mf$variables), "\n")
  print(svyby(~medfam, ~macro + area, des_mf, svymean))
  mA2 <- ajusta(des_mf)
  mC  <- ajusta(des_mf, "+ esf + plano + medfam")
  r_mf <- relatar(mC, "medfam", "médico da família")
  relatar(mC, "esf", "ESF (com médico da família)")
  aten2 <- 1 - coef(mC)["cap"] / coef(mA2)["cap"]
  cat(sprintf("  log-OR capital: A = %.3f  C = %.3f  -> atenuação = %.0f%%\n",
              coef(mA2)["cap"], coef(mC)["cap"], 100 * aten2))
  v_mf <- classificar(r_mf["lo"], r_mf["hi"])
  cat(">> H_ap (via médico da família):", decide(precond, v_mf, aten2), "\n")
  resumo <- rbind(resumo, data.frame(teste = "medico_familia", est = r_mf["est"],
                  lo = r_mf["lo"], hi = r_mf["hi"], capA = coef(mA2)["cap"],
                  capB = coef(mC)["cap"], atenuacao = aten2))
}

write.csv(resumo, CFG7$saida_csv, row.names = FALSE)
cat("\nArquivo:", CFG7$saida_csv, "\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r7 <- list(
  n          = nrow(des7$variables),
  precond    = precond,
  precond_OR = exp(c(est = unname(b_pre), lo = unname(ic_pre[1]), hi = unname(ic_pre[2]))),
  den        = c(est = unname(b), lo = unname(ic[1]), hi = unname(ic[2])),
  esf        = r_esf,
  plano      = r_plano,
  capA       = unname(capA),
  capB       = unname(capB),
  aten       = unname(aten),
  sdUF       = tente(sd_uf(mB) / sd_uf(mA)),
  veredito   = decide(precond, v_esf, aten)
)
