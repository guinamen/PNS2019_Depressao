# =====================================================================
# Rodada 7b — Intensidade de contato com a equipe de Saúde da Família
# Rodar na MESMA sessão, logo depois de etau_rodada7_atencao_primaria.R
# (usa d, des2, num(), CFG7, sint(), ajusta(), relatar(), classificar(),
#  decide(), sd_uf())
#
# O cadastro (B001) mede só vínculo formal. B003 mede contato efetivo:
#   B003 — nos últimos 12 meses, visitas de ACS ou da equipe de SF:
#     1 mensalmente | 2 a cada 2 meses | 3 de 2 a 4 vezes | 4 uma vez
#     5 nunca recebeu | 9 ignorado | vazio = não aplicável
#
# Escala usada: visitas = 0 (nunca) .. 4 (mensal); domicílio não cadastrado
# (B001 = 2) recebe 0. Para o teste, visitas01 = visitas / 4, de modo que o
# coeficiente é o contraste "mensal vs. nunca".
#
# H_vis: mais contato com a equipe -> mais diagnóstico entre sintomáticos,
#        e isso explica a vantagem do interior.
# Mesmo critério da rodada 7 (pré-condição + efeito + atenuação >= 50%).
#
# Ressalva de causalidade reversa: ACS priorizam domicílios com doentes
# crônicos, e o diagnóstico é de vida (pode ter vindo antes das visitas).
# Ambos INFLARIAM a associação positiva; um efeito nulo é, portanto,
# informativo, mas um efeito positivo não prova o mecanismo.
# =====================================================================

CFG7b <- list(visita = "B003")
if (!CFG7b$visita %in% names(d)) stop("B003 ausente")

b1 <- num(d[[CFG7$esf]]); b3 <- num(d[[CFG7b$visita]])
cat("Conferência do salto (B001 x B003):\n")
print(table(B001 = b1, B003 = b3, useNA = "ifany"))

vis <- c(4, 3, 2, 1, 0)[match(b3, 1:5)]
vis[b1 %in% 2 & is.na(b3)] <- 0          # não cadastrado: sem visitas
des2$variables$visitas   <- vis
des2$variables$visitas01 <- vis / 4

# ---------------------------------------------------------------------
# Pré-condição: menos visitas na capital (mesma UF)?
# ---------------------------------------------------------------------
cat("\n=== Pré-condição: visitas entre sintomáticos ===\n")
s7b <- subset(sint(), !is.na(visitas) & !is.na(plano))
cat("Amostra:", nrow(s7b$variables), "\n")
print(svyby(~visitas, ~macro + area, s7b, svymean))
m_pre_v <- svyglm(visitas ~ cap + met + UF, s7b)
b <- coef(m_pre_v)["cap"]; ic <- confint(m_pre_v)["cap", ]
precond_v <- unname(ic[2] < 0)
cat(sprintf("Capital − interior (escala 0–4): %.3f [%.3f; %.3f]  -> pré-condição %s\n",
            b, ic[1], ic[2], if (precond_v) "VALE" else "FALHA"))

# ---------------------------------------------------------------------
# Efeito e forma da relação
# ---------------------------------------------------------------------
cat("\n=== Efeito das visitas sobre o diagnóstico ===\n")
mA_v <- ajusta(s7b)
mV   <- ajusta(s7b, "+ visitas01 + plano")
r_v  <- relatar(mV, "visitas01", "mensal vs nunca (linear)")

cat("\nForma da relação (categorias, referência = nunca):\n")
mVc <- ajusta(s7b, "+ factor(visitas) + plano")
bc  <- coef(mVc); icc <- confint(mVc)
k   <- grep("^factor\\(visitas\\)", names(bc))
print(round(cbind(OR = exp(bc[k]), lo = exp(icc[k, 1]), hi = exp(icc[k, 2])), 3))

# ---------------------------------------------------------------------
# Mediação
# ---------------------------------------------------------------------
cat("\n=== Mediação ===\n")
capA <- coef(mA_v)["cap"]; capV <- coef(mV)["cap"]
aten_v <- 1 - capV / capA
cat(sprintf("  log-OR capital: A = %.3f  com visitas = %.3f  -> atenuação = %.0f%%\n",
            capA, capV, 100 * aten_v))
cat(sprintf("  Dispersão dos efeitos de UF: %.0f%% de A\n", 100 * sd_uf(mV) / sd_uf(mA_v)))

v_vis <- classificar(r_v["lo"], r_v["hi"])
cat("\n>> H_vis:", decide(precond_v, v_vis, aten_v), "\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r7b <- list(
  n           = nrow(s7b$variables),
  precond     = precond_v,
  precond_est = c(est = unname(b), lo = unname(ic[1]), hi = unname(ic[2])),
  vis         = r_v,
  cats        = tente(data.frame(categoria = names(bc[k]), OR = exp(unname(bc[k])),
                                 lo = exp(unname(icc[k, 1])), hi = exp(unname(icc[k, 2])),
                                 row.names = NULL)),
  aten        = unname(aten_v),
  sdUF        = tente(sd_uf(mV) / sd_uf(mA_v)),
  veredito    = decide(precond_v, v_vis, aten_v)
)
