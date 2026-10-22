# =====================================================================
# Rodada 8 — O denominador explica a desvantagem da capital?
# Rodar na MESMA sessão, depois das rodadas 1 a 7
# (usa d, des2, CFG, CFG4, covs$M3, sint(), ajusta())
#
# Rival R_den: a capital tem um excesso de sintomáticos (≈ +1,2 p.p.) que
# NÃO é depressão (pontua alto no PHQ-9 por outras razões) e nunca seria
# diagnosticado. Isso diluiria a cobertura da capital.
#
# Parte A — Análise de limites (construção minha)
#   f = fração do excesso de sintomáticos da capital que é espúria.
#   Para cada f, removem-se (por repesagem) f·Δ da população da capital
#   dentre os sintomáticos NÃO diagnosticados da capital, UF a UF, e
#   reajusta-se o modelo de P1 (capital vs interior, efeitos fixos de UF,
#   composição M3). f não é estimável com a PNS: a análise diz quão forte
#   precisa ser a suposição para o efeito sumir.
#   Leitura (fixada antes de rodar):
#     f* = menor f com log-OR >= 0.
#     f* <= 1 -> R_den é SUFICIENTE (basta parte do excesso ser espúria)
#     f* >  1 -> R_den é IMPLAUSÍVEL como explicação única
#   Repete-se com Δ no limite superior do IC (cenário mais generoso ao rival).
#
# Parte B — Teste direto pelo perfil de sintomas
#   Se o excesso da capital fosse "não depressão", os sintomáticos da
#   capital teriam MENOS peso dos sintomas nucleares (anedonia e humor
#   deprimido) e mais dos inespecíficos (sono, cansaço, apetite).
#   Critério: R_den CORROBORADO se o IC da diferença de participação dos
#   itens nucleares (capital − interior) estiver inteiro abaixo de zero;
#   REFUTADO se o IC excluir reduções maiores que 5% da média;
#   INCONCLUSIVO caso contrário.
# =====================================================================

CFG8 <- list(
  fs = seq(0, 2, by = 0.1),
  # Itens nucleares do PHQ-9 (anedonia, humor deprimido).
  # ATENÇÃO: a ordem dos itens na PNS difere do PHQ-9 original — CONFERIR.
  core_itens = c("N012", "N016"),
  saida_csv = "limites_denominador.csv"
)
idx_core <- match(CFG8$core_itens, CFG$phq_itens)
if (anyNA(idx_core)) stop("Itens nucleares não encontrados em CFG$phq_itens")
cat("Itens nucleares usados:", CFG8$core_itens, "-> i", idx_core, "\n")

# ---------------------------------------------------------------------
# Δ: excesso de sintomáticos (classificação dura, a mesma do numerador)
# ---------------------------------------------------------------------
todos8 <- subset(des2, !is.na(post_alvo) & !is.na(area) & !is.na(UF))
todos8$variables$sint <- as.integer(todos8$variables$post_alvo > CFG4$corte_post)
m_d   <- svyglm(sint ~ cap + met + UF, todos8)
delta <- unname(coef(m_d)["cap"]); ci_d <- unname(confint(m_d)["cap", ])
cat(sprintf("Δ (capital − interior, sintomáticos, mesma UF): %.4f [%.4f; %.4f]\n",
            delta, ci_d[1], ci_d[2]))

pc  <- svyby(~sint, ~UF, subset(todos8, area == "capital"), svymean)
pcv <- setNames(coef(pc), as.character(pc$UF))

# ---------------------------------------------------------------------
# A. Limites
# ---------------------------------------------------------------------
s8  <- sint()
v8  <- s8$variables
w0  <- 1 / s8$prob
uf  <- as.character(v8$UF)
cap_i <- v8$area == "capital"
und_i <- cap_i & v8$D == 0
Wsint <- tapply(w0[cap_i], uf[cap_i], sum)
Wund  <- tapply(w0[und_i], uf[und_i], sum)

efeito_f <- function(f, dlt) {
  s_uf <- pmin(f * dlt / pcv[names(Wsint)], 1)        # fração espúria dos sintomáticos da capital
  k    <- 1 - (s_uf * Wsint) / Wund[names(Wsint)]
  k[is.na(k)] <- 1
  esgotou <- any(k <= 0)
  k <- pmax(k, 0.01)
  des_f <- s8
  ii <- which(und_i)
  des_f$prob[ii] <- s8$prob[ii] / k[uf[ii]]
  m <- ajusta(des_f)
  ic <- confint(m)["cap", ]
  data.frame(f = f, logOR = unname(coef(m)["cap"]), lo = unname(ic[1]), hi = unname(ic[2]),
             esgotou_nao_diag = esgotou)
}

resumir <- function(tab, rotulo) {
  cat("\n---", rotulo, "---\n")
  tp <- tab; tp[2:4] <- round(tp[2:4], 3)
  print(tp, row.names = FALSE)
  f_ns   <- tab$f[which(tab$hi >= 0)[1]]
  f_star <- tab$f[which(tab$logOR >= 0)[1]]
  cat(sprintf("f em que o efeito deixa de ser significativo: %s\n",
              ifelse(is.na(f_ns), "> 2", f_ns)))
  cat(sprintf("f* (efeito pontual chega a zero): %s  ->  %s\n",
              ifelse(is.na(f_star), "> 2", f_star),
              if (!is.na(f_star) && f_star <= 1) "R_den SUFICIENTE"
              else "R_den IMPLAUSÍVEL como explicação única"))
}

cat("\n=== A. Análise de limites ===\n")
tabA  <- do.call(rbind, lapply(CFG8$fs, efeito_f, dlt = delta))
resumir(tabA, "Δ pontual")
tabA2 <- do.call(rbind, lapply(CFG8$fs, efeito_f, dlt = ci_d[2]))
resumir(tabA2, "Δ no limite superior do IC (generoso ao rival)")
write.csv(rbind(cbind(cenario = "delta_pontual", tabA),
                cbind(cenario = "delta_sup", tabA2)), CFG8$saida_csv, row.names = FALSE)

# ---------------------------------------------------------------------
# B. Perfil de sintomas
# ---------------------------------------------------------------------
cat("\n=== B. Participação dos itens nucleares entre sintomáticos ===\n")
v <- des2$variables
core <- rowSums(sapply(idx_core, function(j) v[[paste0("i", j)]] - 1))
des2$variables$core_share <- core / pmax(v$phq_soma, 1)
sB <- sint()
med <- coef(svymean(~core_share, sB, na.rm = TRUE))
print(svyby(~core_share, ~macro + area, sB, svymean, na.rm = TRUE))
mB8 <- svyglm(as.formula(paste("core_share ~ cap + met + UF +", covs$M3)), sB)
b <- unname(coef(mB8)["cap"]); ic <- unname(confint(mB8)["cap", ])
lim <- -0.05 * med
veredito <- if (ic[2] < 0) "R_den CORROBORADO" else if (ic[1] > lim) "R_den REFUTADO" else "INCONCLUSIVO"
cat(sprintf("\nMédia da participação nuclear: %.3f | capital − interior: %.4f [%.4f; %.4f] (limite %.4f) -> %s\n",
            med, b, ic[1], ic[2], lim, veredito))
cat("Arquivo:", CFG8$saida_csv, "\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
f_limites <- function(tab) c(f_ns   = tab$f[which(tab$hi >= 0)[1]],
                             f_star = tab$f[which(tab$logOR >= 0)[1]])
RES$r8 <- list(
  core_itens  = CFG8$core_itens,
  delta       = c(est = delta, lo = ci_d[1], hi = ci_d[2]),
  limites     = tabA,
  limites_sup = tabA2,
  f           = f_limites(tabA),
  f_sup       = f_limites(tabA2),
  core        = c(media = unname(med), est = b, lo = ic[1], hi = ic[2], limite = unname(lim)),
  veredito_B  = veredito,
  core_area   = tente(sb_df(svyby(~core_share, ~macro + area, sB, svymean, na.rm = TRUE)))
)
