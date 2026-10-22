# =====================================================================
# Rodada 5 — O efeito de UF sobrevive à composição?
# Rodar na MESMA sessão, depois das rodadas 1 a 4
# (usa d, des2 com UF/mulher/post_alvo/idade/g, num(), CFG4)
#
# Hipótese H_cap: as diferenças de cobertura entre UFs NÃO se reduzem à
# composição da população de sintomáticos.
#
# Modelos aninhados (mesma amostra em todos, para serem comparáveis):
#   M0  idade + sexo + escolaridade            (o da rodada 4)
#   M1  M0 + rural
#   M2  M1 + renda domiciliar per capita
#   M3  M2 + cor/raça
#
# Cor/raça entra como MARCADOR de desigualdade de acesso já documentada
# na PNS, não como causa. O objetivo é decompor, não explicar.
#
# Critério (fixado antes de rodar), avaliado em M3:
#   H_cap sobrevive se  (a) Wald de UF com p < 0,001  E
#                       (b) o desvio-padrão do log(O/E) entre UFs mantiver
#                           ao menos 50% do valor em M0.
#   Caso contrário, o "efeito de estado" se reduz à composição.
# =====================================================================

CFG5 <- list(
  urbano = "V0026",   # situação do domicílio: 1 = urbano, 2 = rural (CONFERIR)
  renda  = "VDF004",  # faixa de rendimento domiciliar per capita, 1..7 (CONFERIR)
  raca   = "C009",    # cor ou raça: 1 branca, 2 preta, 3 amarela, 4 parda, 5 indígena (CONFERIR)
  saida_csv = "cobertura_uf_composicao.csv"
)
falta <- setdiff(unlist(CFG5[c("urbano", "renda", "raca")]), names(d))
if (length(falta)) stop("Ajuste CFG5 — ausentes: ", paste(falta, collapse = ", "))

v <- des2$variables
v$rural <- as.integer(num(d[[CFG5$urbano]]) == 2)
r <- num(d[[CFG5$renda]]); r[!(r %in% 1:7)] <- NA
v$renda <- factor(r)
rc <- num(d[[CFG5$raca]])
v$raca <- factor(ifelse(rc == 1, "branca", ifelse(rc == 2, "preta",
                 ifelse(rc == 4, "parda", ifelse(rc %in% c(3, 5), "outras", NA)))),
                 levels = c("branca", "parda", "preta", "outras"))
des2$variables <- v

cat("Distribuição das novas variáveis entre sintomáticos (sem peso):\n")
s <- v$post_alvo > CFG4$corte_post & !is.na(v$D)
print(table(rural = v$rural[s], useNA = "ifany"))
print(table(renda = v$renda[s], useNA = "ifany"))
print(table(raca  = v$raca[s],  useNA = "ifany"))

# Amostra comum a todos os modelos
dura5 <- subset(des2, !is.na(D) & !is.na(post_alvo) & post_alvo > CFG4$corte_post &
                      !is.na(UF) & !is.na(idade) & !is.na(mulher) & !is.na(g) &
                      !is.na(rural) & !is.na(renda) & !is.na(raca))
cat("\nAmostra comum:", nrow(dura5$variables), "sintomáticos\n")

covs <- list(
  M0 = "idade + I(idade^2) + mulher + factor(g)",
  M1 = "idade + I(idade^2) + mulher + factor(g) + rural",
  M2 = "idade + I(idade^2) + mulher + factor(g) + rural + renda",
  M3 = "idade + I(idade^2) + mulher + factor(g) + rural + renda + raca"
)

rodar <- function(cv) {
  m_nac <- svyglm(as.formula(paste("D ~", cv)), dura5, family = quasibinomial())
  f <- as.numeric(fitted(m_nac))
  stopifnot(length(f) == nrow(dura5$variables))
  dz <- dura5; dz$variables$D_esp <- f
  oe <- svyby(~D, ~UF, dz, svyratio, denominator = ~D_esp)
  m_uf <- svyglm(as.formula(paste("D ~ UF +", cv)), dura5, family = quasibinomial())
  w <- regTermTest(m_uf, ~UF)
  e <- coef(oe)
  list(oe = oe, F = as.numeric(w$Ftest), p = as.numeric(w$p),
       sd_log = sd(log(e)), razao = max(e) / min(e),
       uf_max = names(e)[which.max(e)], uf_min = names(e)[which.min(e)])
}

res <- lapply(covs, rodar)

cat("\n=== Heterogeneidade remanescente por modelo ===\n")
resumo <- data.frame(
  modelo   = names(res),
  F_UF     = round(sapply(res, `[[`, "F"), 2),
  p_UF     = signif(sapply(res, `[[`, "p"), 3),
  sd_logOE = round(sapply(res, `[[`, "sd_log"), 3),
  pct_M0   = round(100 * sapply(res, `[[`, "sd_log") / res$M0$sd_log, 1),
  razao_max_min = round(sapply(res, `[[`, "razao"), 2),
  maior    = sapply(res, `[[`, "uf_max"),
  menor    = sapply(res, `[[`, "uf_min"),
  row.names = NULL
)
print(resumo, row.names = FALSE)

# Decisão pelo critério fixado
ok_a <- res$M3$p < 0.001
ok_b <- res$M3$sd_log >= 0.5 * res$M0$sd_log
cat(sprintf("\nCritério em M3: (a) p < 0,001: %s | (b) dispersão >= 50%% de M0: %s  ->  %s\n",
            ok_a, ok_b,
            if (ok_a && ok_b) "H_cap SOBREVIVE: o efeito de UF não se reduz à composição"
            else "H_cap REFUTADA: o efeito de UF é em grande parte composição"))

# Tabela por UF: O/E em M0 e M3, com IC de M3
ufs <- levels(dura5$variables$UF)
ic  <- function(o) cbind(lo = coef(o) - 1.96 * SE(o), hi = coef(o) + 1.96 * SE(o))
tab5 <- data.frame(
  UF    = ufs,
  OE_M0 = coef(res$M0$oe)[ufs],
  OE_M1 = coef(res$M1$oe)[ufs],
  OE_M2 = coef(res$M2$oe)[ufs],
  OE_M3 = coef(res$M3$oe)[ufs],
  M3_lo = ic(res$M3$oe)[ufs, "lo"],
  M3_hi = ic(res$M3$oe)[ufs, "hi"],
  row.names = NULL
)
tab5 <- tab5[order(-tab5$OE_M3), ]
tab5$grupo_M3 <- ifelse(tab5$M3_lo > 1, "acima", ifelse(tab5$M3_hi < 1, "abaixo", "média"))
tp <- tab5; tp[2:7] <- round(tp[2:7], 3)
cat("\n=== O/E por UF ao longo dos ajustes (ordenado por M3) ===\n")
print(tp, row.names = FALSE)
cat("\nSpearman O/E M0 x M3:", round(cor(tab5$OE_M0, tab5$OE_M3, method = "spearman"), 3), "\n")
cat("UFs por grupo em M3:\n"); print(table(tab5$grupo_M3))
write.csv(tab5, CFG5$saida_csv, row.names = FALSE)
cat("Arquivo:", CFG5$saida_csv, "\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r5 <- list(
  n        = nrow(dura5$variables),
  resumo   = resumo,
  tab5     = tab5,
  ok_a     = ok_a,
  ok_b     = ok_b,
  spearman = tente(cor(tab5$OE_M0, tab5$OE_M3, method = "spearman"))
)
