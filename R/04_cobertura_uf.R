# =====================================================================
# Rodada 4 — Capacidade de diagnóstico por UF
# Rodar na MESMA sessão, depois das rodadas 1, 2 e 3
# (usa d, des2, num(), CFG2, post_alvo, idade, g; mulher se existir)
#
# Pergunta: entre quem tem sintomas compatíveis com depressão (classe-alvo
# do LCA com K = 5, definida SÓ pelo PHQ-9), que fração tem diagnóstico?
#
# Três estimadores:
#   1. Suave   — razão  Soma w·post·D / Soma w·post   (usa todos, sem corte)
#   2. Duro    — média de D entre quem tem post_alvo > corte
#   3. O/E     — padronização indireta: diagnósticos observados / esperados
#                dados idade, sexo e escolaridade (modelo nacional).
#                O/E > 1: a UF diagnostica mais do que sua composição prevê.
# Mais: prevalência de sintomáticos e carga de sintomáticos SEM diagnóstico.
# =====================================================================

CFG4 <- list(
  uf         = "V0001",           # Unidade da Federação (CONFERIR)
  sexo       = "C006",            # 1 = homem, 2 = mulher (CONFERIR)
  corte_post = CFG2$corte_post,   # 0,8
  saida_csv  = "cobertura_diagnostico_uf.csv",
  saida_png  = "cobertura_diagnostico_uf.png"
)
if (!CFG4$uf %in% names(d)) stop("Variável de UF ausente — ajuste CFG4$uf")

siglas <- c(`11`="RO", `12`="AC", `13`="AM", `14`="RR", `15`="PA", `16`="AP", `17`="TO",
            `21`="MA", `22`="PI", `23`="CE", `24`="RN", `25`="PB", `26`="PE", `27`="AL",
            `28`="SE", `29`="BA", `31`="MG", `32`="ES", `33`="RJ", `35`="SP", `41`="PR",
            `42`="SC", `43`="RS", `50`="MS", `51`="MT", `52`="GO", `53`="DF")

v <- des2$variables
v$UF <- factor(siglas[as.character(num(d[[CFG4$uf]]))], levels = siglas)
if (is.null(v$mulher)) v$mulher <- as.integer(num(d[[CFG4$sexo]]) == 2)
v$pD  <- v$post_alvo * v$D
v$pND <- v$post_alvo * (1 - v$D)
des2$variables <- v

base <- subset(des2, !is.na(D) & !is.na(post_alvo) & !is.na(UF))

# ---------------------------------------------------------------------
# Brasil
# ---------------------------------------------------------------------
cat("=== Brasil ===\n")
nac <- svyratio(~pD, ~post_alvo, base)
cat(sprintf("Cobertura suave nacional: %.3f (EP %.3f)\n", coef(nac), SE(nac)))

# ---------------------------------------------------------------------
# 1. Cobertura suave por UF
# ---------------------------------------------------------------------
cob_s <- svyby(~pD, ~UF, base, svyratio, denominator = ~post_alvo)

# ---------------------------------------------------------------------
# 2. Cobertura dura por UF
# ---------------------------------------------------------------------
dura  <- subset(base, post_alvo > CFG4$corte_post)
cob_d <- svyby(~D, ~UF, dura, svymean)
n_d   <- table(dura$variables$UF)

# ---------------------------------------------------------------------
# 3. Padronização indireta (O/E)
# ---------------------------------------------------------------------
des_c4 <- subset(dura, !is.na(g) & !is.na(idade) & !is.na(mulher))
m_nac  <- svyglm(D ~ idade + I(idade^2) + mulher + factor(g), des_c4, family = quasibinomial())
f      <- as.numeric(fitted(m_nac))
stopifnot(length(f) == nrow(des_c4$variables))
des_c4$variables$D_esp <- f
oe <- svyby(~D, ~UF, des_c4, svyratio, denominator = ~D_esp)

cat("\n=== Heterogeneidade entre UFs (ajustada por idade, sexo, escolaridade) ===\n")
m_uf <- svyglm(D ~ UF + idade + I(idade^2) + mulher + factor(g), des_c4, family = quasibinomial())
print(regTermTest(m_uf, ~UF))

# ---------------------------------------------------------------------
# 4. Prevalência de sintomáticos e carga sem diagnóstico (% da população)
# ---------------------------------------------------------------------
prev <- svyby(~post_alvo, ~UF, base, svymean)
carga <- svyby(~pND, ~UF, base, svymean)

# ---------------------------------------------------------------------
# Tabela
# ---------------------------------------------------------------------
ic <- function(est, se) cbind(lo = est - 1.96 * se, hi = est + 1.96 * se)
ufs <- levels(base$variables$UF)
tab <- data.frame(
  UF          = ufs,
  n_sint      = as.integer(n_d[ufs]),
  prev_sint   = coef(prev)[ufs],
  cob_suave   = coef(cob_s)[ufs],
  cob_s_lo    = ic(coef(cob_s), SE(cob_s))[ufs, "lo"],
  cob_s_hi    = ic(coef(cob_s), SE(cob_s))[ufs, "hi"],
  cob_dura    = coef(cob_d)[ufs],
  OE          = coef(oe)[ufs],
  OE_lo       = ic(coef(oe), SE(oe))[ufs, "lo"],
  OE_hi       = ic(coef(oe), SE(oe))[ufs, "hi"],
  sem_diag_pop = coef(carga)[ufs],
  row.names = NULL
)
tab <- tab[order(-tab$cob_suave), ]
num_cols <- sapply(tab, is.numeric) & names(tab) != "n_sint"
tab_print <- tab; tab_print[num_cols] <- round(tab_print[num_cols], 3)
cat("\n=== Capacidade de diagnóstico por UF (ordenada pela cobertura suave) ===\n")
print(tab_print, row.names = FALSE)
write.csv(tab, CFG4$saida_csv, row.names = FALSE)

cat("\nCorrelação de Spearman entre cobertura suave e O/E:",
    round(cor(tab$cob_suave, tab$OE, method = "spearman"), 3), "\n")

# ---------------------------------------------------------------------
# Gráfico: cobertura suave com IC 95%, linha tracejada = Brasil
# ---------------------------------------------------------------------
png(CFG4$saida_png, width = 900, height = 1000, res = 120)
o <- order(tab$cob_suave)
par(mar = c(4.5, 4, 2, 1))
plot(tab$cob_suave[o], seq_along(o), pch = 19,
     xlim = range(c(tab$cob_s_lo, tab$cob_s_hi), na.rm = TRUE),
     yaxt = "n", ylab = "", xlab = "Fração diagnosticada entre sintomáticos (suave, IC 95%)",
     main = "Capacidade de diagnóstico de depressão por UF — PNS 2019")
segments(tab$cob_s_lo[o], seq_along(o), tab$cob_s_hi[o], seq_along(o))
axis(2, at = seq_along(o), labels = tab$UF[o], las = 1, cex.axis = 0.8)
abline(v = coef(nac), lty = 2)
dev.off()
cat("\nArquivos:", CFG4$saida_csv, "e", CFG4$saida_png, "\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r4 <- list(
  nac      = tente(c(est = as.numeric(coef(nac)), se = as.numeric(SE(nac)))),
  wald     = tente({ w4 <- regTermTest(m_uf, ~UF)
                     c(F = as.numeric(w4$Ftest), p = as.numeric(w4$p)) }),
  tab      = tab,
  spearman = tente(cor(tab$cob_suave, tab$OE, method = "spearman")),
  png      = normalizePath(CFG4$saida_png)
)
