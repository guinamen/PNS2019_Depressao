# =====================================================================
# Depressão na PNS 2019 como proposição anotada da lógica Eτ (LPA2v)
#
#   p(mu, lambda)   mu = evidência favorável a "depressão atual (tratada ou não)"
#             lambda = evidência contrária
#   Gc  = mu − lambda        (grau de certeza)
#   Gct = mu + lambda − 1    (grau de contradição)
#
# Os pesos de evidência NÃO são escolhidos à mão: vêm de razões de
# verossimilhança (LR) estimadas por um modelo de classes latentes (LCA)
# com os 9 itens do PHQ-9 + uma variável de diagnóstico/tratamento.
#
# Etapas
#   0. leitura e conferência de variáveis
#   1. construção das variáveis (PHQ-9, diagnóstico, tratamento)
#   2. LCA (global e, opcionalmente, por grupo de acesso)
#   3. LR por fonte -> (mu, lambda) -> Gc, Gct, regiões do reticulado
#   4. estimativas com desenho amostral complexo
#   5. testes de falseamento (T1, T2, T3)
#   6. análise de sensibilidade (escala dos log-LR)
#
# Pacotes: install.packages(c("survey", "poLCA"))
# =====================================================================

suppressPackageStartupMessages({
  library(survey)
  library(poLCA)
})
options(survey.lonely.psu = "adjust")
set.seed(20261021)

# ---------------------------------------------------------------------
# Utilidades de registro: cada rodada guarda seus resultados em RES,
# que o relatório (R/relatorio.R) lê para montar o placar e o texto.
# ---------------------------------------------------------------------
if (!exists("RES")) RES <- list()
tente <- function(x) tryCatch(x, error = function(e) {
  message("  [registro] falhou: ", conditionMessage(e)); NA })
ef <- function(m, termo) {
  b <- coef(m)[termo]; ic <- confint(m)[termo, ]
  c(est = unname(b), lo = unname(ic[1]), hi = unname(ic[2]))
}
sb_df <- function(x) data.frame(grupo = names(coef(x)), est = unname(coef(x)),
                                se = unname(SE(x)), row.names = NULL)

# ---------------------------------------------------------------------
# Configuração — CONFERIR TODOS OS CÓDIGOS NO DICIONÁRIO DA PNS 2019
# ---------------------------------------------------------------------
CFG <- list(
  arquivo   = "data/pns2019.rds",
  phq_itens = sprintf("N0%02d", 10:18),  # N010..N018, códigos 1..4 (CONFERIR)
  diag      = "Q092",     # diagnóstico de depressão por profissional (CONFERIR)
  med       = "Q09606",   # usa medicamento p/ depressão atualmente (PLACEHOLDER)
  psi       = "Q09605",   # faz psicoterapia atualmente (PLACEHOLDER)
  peso      = "V00291",   # peso do morador selecionado (CONFERIR)
  estrato   = "V0024",
  idade      = "C008",   # idade do morador (CONFERIR)
  idade_diag = "Q093",   # idade no primeiro diagnóstico de depressão (CONFERIR)
  upa       = "UPA_PNS",
  grupo     = "VDD004A",  # proxy de acesso: nível de instrução (CONFERIR); NULL desliga
  desfecho  = "N001",     # autoavaliação de saúde, 1=muito boa .. 5=muito ruim (CONFERIR)
  K             = 2,      # nº de classes latentes
  classes_alvo  = NULL,   # NULL = classe com maior escore esperado no PHQ-9
  nrep          = 5,      # reinícios do EM no poLCA
  agregacao     = "psum", # "psum" (soma probabilística) ou "max"
  limiar        = 0.5,    # limiar de decisão do para-analisador
  lca_por_grupo = FALSE,  # TRUE = LR específicas por grupo de acesso (teste T3)
  escalas_sens  = c(0.5, 0.75, 1, 1.5, 2)

)

# ---------------------------------------------------------------------
# 0. Leitura
# ---------------------------------------------------------------------
obj   <- readRDS(path.expand(CFG$arquivo))
colnames(obj) <- toupper(colnames(obj))
dados <- if (inherits(obj, "survey.design")) obj$variables else as.data.frame(obj)
rm(obj)

# Procura variáveis pelo nome ou rótulo — ex.: achar(dados, "depress")
achar <- function(df, padrao) {
  lab <- vapply(df, function(x) {
    l <- attr(x, "label"); if (is.null(l)) "" else as.character(l)[1]
  }, "")
  i <- grepl(padrao, lab, ignore.case = TRUE) | grepl(padrao, names(df), ignore.case = TRUE)
  data.frame(var = names(df)[i], rotulo = lab[i], row.names = NULL)
}

vars_cfg <- unlist(CFG[c("phq_itens", "diag", "med", "psi", "peso",
                         "estrato", "upa", "grupo", "desfecho")])
falta <- setdiff(vars_cfg, names(dados))
if (length(falta)) {
  message("Variáveis ausentes: ", paste(falta, collapse = ", "))
  message("Use achar(dados, 'depress'), achar(dados, 'psicoter') etc. e ajuste CFG.")
  stop("Configuração incompleta.")
}

num <- function(x) {
  if (is.factor(x) || is.character(x)) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(y[!is.na(x)])))
      stop("Variável com rótulos de texto: leia os microdados com códigos numéricos.")
    y
  } else as.numeric(x)
}

# Morador selecionado (quem responde o módulo N)
d <- dados[!is.na(num(dados[[CFG$peso]])), , drop = FALSE]
rm(dados)
cat("Moradores selecionados:", nrow(d), "\n")

# ---------------------------------------------------------------------
# 1. Variáveis
# ---------------------------------------------------------------------
itens <- sapply(CFG$phq_itens, function(v) num(d[[v]]))
itens[!(itens %in% 1:4)] <- NA
for (j in 1:9) d[[paste0("i", j)]] <- as.integer(itens[, j])
d$phq_soma <- rowSums(itens - 1)          # 0..27; NA se algum item faltar

sim_nao <- function(x) ifelse(x == 1, 1L, ifelse(x == 2, 0L, NA_integer_))
d$D <- sim_nao(num(d[[CFG$diag]]))
d$M <- sim_nao(num(d[[CFG$med]]))
d$P <- sim_nao(num(d[[CFG$psi]]))

cat("\nConferência do salto (tratamento perguntado só a quem tem diagnóstico?):\n")
print(table(D = d$D, M = d$M, useNA = "ifany"))
print(table(D = d$D, P = d$P, useNA = "ifany"))
RES$r1 <- list(salto_DM = tente(table(D = d$D, M = d$M, useNA = "ifany")),
               salto_DP = tente(table(D = d$D, P = d$P, useNA = "ifany")))

# Se o salto se confirmar, sem diagnóstico => sem tratamento registrado
d$M[d$D %in% 0] <- 0L
d$P[d$D %in% 0] <- 0L
d$trat <- ifelse(d$M %in% 1 | d$P %in% 1, 1L,
                 ifelse(d$M %in% 0 & d$P %in% 0, 0L, NA_integer_))

# T3: 1 = sem diagnóstico | 2 = diagnóstico sem tratamento | 3 = diagnóstico com tratamento
d$T3 <- with(d, ifelse(is.na(D), NA_integer_,
                ifelse(D == 0, 1L,
                ifelse(is.na(trat), NA_integer_,
                ifelse(trat == 1, 3L, 2L)))))

y <- num(d[[CFG$desfecho]])
d$y_ext <- ifelse(y %in% 4:5, 1L, ifelse(y %in% 1:3, 0L, NA_integer_))

if (!is.null(CFG$grupo)) d$g <- num(d[[CFG$grupo]])

# ---------------------------------------------------------------------
# 2. Modelo de classes latentes
#    Observação: poLCA não usa pesos amostrais. O modelo de MEDIDA
#    (probabilidades condicionais) é ajustado sem pesos; as estimativas
#    populacionais (etapa 4) usam o desenho completo.
# ---------------------------------------------------------------------
vars_lca <- c(paste0("i", 1:9), "T3")
f_lca <- as.formula(paste0("cbind(", paste(vars_lca, collapse = ","), ") ~ 1"))

ajustar_lca <- function(df, K = CFG$K) {
  cc <- complete.cases(df[, vars_lca])
  m  <- poLCA(f_lca, data = df[cc, ], nclass = K, nrep = CFG$nrep,
              maxiter = 5000, verbose = FALSE, calc.se = FALSE)
  esc <- sapply(seq_len(K), function(k)
    sum(sapply(paste0("i", 1:9), function(v) {
      p <- m$probs[[v]][k, ]; sum(p * (seq_along(p) - 1))
    })))
  alvo <- if (is.null(CFG$classes_alvo)) which.max(esc) else CFG$classes_alvo
  list(modelo = m, alvo = alvo, escore_phq_esperado = esc)
}

cat("\nAjustando LCA global (K =", CFG$K, ")...\n")
aj <- ajustar_lca(d)
cat("BIC:", aj$modelo$bic, " | tamanhos de classe:", round(aj$modelo$P, 3), "\n")
cat("Escore PHQ-9 esperado por classe:", round(aj$escore_phq_esperado, 2),
    "| classe-alvo:", aj$alvo, "\n")
cat("P(T3 | classe):\n"); print(round(aj$modelo$probs$T3, 3))

# ---------------------------------------------------------------------
# 3. Razões de verossimilhança -> evidência -> Eτ
# ---------------------------------------------------------------------
# P(x_fonte | classe k) sob independência local (matriz n x K)
lik_fonte <- function(m, vars, df) {
  K <- length(m$P)
  L <- matrix(1, nrow(df), K)
  for (v in vars) {
    pr <- m$probs[[v]]
    x  <- df[[v]]
    for (k in seq_len(K)) L[, k] <- L[, k] * pr[k, x]
  }
  L
}

# LR do conjunto de classes-alvo A contra o restante (misturas ponderadas)
lr_alvo <- function(L, prior, A) {
  B  <- setdiff(seq_along(prior), A)
  pA <- as.vector(L[, A, drop = FALSE] %*% prior[A]) / sum(prior[A])
  pB <- as.vector(L[, B, drop = FALSE] %*% prior[B]) / sum(prior[B])
  pA / pB
}

# LR -> (mu, lambda). LR > 1 só gera evidência favorável; LR < 1 só contrária;
# LR = 1 (fonte não informativa) não gera nenhuma.
# Para teste binário: mu = 1 − (1−Sp)/Se e lambda = 1 − (1−Se)/Sp.
evid <- function(lr) cbind(mu = pmax(0, 1 - 1 / lr), lambda = pmax(0, 1 - lr))

psum <- function(a, b) 1 - (1 - a) * (1 - b)

calcular_etau <- function(df, ajuste, s = 1, agreg = CFG$agregacao) {
  m  <- ajuste$modelo
  l1 <- lr_alvo(lik_fonte(m, paste0("i", 1:9), df), m$P, ajuste$alvo)^s
  l2 <- lr_alvo(lik_fonte(m, "T3", df),             m$P, ajuste$alvo)^s
  e1 <- evid(l1); e2 <- evid(l2)
  comb <- if (agreg == "max") pmax else psum
  mu <- comb(e1[, "mu"], e2[, "mu"])
  la <- comb(e1[, "lambda"], e2[, "lambda"])
  data.frame(lr_phq = l1, lr_t3 = l2, mu = mu, lambda = la,
             Gc = mu - la, Gct = mu + la - 1,
             hesit = pmax(0, 1 - mu - la), confl = pmax(0, mu + la - 1))
}

regiao <- function(Gc, Gct, v = CFG$limiar) {
  r <- ifelse(Gc  >=  v, "doente",
       ifelse(Gc  <= -v, "saudavel",
       ifelse(Gct >=  v, "inconsistente",
       ifelse(Gct <= -v, "paracompleto", "zona_central"))))
  factor(r, levels = c("doente", "saudavel", "inconsistente", "paracompleto", "zona_central"))
}

etau <- calcular_etau(d, aj)

# Opcional: LCA por grupo de acesso (necessário para o teste T3)
por_grupo <- NULL
if (!is.null(CFG$grupo)) {
  cat("\nAjustando LCA por grupo de", CFG$grupo, "...\n")
  idx <- split(seq_len(nrow(d)), d$g)
  por_grupo <- lapply(idx, function(ix) ajustar_lca(d[ix, , drop = FALSE]))
  if (isTRUE(CFG$lca_por_grupo)) {
    for (g in names(idx)) etau[idx[[g]], ] <- calcular_etau(d[idx[[g]], , drop = FALSE], por_grupo[[g]])
  }
}

d <- cbind(d, etau)
d$regiao <- regiao(d$Gc, d$Gct)

# ---------------------------------------------------------------------
# 4. Estimativas com desenho amostral
#    (o IBGE ainda pós-estratifica; para replicar números oficiais,
#     calibre com as projeções populacionais)
# ---------------------------------------------------------------------
des <- svydesign(ids     = as.formula(paste0("~", CFG$upa)),
                 strata  = as.formula(paste0("~", CFG$estrato)),
                 weights = as.formula(paste0("~", CFG$peso)),
                 data = d, nest = TRUE)
des_ok <- subset(des, !is.na(mu))

cat("\n=== Médias populacionais ===\n")
print(svymean(~mu + lambda + Gc + Gct + hesit + confl, des_ok))
cat("\n=== Proporção em cada região do reticulado (limiar", CFG$limiar, ") ===\n")
print(svymean(~regiao, des_ok))
cat("\nReferência ingênua — PHQ-9 >= 10:\n")
print(svymean(~I(phq_soma >= 10), des_ok))

# ---------------------------------------------------------------------
# 5. Testes de falseamento
# ---------------------------------------------------------------------
# T1 — Hipótese: o conflito concentra-se em quem está em tratamento
#      (doença controlada). Refutada se a média de 'confl' em T3 = 3
#      não for maior que em T3 = 1 e T3 = 2.
cat("\n=== T1: conflito por situação de diagnóstico/tratamento ===\n")
print(svyby(~confl, ~factor(T3), des_ok, svymean))
cat("Tratados: conflito com PHQ < 10 vs >= 10\n")
print(svyby(~confl, ~I(phq_soma < 10), subset(des_ok, T3 == 3), svymean))

# T2 — Validade externa com desfecho que não entrou no modelo.
#      Critério: o par (mu, lambda) deve ajustar ao menos tão bem quanto o
#      PHQ-9 bruto; se perder para "phq_soma + T3" (regressão comum com
#      as mesmas fontes), a transformação Eτ está descartando informação.
cat("\n=== T2: autoavaliação de saúde ruim/muito ruim ===\n")
des_y <- subset(des_ok, !is.na(y_ext))
mods <- list(
  phq_soma      = svyglm(y_ext ~ phq_soma,               des_y, family = quasibinomial()),
  phq_corte10   = svyglm(y_ext ~ I(phq_soma >= 10),      des_y, family = quasibinomial()),
  Gc            = svyglm(y_ext ~ Gc,                     des_y, family = quasibinomial()),
  mu_lambda     = svyglm(y_ext ~ mu + lambda,            des_y, family = quasibinomial()),
  phq_mais_T3   = svyglm(y_ext ~ phq_soma + factor(T3),  des_y, family = quasibinomial())
)
print(t(sapply(mods, function(m) AIC(m)[c("eff.p", "AIC")])))

# T3 — Hipótese de acesso: a probabilidade de diagnóstico dado a
#      classe-alvo varia entre grupos. Refutada se P(T3 >= 2 | alvo)
#      for praticamente igual entre os grupos.
if (!is.null(por_grupo)) {
  cat("\n=== T3: P(T3 | classe-alvo) por grupo de acesso ===\n")
  tab <- t(sapply(por_grupo, function(a) {
    pr <- a$modelo$probs$T3; P <- a$modelo$P; A <- a$alvo
    colSums(pr[A, , drop = FALSE] * P[A]) / sum(P[A])
  }))
  colnames(tab) <- c("sem_diag", "diag_sem_trat", "diag_com_trat")
  print(round(tab, 3))
}

# ---------------------------------------------------------------------
# 6. Sensibilidade: escala s aplicada aos log-LR (LR^s)
#    s < 1 enfraquece todas as fontes; s > 1 fortalece.
#    A amplitude das estimativas é uma primeira medida de quanto a
#    conclusão depende da calibração (ponte natural para o tipo 2).
# ---------------------------------------------------------------------
cat("\n=== Sensibilidade das proporções por região ===\n")
sens <- sapply(CFG$escalas_sens, function(s) {
  e <- calcular_etau(d, aj, s = s)
  des_s <- des
  des_s$variables$reg_s <- regiao(e$Gc, e$Gct)
  coef(svymean(~reg_s, subset(des_s, !is.na(reg_s))))
})
colnames(sens) <- paste0("s=", CFG$escalas_sens)
rownames(sens) <- levels(d$regiao)
print(round(sens, 4))

saveRDS(list(cfg = CFG, lca = aj, lca_grupo = por_grupo,
             etau = d[, c("mu", "lambda", "Gc", "Gct", "hesit", "confl", "regiao")]),
        "etau_depressao_resultados.rds")
cat("\nResultados salvos em etau_depressao_resultados.rds\n")

# ---------------------------------------------------------------------
# Registro para o relatório
# ---------------------------------------------------------------------
RES$r1$n_sel   <- nrow(d)
RES$r1$K2      <- tente(c(tamanho = unname(aj$modelo$P[aj$alvo]),
                          phq = unname(aj$escore_phq_esperado[aj$alvo])))
RES$r1$regioes <- tente(coef(svymean(~regiao, des_ok)))
RES$r1$phq10   <- tente(unname(coef(svymean(~I(phq_soma >= 10), des_ok))[2]))
RES$r1$T1      <- tente(sb_df(svyby(~confl, ~factor(T3), des_ok, svymean)))
RES$r1$T2_aic  <- tente(t(sapply(mods, function(m) AIC(m)[c("eff.p", "AIC")])))
RES$r1$sens    <- tente(sens)
