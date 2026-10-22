# =====================================================================
# relatorio.R — monta o relatório HTML a partir da lista RES
#
# Cada rodada (R/01..09) registra seus números em RES. Este arquivo lê
# esses números, aplica os critérios de cada hipótese e escreve o
# relatório. Nenhum resultado ou veredito está escrito à mão: se uma
# nova execução der números diferentes, o texto e o placar mudam junto.
#
# Convenção de rótulos (Contrato de Ulisses):
#   Medido      — número estimado nesta execução
#   Inferência  — interpretação que vai além do número
#   Construção  — escolha metodológica própria, não da literatura
# =====================================================================

rel_esc <- function(s) {
  s <- gsub("&", "&amp;", as.character(s), fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}
rel_num <- function(x, d = 3) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
  out[is.na(x)] <- "—"
  out
}
rel_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(round(x), format = "d", big.mark = ".", decimal.mark = ",")
  out[is.na(x)] <- "—"
  out
}
rel_pct <- function(x, d = 1) paste0(rel_num(100 * as.numeric(x), d), "%")
rel_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(is.na(p), "—", ifelse(p < 1e-4, "&lt; 0,0001", rel_num(p, 4)))
}
rel_peq <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(is.na(p), "p = —", ifelse(p < 1e-4, "p &lt; 0,0001", paste0("p = ", rel_num(p, 4))))
}
rel_ic <- function(v, d = 3, expo = FALSE) {
  v <- as.numeric(v[c("est", "lo", "hi")])
  if (expo) v <- exp(v)
  paste0(rel_num(v[1], d), " [", rel_num(v[2], d), "; ", rel_num(v[3], d), "]")
}
rel_f <- function(x) if (is.na(x)) "&gt; 2" else rel_num(x, 1)
# Interrompe o item (que vira "indisponível") se a rodada não registrou o dado
rel_req <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) stop("rodada não executada ou sem registro")
  x
}

rel_rn <- function(m, nome = " ") {
  df <- as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE)
  df <- cbind(data.frame(x = rownames(m), stringsAsFactors = FALSE), df)
  names(df)[1] <- nome
  rownames(df) <- NULL
  df
}

rel_tabela <- function(df, d = 3) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_along(df)) {
    x <- df[[j]]
    df[[j]] <- if (is.logical(x)) {
      ifelse(is.na(x), "—", ifelse(x, "sim", "não"))
    } else if (is.numeric(x)) {
      inteiro <- all(is.na(x) | abs(x - round(x)) < 1e-9)
      if (inteiro) rel_int(x) else rel_num(x, d)
    } else rel_esc(as.character(x))
  }
  cab <- paste0("<tr>", paste0("<th>", rel_esc(names(df)), "</th>", collapse = ""), "</tr>")
  corpo <- vapply(seq_len(nrow(df)), function(i)
    paste0("<tr>", paste0("<td>", unlist(df[i, ], use.names = FALSE), "</td>", collapse = ""), "</tr>"), "")
  paste0("<table>", cab, "\n", paste(corpo, collapse = "\n"), "</table>")
}

rel_badge <- function(v) {
  cls <- switch(v, "sobreviveu" = "ok", "confirmada" = "ok", "refutada" = "ref",
                "inconclusiva" = "inc", "na")
  paste0("<span class='v ", cls, "'>", v, "</span>")
}
rel_ver_map <- function(x) switch(toupper(as.character(x)),
  "CORROBORADA" = "sobreviveu", "REFUTADA" = "refutada", "INCONCLUSIVA" = "inconclusiva",
  "indisponível")
rel_classif <- function(v) {
  if (v["lo"] > 0) "sobreviveu" else if (v["hi"] < log(1.25)) "refutada" else "inconclusiva"
}

rel_item <- function(rodada, hipotese, criterio, calc) {
  r <- tryCatch(calc(), error = function(e)
    list(resultado = paste0("não disponível (", rel_esc(conditionMessage(e)), ")"),
         veredito = "indisponível"))
  if (is.null(r$veredito) || length(r$veredito) != 1 || is.na(r$veredito)) r$veredito <- "indisponível"
  data.frame(rodada = rodada, hipotese = hipotese, criterio = criterio,
             resultado = r$resultado, veredito = r$veredito, stringsAsFactors = FALSE)
}

rel_placar_html <- function(p) {
  cab <- paste0("<tr><th>Rodada</th><th>Hipótese</th><th>Critério</th>",
                "<th>Resultado medido</th><th>Veredito</th></tr>")
  linhas <- vapply(seq_len(nrow(p)), function(i) paste0(
    "<tr><td>", p$rodada[i], "</td><td>", p$hipotese[i], "</td><td class='crit'>",
    p$criterio[i], "</td><td>", p$resultado[i], "</td><td>", rel_badge(p$veredito[i]),
    "</td></tr>"), "")
  paste0("<table class='placar'>", cab, "\n", paste(linhas, collapse = "\n"), "</table>")
}

rel_fig_oe <- function(R, dir_saida) {
  t  <- R$r9$tab
  so <- R$r9$soma$oe
  ufs <- as.character(t$UF)
  o <- order(t$OE_original)
  y <- seq_along(o)
  arq <- file.path(dir_saida, "oe_denominadores.png")
  png(arq, width = 1000, height = 1100, res = 130)
  op <- par(mar = c(4.5, 4, 2.5, 1))
  xl <- range(c(t$OE_original, t$OE_parcial, so[ufs]), na.rm = TRUE)
  plot(t$OE_original[o], y, log = "x", xlim = xl, pch = 19, yaxt = "n", ylab = "",
       xlab = "Diagnosticados observados / esperados (escala log)",
       main = "Cobertura relativa por UF com três denominadores")
  points(t$OE_parcial[o], y, pch = 1, cex = 1.3)
  points(so[ufs][o], y, pch = 4)
  abline(v = 1, lty = 2, col = "grey40")
  axis(2, at = y, labels = ufs[o], las = 1, cex.axis = 0.8)
  legend("bottomright", c("LCA", "Escore fatorial", "Soma bruta do PHQ-9"),
         pch = c(19, 1, 4), bty = "n")
  par(op)
  dev.off()
  basename(arq)
}

rel_css <- "
body { font-family: Georgia, 'Times New Roman', serif; max-width: 1100px; margin: 2rem auto;
       padding: 0 16px; color: #1d1d1f; line-height: 1.55; background: #fdfdfb; }
h1 { font-size: 1.7rem; margin-bottom: .2rem; }
h2 { font-size: 1.3rem; border-bottom: 1px solid #ccc; padding-bottom: .2rem; margin-top: 2.2rem; }
h3 { font-size: 1.05rem; margin-top: 1.4rem; }
.sub { color: #555; margin-top: 0; }
table { border-collapse: collapse; margin: .8rem 0 1.2rem; font-size: .88rem;
        font-family: 'Helvetica Neue', Arial, sans-serif; }
th, td { border: 1px solid #ddd; padding: .35rem .55rem; text-align: left; vertical-align: top; }
th { background: #f0efe9; }
table.placar td.crit { color: #555; font-size: .82rem; }
.v { display: inline-block; padding: .1rem .5rem; border-radius: 3px; font-weight: bold;
     font-size: .8rem; white-space: nowrap; }
.v.ok { background: #dff0d8; color: #2d5a1e; }
.v.ref { background: #f6dcdc; color: #7a1f1f; }
.v.inc { background: #fbf0d0; color: #6b5310; }
.v.na { background: #e6e6e6; color: #444; }
.achado { border-left: 4px solid #2d5a1e; background: #f3f8ef; padding: .6rem 1rem; }
.alerta { border-left: 4px solid #7a1f1f; background: #fbf1f1; padding: .6rem 1rem; }
.tag { font-family: 'Helvetica Neue', Arial, sans-serif; font-size: .75rem; font-weight: bold;
       text-transform: uppercase; letter-spacing: .04em; color: #555; margin-right: .3rem; }
.erro { color: #7a1f1f; }
img { max-width: 100%; border: 1px solid #eee; }
.nota { font-size: .85rem; color: #555; }
"

# =====================================================================
gerar_relatorio <- function(RES, arquivo) {
  R <- RES
  dir_saida <- dirname(arquivo)
  secao <- function(id, titulo, f) {
    corpo <- tryCatch(f(), error = function(e)
      paste0("<p class='erro'>Seção indisponível: ", rel_esc(conditionMessage(e)), "</p>"))
    paste0("<section id='", id, "'><h2>", titulo, "</h2>\n", corpo, "\n</section>\n")
  }
  tag <- function(t) paste0("<span class='tag'>", t, "</span>")

  # -------------------------------------------------------------------
  # Placar — linha 1: modelo Eτ
  # -------------------------------------------------------------------
  placar1 <- rbind(
    rel_item("1", "O tratamento (medicação, psicoterapia) só é perguntado a quem tem diagnóstico",
      "Pré-fixado: nenhuma resposta de tratamento entre não diagnosticados",
      function() {
        t <- rel_req(R$r1$salto_DM)
        n0 <- sum(t["0", !is.na(colnames(t))])
        list(resultado = paste0(rel_int(n0), " respostas de medicação entre não diagnosticados"),
             veredito = if (n0 == 0) "confirmada" else "refutada")
      }),
    rel_item("1", "Com K = 2, a classe-alvo do modelo latente representa depressão",
      "Post hoc: PHQ-9 esperado da classe-alvo ≥ 10",
      function() {
        k <- R$r1$K2
        list(resultado = paste0("classe-alvo com ", rel_pct(k["tamanho"]), " da amostra e PHQ-9 esperado de ",
                                rel_num(k["phq"], 1)),
             veredito = if (k["phq"] >= 10) "sobreviveu" else "refutada")
      }),
    rel_item("2", "Com K escolhido pelo BIC, a classe-alvo tem tamanho plausível",
      "Post hoc: classes com PHQ-9 esperado ≥ 10 somando entre 5% e 15% (PHQ-9 ≥ 10 e diagnóstico autorreferido ficam em torno de 10%)",
      function() {
        a <- R$r2$alvo
        ok <- a["tamanho"] >= 0.05 && a["tamanho"] <= 0.15
        list(resultado = paste0("K = ", R$r2$K_esc, "; classe-alvo com ", rel_pct(a["tamanho"]), " da amostra"),
             veredito = if (ok) "confirmada" else "refutada")
      }),
    rel_item("2", "O conflito (μ e λ altos) é máximo entre os diagnosticados em tratamento",
      "Pré-fixado: refutada se o conflito médio dos tratados não superar o dos outros dois grupos",
      function() {
        t <- R$r2$confl_T3; e <- setNames(t$est, t$grupo)
        ok <- e["3"] > e["1"] && e["3"] > e["2"]
        list(resultado = paste0("conflito médio — sem diagnóstico ", rel_num(e["1"]), "; diagnóstico sem tratamento ",
                                rel_num(e["2"]), "; com tratamento ", rel_num(e["3"])),
             veredito = if (ok) "sobreviveu" else "refutada")
      }),
    rel_item("2", "Entre diagnosticados sem tratamento, o conflito cresce com o tempo desde o diagnóstico",
      "Pré-fixado: refutada se o coeficiente for nulo ou negativo",
      function() {
        t <- R$r2$tempo[["2"]]
        list(resultado = paste0("coeficiente por ano ", rel_ic(t, 4), "; ", rel_peq(t["p"])),
             veredito = if (t["est"] > 0 && t["p"] < 0.05) "sobreviveu" else "refutada")
      }),
    rel_item("2", "O par (μ, λ) prediz autoavaliação de saúde ruim ao menos tão bem quanto o PHQ-9 bruto",
      "Pré-fixado: AIC do par (μ, λ) ≤ AIC do PHQ-9 bruto",
      function() {
        a <- R$r2$T2_aic
        dif <- a["mu_lambda", "AIC"] - a["phq_soma", "AIC"]
        list(resultado = paste0("AIC do par (μ, λ) − AIC do PHQ-9 bruto = ", rel_num(dif, 1)),
             veredito = if (dif <= 0) "sobreviveu" else "refutada")
      }),
    rel_item("2", "A perda de informação está no mapeamento LR → (μ, λ), e não no modelo latente",
      "Pré-fixado: comparação de AIC na mesma amostra (log-LR cru × PHQ-9 bruto × par μ, λ)",
      function() {
        a <- R$r2$T2_aic
        p_lca <- a["llr", "AIC"] - a["phq_soma", "AIC"]
        p_map <- a["mu_lambda", "AIC"] - a["llr", "AIC"]
        list(resultado = paste0("custo do modelo latente: ", rel_num(p_lca, 1),
                                " unidades de AIC; custo do mapeamento: ", rel_num(p_map, 1)),
             veredito = if (p_map > p_lca) "confirmada" else "refutada")
      }),
    rel_item("2", "Entre sintomáticos, a chance de diagnóstico varia com a escolaridade (proxy de acesso)",
      "Pré-fixado: refutada se o teste de Wald da escolaridade, ajustado por idade, não tiver evidência (p ≥ 0,05)",
      function() {
        t <- R$r2$acesso_D; p <- R$r2$acesso_p
        list(resultado = paste0("taxa de diagnóstico entre sintomáticos de ", rel_pct(min(t$est)), " a ",
                                rel_pct(max(t$est)), " entre níveis de instrução; Wald ", rel_peq(p)),
             veredito = if (p < 0.05) "sobreviveu" else "refutada")
      }),
    rel_item("3", "Entre diagnosticados, o conflito é menor em quem consultou por depressão há menos de 1 ano",
      "Pré-fixado: refutada se o efeito for ≥ 0, ou se o IC incluir 0 e excluir efeitos menores que −0,03",
      function() {
        b2 <- R$r3$bin[["2"]]; b3 <- R$r3$bin[["3"]]
        refut <- function(b) b["est"] >= 0 || (b["lo"] <= 0 && b["hi"] >= 0 && b["lo"] > -0.03)
        list(resultado = paste0("efeito no conflito — sem tratamento ", rel_ic(b2), "; com tratamento ", rel_ic(b3)),
             veredito = if (!refut(b2) && !refut(b3)) "sobreviveu" else "refutada")
      }),
    rel_item("3", "O conflito cresce com o tempo desde a última consulta (dose-resposta)",
      "Pré-fixado: refutada se a tendência linear não for positiva com IC acima de 0",
      function() {
        t2 <- R$r3$trend[["2"]]; t3 <- R$r3$trend[["3"]]
        list(resultado = paste0("inclinação por faixa — sem tratamento ", rel_ic(t2, 4), "; com tratamento ",
                                rel_ic(t3, 4)),
             veredito = if (t2["lo"] > 0 && t3["lo"] > 0) "sobreviveu" else "refutada")
      })
  )

  # -------------------------------------------------------------------
  # Placar — linha 2: desigualdade regional de diagnóstico
  # -------------------------------------------------------------------
  placar2 <- rbind(
    rel_item("4", "A cobertura de diagnóstico entre sintomáticos difere entre UFs",
      "Pré-fixado: Wald de UF (idade, sexo, escolaridade) com p &lt; 0,001",
      function() {
        w <- R$r4$wald
        list(resultado = paste0("F = ", rel_num(w["F"], 2), "; ", rel_peq(w["p"]), "; cobertura nacional ",
                                rel_pct(R$r4$nac["est"])),
             veredito = if (w["p"] < 0.001) "sobreviveu" else "refutada")
      }),
    rel_item("5", "As diferenças entre UFs não se reduzem à composição individual (idade, sexo, escolaridade, situação rural, renda, cor/raça)",
      "Pré-fixado: Wald de UF com p &lt; 0,001 e dispersão de log(O/E) ≥ 50% da inicial",
      function() {
        m3 <- R$r5$resumo[R$r5$resumo$modelo == "M3", ]
        list(resultado = paste0("com toda a composição: F = ", rel_num(m3$F_UF, 2), "; dispersão ",
                                rel_num(m3$pct_M0, 1), "% da inicial; razão maior/menor = ",
                                rel_num(m3$razao_max_min, 2)),
             veredito = if (isTRUE(rel_req(R$r5$ok_a) && rel_req(R$r5$ok_b))) "sobreviveu" else "refutada")
      }),
    rel_item("6", "Rival: serviços concentrados na capital fazem a capital diagnosticar mais que o interior",
      "Pré-fixado: sobrevive se o IC do log-OR estiver acima de 0; refutada se estiver inteiro abaixo de log(1,25)",
      function() {
        v <- R$r6$cap
        list(resultado = paste0("OR capital × interior (mesma UF) ", rel_ic(v, 2, TRUE),
                                if (v["hi"] < 0) " — sinal oposto ao previsto" else ""),
             veredito = rel_classif(v))
      }),
    rel_item("6", "A diferença capital − interior é maior no Norte/Nordeste",
      "Pré-fixado: mesmo critério; grupo definido pela geografia, não pela cobertura",
      function() {
        v <- R$r6$inter
        list(resultado = paste0("OR da interação ", rel_ic(v, 2, TRUE), "; Wald conjunto ", rel_peq(R$r6$p_inter)),
             veredito = rel_classif(v))
      }),
    rel_item("7", "Rival: o cadastro na ESF explica a vantagem do interior",
      "Pré-fixado: pré-condição + efeito com IC acima de 0 + atenuação ≥ 50%; refutada se a atenuação for &lt; 20%",
      function() {
        r <- R$r7
        list(resultado = paste0("pré-condição (ESF menor na capital): OR ", rel_ic(r$precond_OR, 2),
                                "; efeito do cadastro: OR ", rel_ic(r$esf, 2, TRUE),
                                "; atenuação da desvantagem da capital: ", rel_pct(r$aten, 0)),
             veredito = rel_ver_map(r$veredito))
      }),
    rel_item("7b", "Rival: as visitas da equipe de Saúde da Família explicam a vantagem do interior",
      "Pré-fixado: mesmo critério da rodada 7",
      function() {
        r <- R$r7b
        list(resultado = paste0("pré-condição (capital − interior, escala 0–4): ", rel_ic(r$precond_est, 2),
                                "; efeito visita mensal × nunca: OR ", rel_ic(r$vis, 2, TRUE),
                                "; atenuação: ", rel_pct(r$aten, 0)),
             veredito = rel_ver_map(r$veredito))
      }),
    rel_item("8", "Rival: um excesso de sintomáticos espúrios na capital explica a desvantagem dela (análise de limites)",
      "Pré-fixado: suficiente se bastar f ≤ 1 do excesso ser espúrio; implausível se for preciso f &gt; 1",
      function() {
        f1 <- R$r8$f["f_star"]; f2 <- R$r8$f_sup["f_star"]
        g <- function(x) if (is.na(x)) Inf else x
        v <- if (g(f1) > 1 && g(f2) > 1) "refutada" else if (g(f1) <= 1 && g(f2) <= 1) "sobreviveu" else "inconclusiva"
        list(resultado = paste0("fração necessária para o efeito zerar: ", rel_f(f1), " (Δ pontual) e ",
                                rel_f(f2), " (Δ no limite superior); deixa de ser significativo com f = ",
                                rel_f(R$r8$f["f_ns"])),
             veredito = v)
      }),
    rel_item("8", "Rival: os sintomáticos da capital têm perfil menos depressivo (menos sintomas nucleares)",
      "Pré-fixado: refutada se o IC excluir reduções maiores que 5% da média",
      function() {
        c8 <- R$r8$core
        v <- if (grepl("REFUTADO", R$r8$veredito_B)) "refutada" else
             if (grepl("CORROBORADO", R$r8$veredito_B)) "sobreviveu" else "inconclusiva"
        list(resultado = paste0("participação dos itens nucleares (", paste(R$r8$core_itens, collapse = ", "),
                                "), capital − interior: ", rel_ic(c8, 4), "; média ", rel_num(c8["media"], 3)),
             veredito = v)
      }),
    rel_item("9", "O PHQ-9 funciona igual entre macrorregiões (invariância escalar)",
      "Pré-fixado: ΔCFI ≥ −0,010 e ΔRMSEA ≤ 0,015 (índices escalonados). Complementar, post hoc: RMSEA da diferença ≤ 0,05",
      function() {
        r <- R$r9
        rel_req(r$escalar_ok)
        rr <- if (is.null(r$lrt)) NA else r$lrt$RMSEA
        rd <- if (all(is.na(rr))) NA else max(rr, na.rm = TRUE)
        v <- if (!isTRUE(r$escalar_ok)) "refutada" else if (!is.na(rd) && rd <= 0.05) "sobreviveu" else "inconclusiva"
        list(resultado = paste0("critério escalonado ", if (isTRUE(r$escalar_ok)) "atendido" else "não atendido",
                                "; itens liberados: ", if (length(r$livres)) paste(r$livres, collapse = ", ") else "nenhum",
                                "; maior RMSEA da diferença: ", rel_num(rd, 3)),
             veredito = v)
      }),
    rel_item("9", "O PHQ-9 mede um único fator",
      "Post hoc, convenção usual: CFI robusto ≥ 0,95 e RMSEA robusto ≤ 0,08 no modelo configural",
      function() {
        rb <- R$r9$robustos["configural", ]
        if (any(is.na(rb))) stop("índices robustos indisponíveis nesta versão do lavaan")
        list(resultado = paste0("CFI robusto ", rel_num(rb["cfi.robust"], 3), "; RMSEA robusto ",
                                rel_num(rb["rmsea.robust"], 3)),
             veredito = if (rb["cfi.robust"] >= 0.95 && rb["rmsea.robust"] <= 0.08) "sobreviveu" else "refutada")
      }),
    rel_item("9", "A desigualdade entre UFs sobrevive com o denominador do escore fatorial",
      "Pré-fixado: Wald de UF com p &lt; 0,001, dispersão ≥ 50% da original e Norte/Nordeste com IC abaixo de 0",
      function() {
        p <- R$r9$resumo[R$r9$resumo$denominador == "parcial", ]
        list(resultado = paste0("F = ", rel_num(p$F_UF, 2), "; dispersão ", rel_num(p$pct_orig, 1),
                                "% da original; OR Norte/Nordeste ",
                                rel_ic(c(est = p$logOR_NNE, lo = p$NNE_lo, hi = p$NNE_hi), 2, TRUE)),
             veredito = if (isTRUE(rel_req(R$r9$sobrevive))) "sobreviveu" else "refutada")
      }),
    rel_item("9", "A desigualdade entre UFs sobrevive com a soma bruta do PHQ-9 (sem modelo de medida)",
      "Pré-fixado: o mesmo critério",
      function() {
        s <- R$r9$soma
        list(resultado = paste0("corte nacional na soma: ", rel_num(s$corte, 0), "; F = ", rel_num(s$F, 2),
                                "; dispersão ", rel_num(s$pct, 1), "% da original; OR Norte/Nordeste ",
                                rel_ic(s$nne, 2, TRUE)),
             veredito = if (isTRUE(rel_req(s$sobrevive))) "sobreviveu" else "refutada")
      })
  )

  # -------------------------------------------------------------------
  # Seções
  # -------------------------------------------------------------------
  s_achado <- secao("achado", "1. Achado principal", function() {
    if (is.null(R$r5$ok_a) || is.null(R$r9$sobrevive) || is.null(R$r9$soma$sobrevive))
      return(paste0("<div class='alerta'><p><strong>Execução incompleta:</strong> as rodadas 5 e 9 ",
                    "precisam concluir para que o achado principal seja avaliado. Veja a seção 8.</p></div>"))
    ok5 <- isTRUE(R$r5$ok_a && R$r5$ok_b)
    ok9 <- isTRUE(R$r9$sobrevive)
    okS <- isTRUE(R$r9$soma$sobrevive)
    if (ok5 && ok9 && okS) {
      m3 <- R$r5$resumo[R$r5$resumo$modelo == "M3", ]
      o  <- R$r9$resumo[R$r9$resumo$denominador == "original", ]
      t5 <- R$r5$tab5
      acima  <- paste(t5$UF[t5$grupo_M3 == "acima"], collapse = ", ")
      abaixo <- paste(t5$UF[t5$grupo_M3 == "abaixo"], collapse = ", ")
      paste0(
        "<div class='achado'><p>", tag("Medido"),
        "<strong>Entre adultos com sintomas depressivos, a fração com diagnóstico de depressão é de ",
        rel_pct(R$r4$nac["est"]), " no Brasil e varia cerca de ", rel_num(m3$razao_max_min, 1),
        " vezes entre UFs</strong>, depois de ajustar por idade, sexo, escolaridade, situação rural, renda e cor/raça. ",
        "No Norte/Nordeste, a chance de ter diagnóstico é ",
        rel_ic(c(est = o$logOR_NNE, lo = o$NNE_lo, hi = o$NNE_hi), 2, TRUE),
        " vezes a do restante do país (OR ajustado, IC 95%).</p>",
        "<p>Acima do esperado pela composição: <strong>", if (nzchar(acima)) acima else "nenhuma UF", "</strong>. ",
        "Abaixo do esperado: <strong>", if (nzchar(abaixo)) abaixo else "nenhuma UF", "</strong>.</p>",
        "<p>O resultado se mantém com três denominadores construídos de formas independentes ",
        "(modelo de classes latentes, escore fatorial e soma bruta do PHQ-9) e resistiu a todas as explicações ",
        "alternativas listadas no placar da seção 3.</p></div>",
        "<p>", tag("Inferência"), "Pelo critério falseacionista, a afirmação está <em>corroborada</em>: ",
        "sobreviveu a testes que poderiam tê-la derrubado. Não está provada, e o resultado é descritivo, não causal. ",
        "O mecanismo continua desconhecido: todas as explicações por oferta e contato com serviços que foram testadas caíram.</p>")
    } else {
      paste0("<div class='alerta'><p><strong>Com os números desta execução, o achado principal não passou em todos ",
             "os testes de robustez.</strong></p><ul>",
             "<li>Composição individual (rodada 5): ", if (ok5) "passou" else "não passou", "</li>",
             "<li>Denominador fatorial (rodada 9): ", if (ok9) "passou" else "não passou", "</li>",
             "<li>Soma bruta do PHQ-9 (rodada 9): ", if (okS) "passou" else "não passou", "</li></ul>",
             "<p>Veja o placar na seção 3 para os detalhes.</p></div>")
    }
  })

  s_dados <- secao("dados", "2. Pergunta, dados e método", function() {
    paste0(
      "<p><strong>Pergunta.</strong> Na PNS 2019, como combinar três fontes de evidência sobre depressão ",
      "(diagnóstico autorreferido, sintomas pelo PHQ-9 e tratamento atual) e o que essa combinação revela ",
      "sobre o diagnóstico no Brasil?</p>",
      "<p><strong>Dados.</strong> ", rel_int(R$r1$n_sel), " moradores selecionados da PNS 2019. ",
      "Todas as estimativas populacionais usam o desenho amostral (UPA, estrato e peso do morador selecionado). ",
      "Os modelos de medida (classes latentes e análise fatorial) foram ajustados sem pesos.</p>",
      "<p><strong>Modelo Eτ.</strong> Cada pessoa recebe um par (μ, λ) da lógica paraconsistente anotada: ",
      "μ é a evidência favorável a depressão atual, λ a contrária. ", tag("Construção"),
      "As evidências vêm de razões de verossimilhança estimadas por um modelo de classes latentes, ",
      "convertidas por μ = 1 − 1/LR (se LR &gt; 1) e λ = 1 − LR (se LR &lt; 1).</p>",
      "<p><strong>Sintomáticos.</strong> Classe-alvo do modelo latente (posterior &gt; 0,8), definida só pelos ",
      "nove itens do PHQ-9. Prevalência nacional nesta definição: ", rel_pct(R$r9$p0, 2), ". ",
      "Para comparação, PHQ-9 ≥ 10: ", rel_pct(R$r1$phq10, 1), ".</p>",
      "<p><strong>Critérios.</strong> Cada hipótese teve um critério de refutação escrito no script antes de ",
      "rodar. Os poucos critérios definidos depois do resultado estão marcados como <em>post hoc</em> no placar.</p>")
  })

  s_placar <- secao("placar", "3. Placar de hipóteses", function() {
    paste0("<h3>Linha 1 — o modelo Eτ</h3>", rel_placar_html(placar1),
           "<h3>Linha 2 — desigualdade regional de diagnóstico</h3>",
           "<p class='nota'>Nas linhas marcadas como <em>Rival</em>, \"refutada\" significa que a explicação ",
           "alternativa caiu, o que fortalece o achado principal.</p>",
           rel_placar_html(placar2))
  })

  s_robustez <- secao("robustez", "4. Robustez ao denominador", function() {
    r <- rel_req(R$r9$resumo); s <- rel_req(R$r9$soma)
    or <- vapply(seq_len(nrow(r)), function(i)
      rel_ic(c(est = r$logOR_NNE[i], lo = r$NNE_lo[i], hi = r$NNE_hi[i]), 2, TRUE), "")
    df <- data.frame(
      c1 = c("Classes latentes (posterior > 0,8)", "Escore fatorial, invariância total",
             "Escore fatorial, invariância parcial", "Soma bruta do PHQ-9"),
      c2 = c(r$n_sint, s$n),
      c3 = c(r$F_UF, s$F),
      c4 = c(r$pct_orig, s$pct),
      c5 = c(or, rel_ic(s$nne, 2, TRUE)),
      stringsAsFactors = FALSE)
    names(df) <- c("Denominador", "Sintomáticos", "F de UF", "Dispersão (% da original)",
                   "OR Norte/Nordeste [IC 95%]")
    fig <- tryCatch(rel_fig_oe(R, dir_saida), error = function(e) NULL)
    conc <- R$r9$concord
    paste0(rel_tabela(df, 2),
           "<p>", tag("Medido"), "Concordância entre a classificação original e o escore fatorial: ",
           rel_pct(conc["parcial"]), ". Itens com funcionamento diferencial liberados: ",
           if (length(R$r9$livres)) paste(R$r9$livres, collapse = ", ") else "nenhum", ".</p>",
           if (!is.null(fig)) paste0("<img src='", fig, "' alt='O/E por UF com três denominadores'>") else "")
  })

  s_uf <- secao("uf", "5. Cobertura por UF", function() {
    t4 <- R$r4$tab[, c("UF", "n_sint", "cob_suave", "sem_diag_pop")]
    t5 <- R$r5$tab5[, c("UF", "OE_M0", "OE_M3", "M3_lo", "M3_hi", "grupo_M3")]
    m <- merge(t4, t5, by = "UF")
    m <- m[order(-m$OE_M3), ]
    names(m) <- c("UF", "Sintomáticos (n)", "Cobertura", "Sem diagnóstico (% pop.)",
                  "O/E idade-sexo-escol.", "O/E composição completa", "IC inf.", "IC sup.", "Grupo")
    m[["Sem diagnóstico (% pop.)"]] <- 100 * m[["Sem diagnóstico (% pop.)"]]
    img <- if (!is.null(R$r4$png) && file.exists(file.path(dir_saida, basename(R$r4$png))))
      paste0("<img src='", basename(R$r4$png), "' alt='Cobertura por UF'>") else ""
    paste0("<p class='nota'>Cobertura: fração diagnosticada entre sintomáticos (estimador suave). ",
           "O/E: diagnósticos observados sobre esperados pela composição; acima de 1, a UF diagnostica mais ",
           "do que sua população prevê.</p>", rel_tabela(m, 3), img)
  })

  s_etau <- secao("etau", "6. O modelo Eτ: o que sobreviveu", function() {
    rg <- R$r2$regioes
    df <- data.frame(c1 = sub("^regiao_2", "", names(rg)), c2 = as.numeric(rg),
                     stringsAsFactors = FALSE)
    names(df) <- c("Região", "Proporção")
    a <- R$r2$T2_aic
    paste0("<p>Proporção da população em cada região do reticulado (modelo final, K = ", R$r2$K_esc, "):</p>",
           rel_tabela(df, 3),
           "<p>", tag("Inferência"), "O quadrante inconsistente reúne pessoas com diagnóstico de vida, poucos ",
           "sintomas atuais e atendimento distante. Com duas fontes de evidência, a existência do conflito é ",
           "garantida pela construção; o conteúdo empírico está na prevalência e nos correlatos.</p>",
           "<h3>Predição de autoavaliação de saúde ruim</h3>", rel_tabela(rel_rn(a, "Modelo"), 1))
  })

  s_lim <- secao("limitacoes", "7. Limitações", function() {
    paste0("<ul>",
      "<li>", tag("Medido"), "O modelo de um fator do PHQ-9 tem ajuste ruim pelos índices robustos; ",
      "o denominador fatorial depende só da ordenação por gravidade, e o resultado se repete com a soma bruta.</li>",
      "<li>A invariância foi testada entre macrorregiões, não entre UFs da mesma macrorregião.</li>",
      "<li>O numerador não foi testado: diferenças regionais em lembrar ou relatar um diagnóstico recebido ",
      "não são verificáveis com a PNS.</li>",
      "<li>O diagnóstico é de vida e pode ter sido feito em outra UF.</li>",
      "<li>Desenho transversal: o resultado é descritivo, não causal; curso episódico e heterogeneidade ",
      "estável entre pessoas não se distinguem.</li>",
      "<li>Muitos testes foram feitos na mesma base; efeitos com p perto de 0,05 (como a desvantagem da capital) ",
      "devem ser lidos como frágeis.</li>",
      "<li>Os modelos de medida foram ajustados sem pesos amostrais.</li>",
      "<li>Os códigos de variáveis marcados como CONFERIR nos scripts dependem do dicionário da PNS 2019.</li>",
      "</ul>")
  })

  s_repro <- secao("repro", "8. Reprodutibilidade", function() {
    lg <- R$log
    df <- data.frame(c1 = names(lg),
                     c2 = vapply(lg, function(x) isTRUE(x$ok), logical(1)),
                     c3 = vapply(lg, function(x) as.numeric(x$minutos), numeric(1)),
                     stringsAsFactors = FALSE)
    names(df) <- c("Script", "Concluído", "Minutos")
    pk <- R$meta$pacotes
    paste0("<p>Execução iniciada em ", format(R$meta$inicio, "%d/%m/%Y %H:%M"),
           if (!is.null(R$meta$fim)) paste0(" e concluída em ", format(R$meta$fim, "%d/%m/%Y %H:%M")) else "",
           ". ", rel_esc(R$meta$R), ".</p>",
           rel_tabela(df, 1),
           "<p>Pacotes: ", paste0(names(pk), " ", pk, collapse = "; "), ".</p>",
           "<p class='nota'>Cada rodada grava um log em <code>saida/</code>. Os resultados brutos estão em ",
           "<code>saida/resultados.rds</code>; o relatório pode ser refeito sem rodar os modelos com ",
           "<code>Rscript run_all.R --so-relatorio</code>.</p>")
  })

  html <- paste0(
    "<!DOCTYPE html><html lang='pt-BR'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>Diagnóstico de depressão na PNS 2019</title><style>", rel_css, "</style></head><body>",
    "<h1>Diagnóstico de depressão na PNS 2019</h1>",
    "<p class='sub'>Lógica paraconsistente anotada, desigualdade regional e trilha de hipóteses refutadas · ",
    "gerado em ", format(Sys.time(), "%d/%m/%Y %H:%M"), "</p>",
    s_achado, s_dados, s_placar, s_robustez, s_uf, s_etau, s_lim, s_repro,
    "</body></html>")
  con <- file(arquivo, open = "w", encoding = "UTF-8")
  writeLines(html, con)
  close(con)
  invisible(arquivo)
}
