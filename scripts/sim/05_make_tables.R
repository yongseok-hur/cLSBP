#!/usr/bin/env Rscript
# 05_make_tables.R -- LaTeX row bodies of the simulation tables in the paper, from the summaries
# written by 04_harvest.R (results/kang_summary.csv, results/hahn_summary.csv).
#
#   Table 1        Simulation 1, main          (cLSBP/BCF with/without BART pi-hat; CF with BART pi-hat)
#   Tables 2, 3    Simulation 2, main          (linear / nonlinear mu)
#   Appendix C     Simulation 1, PS estimator  (cLSBP/BCF: GLM, BART; CF: default, GLM, BART)
#   Appendix C     Simulation 1, truncation H  (cLSBP, BART pi-hat, H = 10, 20, 30)
#   Appendix D     Simulation 2, PS estimator  (linear / nonlinear mu)
#
# Cells are \V{value}{MCSE}; the paper defines \newcommand{\V}[2]{#1\,{\scriptsize(#2)}}.
# Bold (main tables only): smallest RMSE and COV closest to 0.95 among all methods in a block,
# compared after rounding to two decimals (ties are all bold).
# Usage (from the repository root): Rscript scripts/sim/05_make_tables.R [output file]
suppressMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
OUT <- if (length(args) >= 1) args[1] else "tables/table_bodies.tex"

rd <- function(f) read.csv(f, stringsAsFactors = FALSE) %>% filter(what_name == "tau", confi == 95)
S <- bind_rows(rd("results/kang_summary.csv") %>% mutate(sim = "kang"),
               rd("results/hahn_summary.csv") %>% mutate(sim = "hahn"))

get <- function(sim_, model_, ps_, sc, tg, H_ = 20) {
  q <- S %>% filter(sim == sim_, model == model_, ps == ps_, scenario == sc, target == tg)
  if (model_ == "cLSBP") q <- q %>% filter(!is.na(H), H == H_)
  if (nrow(q) != 1) stop(sprintf("expected one row: %s %s %s %s %s H=%s (got %d)", sim_, model_, ps_, sc, tg, H_, nrow(q)))
  q
}
r2 <- function(x) as.numeric(sprintf("%.2f", x))          # rounding as used for the printed values
m3 <- function(x) { s <- sprintf("%.3f", x); ifelse(startsWith(s, "0."), substring(s, 2), s) }
V  <- function(v, m, b = FALSE) { s <- sprintf("\\V{%.2f}{%s}", v, m3(m)); if (b) paste0("{\\bf ", s, "}") else s }
rowobj <- function(sim_, model_, ps_, sc, H_ = 20) list(ATE = get(sim_, model_, ps_, sc, "ATE", H_),
                                                       CATE = get(sim_, model_, ps_, sc, "CATE", H_))
cells <- function(rows, bold) {
  out <- vector("list", length(rows))
  for (tg in c("ATE", "CATE")) {
    rm <- sapply(rows, function(r) r2(r[[tg]]$rmse))
    cv <- sapply(rows, function(r) r2(abs(r2(r[[tg]]$cover) - 0.95)))
    for (i in seq_along(rows)) {
      x <- rows[[i]][[tg]]
      out[[i]] <- c(out[[i]], V(x$rmse, x$mcse_rmse, bold && rm[i] == min(rm)),
                    V(x$cover, x$mcse_cover, bold && cv[i] == min(cv)), V(x$len, x$mcse_len))
    }
  }
  out
}
KT <- c("ts-0264" = "$t=-2$", "ts-0132" = "$t=-1$", "ts00" = "$t=0$", "ts0132" = "$t=1$", "ts0264" = "$t=2$")
L <- character(0); add <- function(...) L <<- c(L, paste0(...))
cm <- function(i, rng) if (i %in% c(2, 4)) paste0(" \\cmidrule{", rng, "}") else ""

## ---------- Table 1 ----------
add("%%%%% TABLE 1 BODY %%%%%")
spec <- list(c("cLSBP", "BART", "cLSBP", "With"), c("cLSBP", "noPS", "", "Without"),
             c("BCF", "BART", "BCF", "With"), c("BCF", "noPS", "", "Without"), c("CF", "BART", "CF", "With"))
for (k in seq_along(KT)) {
  sc <- names(KT)[k]; rows <- lapply(spec, function(s) rowobj("kang", s[1], s[2], sc)); cs <- cells(rows, TRUE)
  for (i in seq_along(spec)) {
    pre <- if (i > 1) "    " else if (k > 1) "    \\midrule\n    " else "    "
    add(pre, if (i == 1) KT[[k]] else "", " & ", spec[[i]][3], " & ", spec[[i]][4], " & ",
        paste(cs[[i]], collapse = " & "), "\n    \\\\", cm(i, "2-9"))
  }
}
add("    \\bottomrule")

## ---------- Tables 2/3 ----------
specH <- list(c("cLSBP", "BART", "cLSBP", "With"), c("cLSBP", "noPS", "cLSBP", "Without"),
              c("BCF", "BART", "BCF", "With"), c("BCF", "noPS", "BCF", "Without"), c("CF", "BART", "CF", "With"))
for (lin in c("lin", "non")) {
  add("\n%%%%% ", if (lin == "lin") "TABLE 2 BODY (linear)" else "TABLE 3 BODY (nonlinear)", " %%%%%")
  first_block <- TRUE
  for (he in c("hom", "het")) {
    hl <- if (he == "hom") c("Homo-", "geneous") else c("Hetero-", "geneous")
    for (nn in c("250", "500")) {
      sc <- paste(lin, he, nn, sep = "_"); rows <- lapply(specH, function(s) rowobj("hahn", s[1], s[2], sc)); cs <- cells(rows, TRUE)
      for (i in seq_along(specH)) {
        c0 <- if (nn == "250" && i == 1) hl[1] else if (nn == "250" && i == 2) hl[2] else ""
        c1 <- if (i == 1) paste0("$", nn, "$") else ""
        if (i == 1 && !first_block) add(if (nn == "250") "    \\cmidrule{1-10}" else "    \\cmidrule{2-10}")
        first_block <- FALSE
        add("    ", c0, " & ", c1, " & ", specH[[i]][3], " & ", specH[[i]][4], " & ",
            paste(cs[[i]], collapse = " & "), "\n    \\\\", cm(i, "3-10"))
      }
    }
  }
  add("    \\bottomrule")
}

## ---------- Appendix: Simulation 1, PS estimator ----------
specA <- list(c("cLSBP", "GLM", "cLSBP", "GLM"), c("cLSBP", "BART", "", "BART"), c("BCF", "GLM", "BCF", "GLM"),
              c("BCF", "BART", "", "BART"), c("CF", "default", "CF", "default"), c("CF", "GLM", "", "GLM"),
              c("CF", "BART", "", "BART"))
add("\n%%%%% APPENDIX Sim1 combined BODY %%%%%")
for (k in seq_along(KT)) {
  sc <- names(KT)[k]; rows <- lapply(specA, function(s) rowobj("kang", s[1], s[2], sc)); cs <- cells(rows, FALSE)
  for (i in seq_along(specA)) {
    pre <- if (k > 1 && i == 1) "    \\midrule\n    " else "    "
    add(pre, if (i == 1) KT[[k]] else "", " & ", specA[[i]][3], " & ", specA[[i]][4], " & ",
        paste(cs[[i]], collapse = " & "), "\n    \\\\", cm(i, "2-9"))
  }
}
add("    \\bottomrule")

## ---------- Appendix: Simulation 2, PS estimator ----------
for (lin in c("lin", "non")) {
  add("\n%%%%% ", if (lin == "lin") "APPENDIX Sim2 linear BODY" else "APPENDIX Sim2 nonlinear BODY", " %%%%%")
  k <- 0
  for (he in c("hom", "het")) for (nn in c("250", "500")) {
    sc <- paste(lin, he, nn, sep = "_"); rows <- lapply(specA, function(s) rowobj("hahn", s[1], s[2], sc)); cs <- cells(rows, FALSE)
    for (i in seq_along(specA)) {
      pre <- if (k > 0 && i == 1) "    \\midrule\n    " else "    "
      add(pre, if (i == 1) paste0(if (he == "hom") "Hom" else "Het", "/", nn) else "", " & ", specA[[i]][3], " & ",
          specA[[i]][4], " & ", paste(cs[[i]], collapse = " & "), "\n    \\\\", cm(i, "2-9"))
    }
    k <- k + 1
  }
  add("    \\bottomrule")
}

## ---------- Appendix: Simulation 1, truncation level H ----------
add("\n%%%%% H TABLE Sim1 BART BODY %%%%%")
for (k in seq_along(KT)) for (i in 1:3) {
  H <- c(10, 20, 30)[i]; r <- rowobj("kang", "cLSBP", "BART", names(KT)[k], H)
  c6 <- c(V(r$ATE$rmse, r$ATE$mcse_rmse), V(r$ATE$cover, r$ATE$mcse_cover), V(r$ATE$len, r$ATE$mcse_len),
          V(r$CATE$rmse, r$CATE$mcse_rmse), V(r$CATE$cover, r$CATE$mcse_cover), V(r$CATE$len, r$CATE$mcse_len))
  pre <- if (k > 1 && i == 1) "    \\midrule\n    " else "    "
  add(pre, if (i == 1) KT[[k]] else "", " & $", H, "$ & ", paste(c6, collapse = " & "), "\n    \\\\")
}
add("    \\bottomrule")

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
writeChar(paste(L, collapse = "\n"), OUT, eos = NULL)
message("written: ", OUT)
