library(tidyverse)

# ── 1. 데이터 로드 & 전처리 ─────────────────────────────────
dat <- read.csv("data.csv", stringsAsFactors = FALSE)
dat <- dat[!is.na(dat$year) & dat$year != "", ]

dat <- transform(dat,
  year              = as.integer(year),
  month             = as.integer(month),
  kospi             = as.numeric(gsub(",", "", kospi)),
  approval_rate     = as.numeric(approval_rate),
  cpi               = as.numeric(cpi),
  unemployment_rate = as.numeric(unemployment_rate),
  interest_rate     = as.numeric(interest_rate),
  ccsi              = as.numeric(ccsi),
  scandal_presence  = as.integer(scandal_presence),
  scandal_type      = as.integer(scandal_type),
  date              = as.Date(paste(year, month, "01", sep = "-"))
)

# ── 2. Table 3: 기술통계 ────────────────────────────────────
cat("\n")
cat("================================================================\n")
cat(" Table 3. Descriptive Statistics of Variables (2022.05~2024.11)\n")
cat("================================================================\n")
cat(sprintf("%-30s %8s %8s %8s %8s\n", "Variable", "Min", "Max", "Mean", "SD"))
cat(sprintf("%s\n", strrep("-", 66)))

vars   <- c("approval_rate","cpi","unemployment_rate","kospi","interest_rate","ccsi")
labels <- c("Presidential Approval Rate","Consumer Price Index (CPI)",
            "Unemployment Rate","KOSPI","Interest Rate","CCSI")

for (i in seq_along(vars)) {
  x <- dat[, vars[i]]
  cat(sprintf("%-30s %8.2f %8.2f %8.2f %8.2f\n",
              labels[i], min(x, na.rm = TRUE), max(x, na.rm = TRUE),
              mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
}
cat(sprintf("%s\n\n", strrep("-", 66)))

# ── 3. 회귀분석 데이터 준비 ──────────────────────────────────
dat_m <- subset(dat,
                !is.na(approval_rate) &
                !is.na(cpi) &
                !is.na(unemployment_rate) &
                !is.na(kospi) &
                !is.na(interest_rate) &
                !is.na(ccsi))

dat_t <- subset(dat_m, !is.na(scandal_type))

# ── 4. 모형 추정 ─────────────────────────────────────────────
m1 <- lm(approval_rate ~ cpi + unemployment_rate + kospi + interest_rate + ccsi,
         data = dat_m)

m2 <- lm(approval_rate ~ cpi + unemployment_rate + kospi + interest_rate + ccsi
         + scandal_presence,
         data = dat_m)

m3 <- lm(approval_rate ~ cpi + unemployment_rate + kospi + interest_rate + ccsi
         + scandal_presence
         + cpi:scandal_presence + unemployment_rate:scandal_presence
         + kospi:scandal_presence + interest_rate:scandal_presence
         + ccsi:scandal_presence,
         data = dat_m)

m4 <- lm(approval_rate ~ cpi + unemployment_rate + kospi + interest_rate + ccsi
         + scandal_type,
         data = dat_t)

m5 <- lm(approval_rate ~ cpi + unemployment_rate + kospi + interest_rate + ccsi
         + scandal_type
         + cpi:scandal_type + unemployment_rate:scandal_type
         + kospi:scandal_type + interest_rate:scandal_type
         + ccsi:scandal_type,
         data = dat_t)

# ── 5. Table 4: 회귀분석 결과 ───────────────────────────────
models     <- list(m1, m2, m3, m4, m5)
all_terms  <- c("(Intercept)", "cpi", "unemployment_rate", "kospi",
                "interest_rate", "ccsi", "scandal_presence", "scandal_type",
                "cpi:scandal_presence", "unemployment_rate:scandal_presence",
                "cpi:scandal_type", "unemployment_rate:scandal_type")
row_labels <- c("(Intercept)", "cpi", "unemployment_rate", "kospi",
                "interest_rate", "ccsi", "scandal_presence", "scandal_type",
                "cpi x scandal_presence", "unemployment_rate x scandal_presence",
                "cpi x scandal_type", "unemployment_rate x scandal_type")

col_w  <- 22
header <- sprintf("%-32s", "Variable")
for (i in 1:5) header <- paste0(header, sprintf("%*s", col_w, paste0("Model ", i)))
sep    <- strrep("=", 32 + col_w * 5)

cat(sep, "\n")
cat(" Table 4. Regression Results Summary\n")
cat(sep, "\n")
cat(header, "\n")
cat(strrep("-", 32 + col_w * 5), "\n")

for (j in seq_along(all_terms)) {
  term <- all_terms[j]
  row  <- sprintf("%-32s", row_labels[j])
  for (m in models) {
    ct <- coef(summary(m))
    if (term %in% rownames(ct)) {
      b   <- ct[term, 1]
      se  <- ct[term, 2]
      p   <- ct[term, 4]
      sig <- ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))
      row <- paste0(row, sprintf("%*s", col_w,
                                 sprintf("%.2f (%.2f)%s", b, se, sig)))
    } else {
      row <- paste0(row, sprintf("%*s", col_w, ""))
    }
  }
  cat(row, "\n")
}

cat(strrep("-", 32 + col_w * 5), "\n")

for (stat in c("N", "R2", "Adj.R2")) {
  row <- sprintf("%-32s", stat)
  for (m in models) {
    val <- switch(stat,
      "N"      = sprintf("%d",    nobs(m)),
      "R2"     = sprintf("%.3f", summary(m)$r.squared),
      "Adj.R2" = sprintf("%.3f", summary(m)$adj.r.squared)
    )
    row <- paste0(row, sprintf("%*s", col_w, val))
  }
  cat(row, "\n")
}
cat(sep, "\n")
cat("Significance codes: * p<.10  ** p<.05  *** p<.01\n\n")

# ── 6. Figure 1: 지지율 추이 ────────────────────────────────
png("figure1_approval.png", width = 800, height = 400, res = 100)

p <- ggplot(dat, aes(x = date, y = approval_rate)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "red", size = 2.5) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(10, 55), breaks = seq(10, 55, 10)) +
  labs(title = "Figure 1. Changes in Presidential Approval Ratings",
       x = "year-month", y = "approval rate") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)
dev.off()
cat("Figure 1 저장 완료: figure1_approval.png\n")
