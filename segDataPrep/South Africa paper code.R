library("haven")
library("data.table")
library("segregation")
library("ggplot2")
library("ggrepel")
library("here")
library("patchwork")
library("knitr")
library("kableExtra")

## major municipalities
metros <- c(
  "City of Johannesburg Metropolitan Municipality",
  "City of Cape Town Metropolitan Municipality",
  "Ethekwini Metropolitan Municipality",
  "Ekurhuleni Metropolitan Municipality",
  "City of Tshwane Metropolitan Municipality",
  "Nelson Mandela Bay Metropolitan Municipality",
  "Mangaung Metropolitan Municipality",
  "Buffalo City Metropolitan Municipality"
)

##########################
### load register data ###
##########################

d <- read_dta("03_posted/ASSdata2021.dta")
setDT(d)
d[, municipalityID := as_factor(municipalityID)]
d[, provID := as_factor(provID)]

prov_sizes <- d[, .(n = sum(totstudents)), by = .(provID)]
pop <- tibble::tribble(
  ~provID, ~provName, ~pop2011,
  "EC", "Eastern Cape", 6562053,
  "FS", "Free State", 2745590,
  "GT", "Gauteng", 12272263,
  "KZN", "KwaZulu-Natal", 10267300,
  "LP", "Limpopo", 5404868,
  "MP", "Mpumalanga", 4039939,
  "NC", "Northern Cape", 1145861,
  "NW", "North-West", 3509953,
  "WC", "Western Cape", 5822734
)
setDT(pop)
merge(prov_sizes, pop, all = TRUE)[, cor(n, pop2011)]

d[, .(White = sum(White)), by = .(provID)]

d[pop, province := i.provName, on = .(provID)]

for (col in names(d)) {
  if (typeof(d[[col]]) == "character") {
    d[[col]] <- trimws(d[[col]])
    d[[col]] <- ifelse(d[[col]] == "", NA, d[[col]])
  }
  if (is.factor(d[[col]])) {
    d[[col]] <- droplevels(d[[col]])
  }
}
skimr::skim(d)

d[, exdept := as_factor(exdept)]
d[, exdept := forcats::fct_explicit_na(exdept)]
d[, exdept := droplevels(exdept)]

d[, quintile := as_factor(quintile)]
d[, quintile := forcats::fct_explicit_na(quintile)]

ids <- c(
  "provID", "province", "municipalityID", "wardID", "schoolID",
  "quintile", "independent", "exdept"
)

setnames(d, "Asian", "Indian")
long <- melt(
  d, ids,
  c("White", "Black", "Indian", "Coloured"),
  "group", "n"
)
setorder(long, schoolID)

long[, municipalityID := forcats::fct_explicit_na(municipalityID, "(Missing)")]

fst::write.fst(long, here("03_posted", "long_2021.fst"))
haven::write_dta(long, here("03_posted", "long_2021.dta"))

##########################
### basic segregation ###
##########################

total_H <- mutual_total(long, "group", "schoolID", weight = "n")[stat == "H", est]

# decomposition
decomp_nested <- mutual_total_nested(long, "group",
  c("provID", "municipalityID", "wardID", "schoolID"),
  weight = "n"
)[stat == "H"]

decomp_nested <- rbindlist(list(
  data.table(between = "total", within = "total", est = total_H),
  decomp_nested[, -"stat"]
))
decomp_nested[, pct := 100 * est / est[1]]

cat(kable(decomp_nested, "html", digits = 3),
  file = here("06_tables", "decomposition_H.html"), sep = "\n"
)

############################
### exposure / isolation ###
############################

isolation(long, "group", "schoolID", weight = "n")

exp <- exposure(long, "group", "schoolID", weight = "n")
exp[, exposure := exposure * 100]
exp <- dcast(exp, of ~ to,
  value.var = "exposure"
)
cat(kable(exp, "html", digits = 1),
  file = here("06_tables", "exposure.html"), sep = "\n"
)

################################
### segregation contribution ###
################################

create_tab <- function(var) {
  total_H <- mutual_total(long, "group", "schoolID", weight = "n")[stat == "H", est]
  group_entropy <- entropy(long, "group", weight = "n")

  ls <- mutual_local(long, "group", c(var, "schoolID"), weight = "n", wide = TRUE)
  tab <- ls[, .(
    N = .N,
    p = 100 * sum(p),
    mean_ls = weighted.mean(ls / group_entropy, p),
    pct_contrib = (100 * sum(ls * p) / group_entropy) / total_H
  ),
  by = var
  ]
  tab <- merge(tab, long[, .(entropy = 100 * entropy(.SD, "group", weight = "n") / log(5)), by = var])
  setnames(tab, var, "type")
  tab
}

tab_all <- rbindlist(list(create_tab("exdept"), create_tab("independent"))) |>
  kable("html", digits = 1, col.names = c("", "Number", "%", "Average", "% contribution", "")) |>
  add_header_above(c(" ", "Schools" = 2, "Segregation" = 2, "Diversity" = 1)) |>
  pack_rows("Former department", 1, 5) |>
  pack_rows("Independent/Public", 6, 7) |>
  cat(file = here("06_tables", "segregation_contribution.html"), sep = "\n")


###########################
### within municipality ###
###########################

within_muni <- mutual_within(long, "group", "schoolID",
  within = c("province", "municipalityID"), wide = TRUE, weight = "n"
)
within_muni[, sum(p * H * ent_ratio)]
within_muni[, contrib_H := p * H * ent_ratio]
setorder(within_muni, -contrib_H)
within_muni[, sum(contrib_H)]
within_muni[, contrib_H_cumul := cumsum(contrib_H)]
within_muni

p <- ggplot(within_muni, aes(x = 1:212, y = contrib_H_cumul / within_muni$contrib_H_cumul[212])) +
  geom_point() +
  geom_text(
    data = within_muni[1:15], aes(x = 1:15, label = municipalityID),
    hjust = 0, size = 2, nudge_x = 2
  ) +
  geom_hline(yintercept = c(0, 1), color = "orange") +
  scale_y_continuous(labels = scales::percent_format(), limit = c(0, 1)) +
  labs(title = "Cumulative contribution of munipicalities", x = "Municipalities", y = "%") +
  theme_light()
ggsave(here("05_figures", "Ben_cumul_municipalities_2021.pdf"), p, width = 7, height = 7)

muni_ent <- long[, .(entropy = 100 * entropy(.SD, "group", weight = "n") / log(5)), by = "municipalityID"]
within_muni <- merge(within_muni, muni_ent)

p <- ggplot(within_muni, aes(x = H, y = entropy, size = p, color = province)) +
  geom_point() +
  labs(title = "Segregation vs. Diversity at the municipal level", x = "Segregation (H)", y = "Diversity (Entropy)") +
  theme_light()
ggsave(here("05_figures", "Ben_segregation_vs_diversity.pdf"), p, width = 7, height = 7)


#################
### dot plots ###
#################

muni_sizes <- long[, .(n = sum(n)), by = .(provID, municipalityID)]
summary(muni_sizes$n)
muni_sizes[order(-n)][1:20]
muni_sizes[grepl("Metropolitan", municipalityID)][order(-n)]

pairwise_by_muni <- function(munip, within = NULL) {
  munip <- copy(munip)
  pairs <- list(
    c("White", "Black"),
    c("White", "Indian"),
    c("White", "Coloured"),
    c("Black", "Indian"),
    c("Black", "Coloured"),
    c("Indian", "Coloured")
  )
  if (is.null(within)) {
    multigroup <- mutual_total(munip, "group", "schoolID", weight = "n")[stat == "H", est]
  } else {
    # multigroup <- mutual_total(munip, "group", "schoolID", within = within, weight = "n")[stat == "H", est]
    multigroup <- mutual_within(munip, "group", "schoolID", within = within, weight = "n", wide = TRUE)[, sum(p * H)]
  }

  seg <- lapply(pairs, function(pair) {
    subset <- munip[group %in% pair]
    if (is.null(within)) {
      H <- mutual_total(subset, "group", "schoolID", weight = "n")[stat == "H", est]
    } else {
      # H <- mutual_total(subset, "group", "schoolID", within = within, weight = "n")[stat == "H", est]
      H <- mutual_within(subset, "group", "schoolID", within = within, weight = "n", wide = TRUE)[, sum(p * H)]
    }
    p <- sum(subset$n) / sum(munip$n)
    data.table(H = H, p = p, pair = paste0(pair, collapse = "-"))
  })
  seg <- rbindlist(seg)

  seg[, multigroup := multigroup]
  seg[]
}

# 8 metros
pw <- long[municipalityID %in% metros, pairwise_by_muni(.SD), by = .(municipalityID)]
levels(pw$municipalityID) <- stringr::str_replace(levels(pw$municipalityID), " Metropolitan Municipality", "")
levels(pw$municipalityID) <- stringr::str_replace(levels(pw$municipalityID), "Mangaung", "Mangaung (Bloemfontein)")
levels(pw$municipalityID) <- stringr::str_replace(levels(pw$municipalityID), "Ethekwini", "Ethekwini (Durban)")
levels(pw$municipalityID) <- stringr::str_replace(levels(pw$municipalityID), "City of Tshwane", "City of Tshwane (Pretoria)")

dat <- rbindlist(list(
  pw[, .(municipalityID, H, pair)],
  unique(pw[, .(municipalityID, H = multigroup, pair = "Multigroup")])
))

overall <- pairwise_by_muni(long, within = "municipalityID")
overall <- rbindlist(list(
  overall[, .(H, pair)],
  unique(overall[, .(H = multigroup, pair = "Multigroup")])
))

# check: multigroup
mutual_total(long, "group", "schoolID", within = "municipalityID", weight = "n")
mg <- mutual_within(long, "group", "schoolID", within = "municipalityID", weight = "n", wide = TRUE)
mg[, marked := municipalityID %in% metros]
p <- ggplot(mg, aes(y = 1, x = H, size = p, color = marked)) +
  geom_jitter(alpha = 0.5)
ggsave(here("05_figures", "distribution_multigroup.png"), p, width = 7, height = 2, dpi = 600)

vals <- c(
  "Multigroup", "Indian-Coloured",
  "Black-Indian", "Black-Coloured", "White-Indian", "White-Black", "White-Coloured"
)

p <- ggplot(
  dat,
  aes(y = pair, x = H, color = municipalityID)
) +
  geom_point(data = overall, color = "black") +
  geom_point(alpha = 0.5, size = 3) +
  annotate(
    geom = "text", x = 0.35, y = 7.4, size = 3,
    label = "Nationwide average", hjust = "left",
    family = "Times"
  ) +
  annotate(
    geom = "curve", x = 0.51, y = 7.4, xend = 0.542, yend = 7.15,
    curvature = -.3, arrow = arrow(length = unit(2, "mm")),
  ) +
  labs(
    y = NULL,
    color = NULL
  ) +
  scale_y_discrete(limit = rev(vals)) +
  theme_light() +
  theme(text = element_text(family = "Times"))
ggsave(here("05_figures", "dotplot_largest_metros.png"), p, width = 7, height = 3, dpi = 600)

mangaung <- long[municipalityID == "Mangaung Metropolitan Municipality"]
wide_mangaung <- dcast(mangaung, schoolID ~ group, value.var = "n")
wide_mangaung[, pct_white := White / (White + Black + Indian + Coloured)]

##########################
### other dimensions ###
##########################

wb <- function(munip, var) {
  total <- mutual_total(munip, "group", "schoolID", weight = "n")
  within <- mutual_total(munip, "group", "schoolID", within = var, weight = "n")
  between <- mutual_total(munip, "group", var, weight = "n")
  all <- rbindlist(list(total, within, between))
  all <- all[stat == "H"]
  all[, stat := NULL]
  all[, variable := paste(var, collapse = "+")]
  all[, decomp := c("total", "within", "between")]
  all
}

decomps <- rbindlist(list(
  long[municipalityID %in% metros, wb(.SD, "independent"), by = .(municipalityID)],
  long[municipalityID %in% metros, wb(.SD, "exdept"), by = .(municipalityID)],
  long[municipalityID %in% metros, wb(.SD, "quintile"), by = .(municipalityID)],
  long[municipalityID %in% metros, wb(.SD, c("quintile", "exdept")), by = .(municipalityID)]
))
decomps[, municipalityID := gsub("Metropolitan Municipality", "", municipalityID, fixed = TRUE)]
decomps <- dcast(decomps, variable + municipalityID ~ decomp, value.var = "est")
decomps <- decomps[, .(variable, municipalityID, total, between, within)]
decomps[, `between %` := paste0(round(100 * between / total), "%")]
decomps[, `within %` := paste0(round(100 * within / total), "%")]

cat(kable(decomps, "html", digits = 2),
  file = here("06_tables", "decomposition_characteristics_H.html"), sep = "\n"
)
