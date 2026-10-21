source("_extensions/mcanouil/typst-render/_resources/typst_define.R")


library(tidyverse)
library(distributional)
library(marginaleffects)
library(ggdist)


ptol_red <- "#EE6677"
ptol_blue <- "#4477AA"
ptol_green <- "#228833"
ptol_teal <- "#66ccee"
ptol_purple <- "#AA3377"

theme_set(
  theme_ggdist(base_size = 16) +
    theme(
      text = element_text(family = "Public Sans"),
      palette.colour.discrete = c(ptol_red, ptol_blue, ptol_green, ptol_purple),
      # c(
      #   "#BB5566", "#4477AA", "#228833", "#AA3377",
      #   "#666633", "#882255", "#225555",
      #   "#004488"
      # ),
      palette.colour.continuous = scico::scico(10, palette = "managua")
    ) +
    theme_sub_panel(
      grid = element_blank(),
    ) +
    theme_sub_legend(
      key = element_blank(),
      background = element_blank()
    ) +
    theme_sub_axis(
      ticks = element_blank(),
      line = element_line(color = "grey60", linewidth = 0.2)
    ) +
    NULL
)

theme_no_y <- function(...) {
  theme_sub_axis_y(
    text = element_blank(),
    title = element_blank(),
    ...
  )
}

theme_no_x <- function(...) {
  theme_sub_axis_x(
    text = element_blank(),
    title = element_blank()
  )
}

scale_y_tight = function(...) {
  scale_y_continuous(
    expand = expansion(0),
    ...
  )
}

make_arate <- function(age, Xs = 0, age_slope = -1, xs_slope = 0){
  (age * age_slope) + (Xs * xs_slope)
}

make_adur <- function(arate, rate_slope){
  arate * rate_slope
}

make_rdur <- function(Xt, xt_slope = 2){
  Xt * xt_slope
}

make_vq <- function(
    dob,
    age,
    Xs,
    dur,
    Xt,
    
    dob_vq_slope = 0,
    age_vq_slope = 0,
    xs_vq_slope = 0,
    dur_vq_slope = 0,
    xt_vq_slope = 0
){
  
  (dob * dob_vq_slope) + 
    (Xs * xs_vq_slope) +
    (dur * dur_vq_slope) +
    (Xt * xt_vq_slope)
}

normalize <- function(x) {
  (x - mean(x))/sd(x)
}

simulate_data <- function(
    nspeak = 100,
    age_slope = 0,
    xs_slope = 0,
    rate_slope = 2,
    xt_slope = 0,
    dob_vq_slope = 0,
    age_vq_slope = 0,
    xs_vq_slope = 0,
    dur_vq_slope = 2,
    xt_vq_slope = 0,   
    ndat = 30
){
  tibble(
    year = runif(nspeak, min = -1, max = 1),
    age = runif(nspeak, min = -1, max = 1),
    Xs = sample(x = c(0,1), size = nspeak, replace = T)
  ) |> 
    mutate(
      speaker = row_number(),
      dob = year - age,
      
      rate_mu = make_arate(normalize(age), normalize(Xs), age_slope, xs_slope),
      rate_dist = dist_normal(rate_mu),
      rate = generate(rate_dist, 1) |> unlist(),
      
      dur_mu = make_adur(normalize(rate), rate_slope),
      dur_dist = dist_normal(dur_mu),
      adur = generate(dur_dist, 1) |> unlist(),
      
      ndat = rpois(nspeak, ndat),
      
      xt_dist = dist_normal(),
      Xt = generate(xt_dist, ndat)
      
    ) |> 
    unnest(Xt) |> 
    mutate(
      rdur_mu = make_rdur(normalize(Xt), xt_slope),
      rdur_dist = dist_normal(rdur_mu),
      rdur = generate(rdur_dist, 1) |> unlist(),
      
      dur = adur + rdur,
      
      vq_mu = make_vq(
        normalize(dob), normalize(age), normalize(Xs), normalize(dur), Xt,
        dob_vq_slope, dob_age_slope, xs_vq_slope,
        dur_vq_slope, xt_vq_slope   
      ),
      vq_dist = dist_normal(vq_mu),
      vq = generate(vq_dist, 1) |> unlist()
    )
}


sim_fit <- function(
    nspeak = 100,
    age_slope = -2,
    xs_slope = 0,
    rate_slope = 2,
    xt_slope = 0,
    dob_vq_slope = 0,
    age_vq_slope = 0,
    xs_vq_slope = 0,
    dur_vq_slope = -2,
    xt_vq_slope = 0,   
    ndat = 30 
){
  
  dat <- simulate_data(
    nspeak = nspeak,
    age_slope = age_slope,
    xs_slope = xs_slope,
    rate_slope = rate_slope,
    xt_slope = xt_slope,
    dob_vq_slope = dob_vq_slope,
    age_vq_slope = age_vq_slope,
    xs_vq_slope = xs_vq_slope,
    dur_vq_slope = dur_vq_slope,
    xt_vq_slope = xt_vq_slope,   
    ndat = ndat
  ) 
  
  mod0 <- lm(
    vq ~ dob, data = dat
  )
  
  mod0.5 <- lm(
    vq ~ dob + age, data = dat
  )
  
  mod1 <- lm(
    vq ~ dob + age + adur, data = dat
  )
  
  mod2 <- lm(
    vq ~ dob + age + dur, data = dat
  )
  
  mod3 <- lm(
    vq ~ dob + age + adur + Xs, data = dat
  )
  
  list(
    `~dob` = mod0,
    `~dob + age` = mod0.5,
    `~dob + age + adur` = mod1,
    `~dob + age + dur` = mod2,
    `~dob + age + adur + Xs` = mod3
  ) |> 
    map(
      ~slopes(.x, newdata = datagrid())
    ) |> 
    list_rbind(names_to = "mod") |> 
    mutate(
      mu_dist = dist_normal(estimate, std.error),
      nspeak = nspeak,
      age_slope = age_slope,
      xs_slope = xs_slope,
      rate_slope = rate_slope,
      xt_slope = xt_slope,
      dob_vq_slope = dob_vq_slope,
      xs_vq_slope = xs_vq_slope,
      dur_vq_slope = dur_vq_slope,
      xt_vq_slope = xt_vq_slope,   
      ndat = ndat
    )
  
}