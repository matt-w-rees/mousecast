




# FORECAST PLOTS ----------------------------------------------------------
## current - forecast 

# also remove tas from aus_shp outline
aus_shp <- aus_shp %>%
  filter(ST_NAME != "TAS")



# predicted abundance
p1 <- model_lag_preds_poly2 %>%
  filter(time %in% c(48:49)) %>%
  ggplot() + 
  geom_sf(data = aus_shp, fill = "white") + 
  geom_sf(aes(fill = log(pred_mean_ae))) +
  coord_sf(xlim = c(128.5, 154.2), ylim = c(-39.5, -20.5), expand = FALSE) + 
  #scale_fill_scico(palette = "bilbao", direction = -1, #high = "#f03b20", 
  scale_fill_viridis_c(
    limits=c(log(min(model_lag_preds_poly2$pred_mean_ae)), log(max(model_lag_preds_poly2$pred_mean_ae))),
    breaks=c(log(min(model_lag_preds_poly2$pred_mean_ae)), log(max(model_lag_preds_poly2$pred_mean_ae))), 
    labels=c("Low", "High"), name = "Relative abundance\n(log-scaled)") +
  facet_wrap(~year_season) + 
  theme(legend.position = "right")

# predicted difference abundance
p2 <- model_lag_preds_poly2 %>%
  filter(time %in% c(48:49)) %>%
  ggplot() + 
  geom_sf(data = aus_shp, fill = "white") + 
  geom_sf(aes(fill = Prop_Change)) +
  coord_sf(xlim = c(128.5, 154.2), ylim = c(-39.5, -20.5), expand = FALSE) + 
  scale_fill_scico(palette = "vik", midpoint = 1, name = "Proportional difference\nfrom previous season") +
  facet_wrap(~year_season) + 
  theme(legend.position = "right")

# categorical abundance relative to each ae zone
p3 <- model_lag_preds_poly2 %>%
  filter(time %in% c(48:49)) %>%
  ggplot() + 
  geom_sf(data = aus_shp, fill = "white") + 
  geom_sf(aes(fill = pred_cat)) +
  coord_sf(xlim = c(128.5, 154.2), ylim = c(-39.5, -20.5), expand = FALSE) + 
  scale_fill_manual(values=c("#91cf60", "#ffeda0", "#f03b20"), name = "Region-specific\ncategorical abundance") +
  facet_wrap(~year_season) + 
  theme(legend.position = "right")

# assemble and save 
png(paste0(out_path, "forecast_summer_25.png"), width = 7, height = 8, res = 600, units = "in")
(p1 / p2 / p3) + plot_annotation(
  subtitle = '                                Current                                  Forecast',
  title = 'Mice forecast for summer 2025',
  caption = 'Predicted 2/12/2024',
  tag_levels = 'a')
dev.off()


# PLOT AE ZONE ------------------------------------------------------------


png(paste0(out_path, "adjusted_ae_zone_adj_shp.png"), width = 8, height = 6, res = 600, units = "in")
ggplot(aus_shp) +   
  geom_sf(fill = "white") + 
  geom_sf(data = model_lag_preds_poly2, aes(group = model_lag_preds_poly2$ae_zone, fill = model_lag_preds_poly2$ae_zone)) + 
  labs(fill = "GRDC agroecological zone")
dev.off()




