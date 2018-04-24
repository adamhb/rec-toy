#exploring relationship 
ggplot(data = l_shd_data, mapping = aes(x = shd, y = lnRI)) +
  geom_point()



ggplot(data = l_shd_data, mapping = aes(x = shd, y = lnRI)) +
  geom_point() +
  geom_line(mapping = aes(x = shd, y = predict(l_shd_m_quad, newdata = l_shd_data))) +
  geom_line(mapping = aes(x = shd, y = predict(l_shd_m, newdata = l_shd_data))) +
  geom_line(mapping = aes(x = shd, y = predict(l_shd_m_3, newdata = l_shd_data)))



ggplot(data = seed_dyn_c, mapping = aes(x = shade, y = RI3)) +
  geom_point()