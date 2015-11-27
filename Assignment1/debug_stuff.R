for (i in which(sapply(m_sales$NEIGHBORHOOD, is.numeric))) {
    # m_sales[is.na(m_sales$NEIGHBORHOOD <- mean)]
    print(m_sales[i,])
}
