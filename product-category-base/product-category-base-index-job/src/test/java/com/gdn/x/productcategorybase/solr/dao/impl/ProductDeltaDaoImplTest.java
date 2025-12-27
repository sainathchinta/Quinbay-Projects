package com.gdn.x.productcategorybase.solr.dao.impl;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.jdbc.core.JdbcTemplate;

public class ProductDeltaDaoImplTest {

  private ProductDeltaDaoImpl productDeltaDaoImpl;

  private JdbcTemplate jdbcTemplate;

  @BeforeEach
  public void initializeTst() throws Exception {
    jdbcTemplate = Mockito.mock(JdbcTemplate.class);
    productDeltaDaoImpl = new ProductDeltaDaoImpl(jdbcTemplate);
  }

  @Test
  public void fetchUpdatedProductsTest() throws Exception {
    productDeltaDaoImpl.fetchUpdatedProducts();
  }

  @Test
  public void updateElementStateToProcessingTest() throws Exception {
    List<String> productIds = new ArrayList<>();
    productIds.add("1");
    productDeltaDaoImpl.updateElementStateToProcessing(productIds);
  }

  @Test
  public void deleteProcessedElementsTest() throws Exception {
    productDeltaDaoImpl.deleteProcessedElements();
  }

  @Test
  public void updateAllElementsToProcessingTest() throws Exception {
    productDeltaDaoImpl.updateAllElementsToProcessing();
  }
}
