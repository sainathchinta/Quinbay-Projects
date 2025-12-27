package com.gdn.mta.product.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductImageQcBacklog;
import com.gdn.mta.product.repository.ProductImageQcBacklogRepository;

public class ProductImageQcBacklogServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String STATUS = "status";
  private static final String ORDER_BY = "updated_date";
  private static final String ORDER_IN = "DESC";
  private static final String PRODUCT_CODE = "productCode";

  @InjectMocks
  private ProductImageQcBacklogServiceImpl productImageQcBacklogService;

  @Mock
  private ProductImageQcBacklogRepository productImageQcBacklogRepository;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productImageQcBacklogRepository);
  }

  @Test
  public void findProductImageQcBacklogByStoreIdAndStatus() {
    productImageQcBacklogService.findProductImageQcBacklogByStoreIdAndStatus(STORE_ID, STATUS, ORDER_BY, ORDER_IN, 10);
    Mockito.verify(productImageQcBacklogRepository).findByStoreIdAndStatus(STORE_ID, STATUS, ORDER_BY, 10);
  }

  @Test
  public void findByStoreIdAndProductCodeAndStatusTest() {
    productImageQcBacklogService.findByStoreIdAndProductCodeAndStatus(STORE_ID, PRODUCT_CODE, STATUS);
    Mockito.verify(productImageQcBacklogRepository)
        .findByStoreIdAndProductCodeAndStatus(STORE_ID, PRODUCT_CODE, STATUS);
  }

  @Test
  public void saveTest() {
    productImageQcBacklogService.saveProductImageQcBacklog(new ProductImageQcBacklog());
    Mockito.verify(productImageQcBacklogRepository).save(new ProductImageQcBacklog());
  }
}