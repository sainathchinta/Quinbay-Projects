package com.gdn.x.product.dao.impl;

import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.mongodb.core.MongoTemplate;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class ProductRepositoryImplTest {

  private static final String READ_PREFERENCE = "SECONDARY_PREFERRED";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String STORE_ID = "storeId";
  private static final String USER_NAME = "username";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String[] fields = new String[] {"productSku", "markForDelete"};
  private static final HashSet productSkuSet = new HashSet(Arrays.asList(PRODUCT_SKU));

  @InjectMocks
  private ProductRepositoryImpl productRepositoryImpl;

  @Mock
  private MongoTemplate mongoTemplate;

  @Mock
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.when(mandatoryParameterHelper.getReadPreference()).thenReturn(READ_PREFERENCE);
    Mockito.when(mongoTemplateFactory.get(READ_PREFERENCE)).thenReturn(mongoTemplate);
  }

  @Test
  public void updatePickupPointCodesTest() {
    Product product = productRepositoryImpl.updatePickupPointCodes(STORE_ID, USER_NAME, PRODUCT_SKU,
      Collections.singleton(PICKUP_POINT_CODE), true);
    Assertions.assertNull(product);
  }

  @Test
  public void updateFbbFlagAtProductTest() {
    Product product = productRepositoryImpl.updateFbbFlagAtProduct(STORE_ID, PRODUCT_SKU, true);
    Assertions.assertNull(product);
  }

  @Test
  public void findByStoreIdAndProductSkusTest() {
    List<Product> productList =
        productRepositoryImpl.findByStoreIdAndProductSkus(STORE_ID, productSkuSet, fields, true);
    Assertions.assertNotNull(productList);
  }

  @Test
  public void findByStoreIdAndProductSkusWithShowdeletedFalseTest() {
    List<Product> productList =
        productRepositoryImpl.findByStoreIdAndProductSkus(STORE_ID, productSkuSet, fields, false);
    Assertions.assertNotNull(productList);
  }

  @Test
  public void updateCncActivatedByProductSkusMarkForDeleteFalseTest() {
    List<Product> productList =
        productRepositoryImpl.updateCncActivatedByProductSkusMarkForDeleteFalse(STORE_ID, productSkuSet, false, USER_NAME);
    Assertions.assertNotNull(productList);
  }
}
