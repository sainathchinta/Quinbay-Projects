package com.gdn.x.productcategorybase.service.impl;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class ProductItemAttributeValueServiceTest {

  @Mock
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Mock
  private AttributeService attributeService;

  @InjectMocks
  private ProductItemAttributeValueServiceBean productItemAttributeValueServiceBean;

  private static final String STORE_ID = "10001";
  private static final String ITEM_ID = "itemId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String VALUE = "value";
  private static final String NEW_VALUE = "newValue";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String BRAND_NAME = "Samsung";

  private ProductItemAttributeValue productItemAttributeValue;
  private ProductItem productItem;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setId(ITEM_ID);
    productItemAttributeValue.setValue(VALUE);
    productItemAttributeValue.setAttributeId(ATTRIBUTE_ID);
    
    productItem = new ProductItem();
    productItem.setId(PRODUCT_ITEM_ID);
    productItem.setSkuCode("TEST-SKU-001");
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productItemAttributeRepository);
    verifyNoMoreInteractions(attributeService);
  }

  @Test
  public void getProductItemAttributeValuesByStoreIdAndProductItemIdsTest() {
    ReflectionTestUtils.setField(productItemAttributeValueServiceBean,"productItemAttributesPartitionSize",1);
    Mockito.when(productItemAttributeRepository.findByStoreIdAndProductItemIdIn(
        STORE_ID, Collections.singletonList(ITEM_ID))).thenReturn(Collections.singletonList(productItemAttributeValue));
    List<ProductItemAttributeValue> response =
        productItemAttributeValueServiceBean.getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(
            STORE_ID, Collections.singletonList(ITEM_ID));
    Mockito.verify(productItemAttributeRepository).findByStoreIdAndProductItemIdIn(
        STORE_ID, Collections.singletonList(ITEM_ID));
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
        STORE_ID, Collections.singletonList(ATTRIBUTE_ID));
    Assertions.assertEquals(productItemAttributeValue, response.get(0));
  }

  @Test
  void deleteByProductItemAttributeIdsTest() {
    List<String> productItemAttributeIds = List.of("itemAttribute1", "itemAttribute2");
    productItemAttributeValueServiceBean.deleteByProductItemAttributeIds(productItemAttributeIds);
    Mockito.verify(productItemAttributeRepository).deleteAllById(productItemAttributeIds);
  }

  // Test cases for updateOnlyValueForProductItemAttributeValuesByAttributeId method

  @Test
  void testUpdateOnlyValueForProductItemAttributeValuesByAttributeId_Success() {
    // Given
    ProductItemAttributeValue existingAttributeValue = new ProductItemAttributeValue();
    existingAttributeValue.setId(ITEM_ID);
    existingAttributeValue.setValue("Old Value");
    existingAttributeValue.setAttributeId(ATTRIBUTE_ID);
    
    when(productItemAttributeRepository.findByStoreIdAndProductItemIdAndAttributeId(
        STORE_ID, PRODUCT_ITEM_ID, ATTRIBUTE_ID))
        .thenReturn(existingAttributeValue);
    when(productItemAttributeRepository.save(existingAttributeValue))
        .thenReturn(existingAttributeValue);

    // When
    productItemAttributeValueServiceBean.updateOnlyValueForProductItemAttributeValuesByAttributeId(
        STORE_ID, ATTRIBUTE_ID, NEW_VALUE, productItem);

    // Then
    verify(productItemAttributeRepository).findByStoreIdAndProductItemIdAndAttributeId(
        STORE_ID, PRODUCT_ITEM_ID, ATTRIBUTE_ID);
    verify(productItemAttributeRepository).save(existingAttributeValue);
    Assertions.assertEquals(NEW_VALUE, existingAttributeValue.getValue());
  }

  @Test
  void testUpdateOnlyValueForProductItemAttributeValuesByAttributeId_nullObject() {
    when(productItemAttributeRepository.findByStoreIdAndProductItemIdAndAttributeId(
        STORE_ID, PRODUCT_ITEM_ID, ATTRIBUTE_ID))
        .thenReturn(null);

    productItemAttributeValueServiceBean.updateOnlyValueForProductItemAttributeValuesByAttributeId(
        STORE_ID, ATTRIBUTE_ID, NEW_VALUE, productItem);

    verify(productItemAttributeRepository).findByStoreIdAndProductItemIdAndAttributeId(
        STORE_ID, PRODUCT_ITEM_ID, ATTRIBUTE_ID);
  }
}