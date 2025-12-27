package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;

public class ProductAttributeServiceTest {

  private static final String ID = "id";
  private static final String PRODUCT_ATT_NAME = "PRODUCT_ATT_NAME";
  private static final String PRODUCT_ATT_NAME2 = "PRODUCT_ATT_NAME2";
  private static final VerificationMode AT_LEAST_ONCE = Mockito.times(1);
  private static final String STORE_ID = "10001";

  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";

  @Mock
  private ProductAttributeRepository repository;

  @InjectMocks
  private ProductAttributeServiceBean service;

  @Mock
  private ProductAttributeValueServiceBean productAttributeValueService;

  @Mock
  private AttributeService attributeService;

  @Mock
  private AllowedAttributeValueService allowedAttributeValueService;

  @Mock
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Captor
  private ArgumentCaptor<List<String>> stringListArgumentCaptor;

  private Attribute attribute1;
  private Attribute attribute2;
  private Attribute attribute3;
  private ProductAttribute productAttribute1;
  private ProductAttribute productAttribute2;
  private ProductAttribute productAttribute3;
  private List<ProductAttributeValue> productAttributeValues;
  private AllowedAttributeValue allowedAttributeValue;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue;
  private static final String PRODUCT_ATTRIBUTE_ID_1 = "productAttributeId1";
  private static final String PRODUCT_ATTRIBUTE_ID_2 = "productAttributeId2";
  private static final String PRODUCT_ATTRIBUTE_ID_3 = "productAttributeId3";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String ATTRIBUTE_ID_3 = "attributeId3";
  private static final String ALLOWED_ATTRIBUTE_VALUE_ID = "allowedAttributeValueId";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID = "predefinedAllowedAttributeId";

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    attribute1 = new Attribute();
    attribute1.setId(ID);
    attribute1.setName(PRODUCT_ATT_NAME);
    attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    this.productAttribute1 = new ProductAttribute(attribute1, new Product(),
        ProductAttributeServiceTest.PRODUCT_ATT_NAME, false, Integer.valueOf(1), ProductAttributeServiceTest.STORE_ID);
    productAttribute1.setId(PRODUCT_ATTRIBUTE_ID_1);
    productAttribute1.setAttributeId(ID);
    productAttribute2 = new ProductAttribute();
    productAttribute2.setId(PRODUCT_ATTRIBUTE_ID_2);
    productAttribute2.setAttributeId(ATTRIBUTE_ID_2);
    productAttribute3 = new ProductAttribute();
    productAttribute3.setId(PRODUCT_ATTRIBUTE_ID_3);
    productAttribute3.setAttributeId(ATTRIBUTE_ID_3);
    productAttributeValues = new ArrayList<>();
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_1);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_2);
    productAttributeValue2.setAllowedAttributeValueId(ALLOWED_ATTRIBUTE_VALUE_ID);
    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    productAttributeValue3.setProductAttributeId(PRODUCT_ATTRIBUTE_ID_3);
    productAttributeValue3.setPredefinedAllowedAttributeValueId(PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID);
    productAttributeValues.add(productAttributeValue1);
    productAttributeValues.add(productAttributeValue2);
    productAttributeValues.add(productAttributeValue3);
    allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId(ALLOWED_ATTRIBUTE_VALUE_ID);
    predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setId(PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID);
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.repository);
    verifyNoMoreInteractions(this.productAttributeValueService);
    verifyNoMoreInteractions(attributeService);
    verifyNoMoreInteractions(allowedAttributeValueService);
    verifyNoMoreInteractions(predefinedAllowedAttributeValueService);
  }

  @Test
  public void testFindById() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    this.productAttribute1.setId(id);
    when(this.repository.findById(id)).thenReturn(Optional.of(this.productAttribute1));
    ProductAttribute savedProductAttribute = this.service.findById(id);
    Assertions.assertEquals(savedProductAttribute, (this.productAttribute1));
    verify(this.repository, Mockito.times(1)).findById(id);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    this.productAttribute1.setId(id);
    List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    productAttributes.add(this.productAttribute1);
    productAttributes.add(new ProductAttribute(new Attribute(), new Product(),
        ProductAttributeServiceTest.PRODUCT_ATT_NAME2, false, Integer.valueOf(2), ProductAttributeServiceTest.STORE_ID));

    Page<ProductAttribute> productAttributePage = new PageImpl<ProductAttribute>(productAttributes,
        ProductAttributeServiceTest.DEFAULT_PAGE_REQUEST, productAttributes.size());
    when(this.repository.findByStoreIdAndMarkForDeleteFalse(ProductAttributeServiceTest.STORE_ID,
        ProductAttributeServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(productAttributePage);
    assertTrue(this.service
        .findByStoreId(ProductAttributeServiceTest.STORE_ID, ProductAttributeServiceTest.DEFAULT_PAGE_REQUEST)
        .getTotalElements() == productAttributePage.getTotalElements());
    verify(this.repository, Mockito.times(1)).findByStoreIdAndMarkForDeleteFalse(ProductAttributeServiceTest.STORE_ID,
        ProductAttributeServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindByStoreIdAndId() {
    ProductAttribute savedProductAttribute = new ProductAttribute();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttribute1.setId(uuid);
    BeanUtils.copyProperties(this.productAttribute1, savedProductAttribute);

    when(this.repository.findByIdAndMarkForDeleteFalse(ProductAttributeServiceTest.ID))
        .thenReturn(savedProductAttribute);

    Assertions.assertEquals(this.productAttribute1, (
        this.service.findByStoreIdAndId(ProductAttributeServiceTest.STORE_ID, ProductAttributeServiceTest.ID)));

    verify(this.repository).findByIdAndMarkForDeleteFalse(ProductAttributeServiceTest.ID);
  }

  @Test
  public void testSaveProductAttributeSuccessfully() throws Exception {
    productAttribute1.setId(null);
    ProductAttribute savedProductAttribute = new ProductAttribute();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(this.productAttribute1, savedProductAttribute);
    savedProductAttribute.setId(uuid);
    when(this.repository.saveAndFlush(this.productAttribute1)).thenReturn(savedProductAttribute);
    Assertions.assertEquals(this.service.save(this.productAttribute1), (uuid));
    verify(this.repository, ProductAttributeServiceTest.AT_LEAST_ONCE).saveAndFlush(this.productAttribute1);
  }

  @Test
  public void testSaveProductAttributeWithEmptyId() {
    ProductAttribute savedProductAttribute = new ProductAttribute();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttribute1.setId(uuid);
    BeanUtils.copyProperties(this.productAttribute1, savedProductAttribute);
    when(this.repository.findById(this.productAttribute1.getId())).thenReturn(Optional.of(savedProductAttribute));
    try {
      this.service.save(this.productAttribute1);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductAttributeServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, ProductAttributeServiceTest.AT_LEAST_ONCE).findById(this.productAttribute1.getId());
    }

  }

  @Test
  public void testUpdateProductAttributeNonExistenceEntity() {
    ProductAttribute savedProductAttribute = new ProductAttribute();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttribute1.setId(uuid);
    BeanUtils.copyProperties(this.productAttribute1, savedProductAttribute);
    when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(this.productAttribute1);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductAttributeServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, Mockito.times(1)).findById(this.productAttribute1.getId());
    }
  }

  @Test
  public void testUpdateProductAttributeSuccessfully() throws Exception {
    ProductAttribute savedProductAttribute = new ProductAttribute();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttribute1.setId(uuid);
    BeanUtils.copyProperties(this.productAttribute1, savedProductAttribute);
    when(this.repository.findById(this.productAttribute1.getId())).thenReturn(Optional.of(savedProductAttribute));
    this.service.update(this.productAttribute1);
    assertTrue(true);
    verify(this.repository, Mockito.times(1)).findById(this.productAttribute1.getId());
    verify(this.repository, ProductAttributeServiceTest.AT_LEAST_ONCE).saveAndFlush(this.productAttribute1);

  }

  @Test
  public void testUpdateProductAttributeWithEmptyId() {
    productAttribute1.setId(null);
    ProductAttribute savedProductAttribute = new ProductAttribute();
    BeanUtils.copyProperties(this.productAttribute1, savedProductAttribute);
    try {
      this.service.update(this.productAttribute1);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductAttributeServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }

  @Test
  public void getProductAttributesByStoreIdAndProductIdCachedTest() {
    Mockito.when(repository.findByStoreIdAndProductId(STORE_ID, ID))
        .thenReturn(Arrays.asList(productAttribute1, productAttribute2, productAttribute3));
    Mockito.when(attributeService.getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.anyList())).thenReturn(Arrays.asList(attribute1, attribute2, attribute3));
    Set<String> productAttributeIds = new HashSet<>();
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_1);
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_2);
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_3);
    Mockito.when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        STORE_ID, productAttributeIds)).thenReturn(productAttributeValues);
    Set<String> allowedAttributeIds = new HashSet<>();
    allowedAttributeIds.add(ALLOWED_ATTRIBUTE_VALUE_ID);
    Mockito.when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndIds(STORE_ID, allowedAttributeIds))
        .thenReturn(Collections.singletonList(allowedAttributeValue));
    Set<String> predefinedAllowedAttributeIds = new HashSet<>();
    predefinedAllowedAttributeIds.add(PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID);
    Mockito.when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, predefinedAllowedAttributeIds)).thenReturn(Collections.singletonList(predefinedAllowedAttributeValue));
    List<ProductAttribute> response =
        service.getProductAttributesByStoreIdAndProductIdCached(STORE_ID, ID);
    Mockito.verify(repository).findByStoreIdAndProductId(STORE_ID, ID);
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), stringListArgumentCaptor.capture());
    Mockito.verify(productAttributeValueService)
        .getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(STORE_ID, productAttributeIds);
    Mockito.verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, allowedAttributeIds);
    Mockito.verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, predefinedAllowedAttributeIds);
    assertFalse(response.get(0).isOwnByProductItem());
    Assertions.assertEquals(ID, response.get(0).getAttribute().getId());
    Assertions.assertEquals(PRODUCT_ATT_NAME, response.get(0).getAttribute().getName());
    Assertions.assertEquals(3, stringListArgumentCaptor.getValue().size());
    Assertions.assertTrue(stringListArgumentCaptor.getValue().contains(ID));
    Assertions.assertTrue(stringListArgumentCaptor.getValue().contains(ATTRIBUTE_ID_2));
    Assertions.assertTrue(stringListArgumentCaptor.getValue().contains(ATTRIBUTE_ID_3));
  }

  @Test
  public void updateExtractedValueInProductAttributeTest(){
    Mockito.when(repository.findByStoreIdAndProductId(STORE_ID, ID))
      .thenReturn(Arrays.asList(productAttribute1, productAttribute2, productAttribute3));
    Mockito.when(attributeService.getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
      Mockito.eq(STORE_ID), Mockito.anyList())).thenReturn(Arrays.asList(attribute1, attribute2, attribute3));
    Set<String> productAttributeIds = new HashSet<>();
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_1);
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_2);
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_3);
    Mockito.when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
      STORE_ID, productAttributeIds)).thenReturn(productAttributeValues);
    Set<String> allowedAttributeIds = new HashSet<>();
    allowedAttributeIds.add(ALLOWED_ATTRIBUTE_VALUE_ID);
    Mockito.when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndIds(STORE_ID, allowedAttributeIds))
      .thenReturn(Collections.singletonList(allowedAttributeValue));
    Set<String> predefinedAllowedAttributeIds = new HashSet<>();
    predefinedAllowedAttributeIds.add(PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID);
    Mockito.when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndIds(
      STORE_ID, predefinedAllowedAttributeIds)).thenReturn(Collections.singletonList(predefinedAllowedAttributeValue));
    service.updateExtractedValueInProductAttribute(STORE_ID, ID);
    Mockito.verify(repository).findByStoreIdAndProductId(STORE_ID, ID);
    Mockito.verify(attributeService).getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
      Mockito.eq(STORE_ID), stringListArgumentCaptor.capture());
    Mockito.verify(productAttributeValueService)
      .getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(STORE_ID, productAttributeIds);
    Mockito.verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndIds(
      STORE_ID, allowedAttributeIds);
    Mockito.verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndIds(
      STORE_ID, predefinedAllowedAttributeIds);
    Mockito.verify(repository).saveAll(Arrays.asList(productAttribute1,productAttribute2,
      productAttribute3));
  }

  @Test
  public void getProductAttributeValuesTest() {
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    productAttributeMap.put(productAttribute1.getId(), productAttribute1);
    productAttributeMap.put(productAttribute2.getId(), productAttribute2);
    productAttributeMap.put(productAttribute3.getId(), productAttribute3);
    Set<String> productAttributeIds = new HashSet<>();
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_1);
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_2);
    productAttributeIds.add(PRODUCT_ATTRIBUTE_ID_3);
    Mockito.when(productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
        STORE_ID, productAttributeIds)).thenReturn(productAttributeValues);
    Set<String> allowedAttributeIds = new HashSet<>();
    allowedAttributeIds.add(ALLOWED_ATTRIBUTE_VALUE_ID);
    Mockito.when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndIds(STORE_ID, allowedAttributeIds))
        .thenReturn(Collections.singletonList(allowedAttributeValue));
    Set<String> predefinedAllowedAttributeIds = new HashSet<>();
    predefinedAllowedAttributeIds.add(PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID);
    Mockito.when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, predefinedAllowedAttributeIds)).thenReturn(Collections.singletonList(predefinedAllowedAttributeValue));
    service.getProductAttributeValues(STORE_ID, productAttributeMap);
    Mockito.verify(productAttributeValueService)
        .getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(STORE_ID, productAttributeIds);
    Mockito.verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, allowedAttributeIds);
    Mockito.verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, predefinedAllowedAttributeIds);
  }

  @Test
  public void saveMissedProductAttributesTest() {
    ProductAttribute productAttribute = new ProductAttribute();
    Mockito.when(repository.saveAll(List.of(productAttribute)))
        .thenReturn(List.of(productAttribute));
    service.saveMissedProductAttributes(List.of(productAttribute));
    Mockito.verify(repository).saveAll(List.of(productAttribute));
  }

  @Test
  void testAttributeDeletionWithIds() {
    List<String> productAttributeIds = List.of("att1", "attr2");
    service.deleteByProductAttributeIds(productAttributeIds);
    Mockito.verify(repository, times(1)).deleteAllById(productAttributeIds);
  }
}
