package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;

public class ProductAttributeValueServiceTest {
  private static final int PAGE_SIZE = 10;

  private static final int PAGE_NUMBER = 0;

  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);

  private static final String ID = "ID";

  private static final String STORE_ID = "STORE_ID";

  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";
  private static final String MERAH = "Merah";

  @InjectMocks
  ProductAttributeValueServiceBean service;

  @Mock
  ProductAttributeValueRepository repository;

  private ProductAttributeValue productAttributeValue;
  private Pageable pageable;
  private Page<ProductAttributeValue> productAttributeValuePage;
  private AllowedAttributeValue merah;

  @BeforeEach
  public void setUp() {
    this.pageable = PageRequest.of(ProductAttributeValueServiceTest.PAGE_NUMBER,
        ProductAttributeValueServiceTest.PAGE_SIZE);
    MockitoAnnotations.initMocks(this);
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute =
        new Attribute("Warna", AttributeType.DEFINING_ATTRIBUTE, true, ProductAttributeValueServiceTest.STORE_ID);
    this.merah = new AllowedAttributeValue(attribute, MERAH, ProductAttributeValueServiceTest.STORE_ID, 1);
    this.productAttributeValue =
        new ProductAttributeValue(productAttribute, this.merah, null, DescriptiveAttributeValueType.NONE);
    this.productAttributeValue.setId(ProductAttributeValueServiceTest.ID);
    List<ProductAttributeValue> productCategories = new ArrayList<>();
    productCategories.add(this.productAttributeValue);
    this.productAttributeValuePage =
        new PageImpl<>(productCategories, ProductAttributeValueServiceTest.DEFAULT_PAGE_REQUEST, productCategories.size());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.repository);
  }

  @Test
  public void testFindById() throws Exception {
    when(this.repository.findById(ProductAttributeValueServiceTest.ID)).thenReturn(Optional.of(this.productAttributeValue));
    ProductAttributeValue savedProductAttributeValue = this.service.findById(ProductAttributeValueServiceTest.ID);
    Assertions.assertEquals(savedProductAttributeValue, (this.productAttributeValue));
    verify(this.repository, Mockito.times(1)).findById(ProductAttributeValueServiceTest.ID);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    when(this.repository.findByStoreIdAndMarkForDeleteFalse(ProductAttributeValueServiceTest.STORE_ID, this.pageable))
        .thenReturn(this.productAttributeValuePage);
    Assertions.assertEquals(
        this.service.findByStoreId(ProductAttributeValueServiceTest.STORE_ID, this.pageable).getNumberOfElements(),
        (1));
    verify(this.repository).findByStoreIdAndMarkForDeleteFalse(ProductAttributeValueServiceTest.STORE_ID,
        this.pageable);
  }

  @Test
  public void testFindByStoreIdAndAllowedAttributeValue() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
    productAttributeValues.add(this.productAttributeValue);
    when(this.repository.findByStoreIdAndAllowedAttributeValue(ProductAttributeValueServiceTest.STORE_ID, this.merah))
        .thenReturn(productAttributeValues);
    assertTrue(this.service.findByStoreIdAndAllowedAttributeValue(ProductAttributeValueServiceTest.STORE_ID, this.merah)
        .size() == 1);
    verify(this.repository).findByStoreIdAndAllowedAttributeValue(ProductAttributeValueServiceTest.STORE_ID,
        this.merah);
  }

  @Test
  public void testFindByStoreIdAndId() throws Exception {
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductAttributeValueServiceTest.STORE_ID,
        ProductAttributeValueServiceTest.ID)).thenReturn(this.productAttributeValue);
    ProductAttributeValue savedProductAttributeValue =
        this.service.findByStoreIdAndId(ProductAttributeValueServiceTest.STORE_ID, ProductAttributeValueServiceTest.ID);
    Assertions.assertEquals(savedProductAttributeValue, (this.productAttributeValue));
    verify(this.repository, Mockito.times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(
        ProductAttributeValueServiceTest.STORE_ID, ProductAttributeValueServiceTest.ID);
  }

  @Test
  public void testSaveProductAttributeValueSuccessfully() throws Exception {
    this.productAttributeValue.setId(null);
    ProductAttributeValue savedProductAttributeValue = new ProductAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(this.productAttributeValue, savedProductAttributeValue);
    savedProductAttributeValue.setId(uuid);
    when(this.repository.saveAndFlush(this.productAttributeValue)).thenReturn(savedProductAttributeValue);
    Assertions.assertEquals(this.service.save(this.productAttributeValue), (uuid));
    verify(this.repository, Mockito.times(1)).saveAndFlush(this.productAttributeValue);
  }

  @Test
  public void testSaveProductAttributeValueWithEmptyId() {
    ProductAttributeValue savedProductAttributeValue = new ProductAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttributeValue.setId(uuid);
    BeanUtils.copyProperties(this.productAttributeValue, savedProductAttributeValue);
    when(this.repository.findById(this.productAttributeValue.getId())).thenReturn(Optional.of(savedProductAttributeValue));
    try {
      this.service.save(this.productAttributeValue);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductAttributeValueServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, Mockito.times(1)).findById(this.productAttributeValue.getId());
    }

  }

  @Test
  public void testUpdateProductAttributeValueNonExistenceEntity() {
    ProductAttributeValue savedProductAttributeValue = new ProductAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttributeValue.setId(uuid);
    BeanUtils.copyProperties(this.productAttributeValue, savedProductAttributeValue);
    when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(this.productAttributeValue);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductAttributeValueServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, Mockito.times(1)).findById(this.productAttributeValue.getId());
    }
  }

  @Test
  public void testUpdateProductAttributeValueSuccessfully() throws Exception {
    ProductAttributeValue savedProductAttributeValue = new ProductAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productAttributeValue.setId(uuid);
    BeanUtils.copyProperties(this.productAttributeValue, savedProductAttributeValue);
    when(this.repository.findById(this.productAttributeValue.getId())).thenReturn(Optional.of(savedProductAttributeValue));
    this.service.update(this.productAttributeValue);
    assertTrue(true);
    verify(this.repository, Mockito.times(1)).findById(this.productAttributeValue.getId());
    verify(this.repository, Mockito.times(1)).saveAndFlush(this.productAttributeValue);

  }

  @Test
  public void testUpdateProductAttributeValueWithEmptyId() {
    this.productAttributeValue.setId(null);
    ProductAttributeValue savedProductAttributeValue = new ProductAttributeValue();
    BeanUtils.copyProperties(this.productAttributeValue, savedProductAttributeValue);
    try {
      this.service.update(this.productAttributeValue);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductAttributeValueServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }
  
  @Test
  public void testDelete() throws Exception {
    this.service.delete(ID);
  }

  @Test
  public void getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCachedTest() {
    when(repository
        .findByStoreIdAndProductAttributeIdInAndMarkForDeleteFalse(STORE_ID, new HashSet<>(Arrays.asList(ID))))
        .thenReturn(Arrays.asList(productAttributeValue));
    service.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(STORE_ID,
        Collections.singleton(ID));
    verify(repository).findByStoreIdAndProductAttributeIdInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ID));
  }

  @Test
  public void getProductAttributeValuesByStoreIdAndProductAttributeIdsTest() {
    when(repository.findByStoreIdAndProductAttributeIdIn(STORE_ID, new HashSet<>(Arrays.asList(ID)))).thenReturn(
        Arrays.asList(productAttributeValue));
    service.getProductAttributeValuesByStoreIdAndProductAttributeIds(STORE_ID, Collections.singleton(ID));
    verify(repository).findByStoreIdAndProductAttributeIdIn(STORE_ID, Collections.singleton(ID));
  }

  @Test
  public void saveProductAttributeValuesTest() {
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    when(repository.saveAll(List.of(productAttributeValue))).thenReturn(
        List.of(productAttributeValue));
    service.saveProductAttributeValues(List.of(productAttributeValue));
    verify(repository).saveAll(List.of(productAttributeValue));
  }
}
