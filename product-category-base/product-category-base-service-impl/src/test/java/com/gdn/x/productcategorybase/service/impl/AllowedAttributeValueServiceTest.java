package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
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
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.repository.AllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;

public class AllowedAttributeValueServiceTest {
  private static final String VALUE_BIRU = "Biru";
  private static final String VALUE_MERAH = "Merah";
  private static final String ID_1 = "ID-1";
  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;
  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final String ID = "ID";
  private static final String STORE_ID = "STORE_ID";
  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";
  private static final String ATTR_CODE_1 = "ATT-001";
  private static final String ATTR_CODE_2 = "ATT-002";
  private static final String BRAND_1 = "Samsung";
  private static final String BRAND_2 = "Oppo";
  private static final String PREDEFINED_TYPE = AttributeType.PREDEFINED_ATTRIBUTE.toString();
  private static final String DEFINING_TYPE = AttributeType.DEFINING_ATTRIBUTE.toString();
  private static final String VALUE = "value";
  private static final int SEQUENCE = 1;
  private static final String ATTRIBUTE_CODE = "ATT";
  private static final String ALLOWED_ATTRIBUTE_CODE = "ATT-01";
  private static final String KEYWORD = "keyword";
  private static final String OPTION_VALUE = "OPT_VAL";
  private static final String OPTION_VALUE_V2 = "OPT_VAL_V2";
  private static final String OPTION_CODE = "OPT_CODE";
  private static final String OPTION_CODE_V2 = "OPT_CODE_V2";

  @InjectMocks
  AllowedAttributeValueServiceBean service;

  @Mock
  AllowedAttributeValueRepository repository;
  @Mock
  private PredefinedAllowedAttributeValueRepository predefAllowedRepo;

  private AllowedAttributeValue allowedAttributeValueMerah;
  private Pageable pageable;
  private Page<AllowedAttributeValue> allowedAttributeValuePage;
  private AllowedAttributeValue allowedAttributeValueBiru;
  private Attribute attribute;

  @BeforeEach
  public void setUp() {
    this.pageable = PageRequest.of(AllowedAttributeValueServiceTest.PAGE_NUMBER,
        AllowedAttributeValueServiceTest.PAGE_SIZE);
    MockitoAnnotations.initMocks(this);
    new ProductAttribute();
    this.attribute =
        new Attribute("Warna", AttributeType.DEFINING_ATTRIBUTE, true, AllowedAttributeValueServiceTest.STORE_ID);
    this.allowedAttributeValueMerah = new AllowedAttributeValue(this.attribute,
        AllowedAttributeValueServiceTest.VALUE_MERAH, AllowedAttributeValueServiceTest.STORE_ID, 1);
    this.allowedAttributeValueBiru = new AllowedAttributeValue(this.attribute,
        AllowedAttributeValueServiceTest.VALUE_BIRU, AllowedAttributeValueServiceTest.STORE_ID, 2);
    this.allowedAttributeValueMerah.setId(AllowedAttributeValueServiceTest.ID);
    this.allowedAttributeValueBiru.setId(AllowedAttributeValueServiceTest.ID_1);
    List<AllowedAttributeValue> productCategories = new ArrayList<AllowedAttributeValue>();
    productCategories.add(this.allowedAttributeValueMerah);
    productCategories.add(this.allowedAttributeValueBiru);
    this.allowedAttributeValuePage = new PageImpl<AllowedAttributeValue>(productCategories,
        AllowedAttributeValueServiceTest.DEFAULT_PAGE_REQUEST, productCategories.size());
    ReflectionTestUtils.setField(service, "sizeChartValueTypeDelimiter", "-");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.repository);
  }
  
  private List<AllowedAttributeValueDtoRequest> buildAllowedValueRequest() {
    List<AllowedAttributeValueDtoRequest> result = new ArrayList<>();
    AllowedAttributeValueDtoRequest req1 = new AllowedAttributeValueDtoRequest();
    req1.setAttributeCode(ATTR_CODE_1);
    req1.setAttributeType(PREDEFINED_TYPE);
    req1.setValues(Arrays.asList(BRAND_1, BRAND_2));
    result.add(req1);
    
    AllowedAttributeValueDtoRequest req2 = new AllowedAttributeValueDtoRequest();
    req2.setAttributeCode(ATTR_CODE_2);
    req2.setAttributeType(DEFINING_TYPE);
    req2.setValues(Arrays.asList(VALUE_BIRU, VALUE_MERAH));
    result.add(req2);
    return result;
  }
  
  private List<PredefinedAllowedAttributeValue> buildPredefinedAllowedListResponse(){
    Attribute attr = new Attribute();
    attr.setAttributeCode(ATTR_CODE_1);
    
    List<PredefinedAllowedAttributeValue> response = new ArrayList<>();
    PredefinedAllowedAttributeValue resp1 = new PredefinedAllowedAttributeValue();
    resp1.setAttribute(attr);
    resp1.setId(UUID.randomUUID().toString());
    resp1.setValue(BRAND_1);
    response.add(resp1);
    
    PredefinedAllowedAttributeValue resp2 = new PredefinedAllowedAttributeValue();
    resp2.setAttribute(attr);
    resp2.setId(UUID.randomUUID().toString());
    resp2.setValue(BRAND_2);
    response.add(resp2);
    return response;
  }
  
  private List<AllowedAttributeValue> buildDefiningAllowedListResponse() {
    Attribute attr = new Attribute();
    attr.setAttributeCode(ATTR_CODE_2);
    List<AllowedAttributeValue> response = new ArrayList<>();

    AllowedAttributeValue resp1 = new AllowedAttributeValue();
    resp1.setAttribute(attr);
    resp1.setId(UUID.randomUUID().toString());
    resp1.setValue(VALUE_BIRU);
    response.add(resp1);
    
    AllowedAttributeValue resp2 = new AllowedAttributeValue();
    resp2.setAttribute(attr);
    resp2.setId(UUID.randomUUID().toString());
    resp2.setValue(VALUE_MERAH);
    response.add(resp2);
    return response;
  }
  
  @Test
  public void findAllowedPredefiningAndDefiningAttributeValue_HappyFlow_Success() throws Exception {
    List<AllowedAttributeValue> allowedAttributeValues = this.buildDefiningAllowedListResponse();
    allowedAttributeValues.getFirst().getAttribute().setSizeAttribute(true);
    Mockito.when(predefAllowedRepo.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList()))
      .thenReturn(this.buildPredefinedAllowedListResponse());
    Mockito.when(repository.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList()))
      .thenReturn(allowedAttributeValues);
    
    List<AllowedAttributeValueDtoResponse> response = 
        service.findAllowedPredefiningAndDefiningAttributeValue(this.buildAllowedValueRequest());
    
    assertEquals(2, response.size());
    
    Mockito.verify(predefAllowedRepo).findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(repository).findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void findAllowedPredefiningAndDefiningAttributeValueSizeChartTest() throws Exception {
    Mockito.when(predefAllowedRepo.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList()))
        .thenReturn(this.buildPredefinedAllowedListResponse());
    Mockito.when(repository.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList()))
        .thenReturn(this.buildDefiningAllowedListResponse());

    List<AllowedAttributeValueDtoResponse> response =
        service.findAllowedPredefiningAndDefiningAttributeValue(this.buildAllowedValueRequest());

    assertEquals(2, response.size());

    Mockito.verify(predefAllowedRepo).findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(repository).findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList());
  }
  
  @Test
  public void findAllowedPredefiningAndDefiningAttributeValue_NotFoundAllowedValue_ReturningEmptyResponse() throws Exception {
    Mockito.when(predefAllowedRepo.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList()))
      .thenReturn(new ArrayList<>());
    Mockito.when(repository.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList()))
      .thenReturn(new ArrayList<>());
    
    List<AllowedAttributeValueDtoResponse> response = 
        service.findAllowedPredefiningAndDefiningAttributeValue(this.buildAllowedValueRequest());
    
    assertEquals(0, response.size());
    
    Mockito.verify(predefAllowedRepo).findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(repository).findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void testDeleteAllowedAttributeValue() throws Exception {
    this.service.delete(null);
  }

  @Test
  public void testFindById() throws Exception {
    Mockito.when(this.repository.findById(AllowedAttributeValueServiceTest.ID))
        .thenReturn(Optional.of(this.allowedAttributeValueMerah));
    AllowedAttributeValue savedAllowedAttributeValue = this.service.findById(AllowedAttributeValueServiceTest.ID);
    Assertions.assertEquals(savedAllowedAttributeValue, (this.allowedAttributeValueMerah));
    Mockito.verify(this.repository, Mockito.times(1)).findById(AllowedAttributeValueServiceTest.ID);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    Mockito.when(
        this.repository.findByStoreIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID, this.pageable))
        .thenReturn(this.allowedAttributeValuePage);
    Assertions.assertEquals(
        this.service.findByStoreId(AllowedAttributeValueServiceTest.STORE_ID, this.pageable).getNumberOfElements(),
        (2));
    Mockito.verify(this.repository).findByStoreIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
        this.pageable);
  }

  @Test
  public void testFindByStoreIdAndAttributeAndValueAndMarkForDeleteFalse() {
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    Mockito
        .when(this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
            AllowedAttributeValueServiceTest.STORE_ID, this.attribute, AllowedAttributeValueServiceTest.VALUE_MERAH))
        .thenReturn(savedAllowedAttributeValue);

    Assertions.assertEquals(
        this.service.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
            this.attribute, AllowedAttributeValueServiceTest.VALUE_MERAH),
        (this.allowedAttributeValueMerah));
    Mockito.verify(this.repository).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        AllowedAttributeValueServiceTest.STORE_ID, this.attribute, AllowedAttributeValueServiceTest.VALUE_MERAH);
  }

  @Test
  public void testFindByStoreIdAndAttributeAndValueAndMarkForDeleteTrue() {
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    Mockito
        .when(this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(
            AllowedAttributeValueServiceTest.STORE_ID, this.attribute, AllowedAttributeValueServiceTest.VALUE_MERAH))
        .thenReturn(savedAllowedAttributeValue);

    Assertions.assertEquals(
        this.service.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(AllowedAttributeValueServiceTest.STORE_ID,
            this.attribute, AllowedAttributeValueServiceTest.VALUE_MERAH),
        (this.allowedAttributeValueMerah));
    Mockito.verify(this.repository).findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(
        AllowedAttributeValueServiceTest.STORE_ID, this.attribute, AllowedAttributeValueServiceTest.VALUE_MERAH);
  }

  @Test
  public void testFindByStoreIdAndId() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
        AllowedAttributeValueServiceTest.ID)).thenReturn(this.allowedAttributeValueMerah);
    AllowedAttributeValue savedAllowedAttributeValue =
        this.service.findByStoreIdAndId(AllowedAttributeValueServiceTest.STORE_ID, AllowedAttributeValueServiceTest.ID);
    Assertions.assertEquals(savedAllowedAttributeValue, (this.allowedAttributeValueMerah));
    Mockito.verify(this.repository, Mockito.times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(
        AllowedAttributeValueServiceTest.STORE_ID, AllowedAttributeValueServiceTest.ID);
  }

  @Test
  public void testGetSequence() throws Exception {
    Mockito.when(this.repository.getSequenceByAttributeCode(null)).thenReturn(1L);
    Assertions.assertEquals(this.service.getSequence(null), (StringUtils.leftPad("1", 5, "0")));
    Mockito.verify(this.repository).getSequenceByAttributeCode(null);
  }

  @Test
  public void testMarkForDeleteSingle() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
        AllowedAttributeValueServiceTest.ID)).thenReturn(this.allowedAttributeValueMerah);
    Mockito.when(this.repository.findById(AllowedAttributeValueServiceTest.ID))
        .thenReturn(Optional.of(this.allowedAttributeValueMerah));
    this.service.markForDeleteAllowedAttributeValue(AllowedAttributeValueServiceTest.STORE_ID,
        AllowedAttributeValueServiceTest.ID);
    assertTrue(this.allowedAttributeValueMerah.isMarkForDelete());

    Mockito.verify(this.repository).findById(AllowedAttributeValueServiceTest.ID);
    Mockito.verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
        AllowedAttributeValueServiceTest.ID);
    Mockito.verify(this.repository).saveAndFlush(this.allowedAttributeValueMerah);
  }

  @Test
  public void testMarkForDeleteSingleWithEmptyId() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
        AllowedAttributeValueServiceTest.ID)).thenReturn(null);

    try {
      this.service.markForDeleteAllowedAttributeValue(AllowedAttributeValueServiceTest.STORE_ID,
          AllowedAttributeValueServiceTest.ID);
    } catch (Exception e) {
      assertTrue(
          (e instanceof ApplicationException) && e.getMessage().contains("Can not perform delete on un exist data"));
      Mockito.verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AllowedAttributeValueServiceTest.STORE_ID,
          AllowedAttributeValueServiceTest.ID);
    }
  }

  @Test
  public void testSaveAllowedAttributeValueSuccessfully() throws Exception {
    this.allowedAttributeValueMerah.setId(null);
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    savedAllowedAttributeValue.setId(uuid);
    Mockito.when(this.repository.saveAndFlush(this.allowedAttributeValueMerah)).thenReturn(savedAllowedAttributeValue);
    Assertions.assertEquals(this.service.save(this.allowedAttributeValueMerah), (uuid));
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.allowedAttributeValueMerah);
  }

  @Test
  public void testSaveAllowedAttributeValueWithEmptyId() {
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.allowedAttributeValueMerah.setId(uuid);
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    Mockito.when(this.repository.findById(this.allowedAttributeValueMerah.getId()))
        .thenReturn(Optional.of(savedAllowedAttributeValue));
    try {
      this.service.save(this.allowedAttributeValueMerah);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(AllowedAttributeValueServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      Mockito.verify(this.repository, Mockito.times(1)).findById(this.allowedAttributeValueMerah.getId());
    }

  }

  @Test
  public void testUpdateAllowedAttributeValueNonExistenceEntity() {
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.allowedAttributeValueMerah.setId(uuid);
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    Mockito.when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(this.allowedAttributeValueMerah);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(AllowedAttributeValueServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      Mockito.verify(this.repository, Mockito.times(1)).findById(this.allowedAttributeValueMerah.getId());
    }
  }

  @Test
  public void testUpdateAllowedAttributeValueSuccessfully() throws Exception {
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.allowedAttributeValueMerah.setId(uuid);
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    Mockito.when(this.repository.findById(this.allowedAttributeValueMerah.getId()))
        .thenReturn(Optional.of(savedAllowedAttributeValue));
    this.service.update(this.allowedAttributeValueMerah);
    assertTrue(true);
    Mockito.verify(this.repository, Mockito.times(1)).findById(this.allowedAttributeValueMerah.getId());
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.allowedAttributeValueMerah);

  }

  @Test
  public void testUpdateAllowedAttributeValueWithEmptyId() {
    this.allowedAttributeValueMerah.setId(null);
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    BeanUtils.copyProperties(this.allowedAttributeValueMerah, savedAllowedAttributeValue);
    try {
      this.service.update(this.allowedAttributeValueMerah);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(AllowedAttributeValueServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }

  @Test
  public void findByStoreIdAndIds() throws Exception {
    Set<String> ids = new HashSet<>();
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<>();
    allowedAttributeValues.add(new AllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    Mockito.when(this.repository.findByStoreIdAndIdInAndMarkForDeleteFalse(STORE_ID, ids))
        .thenReturn(allowedAttributeValues);
    List<AllowedAttributeValue> result = this.service.findByStoreIdAndIds(STORE_ID, ids);
    Mockito.verify(this.repository).findByStoreIdAndIdInAndMarkForDeleteFalse(STORE_ID, ids);
    assertEquals(result, allowedAttributeValues);

  }

  @Test
  public void findByStoreIdAndAttributeIdForManualSortingTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST)).thenReturn(allowedAttributeValuePage);
    Page<AllowedAttributeValue> allowedAttributeValues = this.service
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, AttributeSortType.MANUAL);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST);
    assertEquals(allowedAttributeValues, allowedAttributeValuePage);
  }

  @Test
  public void findByStoreIdAndAttributeIdForCustomSortingTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST)).thenReturn(allowedAttributeValuePage);
    Page<AllowedAttributeValue> allowedAttributeValues = this.service
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, AttributeSortType.CUSTOM);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST);
    assertEquals(allowedAttributeValues, allowedAttributeValuePage);
  }

  @Test
  public void findByStoreIdAndAttributeIdForAlphabeticalWiseSortingTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST))
        .thenReturn(allowedAttributeValuePage);
    Page<AllowedAttributeValue> allowedAttributeValues = this.service
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, AttributeSortType.ALPHABETICAL);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST);
    assertEquals(allowedAttributeValues, allowedAttributeValuePage);
  }

  @Test
  public void findByStoreIdAndAttributeAndValueTest() {
    Mockito.when(repository.findByStoreIdAndAttributeAndValue(STORE_ID, attribute,
        VALUE_MERAH)).thenReturn(Collections.singletonList(allowedAttributeValueMerah));
    service.findByStoreIdAndAttributeAndValue(STORE_ID, attribute, VALUE_MERAH);
    Mockito.verify(repository).findByStoreIdAndAttributeAndValue(STORE_ID, attribute,
        VALUE_MERAH);
  }
  
  @Test
  public void getAttributeOptionByAttributeCodeAndKeyword_HappyFlow_Success() throws Exception {
    Pageable pageable = PageRequest.of(PAGE_NUMBER, PAGE_SIZE);
    when(this.repository.getDefiningAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE,
        KEYWORD, pageable)).thenReturn(this.attributeOptionsBuilder());
    Page<AttributeOptionDTO>options = this.service.getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    verify(this.repository).getDefiningAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    assertTrue(options.getContent().get(0).getValue().equals(OPTION_VALUE));
    assertTrue(options.getContent().get(1).getValue().equals(OPTION_VALUE_V2));
  }

  @Test
  public void getAttributeOptionByAttributeCodeAndKeyword_EmptyAttributeOptions() throws Exception {
    Pageable pageable = PageRequest.of(PAGE_NUMBER, PAGE_SIZE);
    when(this.repository.getDefiningAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE_NUMBER, PAGE_SIZE), 2));
    when(this.repository.getPredefineAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable))
        .thenReturn(this.attributeOptionsBuilder());
    Page<AttributeOptionDTO> options =
        this.service.getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    verify(this.repository).getDefiningAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    verify(this.repository).getPredefineAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    assertEquals(OPTION_VALUE, options.getContent().get(0).getValue());
    assertEquals(OPTION_VALUE_V2, options.getContent().get(1).getValue());
  }
  
  @Test
  public void getAttributeOptionByAttributeCodeAndKeyword_Exception_Fail() throws Exception {
    Pageable pageable = PageRequest.of(PAGE_NUMBER, PAGE_SIZE);
    when(this.repository.getDefiningAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE,
        KEYWORD, pageable)).thenThrow(RuntimeException.class);
    Page<AttributeOptionDTO> options = new PageImpl<AttributeOptionDTO>(new ArrayList<AttributeOptionDTO>());
    try {
      options = this.service.getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    } catch (Exception e) {
      verify(this.repository).getDefiningAttributeOptionByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
    }
  }
  
  private Page<AttributeOptionDTO> attributeOptionsBuilder(){
    List<AttributeOptionDTO> options = new ArrayList<>();
    AttributeOptionDTO opt1 = new AttributeOptionDTO(OPTION_VALUE, OPTION_CODE);
    AttributeOptionDTO opt2 = new AttributeOptionDTO(OPTION_VALUE_V2, OPTION_CODE_V2);
    options.add(opt1);
    options.add(opt2);
    Page<AttributeOptionDTO> optionsPage = new PageImpl<AttributeOptionDTO>(options, PageRequest.of(PAGE_NUMBER, PAGE_SIZE), 2);
    return optionsPage;
  }

  @Test
  public void getAllowedAttributeValuesByStoreIdAndAttributeIdTest() {
    service.getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(repository).findByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
  }

  @Test
  public void findByStoreIdAndAllowedAttributeCodeTest() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndAllowedAttributeCode(AllowedAttributeValueServiceTest.STORE_ID,
        AllowedAttributeValueServiceTest.ALLOWED_ATTRIBUTE_CODE)).thenReturn(this.allowedAttributeValueMerah);
    AllowedAttributeValue savedAllowedAttributeValue = this.service
        .findByStoreIdAndAllowedAttributeCode(AllowedAttributeValueServiceTest.STORE_ID,
            AllowedAttributeValueServiceTest.ALLOWED_ATTRIBUTE_CODE);
    Assertions.assertEquals(savedAllowedAttributeValue, (this.allowedAttributeValueMerah));
    Mockito.verify(this.repository, Mockito.times(1))
        .findByStoreIdAndAllowedAttributeCode(AllowedAttributeValueServiceTest.STORE_ID,
            AllowedAttributeValueServiceTest.ALLOWED_ATTRIBUTE_CODE);
  }

  @Test
  public void findByStoreIdAndAttributeAndValueOrderByMarkForDeleteTest() throws Exception {
    Attribute attribute = new Attribute();
    Mockito.when(this.repository
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(AllowedAttributeValueServiceTest.STORE_ID, attribute,
            AllowedAttributeValueServiceTest.VALUE_BIRU)).thenReturn(Arrays.asList(this.allowedAttributeValueMerah));
    List<AllowedAttributeValue> savedAllowedAttributeValue = this.service
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(AllowedAttributeValueServiceTest.STORE_ID, attribute,
            AllowedAttributeValueServiceTest.VALUE_BIRU);
    Assertions.assertEquals(savedAllowedAttributeValue.get(0), (this.allowedAttributeValueMerah));
    Mockito.verify(this.repository, Mockito.times(1))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(AllowedAttributeValueServiceTest.STORE_ID, attribute,
            AllowedAttributeValueServiceTest.VALUE_BIRU);
  }

  @Test
  public void getAllowedAttributeValuesByStoreIdAndAttributeIdAndValueTest() {
    when(repository.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, ID, VALUE)).thenReturn(
        new AllowedAttributeValue());
    service.getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(STORE_ID, ID, VALUE);
    verify(repository).findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, ID, VALUE);
  }
}
