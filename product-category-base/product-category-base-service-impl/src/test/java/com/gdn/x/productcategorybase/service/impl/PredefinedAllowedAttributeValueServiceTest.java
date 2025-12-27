package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

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
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.brand.BrandService;

public class PredefinedAllowedAttributeValueServiceTest {
  private static final String VALUE_BIRU = "Biru";

  private static final String VALUE_MERAH = "Merah";

  private static final String ID_1 = "ID-1";

  private static final int PAGE_SIZE = 10;

  private static final int PAGE_NUMBER = 0;

  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);

  private static final String ID = "ID";

  private static final String STORE_ID = "10001";
  private static final String DEFAULT_BRAND_CODE = "BRAND_CODE";
  private static final String DEFAULT_BP_CODE = "BP_CODE";
  private static final String DEFAULT_USERNAME = "username";
  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "ATT-0000001-00001";
  private static final String VALUE = "Red";
  private static final String ATTRIBUTE_ID = "attribute_id";
  private static final String APPROVED = "APPROVED";
  private static final int SEQUENCE = 1;
  private static final Long DEFAULT_OPTLOCK = 0L;
  private BrandWip brandWip = new BrandWip();
  private static final String DEFAULT_BRAND_NAME = "BliBli";
  private Date date = new Date(1,1,1);
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String BRAND_NAME = "brandName";
  private static final String ATTRIBUTE_CODE = "COLOR";
  private static final String UPDATED_BY = "testUser";
  private static final String VALUE_EN = "Red";
  private static final String GENERATED_SEQUENCE = "00001";
  private static final String EXPECTED_CODE = ATTRIBUTE_CODE + Constants.HYPHEN + GENERATED_SEQUENCE;

  @InjectMocks
  private PredefinedAllowedAttributeValueServiceBean service;

  @Mock
  private PredefinedAllowedAttributeValueRepository repository;

  @Mock
  private BrandService brandService;

  @Mock
  private AttributeService attributeService;

  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValueMerah;
  private Pageable pageable;
  private Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValuePage;
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValueBiru;
  private Attribute attribute;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse;
  private Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponsePage;

  @BeforeEach
  public void setUp() {
    this.pageable = PageRequest.of(PredefinedAllowedAttributeValueServiceTest.PAGE_NUMBER,
        PredefinedAllowedAttributeValueServiceTest.PAGE_SIZE);
    MockitoAnnotations.initMocks(this);
    new ProductAttribute();
    this.attribute = new Attribute("Warna", AttributeType.DEFINING_ATTRIBUTE, true,
        PredefinedAllowedAttributeValueServiceTest.STORE_ID);
    this.predefinedAllowedAttributeValueMerah = new PredefinedAllowedAttributeValue(this.attribute,
        PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH, PredefinedAllowedAttributeValueServiceTest.STORE_ID, 1);
    this.predefinedAllowedAttributeValueBiru = new PredefinedAllowedAttributeValue(this.attribute,
        PredefinedAllowedAttributeValueServiceTest.VALUE_BIRU, PredefinedAllowedAttributeValueServiceTest.STORE_ID, 2);
    this.predefinedAllowedAttributeValueMerah.setId(PredefinedAllowedAttributeValueServiceTest.ID);
    this.predefinedAllowedAttributeValueBiru.setId(PredefinedAllowedAttributeValueServiceTest.ID_1);
    List<PredefinedAllowedAttributeValue> productCategories = new ArrayList<PredefinedAllowedAttributeValue>();
    productCategories.add(this.predefinedAllowedAttributeValueMerah);
    productCategories.add(this.predefinedAllowedAttributeValueBiru);
    this.predefinedAllowedAttributeValuePage = new PageImpl<>(productCategories,
        PredefinedAllowedAttributeValueServiceTest.DEFAULT_PAGE_REQUEST, productCategories.size());
    Mockito.when(
        this.repository.findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(this.predefinedAllowedAttributeValueBiru);
    predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueResponse.setValue(VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus(APPROVED);
    predefinedAllowedAttributeValueResponsePage = new PageImpl<>(Arrays.asList(predefinedAllowedAttributeValueResponse));
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    brandWip.setCreatedBy(DEFAULT_USERNAME);
    brandWip.setCreatedDate(date);
    brandWip.setMarkForDelete(false);
    brandWip.setStoreId(STORE_ID);
    brandWip.setUpdatedBy(DEFAULT_USERNAME);
    brandWip.setUpdatedDate(date);
    brandWip.setBrandRequestCode(DEFAULT_BRAND_CODE);
    brandWip.setVersion(DEFAULT_OPTLOCK);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.repository);
    Mockito.verifyNoMoreInteractions(attributeService);
  }

  @Test
  public void testFindById() throws Exception {
    Mockito.when(this.repository.findById(PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(Optional.of(this.predefinedAllowedAttributeValueMerah));
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue =
        this.service.findById(PredefinedAllowedAttributeValueServiceTest.ID);
    Assertions.assertEquals(savedPredefinedAllowedAttributeValue,
        (this.predefinedAllowedAttributeValueMerah));
    Mockito.verify(this.repository, Mockito.times(1)).findById(PredefinedAllowedAttributeValueServiceTest.ID);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndMarkForDeleteFalse(PredefinedAllowedAttributeValueServiceTest.STORE_ID,
        this.pageable)).thenReturn(this.predefinedAllowedAttributeValuePage);
    Assertions.assertEquals(this.service.findByStoreId(PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.pageable)
        .getNumberOfElements(), (2));
    Mockito.verify(this.repository)
        .findByStoreIdAndMarkForDeleteFalse(PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.pageable);
  }

  @Test
  public void testFindByStoreIdAndAttributeAndValueAndMarkForDeleteFalse() {
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedPredefinedAllowedAttributeValue);
    Mockito.when(this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.attribute,
        PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH)).thenReturn(savedPredefinedAllowedAttributeValue);

    Assertions.assertEquals(
        this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.attribute,
            PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH),
        (this.predefinedAllowedAttributeValueMerah));
    Mockito.verify(this.repository).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.attribute,
        PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH);
  }

  @Test
  public void testFindByStoreIdAndAttributeAndValueAndMarkForDeleteTrue() {
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedPredefinedAllowedAttributeValue);
    Mockito.when(this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.attribute,
        PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH)).thenReturn(savedPredefinedAllowedAttributeValue);

    Assertions.assertEquals(
        this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.attribute,
            PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH),
        (this.predefinedAllowedAttributeValueMerah));
    Mockito.verify(this.repository).findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, this.attribute,
        PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH);
  }

  @Test
  public void testFindByStoreIdAndAttributeIdAndMarkForDeleteFalse() {
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedPredefinedAllowedAttributeValue);
    Mockito
        .when(this.repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID,
            PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH,
            PredefinedAllowedAttributeValueServiceTest.DEFAULT_PAGE_REQUEST))
        .thenReturn(this.predefinedAllowedAttributeValuePage);

    Assertions.assertEquals(
        this.repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID,
            PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH,
            PredefinedAllowedAttributeValueServiceTest.DEFAULT_PAGE_REQUEST),
        (this.predefinedAllowedAttributeValuePage));
    Mockito.verify(this.repository).findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID,
        PredefinedAllowedAttributeValueServiceTest.VALUE_MERAH,
        PredefinedAllowedAttributeValueServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindByStoreIdAndId() throws Exception {
    Mockito
        .when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(this.predefinedAllowedAttributeValueMerah);
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = this.service.findByStoreIdAndId(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID);
    Assertions.assertEquals(savedPredefinedAllowedAttributeValue,
        (this.predefinedAllowedAttributeValueMerah));
    Mockito.verify(this.repository, Mockito.times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID);
  }

  @Test
  public void testMarkForDeleteList() throws Exception {
    Mockito
        .when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(this.predefinedAllowedAttributeValueMerah);
    Mockito
        .when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID_1))
        .thenReturn(this.predefinedAllowedAttributeValueBiru);
    Mockito.when(this.repository.findById(PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(Optional.of(this.predefinedAllowedAttributeValueMerah));
    Mockito.when(this.repository.findById(PredefinedAllowedAttributeValueServiceTest.ID_1))
        .thenReturn(Optional.of(this.predefinedAllowedAttributeValueBiru));
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
        new ArrayList<PredefinedAllowedAttributeValue>();
    predefinedAllowedAttributeValues.add(this.predefinedAllowedAttributeValueMerah);
    predefinedAllowedAttributeValues.add(this.predefinedAllowedAttributeValueBiru);

    this.service.markForDeletePredefinedAllowedAttributeValue(PredefinedAllowedAttributeValueServiceTest.STORE_ID,
        predefinedAllowedAttributeValues);
    Assertions.assertTrue(this.predefinedAllowedAttributeValueMerah.isMarkForDelete());

    Mockito.verify(this.repository, Mockito.times(1)).findById(PredefinedAllowedAttributeValueServiceTest.ID);
    Mockito.verify(this.repository, Mockito.times(1)).findById(PredefinedAllowedAttributeValueServiceTest.ID_1);
    Mockito.verify(this.repository, Mockito.times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID);
    Mockito.verify(this.repository, Mockito.times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID_1);
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.predefinedAllowedAttributeValueMerah);
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.predefinedAllowedAttributeValueBiru);
  }

  @Test
  public void testMarkForDeleteSingle() throws Exception {
    Mockito
        .when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(this.predefinedAllowedAttributeValueMerah);
    Mockito.when(this.repository.findById(PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(Optional.of(this.predefinedAllowedAttributeValueMerah));
    this.service.markForDeletePredefinedAllowedAttributeValue(PredefinedAllowedAttributeValueServiceTest.STORE_ID,
        PredefinedAllowedAttributeValueServiceTest.ID);
    Assertions.assertTrue(this.predefinedAllowedAttributeValueMerah.isMarkForDelete());

    Mockito.verify(this.repository).findById(PredefinedAllowedAttributeValueServiceTest.ID);
    Mockito.verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(
        PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID);
    Mockito.verify(this.repository).saveAndFlush(this.predefinedAllowedAttributeValueMerah);
  }

  @Test
  public void testMarkForDeleteSingleWithEmptyId() throws Exception {
    Mockito
        .when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(
            PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID))
        .thenReturn(null);

    try {
      this.service.markForDeletePredefinedAllowedAttributeValue(PredefinedAllowedAttributeValueServiceTest.STORE_ID,
          PredefinedAllowedAttributeValueServiceTest.ID);
    } catch (Exception e) {
      Assertions.assertTrue(
          (e instanceof ApplicationException) && e.getMessage().contains("Can not perform delete on un exist data"));
      Mockito.verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(
          PredefinedAllowedAttributeValueServiceTest.STORE_ID, PredefinedAllowedAttributeValueServiceTest.ID);
    }
  }

  @Test
  public void testSaveAllowedAttributeValueSuccessfully() throws Exception {
    this.predefinedAllowedAttributeValueMerah.setId(null);
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedPredefinedAllowedAttributeValue);
    savedPredefinedAllowedAttributeValue.setId(uuid);
    Mockito.when(this.repository.saveAndFlush(this.predefinedAllowedAttributeValueMerah))
        .thenReturn(savedPredefinedAllowedAttributeValue);
    Assertions.assertEquals(this.service.save(this.predefinedAllowedAttributeValueMerah), (uuid));
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.predefinedAllowedAttributeValueMerah);
  }

  @Test
  public void testSaveAllowedAttributeValueWithEmptyId() {
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.predefinedAllowedAttributeValueMerah.setId(uuid);
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedPredefinedAllowedAttributeValue);
    Mockito.when(this.repository.findById(this.predefinedAllowedAttributeValueMerah.getId()))
        .thenReturn(Optional.of(savedPredefinedAllowedAttributeValue));
    try {
      this.service.save(this.predefinedAllowedAttributeValueMerah);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage()
          .contains(PredefinedAllowedAttributeValueServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
      Mockito.verify(this.repository, Mockito.times(1)).findById(this.predefinedAllowedAttributeValueMerah.getId());
    }

  }


  @Test
  public void testUpdateAllowedAttributeValueNonExistenceEntity() {
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.predefinedAllowedAttributeValueMerah.setId(uuid);
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedAllowedAttributeValue);
    Mockito.when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(this.predefinedAllowedAttributeValueMerah);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage()
          .contains(PredefinedAllowedAttributeValueServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
    } finally {
      Mockito.verify(this.repository, Mockito.times(1)).findById(this.predefinedAllowedAttributeValueMerah.getId());
    }
  }

  @Test
  public void testUpdateAllowedAttributeValueSuccessfully() throws Exception {
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    String uuid = GdnUUIDHelper.generateUUID();
    this.predefinedAllowedAttributeValueMerah.setId(uuid);
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedPredefinedAllowedAttributeValue);
    Mockito.when(this.repository.findById(this.predefinedAllowedAttributeValueMerah.getId()))
        .thenReturn(Optional.of(savedPredefinedAllowedAttributeValue));
    this.service.update(this.predefinedAllowedAttributeValueMerah);
    Assertions.assertTrue(true);
    Mockito.verify(this.repository, Mockito.times(1)).findById(this.predefinedAllowedAttributeValueMerah.getId());
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.predefinedAllowedAttributeValueMerah);

  }

  @Test
  public void testUpdateAllowedAttributeValueWithEmptyId() {
    this.predefinedAllowedAttributeValueMerah.setId(null);
    AllowedAttributeValue savedAllowedAttributeValue = new AllowedAttributeValue();
    BeanUtils.copyProperties(this.predefinedAllowedAttributeValueMerah, savedAllowedAttributeValue);
    try {
      this.service.update(this.predefinedAllowedAttributeValueMerah);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage()
          .contains(PredefinedAllowedAttributeValueServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
    }
  }
  
  @Test
  public void findByStoreIdAndPredefinedAllowedAttributeCodeTest() throws Exception {
    this.service.findByStoreIdAndPredefinedAllowedAttributeCode(PredefinedAllowedAttributeValueServiceTest.STORE_ID,
        PredefinedAllowedAttributeValueServiceTest.PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    Mockito.verify(this.repository).findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString());
  }
  
  @Test
  public void findTopByStoreIdAndAttributeAttributeCodeAndValueAndMarkForDeleteFalseOrderByUpdatedDateDescTest()
      throws Exception {
    Mockito
        .when(
            this.repository
                .findTopByStoreIdAndAttributeAttributeCodeAndValueAndMarkForDeleteFalseOrderByUpdatedDateDesc(
                    STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE, VALUE_BIRU)).thenReturn(
            this.predefinedAllowedAttributeValueBiru);
    this.service
        .findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
            STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE, VALUE_BIRU, false);
    Mockito
        .verify(this.repository)
        .findTopByStoreIdAndAttributeAttributeCodeAndValueAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE, VALUE_BIRU);
  }

  @Test
  public void findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCodeTest() throws Exception {
    Mockito.when(
            this.repository.findByStoreIdAndAttributeAttributeCodeAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(
                STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE, VALUE_BIRU))
        .thenReturn(this.predefinedAllowedAttributeValueBiru);
    this.service.findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(STORE_ID,
        PREDEFINED_ALLOWED_ATTRIBUTE_CODE, VALUE_BIRU, true);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeAttributeCodeAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
            PREDEFINED_ALLOWED_ATTRIBUTE_CODE, VALUE_BIRU);
  }

  @Test
  public void getBrandSuggestionsTest() throws Exception {
    Mockito.when(this.brandService
        .getBrandSuggestions(STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BP_CODE, DEFAULT_PAGE_REQUEST, Boolean.FALSE, false))
        .thenReturn(predefinedAllowedAttributeValueResponsePage);
    this.service.getBrandSuggestions(STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BP_CODE, DEFAULT_PAGE_REQUEST, Boolean.FALSE, false);
    Mockito.verify(this.brandService)
        .getBrandSuggestions(STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BP_CODE, DEFAULT_PAGE_REQUEST, Boolean.FALSE,
            false);
  }

  @Test
  public void findByStoreIdAndAttributeIdWithManualSortingTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST)).thenReturn(predefinedAllowedAttributeValuePage);
    Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = this.service
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, AttributeSortType.MANUAL);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST);
    Assertions.assertEquals(predefinedAllowedAttributeValues, predefinedAllowedAttributeValuePage);
  }

  @Test
  public void findByStoreIdAndAttributeIdWithCustomSortingTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST)).thenReturn(predefinedAllowedAttributeValuePage);
    Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = this.service
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, AttributeSortType.CUSTOM);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(STORE_ID, ATTRIBUTE_ID,
            DEFAULT_PAGE_REQUEST);
    Assertions.assertEquals(predefinedAllowedAttributeValues, predefinedAllowedAttributeValuePage);
  }

  @Test
  public void findByStoreIdAndAttributeIdWithAlphabeticalWiseSortingTest() throws Exception {
    Mockito.when(this.repository
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST))
        .thenReturn(predefinedAllowedAttributeValuePage);
    Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = this.service
        .findByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST, AttributeSortType.ALPHABETICAL);
    Mockito.verify(this.repository)
        .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(STORE_ID, ATTRIBUTE_ID, DEFAULT_PAGE_REQUEST);
    Assertions.assertEquals(predefinedAllowedAttributeValues, predefinedAllowedAttributeValuePage);
  }

  public void findByStoreIdAndPredefinedAllowedAttributeCodesTest() throws Exception {
    Set<String> codes = new HashSet<>();
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    Mockito.when(this.repository.findByStoreIdAndPredefinedAllowedAttributeCodeInAndMarkForDeleteFalse(STORE_ID, codes))
        .thenReturn(predefinedAllowedAttributeValues);
    List<PredefinedAllowedAttributeValue> result =
        this.service.findByStoreIdAndPredefinedAllowedAttributeCodes(STORE_ID, codes);
    Mockito.verify(this.repository)
        .findByStoreIdAndPredefinedAllowedAttributeCodeInAndMarkForDeleteFalse(STORE_ID, codes);
    Assertions.assertEquals(result, predefinedAllowedAttributeValues);
  }

  @Test
  public void findByStoreIdAndIdsTest() throws Exception {
    ReflectionTestUtils.setField(service, "allowRejectedBrandInAttributes", false);
    Set<String> ids = new HashSet<>();
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    Mockito.when(this.repository.findByStoreIdAndIdInAndMarkForDeleteFalse(STORE_ID, ids))
        .thenReturn(predefinedAllowedAttributeValues);
    List<PredefinedAllowedAttributeValue> result = this.service.findByStoreIdAndIds(STORE_ID, ids);
    Mockito.verify(this.repository).findByStoreIdAndIdInAndMarkForDeleteFalse(STORE_ID, ids);
    Assertions.assertEquals(result, predefinedAllowedAttributeValues);
  }

  @Test
  public void findByStoreIdAndIdsNullTest() throws Exception {
    ReflectionTestUtils.setField(service, "allowRejectedBrandInAttributes", true);
    Set<String> ids = new HashSet<>();
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    Mockito.when(this.repository.findByStoreIdAndIdIn(STORE_ID, ids))
        .thenReturn(null);
    List<PredefinedAllowedAttributeValue> result = this.service.findByStoreIdAndIds(STORE_ID, ids);
    Mockito.verify(this.repository).findByStoreIdAndIdIn(STORE_ID, ids);
    Assertions.assertEquals(new ArrayList<>(), result);
  }

  @Test
  public void findByStoreIdAndIdsMarkForDeleteFalseTest() throws Exception {
    ReflectionTestUtils.setField(service, "allowRejectedBrandInAttributes", true);
    Set<String> ids = new HashSet<>();
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    predefinedAllowedAttributeValues.get(0).setMarkForDelete(false);
    Mockito.when(this.repository.findByStoreIdAndIdIn(STORE_ID, ids))
        .thenReturn(predefinedAllowedAttributeValues);
    List<PredefinedAllowedAttributeValue> result = this.service.findByStoreIdAndIds(STORE_ID, ids);
    Mockito.verify(this.repository).findByStoreIdAndIdIn(STORE_ID, ids);
    Assertions.assertEquals(result, predefinedAllowedAttributeValues);
  }

  @Test
  public void findByStoreIdAndIdsMarkForDeleteTrueTest() throws Exception {
    ReflectionTestUtils.setField(service, "allowRejectedBrandInAttributes", true);
    Set<String> ids = new HashSet<>();
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    predefinedAllowedAttributeValues.get(0).setMarkForDelete(true);
    Mockito.when(this.repository.findByStoreIdAndIdIn(STORE_ID, ids))
        .thenReturn(predefinedAllowedAttributeValues);
    List<PredefinedAllowedAttributeValue> result = this.service.findByStoreIdAndIds(STORE_ID, ids);
    Mockito.verify(this.repository).findByStoreIdAndIdIn(STORE_ID, ids);
    Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndIdsMarkForDeleteTrueAndBrand() throws Exception {
    ReflectionTestUtils.setField(service, "allowRejectedBrandInAttributes", true);
    Set<String> ids = new HashSet<>();
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(new PredefinedAllowedAttributeValue(attribute, VALUE, STORE_ID, SEQUENCE));
    predefinedAllowedAttributeValues.get(0).setMarkForDelete(true);
    predefinedAllowedAttributeValues.get(0).getAttribute().setName(Constants.BRAND);
    Mockito.when(this.repository.findByStoreIdAndIdIn(STORE_ID, ids))
        .thenReturn(predefinedAllowedAttributeValues);
    List<PredefinedAllowedAttributeValue> result = this.service.findByStoreIdAndIds(STORE_ID, ids);
    Mockito.verify(this.repository).findByStoreIdAndIdIn(STORE_ID, ids);
    Assertions.assertEquals(result, predefinedAllowedAttributeValues);
  }

  @Test
  public void findByStoreIdAndAttributeAndValueTest() {
    Mockito.when(repository.findByStoreIdAndAttributeAndValue(STORE_ID, attribute,
        VALUE_MERAH)).thenReturn(Collections.singletonList(predefinedAllowedAttributeValueMerah));
    service.findByStoreIdAndAttributeAndValue(STORE_ID, attribute, VALUE_MERAH);
    Mockito.verify(repository).findByStoreIdAndAttributeAndValue(STORE_ID, attribute,
        VALUE_MERAH);
  }

  @Test
  public void generatePredefineAllowedAttributeValue_Test() throws Exception {
    this.service.generatePredefinedAllowedAttributeValue(brandWip, attribute);
  }

  @Test
  public void updatePredefinedAllowedAttributeCodeForApprovedBrandTest() throws ApplicationException {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setSequence(0);
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValue.setAttribute(attribute);
    Mockito.when(this.repository
        .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID, BRAND_REQUEST_CODE))
        .thenReturn(predefinedAllowedAttributeValue);
    Mockito.when(repository.findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        PREDEFINED_ALLOWED_ATTRIBUTE_CODE)).thenReturn(predefinedAllowedAttributeValue);
    service
        .updatePredefinedAllowedAttributeCodeForApprovedBrand(STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE, brand);
    Mockito.verify(repository).findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    Mockito.verify(repository).save(predefinedAllowedAttributeValue);
    Mockito.verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), attribute.getAttributeCode());
  }

  @Test
  public void updateBrandNameForApprovedBrandTest() throws ApplicationException {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    brand.setBrandName(BRAND_NAME);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setSequence(0);
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(BRAND_CODE);
    predefinedAllowedAttributeValue.setAttribute(attribute);
    Mockito.when(this.repository
        .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID, BRAND_REQUEST_CODE))
        .thenReturn(predefinedAllowedAttributeValue);
    Mockito.when(repository.findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        PREDEFINED_ALLOWED_ATTRIBUTE_CODE)).thenReturn(predefinedAllowedAttributeValue);
    service
        .updatePredefinedAllowedAttributeCodeForApprovedBrand(STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE, brand);
    Mockito.verify(repository).findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    Mockito.verify(repository).save(predefinedAllowedAttributeValue);
    Assertions.assertEquals(BRAND_NAME, predefinedAllowedAttributeValue.getValue());
    Mockito.verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), attribute.getAttributeCode());
  }


  @Test
  public void updatePredefinedAllowedAttributeCodeExceptionTest() throws ApplicationException {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    Mockito.when(repository.findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        PREDEFINED_ALLOWED_ATTRIBUTE_CODE)).thenThrow(new ApplicationRuntimeException());
    try {
      service.updatePredefinedAllowedAttributeCodeForApprovedBrand(STORE_ID, PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
          brand);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(repository).findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID,
          PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    }
  }

  @Test
  public void deactivatedTest() throws Exception {
    Mockito.when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID))
        .thenReturn(predefinedAllowedAttributeValueMerah);
    Mockito.when(repository.findById(ID)).thenReturn(Optional.of(predefinedAllowedAttributeValueMerah));
    service.deactivated(ID, STORE_ID);
    Assertions.assertTrue(predefinedAllowedAttributeValueMerah.isMarkForDelete());
    Mockito.verify(repository).findById(ID);
    Mockito.verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID);
    Mockito.verify(repository).saveAndFlush(predefinedAllowedAttributeValueMerah);
  }

  @Test
  public void findByStoreIdAndAttributeAndValueAndMarkForDeleteFalseTest() {
    service.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute, VALUE);
    Mockito.verify(repository).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(STORE_ID, attribute, VALUE);
  }

  @Test
  public void findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalseTest() {
    service.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(STORE_ID, VALUE, ATTRIBUTE_ID, pageable);
    Mockito.verify(repository)
        .findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID, VALUE, pageable);
  }

  @Test
  public void findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalseTest() {
    service.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, VALUE, ATTRIBUTE_ID);
    Mockito.verify(repository).findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID, VALUE);
  }

  @Test
  public void saveWithGeneratedCodeTest() throws Exception {
    attribute.setId(ATTRIBUTE_ID);
    Mockito.when(attributeService.findById(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    Mockito.when(this.repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.anyString(), Mockito.anyString(), Mockito.any(Pageable.class)))
        .thenReturn(predefinedAllowedAttributeValuePage);
    service.saveWithGeneratedCode(STORE_ID, ATTRIBUTE_ID,  predefinedAllowedAttributeValueMerah);
    Mockito.verify(attributeService).findById(STORE_ID, ATTRIBUTE_ID);
    Mockito.verify(repository)
        .findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.eq(ATTRIBUTE_ID), Mockito.eq(VALUE_MERAH), Mockito.any(Pageable.class));
  }

  @Test
  public void saveWithGeneratedCodeAttributeNotFoundTest() throws Exception {
    try {
      service.saveWithGeneratedCode(STORE_ID, ATTRIBUTE_ID, predefinedAllowedAttributeValueMerah);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(attributeService).findById(STORE_ID, ATTRIBUTE_ID);
    }
  }

  @Test
  public void saveWithGeneratedCodePredefinedAllowedAttributeValuesNotFoundTest() throws Exception {
    attribute.setId(ATTRIBUTE_ID);
    predefinedAllowedAttributeValueMerah.setId(null);
    Mockito.when(attributeService.findById(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    Mockito.when(this.repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.anyString(), Mockito.anyString(), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(repository.saveAndFlush(Mockito.any(PredefinedAllowedAttributeValue.class)))
        .thenReturn(predefinedAllowedAttributeValueMerah);
    service.saveWithGeneratedCode(STORE_ID, ATTRIBUTE_ID,  predefinedAllowedAttributeValueMerah);
    Mockito.verify(attributeService).findById(STORE_ID, ATTRIBUTE_ID);
    Mockito.verify(repository)
        .findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.eq(ATTRIBUTE_ID), Mockito.eq(VALUE_MERAH), Mockito.any(Pageable.class));
    predefinedAllowedAttributeValueMerah.setAttribute(attribute);
    Mockito.verify(repository).getSequenceByAttributeCode(attribute.getAttributeCode());
    Mockito.verify(repository).saveAndFlush(predefinedAllowedAttributeValueMerah);
  }

  @Test
  public void updatePredefinedAllowedAttributeCodeForRejectedBrandTest() throws Exception {
    predefinedAllowedAttributeValueBiru.setAttribute(attribute);
    Mockito.when(this.repository
        .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID, BRAND_REQUEST_CODE))
        .thenReturn(predefinedAllowedAttributeValueBiru);
    service.updatePredefinedAllowedAttributeCodeForRejectedBrand(STORE_ID, BRAND_REQUEST_CODE);
    predefinedAllowedAttributeValueBiru.setMarkForDelete(true);
    Mockito.verify(repository)
        .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID, BRAND_REQUEST_CODE);
    Mockito.verify(repository).save(predefinedAllowedAttributeValueBiru);
    Mockito.verify(attributeService).evictAttributeCache(STORE_ID, attribute.getId(), attribute.getAttributeCode());
  }

  @Test
  public void updatePredefinedAllowedAttributeCodeForRejectedBrandExceptionTest() throws Exception {
    Mockito.when(repository
        .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID, BRAND_REQUEST_CODE))
        .thenReturn(null);
    try {
      service.updatePredefinedAllowedAttributeCodeForRejectedBrand(STORE_ID, BRAND_REQUEST_CODE);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(repository)
          .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(STORE_ID, BRAND_REQUEST_CODE);
    }
  }

  @Test
  public void getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdTest() {
    service.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(repository).findByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
  }

  @Test
  public void getPredefinedAllowedAttributeValuesByStoreIdAndIdsTest() {
    service.getPredefinedAllowedAttributeValuesByStoreIdAndIds(
        STORE_ID, Collections.singleton(ATTRIBUTE_ID));
    verify(repository).findByStoreIdAndIdIn(STORE_ID, Collections.singleton(ATTRIBUTE_ID));
  }


  @Test
  public void findByStoreIdAndAttributeAndValueOrderByMarkForDeleteTest() {
    Attribute attribute = new Attribute();
    Mockito.when(repository.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    service.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
    verify(repository).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
  }

  @Test
  public void getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue() {
    Mockito.when(repository.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, ID, VALUE))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    service.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(STORE_ID, ID, VALUE);
    verify(repository).findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, ID, VALUE);
  }

  @Test
  public void getSequenceTest() {
    when(repository.getSequenceByAttributeCode(ATTRIBUTE_CODE)).thenReturn(1L);
    String result = service.getSequence(ATTRIBUTE_CODE);
    assertEquals(GENERATED_SEQUENCE, result);
    verify(repository).getSequenceByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void addPredefinedAllowedAttributeValueTest() throws Exception {
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    Date updatedDate = new Date();
    when(repository.getSequenceByAttributeCode(ATTRIBUTE_CODE)).thenReturn(1L);
    PredefinedAllowedAttributeValue savedValue = new PredefinedAllowedAttributeValue();
    savedValue.setStoreId(STORE_ID);
    savedValue.setAttribute(attribute);
    savedValue.setValue(VALUE);
    savedValue.setValueEn(VALUE_EN);
    savedValue.setSequence(SEQUENCE);
    savedValue.setPredefinedAllowedAttributeCode(EXPECTED_CODE);
    savedValue.setCreatedBy(UPDATED_BY);
    savedValue.setUpdatedBy(UPDATED_BY);
    savedValue.setCreatedDate(updatedDate);
    savedValue.setUpdatedDate(updatedDate);
    when(repository.save(any(PredefinedAllowedAttributeValue.class))).thenReturn(savedValue);
    PredefinedAllowedAttributeValue result = service.addPredefinedAllowedAttributeValue(
        STORE_ID, attribute, UPDATED_BY, VALUE, SEQUENCE, VALUE_EN);
    verify(repository).getSequenceByAttributeCode(ATTRIBUTE_CODE);
    verify(repository).save(any(PredefinedAllowedAttributeValue.class));
    assertEquals(STORE_ID, result.getStoreId());
    assertEquals(attribute, result.getAttribute());
    assertEquals(VALUE, result.getValue());
    assertEquals(VALUE_EN, result.getValueEn());
    assertEquals(SEQUENCE, result.getSequence());
    assertEquals(UPDATED_BY, result.getCreatedBy());
    assertEquals(UPDATED_BY, result.getUpdatedBy());
    assertEquals(EXPECTED_CODE, result.getPredefinedAllowedAttributeCode());
  }

  @Test
  void fetchAttributeIdAndValueLikeIgnoreCase() {
    Mockito.when(
        repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(STORE_ID,
          ATTRIBUTE_ID, VALUE))
      .thenReturn(Collections.singletonList(predefinedAllowedAttributeValueMerah));
    service.getAttributeIdAndValueLikeIgnoreCase(STORE_ID, ATTRIBUTE_ID, VALUE);
    Mockito.verify(repository)
      .findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(STORE_ID,
        ATTRIBUTE_ID, VALUE);
  }
}
