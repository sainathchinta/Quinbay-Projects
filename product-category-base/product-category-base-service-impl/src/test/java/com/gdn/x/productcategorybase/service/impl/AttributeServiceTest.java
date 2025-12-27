package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.Set;

import com.gdn.x.productcategorybase.dto.request.AttributeAndValueByTypeRequest;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeBasicDetailDTO;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.config.ApplicationConfig;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

public class AttributeServiceTest {

  private static final String NEW_VALUE = "NEW VALUE";
  private static final String VALUE = "Value";
  private static final String ATTRIBUTE_CODE_PREFIX = "ATT";
  private static final String ATTRIBUTE1_NAME = "ATTRIBUTE_1_NAME";
  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final String ATTRIBUTE2_NAME = "ATTRIBUTE_2_NAME";
  private static final String ATTRIBUTE3_NAME = "ATTRIBUTE_3_NAME";
  private static final String STORE_ID = "10001";
  private static final VerificationMode AT_LEAST_ONCE = times(1);
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";
  private static final String ATTRIBUTE_CODE = "ATT-CODE";
  private static final String DEFAULT_ATTRIBUTE_NAME = "attribute name";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  public static final String PREDEF_ID_THROWS = "predefIdThrows";
  public static final String ATTR_HIDDEN = "attrHidden";
  public static final String ATTR_VISIBLE = "attrVisible";
  public static final String DUPLICATE_ALLOWED_ID = "duplicateAllowedId";
  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private Attribute attribute = new Attribute();
  private List<Attribute> attributeList = new ArrayList<>();
  private static final String CATEGORY_CODE = "CAT-CODE";
  private static final String CATEGORY_CODE_2 = "CAT-CODE-2";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ID = "id";
  private static final String PRODUCT_ATTRIBUTE_ID = "PRODUCT_ATTRIBUTE_ID";
  private static  final String DS_EXTRACTION = "DS_EXTRACTION";

  private List<Object[]> attrList;
  
  @Mock
  private AttributeRepository repository;

  @InjectMocks
  private AttributeServiceBean service;

  @Mock
  private ApplicationConfig applicationConfig;

  @Mock
  private ProductServiceBean productService;

  @Mock
  private ProductAttributeValueService productAttributeValueService;

  @Mock
  private AllowedAttributeValueServiceBean allowedAttributeValueService;

  @Mock
  private PredefinedAllowedAttributeValueServiceBean predefinedAllowedAttributeValueService;

  @Mock
  private ProductRepository productRepository;
  
  @Mock
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Mock
  private CategoryAttributeRepository categoryAttributeRepository;

  @Mock
  private CategoryServiceBean categoryServiceBean;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private ApplicationContext applicationContext;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<AttributeDomainEventModel> attributeDomainEventModelArgumentCaptor;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private AttributeService attributeService;

  @Mock
  private ProductAttributeRepository productAttributeRepository;

  @Mock
  private ProductAttributeService productAttributeService;
  private Map<AttributeAndValueByTypeRequest, Object> attributeAndValueMap;
  private AttributeAndValueByTypeRequest request1;
  private AttributeAndValueByTypeRequest request2;
  private AttributeAndValueByTypeRequest request3;
  private AttributeAndValueByTypeRequest request4;
  private Attribute attrHidden;
  private Attribute attrNotHidden;
  private PredefinedAllowedAttributeValue paavHidden;
  private PredefinedAllowedAttributeValue paavNotHidden;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);

    MDC.put("storeId", "10001");

    attrList = new ArrayList<Object[]>();
    attrList.add(new Object[]{"AS-1000018", "Category", "MA-0000027", "DESCRIPTIVE_ATTRIBUTE", "Manufacturer",
        false, true, "", "", false, false, true, "Catgeory-English", "Manufacturer-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OS-M000171", "PREDEFINED_ATTRIBUTE", "OS", false, false,
        "", "Android", false, false, false, "Catgeory-English", "OS-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OS-M000171", "PREDEFINED_ATTRIBUTE", "OS", true, false,
        "", "IOS", false, false, false, "Catgeory-English", "OS-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OS-M000171", "PREDEFINED_ATTRIBUTE", "OS", true, true,
        "", "Tizen", false, false, false, "Catgeory-English", "OS-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", false, true,
        "BSD - Teras Kota", "", true, false, false,"Catgeory-English", "Outlet-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", false, true,
        "BSD - AEON", "", true, false, false, "Catgeory-English", "Outlet-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", false, true,
        "Bali - Lippo Plaza", "", true, false, false, "Catgeory-English", "Outlet-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", true, false,
        "Bandung - Paris Van Java", "", true, false, false, "Catgeory-English", "Outlet-English", ""});
    attrList.add(new Object[]{"AS-1000018", "Category", "OU-2000004", "DEFINING_ATTRIBUTE", "Outlet", true, false,
        "Bandung - Paris Van Java", "", true, false, false, "Catgeory-English", "Outlet-English"});
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setBasicView(false);
    attributeList.add(attribute);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute.setName(ATTRIBUTE1_NAME);

    this.attribute.getAllowedAttributeValues().add(new AllowedAttributeValue(this.attribute, VALUE, VALUE, 1));
    this.attribute.getAllowedAttributeValues().add(new AllowedAttributeValue(this.attribute, VALUE, STORE_ID, 2));
    this.attribute.getAllowedAttributeValues().get(1).setMarkForDelete(false);
    this.attribute.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValue(this.attribute, VALUE, STORE_ID, 1));
    this.attribute.getPredefinedAllowedAttributeValues()
        .add(new PredefinedAllowedAttributeValue(this.attribute, VALUE, STORE_ID, 2));
    this.attribute.getPredefinedAllowedAttributeValues().get(1).setMarkForDelete(false);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(service);
    request1 = mock(AttributeAndValueByTypeRequest.class);
    request2 = mock(AttributeAndValueByTypeRequest.class);
    request3 = mock(AttributeAndValueByTypeRequest.class);
    request4 = mock(AttributeAndValueByTypeRequest.class);
    attrHidden = mock(Attribute.class);
    when(attrHidden.isHideForSeller()).thenReturn(true);

    attrNotHidden = mock(Attribute.class);
    when(attrNotHidden.isHideForSeller()).thenReturn(false);

    paavHidden = mock(PredefinedAllowedAttributeValue.class);
    when(paavHidden.getAttribute()).thenReturn(attrHidden);

    paavNotHidden = mock(PredefinedAllowedAttributeValue.class);
    when(paavNotHidden.getAttribute()).thenReturn(attrNotHidden);
    attributeAndValueMap = new HashMap<>();
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.repository);
    verifyNoMoreInteractions(this.allowedAttributeValueService);
    verifyNoMoreInteractions(this.applicationConfig);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productAttributeValueService);
    verifyNoMoreInteractions(this.productRepository);
    verifyNoMoreInteractions(this.categoryAttributeRepository);
    verifyNoMoreInteractions(this.applicationCacheServiceBean);
    verifyNoMoreInteractions(applicationContext);
    verifyNoMoreInteractions(productAttributeService);
    verifyNoMoreInteractions(productAttributeRepository, categoryServiceBean);
    MDC.clear();
  }
  
  @Test
  public void getAttributeDetailByCategoryCodeTest() throws Exception{
    Mockito.when(repository.getAttributeDetailByCategoryCode(anyString())).thenReturn(attrList);

    CategoryAttributeDetailDTO result = service.getAttributeDetailByCategoryCode("", true);
    
    Mockito.verify(repository).getAttributeDetailByCategoryCode(anyString());
    assertEquals(3, result.getAttributes().size());
    assertEquals("MA-0000027", result.getAttributes().get(0).getAttributeCode());
    assertEquals("OS-M000171", result.getAttributes().get(1).getAttributeCode());
    assertEquals("OU-2000004", result.getAttributes().get(2).getAttributeCode());
    assertEquals("DESCRIPTIVE_ATTRIBUTE", result.getAttributes().get(0).getAttributeType());
    assertEquals("Manufacturer", result.getAttributes().get(0).getName());
    assertFalse(result.getAttributes().get(0).isSkuValue());
    assertTrue(result.getAttributes().get(0).isBasicView());
    assertFalse(result.getAttributes().get(0).isMandatory());
    assertFalse(result.getAttributes().get(0).isVariantCreation());
    assertEquals(0, result.getAttributes().get(0).getOptions().size());
    assertTrue(result.getAttributes().get(0).isVariantCreatingUi());
    assertEquals("OS", result.getAttributes().get(1).getName());
    assertTrue(result.getAttributes().get(1).isSkuValue());
    assertTrue(result.getAttributes().get(1).isBasicView());
    assertFalse(result.getAttributes().get(1).isMandatory());
    assertEquals(3, result.getAttributes().get(1).getOptions().size());
    assertFalse(result.getAttributes().get(1).isVariantCreatingUi());
    assertEquals("Outlet", result.getAttributes().get(2).getName());
    assertTrue(result.getAttributes().get(2).isSkuValue());
    assertFalse(result.getAttributes().get(2).isBasicView());
    assertFalse(result.getAttributes().get(2).isMandatory());
    assertEquals(5, result.getAttributes().get(2).getOptions().size());
    assertFalse(result.getAttributes().get(2).isVariantCreatingUi());
    assertEquals("Catgeory-English", result.getEnglishName());
    assertEquals("Manufacturer-English", result.getAttributes().get(0).getEnglishName());
    assertEquals("OS-English", result.getAttributes().get(1).getEnglishName());
    assertEquals("Outlet-English", result.getAttributes().get(2).getEnglishName());
  }

  @Test
  void getAttributeDetailByCategoryCodeDsIgnoreTest() throws Exception {
    ReflectionTestUtils.setField(service, "ignoreDsAttribute", true);
    Mockito.when(repository.getAttributeDetailByCategoryCodeIgnoreDs(anyString()))
      .thenReturn(attrList);

    CategoryAttributeDetailDTO result = service.getAttributeDetailByCategoryCode("", true);

    Mockito.verify(repository).getAttributeDetailByCategoryCodeIgnoreDs(anyString());
    assertEquals(3, result.getAttributes().size());
    assertEquals("MA-0000027", result.getAttributes().get(0).getAttributeCode());
    assertEquals("OS-M000171", result.getAttributes().get(1).getAttributeCode());
    assertEquals("OU-2000004", result.getAttributes().get(2).getAttributeCode());
    assertEquals("DESCRIPTIVE_ATTRIBUTE", result.getAttributes().get(0).getAttributeType());
    assertEquals("Manufacturer", result.getAttributes().get(0).getName());
    assertTrue(result.getAttributes().get(0).isVariantCreatingUi());
    assertEquals("OS", result.getAttributes().get(1).getName());
    assertTrue(result.getAttributes().get(1).isSkuValue());
    assertTrue(result.getAttributes().get(1).isBasicView());
    assertFalse(result.getAttributes().get(1).isMandatory());
    assertEquals(3, result.getAttributes().get(1).getOptions().size());
    assertTrue(result.getAttributes().get(2).isSkuValue());
    assertFalse(result.getAttributes().get(2).isBasicView());
    assertFalse(result.getAttributes().get(2).isMandatory());
    assertEquals(5, result.getAttributes().get(2).getOptions().size());
    assertFalse(result.getAttributes().get(2).isVariantCreatingUi());
    assertEquals("Catgeory-English", result.getEnglishName());
    assertEquals("Manufacturer-English", result.getAttributes().get(0).getEnglishName());
    assertEquals("OS-English", result.getAttributes().get(1).getEnglishName());
    assertEquals("Outlet-English", result.getAttributes().get(2).getEnglishName());
  }

  @Test
  public void getAttributeDetailByCategoryCodeNullTest() throws Exception {
    attrList.forEach(arr -> {
      arr[12] = null;
      arr[13] = null;
    });
    Mockito.when(repository.getAttributeDetailByCategoryCode(anyString())).thenReturn(attrList);
    CategoryAttributeDetailDTO result = service.getAttributeDetailByCategoryCode("", true);
    Mockito.verify(repository).getAttributeDetailByCategoryCode(anyString());
    assertNull(result.getEnglishName());
    assertNull(result.getAttributes().get(0).getEnglishName());
    assertNull(result.getAttributes().get(1).getEnglishName());
    assertNull(result.getAttributes().get(2).getEnglishName());
  }
  
  @Test
  public void getAttributeDetailByCategoryCodeWhenError() throws Exception{
    Mockito.when(repository.getAttributeDetailByCategoryCode(anyString())).thenThrow(RuntimeException.class);
    
    try{
      service.getAttributeDetailByCategoryCode("", true);
    } catch(Exception e){
      Mockito.verify(repository).getAttributeDetailByCategoryCode(anyString());
    }
  }
  
  private List<Attribute> getAttributeWhereSearchableFalse() {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DEFINING_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID));
    return attributes;
  }

  private Page<Attribute> getAttributeWhereSearchableFalsePage() {
    return new PageImpl<Attribute>(this.getAttributeWhereSearchableFalse(), AttributeServiceTest.DEFAULT_PAGE_REQUEST,
        this.getAttributeWhereSearchableFalse().size());
  }

  private List<Attribute> getAttributeWhereSearchableTrue() {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE2_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID));
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE3_NAME, AttributeType.DEFINING_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID));
    return attributes;
  }

  private Page<Attribute> getAttributeWhereSearchableTruePage() {
    return new PageImpl<Attribute>(this.getAttributeWhereSearchableTrue(), AttributeServiceTest.DEFAULT_PAGE_REQUEST,
        this.getAttributeWhereSearchableTrue().size());
  }

  private AllowedAttributeValue getDefaultAllowedAttributeValue() {
    return new AllowedAttributeValue(this.getDefaultAttribute(), AttributeServiceTest.VALUE,
        AttributeServiceTest.STORE_ID, 1);
  }

  private Attribute getDefaultAttribute() {
    return new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DEFINING_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID);
  }

  private List<Attribute> getDefaultAttributeList() {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DEFINING_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID));
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE2_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID));
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE3_NAME, AttributeType.DEFINING_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID));
    return attributes;
  }

  private Page<Attribute> getDefaultAttributePage() {
    return new PageImpl<Attribute>(this.getDefaultAttributeList(), AttributeServiceTest.DEFAULT_PAGE_REQUEST,
        this.getDefaultAttributeList().size());
  }

  private PredefinedAllowedAttributeValue getDefaultPredefinedAllowedAttributeValue() {
    return new PredefinedAllowedAttributeValue(this.getDefaultAttribute(), AttributeServiceTest.VALUE,
        AttributeServiceTest.STORE_ID, 1);
  }

  private List<Attribute> getDefiningAttributeList() {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DEFINING_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID));
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE3_NAME, AttributeType.DEFINING_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID));
    return attributes;
  }

  private Page<Attribute> getDefiningAttributePage() {
    return new PageImpl<Attribute>(this.getDefiningAttributeList(), AttributeServiceTest.DEFAULT_PAGE_REQUEST,
        this.getDefiningAttributeList().size());
  }

  private List<Attribute> getDescriptiveAttributeList() {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(new Attribute(AttributeServiceTest.ATTRIBUTE2_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID));
    return attributes;
  }

  private Page<Attribute> getDescriptiveAttributePage() {
    return new PageImpl<Attribute>(this.getDescriptiveAttributeList(), AttributeServiceTest.DEFAULT_PAGE_REQUEST,
        this.getDescriptiveAttributeList().size());
  }

  @Test
  public void testDeleteAllowedAttributeValueSucess() throws Exception {
    String id = UUID.randomUUID().toString();
    AllowedAttributeValue allowedAttributeValue = this.getDefaultAllowedAttributeValue();
    allowedAttributeValue.getAttribute().setId(ATTRIBUTE_ID);
    List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
    productAttributeValues.add(new ProductAttributeValue(new ProductAttribute(), allowedAttributeValue, null,
        DescriptiveAttributeValueType.NONE));
    when(this.allowedAttributeValueService.findByStoreIdAndId(AttributeServiceTest.STORE_ID, id))
        .thenReturn(allowedAttributeValue);
    when(this.productAttributeValueService.findByStoreIdAndAllowedAttributeValue(AttributeServiceTest.STORE_ID,
        allowedAttributeValue)).thenReturn(productAttributeValues);
    when(this.applicationConfig.isRegenerateWhenDeleteAllowedAttribute()).thenReturn(true);
    when(this.productItemAttributeRepository
        .countByAttribute_IdAndValueAndMarkForDeleteFalse(
            allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue())).thenReturn(0L);
    
    allowedAttributeValue.setId(id);
    this.service.deleteAllowedAttributeValue(AttributeServiceTest.STORE_ID, id);
    verify(this.allowedAttributeValueService).markForDeleteAllowedAttributeValue(AttributeServiceTest.STORE_ID, id);
    verify(this.productService).deleteProductItemsByProductAttributeValues(AttributeServiceTest.STORE_ID,
        productAttributeValues);
    verify(this.allowedAttributeValueService).findByStoreIdAndId(AttributeServiceTest.STORE_ID, id);
    verify(this.applicationConfig).isRegenerateWhenDeleteAllowedAttribute();
    verify(this.productAttributeValueService).findByStoreIdAndAllowedAttributeValue(AttributeServiceTest.STORE_ID,
        allowedAttributeValue);
    verify(this.productItemAttributeRepository).countByAttribute_IdAndValueAndMarkForDeleteFalse(
        allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue());
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID,
        allowedAttributeValue.getAttribute().getAttributeCode());
    verify(this.applicationCacheServiceBean).evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(this.applicationCacheServiceBean).evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(
        STORE_ID, ATTRIBUTE_ID);
  }

  @Test
  public void testDeleteAllowedAttributeValueNotRegenerate() throws Exception {
    String id = UUID.randomUUID().toString();
    AllowedAttributeValue allowedAttributeValue = this.getDefaultAllowedAttributeValue();
    allowedAttributeValue.getAttribute().setId(ATTRIBUTE_ID);
    List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
    productAttributeValues.add(new ProductAttributeValue(new ProductAttribute(), allowedAttributeValue, null,
        DescriptiveAttributeValueType.NONE));
    when(this.applicationConfig.isRegenerateWhenDeleteAllowedAttribute()).thenReturn(false);
    when(this.allowedAttributeValueService.findByStoreIdAndId(AttributeServiceTest.STORE_ID, id))
        .thenReturn(allowedAttributeValue);
    when(this.productItemAttributeRepository
        .countByAttribute_IdAndValueAndMarkForDeleteFalse(
            allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue())).thenReturn(0L);
    
    allowedAttributeValue.setId(id);
    this.service.deleteAllowedAttributeValue(AttributeServiceTest.STORE_ID, id);

    verify(this.allowedAttributeValueService).markForDeleteAllowedAttributeValue(AttributeServiceTest.STORE_ID, id);
    verify(this.applicationConfig).isRegenerateWhenDeleteAllowedAttribute();
    verify(this.productItemAttributeRepository).countByAttribute_IdAndValueAndMarkForDeleteFalse(
        allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue());
    verify(this.allowedAttributeValueService).findByStoreIdAndId(AttributeServiceTest.STORE_ID, id);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID,
        allowedAttributeValue.getAttribute().getAttributeCode());
    verify(this.applicationCacheServiceBean).evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(this.applicationCacheServiceBean).evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(
        STORE_ID, ATTRIBUTE_ID);
  }
  
  @Test
  public void testDeleteAllowedAttributeValueFail() throws Exception{
    String id = UUID.randomUUID().toString();
    AllowedAttributeValue allowedAttributeValue = this.getDefaultAllowedAttributeValue();
    when(this.allowedAttributeValueService.findByStoreIdAndId(AttributeServiceTest.STORE_ID, id))
        .thenReturn(allowedAttributeValue);
    when(this.productItemAttributeRepository
        .countByAttribute_IdAndValueAndMarkForDeleteFalse(
            allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue())).thenReturn(1L);   
    try {
      this.service.deleteAllowedAttributeValue(AttributeServiceTest.STORE_ID, id);
    } catch(Exception e) {
      verify(this.allowedAttributeValueService).findByStoreIdAndId(AttributeServiceTest.STORE_ID, id);
      verify(this.productItemAttributeRepository).countByAttribute_IdAndValueAndMarkForDeleteFalse(
          allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue());
    }
  }

  @Test
  public void testFindByAttributeCode() {
    List<Attribute> attributeList = this.getDefaultAttributeList();
    Page<Attribute> attributePage = this.getDefaultAttributePage();

    when(this.repository.findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeServiceTest.ATTRIBUTE_CODE_PREFIX)).thenReturn(attributeList);
    when(this.repository.findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeServiceTest.ATTRIBUTE_CODE_PREFIX,
        AttributeServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(attributePage);

    assertEquals(3, this.service
        .findByAttributeCode(AttributeServiceTest.STORE_ID, AttributeServiceTest.ATTRIBUTE_CODE_PREFIX).size());
    assertEquals(3, this.service.findByAttributeCode(AttributeServiceTest.STORE_ID,
        AttributeServiceTest.ATTRIBUTE_CODE_PREFIX, AttributeServiceTest.DEFAULT_PAGE_REQUEST).getTotalElements());

    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeServiceTest.ATTRIBUTE_CODE_PREFIX);
    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeServiceTest.ATTRIBUTE_CODE_PREFIX,
        AttributeServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindByAttributeType() {
    Page<Attribute> definingAttributePages = this.getDefiningAttributePage();
    List<Attribute> definingAttributes = definingAttributePages.getContent();

    Page<Attribute> descriptiveAttributePages = this.getDescriptiveAttributePage();
    List<Attribute> descriptiveAttributes = descriptiveAttributePages.getContent();

    when(this.repository.findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeType.DEFINING_ATTRIBUTE)).thenReturn(definingAttributes);
    when(this.repository.findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeType.DEFINING_ATTRIBUTE, AttributeServiceTest.DEFAULT_PAGE_REQUEST))
            .thenReturn(definingAttributePages);

    when(this.repository.findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeType.DESCRIPTIVE_ATTRIBUTE)).thenReturn(descriptiveAttributes);
    when(this.repository.findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeType.DESCRIPTIVE_ATTRIBUTE, AttributeServiceTest.DEFAULT_PAGE_REQUEST))
            .thenReturn(descriptiveAttributePages);

    assertTrue(
        this.service.findByAttributeType(AttributeServiceTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE).size() == 2);

    assertTrue(this.service.findByAttributeType(AttributeServiceTest.STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE)
        .size() == 1);

    assertTrue(this.service.findByAttributeType(AttributeServiceTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE)
        .size() == this.service.findByAttributeType(AttributeServiceTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE,
            AttributeServiceTest.DEFAULT_PAGE_REQUEST).getTotalElements());

    assertTrue(this.service.findByAttributeType(AttributeServiceTest.STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE)
        .size() == this.service.findByAttributeType(AttributeServiceTest.STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE,
            AttributeServiceTest.DEFAULT_PAGE_REQUEST).getTotalElements());

    verify(this.repository, Mockito.times(2)).findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE);
    verify(this.repository, Mockito.times(2)).findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE);

    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeType.DEFINING_ATTRIBUTE, AttributeServiceTest.DEFAULT_PAGE_REQUEST);
    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE, AttributeServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindById() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    when(this.repository.findById(AttributeServiceTest.ATTRIBUTE1_NAME)).thenReturn(Optional.of(attribute));
    assertNotNull(this.service.findById(AttributeServiceTest.ATTRIBUTE1_NAME));
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).findById(AttributeServiceTest.ATTRIBUTE1_NAME);
  }

  @Test
  public void testFindByStoreIdAndAttributeCode() {
    when(this.repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    assertNotNull(this.service.findDetailByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE));
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, attribute.getId());
  }

  @Test
  public void testFindByIdWithStoreId() throws Exception {
    Attribute attribute = new Attribute(AttributeServiceTest.ATTRIBUTE2_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, true,
        AttributeServiceTest.STORE_ID);
    String id = GdnUUIDHelper.generateUUID();
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id))
        .thenReturn(attribute);
    Attribute savedAttribute = this.service.findById(AttributeServiceTest.STORE_ID, id);
    Assertions.assertEquals(savedAttribute, (attribute));
    verify(this.repository, Mockito.times(1)).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        id);
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testFindByAttributeCodesWithStoreId() throws Exception {
    List<String> request = new ArrayList<String>();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeCode(AttributeServiceTest.ATTRIBUTE_CODE);
    attribute.setAllowedAttributeValues(new ArrayList<AllowedAttributeValue>());
    List<Attribute> attributes = Arrays.asList(attribute);
    when(this.repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(
        anyString(), anyList())).thenReturn(attributes);
    List<Attribute> savedAttributes =
        this.service.findDetailByStoreIdAndAttributeCodes(request, false);
    Assertions.assertEquals(savedAttributes.size(), (attributes.size()));
    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(
        anyString(), anyList());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testFindByAttributeCodesWithStoreIdEmptyResult() throws Exception {
    List<String> request = new ArrayList<String>();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeCode(AttributeServiceTest.ATTRIBUTE_CODE);
    attribute.setAllowedAttributeValues(new ArrayList<AllowedAttributeValue>());
    when(this.repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(
        anyString(), anyList())).thenReturn(null);
    List<Attribute> savedAttributes =
        this.service.findDetailByStoreIdAndAttributeCodes(request, false);
    assertNull(savedAttributes);
    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(
        anyString(), anyList());
  }

  @Test
  public void testFindByAttributeCodesWithStoreIdFetchOnlyBasicAttributeDetails() throws Exception {
    List<String> request = new ArrayList<String>();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeCode(AttributeServiceTest.ATTRIBUTE_CODE);
    attribute.setAllowedAttributeValues(new ArrayList<AllowedAttributeValue>());
    List<Attribute> attributes = Arrays.asList(attribute);
    when(this.repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(
        anyString(), anyList())).thenReturn(attributes);
    List<Attribute> savedAttributes =
        this.service.findDetailByStoreIdAndAttributeCodes(request, true);
    Assertions.assertEquals(savedAttributes.size(), (attributes.size()));
    verify(this.repository, Mockito.times(1)).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(
        anyString(), anyList());
  }

  @Test
  public void testFindBySearchAbleFalse() {
    List<Attribute> attributes = this.getAttributeWhereSearchableFalse();
    Page<Attribute> attributePages = this.getAttributeWhereSearchableFalsePage();

    when(this.repository.findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID))
        .thenReturn(attributes);
    when(this.repository.findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(attributePages);

    assertTrue(this.service.findBySearchAbleFalse(AttributeServiceTest.STORE_ID).size() == this.service
        .findBySearchAbleFalse(AttributeServiceTest.STORE_ID, AttributeServiceTest.DEFAULT_PAGE_REQUEST)
        .getTotalElements());

    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE)
        .findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID);
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindBySearchAbleTrue() {
    List<Attribute> attributes = this.getAttributeWhereSearchableTrue();
    Page<Attribute> attributePages = this.getAttributeWhereSearchableTruePage();

    when(this.repository.findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID))
        .thenReturn(attributes);
    when(this.repository.findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(attributePages);

    assertTrue(this.service.findBySearchAbleTrue(AttributeServiceTest.STORE_ID).size() == this.service
        .findBySearchAbleTrue(AttributeServiceTest.STORE_ID, AttributeServiceTest.DEFAULT_PAGE_REQUEST)
        .getTotalElements());

    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE)
        .findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID);
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(
        AttributeServiceTest.STORE_ID, AttributeServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindByAttributeId() {
    List<Attribute> attributes = this.getDefaultAttributeList();
    List<String> attributeIdList = new ArrayList<>();
    attributeIdList.add(ATTRIBUTE1_NAME);
    attributeIdList.add(ATTRIBUTE2_NAME);
    when(this.repository.findByIdInAndStoreIdAndMarkForDeleteFalse(attributeIdList, STORE_ID)).thenReturn(attributes);
    assertNotNull(this.service.findByAttributeIds(STORE_ID,attributeIdList));
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).findByIdInAndStoreIdAndMarkForDeleteFalse(
        attributeIdList,
        STORE_ID);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    List<Attribute> attributes = this.getDefaultAttributeList();
    Page<Attribute> attributePages = this.getDefaultAttributePage();
    when(this.repository.findByStoreIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
        AttributeServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(attributePages);
    assertTrue(attributes.size() == this.service
        .findByStoreId(AttributeServiceTest.STORE_ID, AttributeServiceTest.DEFAULT_PAGE_REQUEST).getTotalElements());
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE)
        .findByStoreIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, AttributeServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindDetailByStoreIdAndId() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setId(id);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndId(AttributeServiceTest.STORE_ID, id, false);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndId_PREDEFINED_ATTRIBUTE() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.PREDEFINED_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID);
    attribute.setId(id);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndId(AttributeServiceTest.STORE_ID, id, false);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id);
    verify(predefinedAllowedAttributeValueService)
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndId_PREDEFINED_MUTIVALUE() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.PREDEFINED_MULTIVALUE, false,
        AttributeServiceTest.STORE_ID);
    attribute.setId(id);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndId(AttributeServiceTest.STORE_ID, id, false);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id);
    verify(predefinedAllowedAttributeValueService)
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndId_DESCRIPTIVE_ATTRIBUTE() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID);
    attribute.setId(id);
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndId(AttributeServiceTest.STORE_ID, id, false);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndIdWithDefaultValue() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setMandatory(true);
    attribute.setId(id);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue
        .setPredefinedAllowedAttributeCode(attribute.getAttributeCode() + Constants.HYPHEN + Constants.DEFAULT);
    when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
        STORE_ID, id)).thenReturn(new ArrayList<>(Collections.singletonList(predefinedAllowedAttributeValue)));
    when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndId(AttributeServiceTest.STORE_ID, id, false);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id);
    verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
        STORE_ID, id);
    assertEquals(0, attribute.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void findDetailByStoreIdAndIdAndValueDefiningAttributeTest() {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(STORE_ID,
        attribute.getId(), VALUE)).thenReturn(getDefaultAllowedAttributeValue());

    AttributeResponse attributeResponse =
        service.findDetailByStoreIdAndIdAndValue(STORE_ID, ATTRIBUTE_ID, VALUE, false);

    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(STORE_ID,
        attribute.getId(), VALUE);
    verify(applicationContext).getBean(AttributeService.class);

    assertEquals(VALUE, attributeResponse.getAllowedAttributeValues().get(0).getValue());
    assertTrue(attributeResponse.getPredefinedAllowedAttributeValues().isEmpty());
  }

  @Test
  public void findDetailByStoreIdAndIdAndValueDefiningAttributeNoValueTest() {
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(STORE_ID,
        attribute.getId(), VALUE)).thenReturn(null);

    AttributeResponse attributeResponse =
        service.findDetailByStoreIdAndIdAndValue(STORE_ID, ATTRIBUTE_ID, VALUE, false);

    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(STORE_ID,
        attribute.getId(), VALUE);
    verify(applicationContext).getBean(AttributeService.class);

    assertTrue(attributeResponse.getAllowedAttributeValues().isEmpty());
    assertTrue(attributeResponse.getPredefinedAllowedAttributeValues().isEmpty());
  }


  @Test
  public void findDetailByStoreIdAndIdAndValuePredefinedAttributeTest() {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
        STORE_ID, attribute.getId(), VALUE)).thenReturn(Arrays.asList(getDefaultPredefinedAllowedAttributeValue()));

    AttributeResponse attributeResponse =
        service.findDetailByStoreIdAndIdAndValue(STORE_ID, ATTRIBUTE_ID, VALUE, false);

    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
        STORE_ID, attribute.getId(), VALUE);
    verify(applicationContext).getBean(AttributeService.class);

    assertEquals(VALUE, attributeResponse.getPredefinedAllowedAttributeValues().get(0).getValue());
    assertTrue(attributeResponse.getAllowedAttributeValues().isEmpty());
  }

  @Test
  public void findDetailByStoreIdAndIdAndValuePredefinedAttributeNoValueTest() {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
        STORE_ID, attribute.getId(), VALUE)).thenReturn(new ArrayList<>());

    AttributeResponse attributeResponse =
        service.findDetailByStoreIdAndIdAndValue(STORE_ID, ATTRIBUTE_ID, VALUE, false);

    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
        STORE_ID, attribute.getId(), VALUE);
    verify(applicationContext).getBean(AttributeService.class);

    assertTrue(attributeResponse.getPredefinedAllowedAttributeValues().isEmpty());
    assertTrue(attributeResponse.getAllowedAttributeValues().isEmpty());
  }

  @Test
  public void findDetailByStoreIdAndIdAndValueDescriptiveAttributeTest() {
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);

    AttributeResponse attributeResponse =
        service.findDetailByStoreIdAndIdAndValue(STORE_ID, ATTRIBUTE_ID, VALUE, false);

    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    verify(applicationContext).getBean(AttributeService.class);

    assertTrue(attributeResponse.getPredefinedAllowedAttributeValues().isEmpty());
    assertTrue(attributeResponse.getAllowedAttributeValues().isEmpty());
  }
  @Test
  public void testFindDetailByStoreIdAndAttributeCode() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setId(id);
    when(this.repository.findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndAttributeCode_PREDEFINED_ATTRIBUTE() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.PREDEFINED_ATTRIBUTE, false,
        AttributeServiceTest.STORE_ID);
    attribute.setId(id);
    when(this.repository.findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService)
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndAttributeCodeWithDefaultvalue() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = this.getDefaultAttribute();
    attribute.setId(id);
    attribute.setMandatory(true);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue
        .setPredefinedAllowedAttributeCode(attribute.getAttributeCode() + Constants.HYPHEN + Constants.DEFAULT);
    when(predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
        STORE_ID, id)).thenReturn(new ArrayList<>(Collections.singletonList(predefinedAllowedAttributeValue)));
    when(this.repository.findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(attribute);
    this.service.findDetailByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
        STORE_ID, id);
    assertEquals(0, attribute.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void testFindDetailByStoreIdAndIdWithEmptyId() {
    String id = UUID.randomUUID().toString();
    try {
      when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id)).thenReturn(null);
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.service.findDetailByStoreIdAndId(AttributeServiceTest.STORE_ID, id, false));
    } finally {
      verify(applicationContext).getBean(AttributeService.class);
      verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testRegenerateAllowedAttributeValue() throws Exception {
    String id = UUID.randomUUID().toString();
    String alId1 = UUID.randomUUID().toString();
    UUID.randomUUID().toString();

    Attribute oldAttribute = this.getDefaultAttribute();
    Attribute newAttribute = this.getDefaultAttribute();
    AllowedAttributeValue newAllowedAttributeValue = this.getDefaultAllowedAttributeValue();
    AllowedAttributeValue oldAllowedAttributeValue = this.getDefaultAllowedAttributeValue();
    newAllowedAttributeValue.setValue(AttributeServiceTest.NEW_VALUE);
    oldAllowedAttributeValue.setId(alId1);
    oldAllowedAttributeValue.setAttribute(oldAttribute);

    oldAttribute.setId(id);
    oldAttribute.getAllowedAttributeValues().add(oldAllowedAttributeValue);
    newAttribute.setId(id);
    newAttribute.getAllowedAttributeValues().add(newAllowedAttributeValue);

    List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
    productAttributeValues.add(new ProductAttributeValue());

    List<String> categoryIdList = new ArrayList<>();

    when(this.repository.findById(id)).thenReturn(Optional.of(oldAttribute));
    when(this.allowedAttributeValueService.findByStoreIdAndId(AttributeServiceTest.STORE_ID, alId1))
        .thenReturn(oldAllowedAttributeValue);
    when(this.productAttributeValueService.findByStoreIdAndAllowedAttributeValue(AttributeServiceTest.STORE_ID,
        oldAllowedAttributeValue)).thenReturn(productAttributeValues);
    when(this.applicationConfig.isRegenerateWhenDeleteAllowedAttribute()).thenReturn(true);
    when(this.categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
            oldAttribute.getId())).thenReturn(categoryIdList);

    this.service.regenerateAllowedAttributeValue(AttributeServiceTest.STORE_ID, oldAttribute, newAttribute);

    verify(this.categoryAttributeRepository, Mockito.times(1))
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(AttributeServiceTest.STORE_ID,
            oldAttribute.getId());
    verify(this.applicationCacheServiceBean, Mockito.times(1))
        .clearCategoryDetails(STORE_ID, categoryIdList);
    verify(this.repository, Mockito.times(1)).findById(id);
    verify(this.repository, Mockito.times(1)).saveAndFlush(oldAttribute);
    verify(this.allowedAttributeValueService).markForDeleteAllowedAttributeValue(AttributeServiceTest.STORE_ID, alId1);
    verify(this.productService).deleteProductItemsByProductAttributeValues(AttributeServiceTest.STORE_ID,
        productAttributeValues);
    verify(this.allowedAttributeValueService).findByStoreIdAndId(AttributeServiceTest.STORE_ID, alId1);
    verify(this.applicationConfig).isRegenerateWhenDeleteAllowedAttribute();
    verify(this.productAttributeValueService).findByStoreIdAndAllowedAttributeValue(AttributeServiceTest.STORE_ID,
        oldAllowedAttributeValue);
    verify(this.applicationCacheServiceBean, times(2))
        .evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, id);
    verify(this.applicationCacheServiceBean, times(2))
        .evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, oldAttribute.getAttributeCode());
    verify(this.applicationCacheServiceBean, times(2))
        .evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, id);
    verify(this.applicationCacheServiceBean, times(2))
        .evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, id);

  }

  @Test
  public void testSaveAttributeSuccessfully() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeCode("AT-2000001");
    attribute.setDsExtraction(true);
    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(attribute, savedAttribute);
    savedAttribute.setId(uuid);
    when(this.repository.saveAndFlush(attribute)).thenReturn(savedAttribute);
    when(domainEventPublisherService.publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(new AttributeDomainEventModel());
    Assertions.assertEquals(this.service.save(attribute), (uuid));
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, attribute.getAttributeCode());
    verify(this.applicationCacheServiceBean).evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    assertEquals(List.of(DS_EXTRACTION), attributeDomainEventModelArgumentCaptor.getValue().getUpdatedFields());
  }

  @Test
  public void testSaveAttributeSuccessfullyWithoutDsExtraction() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeCode("AT-2000001");
    attribute.setDsExtraction(false);
    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(attribute, savedAttribute);
    savedAttribute.setId(uuid);
    when(this.repository.saveAndFlush(attribute)).thenReturn(savedAttribute);
    when(domainEventPublisherService.publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(new AttributeDomainEventModel());
    Assertions.assertEquals(this.service.save(attribute), (uuid));
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, attribute.getAttributeCode());
    verify(this.applicationCacheServiceBean).evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    assertEquals(new ArrayList<>(), attributeDomainEventModelArgumentCaptor.getValue().getUpdatedFields());
  }

  @Test
  public void testSaveAttributeWithEmptyAttributeCode() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<>();
    allowedAttributeValues.add(this.getDefaultAllowedAttributeValue());
    attribute.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(this.getDefaultPredefinedAllowedAttributeValue());
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);

    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(attribute, savedAttribute);
    savedAttribute.setId(uuid);

    when(this.repository.getSequenceByAttributeCode("AT")).thenReturn(1L);
    when(this.allowedAttributeValueService.getSequence("AT-2000001")).thenReturn("00001");
    when(this.predefinedAllowedAttributeValueService.getSequence("AT-2000001")).thenReturn("00001");
    when(this.repository.saveAndFlush(attribute)).thenReturn(savedAttribute);
    when(domainEventPublisherService.publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.capture()))
        .thenReturn(new AttributeDomainEventModel());
    assertEquals("000001", this.service.getSequence("AT"));
    Assertions.assertEquals(this.service.save(attribute), (uuid));
    verify(this.repository, atLeastOnce()).getSequenceByAttributeCode("AT");
    verify(this.allowedAttributeValueService, atLeastOnce()).getSequence("AT-2000001");
    verify(this.predefinedAllowedAttributeValueService, atLeastOnce()).getSequence("AT-2000001");
    verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).saveAndFlush(attribute);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, attribute.getAttributeCode());
    verify(this.applicationCacheServiceBean).evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(domainEventPublisherService)
        .publishMasterAttributeInfoEvent(attributeDomainEventModelArgumentCaptor.getValue());
  }

  @Test
  public void testSaveAttributeWithEmptyAttributeCodeAndPredefinedAttribute() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<>();
    allowedAttributeValues.add(this.getDefaultAllowedAttributeValue());
    attribute.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues.add(this.getDefaultPredefinedAllowedAttributeValue());
    predefinedAllowedAttributeValues.add(
        new PredefinedAllowedAttributeValue(this.getDefaultAttribute(), Constants.HYPHEN, AttributeServiceTest.STORE_ID,
            0));
    attribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);

    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(attribute, savedAttribute);
    savedAttribute.setId(uuid);

    when(this.repository.getSequenceByAttributeCode("AT")).thenReturn(1L);
    when(this.allowedAttributeValueService.getSequence("AT-2000001")).thenReturn("00001");
    when(this.predefinedAllowedAttributeValueService.getSequence("AT-2000001")).thenReturn("00001");
    when(this.repository.saveAndFlush(attribute)).thenReturn(savedAttribute);
    assertEquals("000001", this.service.getSequence("AT"));
    Assertions.assertEquals(this.service.save(attribute), (uuid));
    verify(this.repository, times(2)).getSequenceByAttributeCode("AT");
    verify(this.allowedAttributeValueService, atLeastOnce()).getSequence("AT-2000001");
    verify(this.predefinedAllowedAttributeValueService).getSequence("AT-2000001");
    verify(this.repository).saveAndFlush(attribute);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictAttributeCacheByStoreIdAndAttributeId(STORE_ID, attribute.getAttributeCode());
    verify(this.applicationCacheServiceBean).evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);
    verify(this.applicationCacheServiceBean).evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(STORE_ID, uuid);  }

  @Test
  public void testSaveAttributeWithEmptyId() {
    Attribute attribute = this.getDefaultAttribute();
    attribute.setAttributeCode("AT-2000001");
    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    BeanUtils.copyProperties(attribute, savedAttribute);
    when(this.repository.findById(attribute.getId())).thenReturn(Optional.of(savedAttribute));
    try {
      this.service.save(attribute);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(AttributeServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, AttributeServiceTest.AT_LEAST_ONCE).findById(attribute.getId());
    }
  }

  @Test
  public void testUpdateAttributeNonExistenceEntity() {
    Attribute attribute = this.getDefaultAttribute();
    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    BeanUtils.copyProperties(attribute, savedAttribute);
    when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(attribute);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(AttributeServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
      verify(this.repository, Mockito.times(1)).findById(attribute.getId());
    }
  }

  @Test
  public void testUpdateAttributeSuccessfully() throws Exception {
    Attribute attribute = this.getDefaultAttribute();
    Attribute savedAttribute = new Attribute();
    String uuid = GdnUUIDHelper.generateUUID();
    attribute.setId(uuid);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    List<String> categoryIds = new ArrayList<>();
    BeanUtils.copyProperties(attribute, savedAttribute);
    when(this.repository.findById(attribute.getId())).thenReturn(Optional.of(savedAttribute));
    this.service.update(attribute);
    assertTrue(true);
    verify(this.repository).findById(attribute.getId());
    verify(this.repository).saveAndFlush(attribute);
    verify(this.categoryAttributeRepository)
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, attribute.getId());
    verify(this.applicationCacheServiceBean).clearCategoryDetails(STORE_ID, categoryIds);
  }

  @Test
  public void testUpdateCatalogWithEmptyId() {
    Attribute attribute = this.getDefaultAttribute();
    Attribute savedAttribute = new Attribute();
    BeanUtils.copyProperties(attribute, savedAttribute);
    try {
      this.service.update(attribute);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(AttributeServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        assertTrue(true);
      } else {
        assertTrue(false);
      }
    }
  }

  @Test
  public void findByNameTest() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndNameAndMarkForDeleteFalse(STORE_ID, DEFAULT_ATTRIBUTE_NAME))
        .thenReturn(attributeList);
    List<Attribute> response = this.service.findByName(STORE_ID, DEFAULT_ATTRIBUTE_NAME);
    Mockito.verify(this.repository)
        .findByStoreIdAndNameAndMarkForDeleteFalse(STORE_ID, DEFAULT_ATTRIBUTE_NAME);
    assertEquals(ATTRIBUTE_CODE,response.get(0).getAttributeCode());
    assertFalse(response.get(0).isBasicView());
  }
  
  @Test
  public void testGetAttributeDetailByCategoryCodeWithoutOptionsSuccessfully() throws Exception {
    when(this.repository.getAttributeDetailByCategoryCodeWithoutOption(STORE_ID, CATEGORY_CODE))
        .thenReturn(this.listAttributeSummaryBuilder());
    this.service.getAttributeDetailByCategoryCodeWithoutOptions(STORE_ID, CATEGORY_CODE);
    verify(this.repository).getAttributeDetailByCategoryCodeWithoutOption(STORE_ID, CATEGORY_CODE);
  }
  
  @Test
  public void testGetAttributeDetailByCategoryCodeWithoutOptionsException() throws Exception {
    when(this.repository.getAttributeDetailByCategoryCodeWithoutOption(STORE_ID, CATEGORY_CODE))
      .thenThrow(RuntimeException.class);
    try{
      this.service.getAttributeDetailByCategoryCodeWithoutOptions(STORE_ID, CATEGORY_CODE);
    } catch(Exception e){
      verify(this.repository).getAttributeDetailByCategoryCodeWithoutOption(STORE_ID, CATEGORY_CODE);
    }
  }

  private List<AttributeSummaryDTO> listAttributeSummaryBuilder() {
    AttributeSummaryDTO attr =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, true, true, false,
            false, false, true);
    AttributeSummaryDTO attr2 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE2_NAME, false, true, false,
            false, false, false);
    AttributeSummaryDTO attr3 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.DESCRIPTIVE_ATTRIBUTE, ATTRIBUTE3_NAME, true, true, false,
            false, false, false);
    List<AttributeSummaryDTO> attrList = new ArrayList<>();
    attrList.add(attr);
    attrList.add(attr2);
    attrList.add(attr3);
    return attrList;
  }

  @Test
  public void getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalseTest() {
    Mockito.when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(attribute);
    Attribute response =
        service.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    Mockito.verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    assertEquals(ATTRIBUTE1_NAME, response.getName());
  }

  @Test
  public void findByNameLikeIgnoreCaseTest() {
    Page<Attribute> attributePages = this.getDefaultAttributePage();
    Mockito
        .when(repository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE1_NAME, pageable))
        .thenReturn(attributePages);
    this.service.findByNameLikeIgnoreCase(STORE_ID, ATTRIBUTE1_NAME, pageable);
    Mockito.verify(repository)
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE1_NAME, pageable);
  }

  @Test
  public void getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalseTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    when(repository.findByIdInAndStoreIdAndMarkForDeleteFalse(attributeIds, STORE_ID))
        .thenReturn(Collections.singletonList(attribute));
    service.getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(STORE_ID, attributeIds);
    verify(repository).findByIdInAndStoreIdAndMarkForDeleteFalse(attributeIds, STORE_ID);
  }

  @Test
  public void getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalseExceptionTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(STORE_ID, attributeIds));
    } finally {
      verify(repository).findByIdInAndStoreIdAndMarkForDeleteFalse(attributeIds, STORE_ID);
    }
  }

  @Test
  public void findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalseDefiningAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME))
        .thenReturn(Collections.singletonList(attribute));
    when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new AllowedAttributeValue()));
    List<Attribute> attributes = service.findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE1_NAME,
        AttributeType.DEFINING_ATTRIBUTE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    assertEquals(1, attributes.size());
    assertEquals(ATTRIBUTE_ID, attributes.get(0).getId());
    assertEquals(1, attributes.get(0).getAllowedAttributeValues().size());
  }

  @Test
  public void findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalsePredefinedAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME))
        .thenReturn(Collections.singletonList(attribute));
    when(predefinedAllowedAttributeValueService
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    List<Attribute> attributes = service.findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE1_NAME,
        AttributeType.PREDEFINED_ATTRIBUTE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    verify(predefinedAllowedAttributeValueService)
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    assertEquals(1, attributes.size());
    assertEquals(ATTRIBUTE_ID, attributes.get(0).getId());
    assertEquals(1, attributes.get(0).getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalseDescriptiveAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(Collections.singletonList(attribute));
    List<Attribute> attributes = service.findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE1_NAME,
        AttributeType.DESCRIPTIVE_ATTRIBUTE);
    verify(repository).findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.DESCRIPTIVE_ATTRIBUTE, ATTRIBUTE1_NAME,ATTRIBUTE1_NAME);
    assertEquals(1, attributes.size());
    assertEquals(ATTRIBUTE_ID, attributes.get(0).getId());
    assertEquals(0, attributes.get(0).getPredefinedAllowedAttributeValues().size());
    assertEquals(0, attributes.get(0).getAllowedAttributeValues().size());
  }

  @Test
  public void findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalseAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(new ArrayList<>());
    List<Attribute> attributes = service.findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE1_NAME,
        AttributeType.PREDEFINED_ATTRIBUTE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    assertEquals(0, attributes.size());
  }

  @Test
  public void findAttributeByCodeOrNameAndValueCodeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new AllowedAttributeValue()));
    Attribute attribute = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DEFINING_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService).getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    assertEquals(ATTRIBUTE_ID, attribute.getId());
    assertEquals(1, attribute.getAllowedAttributeValues().size());
  }

  @Test
  public void findAttributeByCodeOrNameAndValueCodeEmptyAttributesTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.DEFINING_ATTRIBUTE,
        ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(new ArrayList<>());
    service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DEFINING_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(repository).
        findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME,
        ATTRIBUTE1_NAME);
  }

  @Test
  public void findAttributeByCodeOrNameAndValueNameMultipleAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(predefinedAllowedAttributeValueService
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.PREDEFINED_ATTRIBUTE,  ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(Arrays.asList(attribute, attribute));
    service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.PREDEFINED_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2))
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(repository).findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,  AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
  }

  @Test
  public void findAttributeByCodeOrNameAndValueNameMultipleDefiningAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE))
        .thenReturn(Arrays.asList(new AllowedAttributeValue()));
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(Arrays.asList(attribute, attribute));
    service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DEFINING_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
    verify(repository).findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,  AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    verify(allowedAttributeValueService, times(2)).
        getAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
  }


  @Test
  public void findAttributeByCodeOrNameAndValueNameTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(predefinedAllowedAttributeValueService
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,  AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(Collections.singletonList(attribute));
    Attribute attribute = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.PREDEFINED_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService)
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(repository).findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID, AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    assertEquals(ATTRIBUTE_ID, attribute.getId());
    assertEquals(1, attribute.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void findAttributeByCodeOrNameAndValueNameDescriptiveAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.DESCRIPTIVE_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME))
        .thenReturn(Collections.singletonList(attribute));
    Attribute attribute = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.DESCRIPTIVE_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    assertEquals(ATTRIBUTE_ID, attribute.getId());
    assertEquals(0, attribute.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void findAttributeByCodeOrNameAndValueTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeList.add(0, attribute);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(predefinedAllowedAttributeValueService
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(attributeList);
    Attribute attribute = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.PREDEFINED_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2))
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    assertEquals(ATTRIBUTE_ID, attribute.getId());
    assertEquals(1, attribute.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void findAttributeByCodeOrNameAndValueDefiningTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeList.add(0, attribute);
    attributeList.stream().forEach(attribute1 -> attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE));
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE))
        .thenReturn(Arrays.asList(new AllowedAttributeValue()));
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(attributeList);
    service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DEFINING_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService)
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    verify(allowedAttributeValueService).findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
  }

  @Test
  public void findAttributeByCodeOrNameAndValueDefiningValueNotFoundTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeList.add(0, attribute);
    attributeList.stream().forEach(attribute1 -> attribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE));
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(allowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE))
        .thenReturn(new ArrayList<>());
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(attributeList);
    Attribute response = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DEFINING_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(allowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.DEFINING_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    assertNull(response);
  }

  @Test
  public void findAttributeByCodeOrNameAndValueAttributeNotFoundTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attributeList.add(0, attribute);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(null);
    when(predefinedAllowedAttributeValueService
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    when(predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE))
        .thenReturn(new ArrayList<>());
    when(repository.findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
        AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME)).thenReturn(attributeList);
    Attribute response = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.PREDEFINED_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService, times(2))
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    verify(predefinedAllowedAttributeValueService, times(2))
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(STORE_ID, attribute, VALUE);
    verify(repository)
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(STORE_ID,
            AttributeType.PREDEFINED_ATTRIBUTE, ATTRIBUTE1_NAME, ATTRIBUTE1_NAME);
    assertNull(response);
  }

  @Test
  public void findAttributeByCodeOrNameAndValueCodeForPredefinedAttributeTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(predefinedAllowedAttributeValueService
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID))
        .thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    Attribute response = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.PREDEFINED_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    verify(predefinedAllowedAttributeValueService)
        .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(STORE_ID, ATTRIBUTE_ID);
    assertEquals(ATTRIBUTE_ID, response.getId());
    assertEquals(1, response.getPredefinedAllowedAttributeValues().size());
  }

  @Test
  public void findAttributeByCodeOrNameAndValueCodeForDescriptiveTest() {
    List<String> attributeIds = new ArrayList<>();
    attributeIds.add(ATTRIBUTE_ID);
    attribute.setId(ATTRIBUTE_ID);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    when(applicationContext.getBean(AttributeService.class)).thenReturn(attributeService);
    when(attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    Attribute response = service.findAttributeByCodeOrNameAndValue(STORE_ID, ATTRIBUTE_CODE, ATTRIBUTE1_NAME, VALUE,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    verify(applicationContext).getBean(AttributeService.class);
    verify(attributeService).getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
    assertEquals(ATTRIBUTE_ID, response.getId());
  }

  @Test
  public void findDetailByStoreIdAndAttributeCodeListTest() {
    when(repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(ATTRIBUTE_CODE)))
        .thenReturn(Arrays.asList(attribute));
    List<Attribute> attributes = service.findDetailByStoreIdAndAttributeCodeList(STORE_ID, Arrays.asList(ATTRIBUTE_CODE));
    verify(repository).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(ATTRIBUTE_CODE));
    assertNotNull(attributes);
    assertFalse(attributes.isEmpty());
  }

  @Test
  public void findProductAttributeValuesByProductCodeAndAttributeCodeTest() {
    attribute.setId(ID);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    productAttributeMap.put(productAttribute.getId(), productAttribute);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    productAttributes.add(productAttribute);
    product.setProductAttributes(productAttributes);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(
        product);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(productAttributeRepository.findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId())).thenReturn(productAttribute);
    when(productAttributeService.getProductAttributeValues(STORE_ID, productAttributeMap)).thenReturn(productAttributeMap);
    String response = service.findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        attribute.getAttributeCode());
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(productAttributeRepository).findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId());
    verify(productAttributeService).getProductAttributeValues(STORE_ID, productAttributeMap);
  }

  @Test
  public void findProductAttributeValuesByProductCodeAndAttributeCodeAttributeValueNullTest() {
    attribute.setId(ID);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    productAttributeMap.put(productAttribute.getId(), productAttribute);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    productAttributes.add(productAttribute);
    product.setProductAttributes(productAttributes);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(
        product);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(productAttributeRepository.findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId())).thenReturn(null);
    when(productAttributeService.getProductAttributeValues(STORE_ID, productAttributeMap)).thenReturn(productAttributeMap);
    String response = service.findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        attribute.getAttributeCode());
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(productAttributeRepository).findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId());
  }

  @Test
  public void findProductAttributeValuesByProductCodeAndAttributeCodeProductAttributeValueMfdTrueTest() {
    attribute.setId(ID);
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue("-");
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    productAttributeMap.put(productAttribute.getId(), productAttribute);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    productAttributes.add(productAttribute);
    product.setProductAttributes(productAttributes);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(
        product);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(productAttributeRepository.findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId())).thenReturn(productAttribute);
    when(productAttributeService.getProductAttributeValues(STORE_ID, productAttributeMap)).thenReturn(productAttributeMap);
    String response = service.findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        attribute.getAttributeCode());
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(productAttributeRepository).findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId());
    verify(productAttributeService).getProductAttributeValues(STORE_ID, productAttributeMap);
  }

  @Test
  public void findProductAttributeValuesByProductCodeAndAttributeCodeProductAttributeValueMfdFalseTest() {
    attribute.setId(ID);
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(true);
    productAttributeValue.setDescriptiveAttributeValue("-");
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    productAttributeMap.put(productAttribute.getId(), productAttribute);
    List<ProductAttribute> productAttributes = new ArrayList<>();
    productAttributes.add(productAttribute);
    product.setProductAttributes(productAttributes);
    when(productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(
        product);
    when(repository.findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE)).thenReturn(attribute);
    when(productAttributeRepository.findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId())).thenReturn(productAttribute);
    when(productAttributeService.getProductAttributeValues(STORE_ID, productAttributeMap)).thenReturn(productAttributeMap);
    String response = service.findProductAttributeValuesByProductCodeAndAttributeCode(STORE_ID, PRODUCT_CODE,
        attribute.getAttributeCode());
    verify(productRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(repository).findByStoreIdAndAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    verify(productAttributeRepository).findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(STORE_ID,
        attribute.getId(), product.getId());
    verify(productAttributeService).getProductAttributeValues(STORE_ID, productAttributeMap);
  }

  @Test
  public void getAttributeBasicDetailByEmptyCategoryCodeTest() {
    List<AttributeBasicDetailDTO> attributeBasicDetailByCategoryCode =
        service.getAttributeBasicDetailByCategoryCode(STORE_ID, StringUtils.EMPTY);
    assertTrue(CollectionUtils.isEmpty(attributeBasicDetailByCategoryCode));
  }

  @Test
  public void getAttributeBasicDetailByCategoryCodeTest() {
    service.getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(repository).getAttributeBasicDetailByCategoryCode(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void findCategoryCodesByAttributeCodeTest() {
    List<String> categoryCodes = Arrays.asList(CATEGORY_CODE, CATEGORY_CODE_2);
    Mockito.when(categoryServiceBean.findCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(categoryCodes);
    List<String> stringList = service.findCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    assertFalse(stringList.isEmpty());
    Mockito.verify(categoryServiceBean).findCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
  }

  @Test
  public void testFindAttributesByStoreIdAndAttributeCodeList_Found() {
    List<String> attributeCodes = Arrays.asList(attribute.getAttributeCode());
    List<Attribute> expectedAttributes = Arrays.asList(attribute);
    when(repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, attributeCodes)).thenReturn(
        expectedAttributes);
    List<Attribute> result = service.findAttributesByStoreIdAndAttributeCodeList(STORE_ID, attributeCodes);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(attribute.getAttributeCode(), result.get(0).getAttributeCode());
    verify(repository, times(1)).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, attributeCodes);
  }

  @Test
  public void testFindAttributesByStoreIdAndAttributeCodeList_FoundOptimisationEnabledTrue() {
    ReflectionTestUtils.setField(service, "dsAttributeExtractionOptimisationEnabled", true);
    List<String> attributeCodes = Arrays.asList(attribute.getAttributeCode());
    List<Attribute> expectedAttributes = Arrays.asList(attribute);
    when(repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, attributeCodes)).thenReturn(
        expectedAttributes);
    List<Attribute> result = service.findAttributesByStoreIdAndAttributeCodeList(STORE_ID, attributeCodes);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(attribute.getAttributeCode(), result.get(0).getAttributeCode());
    verify(repository, times(1)).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, attributeCodes);
  }


  @Test
  public void testFindAttributesByStoreIdAndAttributeCodeList_NotFound() {
    List<String> attributeCodes = Arrays.asList(attribute.getAttributeCode());
    when(repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, attributeCodes)).thenReturn(
        Collections.emptyList());
    List<Attribute> result = service.findAttributesByStoreIdAndAttributeCodeList(STORE_ID, attributeCodes);
    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(repository, times(1)).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID, attributeCodes);
  }

  @Test
  void testFindDetailByStoreIdAndAttributeCodeForPredefinedMultiValue() {
    String id = UUID.randomUUID().toString();
    Attribute attributeUpdated =
      new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.PREDEFINED_MULTIVALUE,
        false, AttributeServiceTest.STORE_ID);
    attributeUpdated.setId(id);
    when(this.repository.findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID,
      ATTRIBUTE_CODE)).thenReturn(attributeUpdated);
    this.service.findDetailByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID,
      ATTRIBUTE_CODE);
    verify(
      predefinedAllowedAttributeValueService).getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
      STORE_ID, id);
  }

  @Test
  void testFindDetailByStoreIdAndAttributeCodeForDescriptiveMultiValueTest() {
    String id = UUID.randomUUID().toString();
    Attribute attribute = new Attribute(AttributeServiceTest.ATTRIBUTE1_NAME, AttributeType.DESCRIPTIVE_MULTIVALUE,
      false,
      AttributeServiceTest.STORE_ID);
    attribute.setId(id);
    when(this.repository.findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE))
      .thenReturn(attribute);
    this.service.findDetailByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
    verify(applicationContext).getBean(AttributeService.class);
    verify(this.repository).findByStoreIdAndAttributeCode(AttributeServiceTest.STORE_ID, ATTRIBUTE_CODE);
  }

  @Test
  void filterSellerHiddenAttributes_FeatureDisabled_ShouldNotFilter() {
    attributeAndValueMap.put(request1, attrHidden);
    attributeAndValueMap.put(request2, paavHidden);
    attributeAndValueMap.put(request3, attrNotHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, false, PRODUCT_CODE);

    assertEquals(3, attributeAndValueMap.size());
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_AttributeHidden_ShouldFilter() {
    attributeAndValueMap.put(request1, attrHidden);
    attributeAndValueMap.put(request2, attrNotHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);

    assertEquals(1, attributeAndValueMap.size());
    assertTrue(attributeAndValueMap.containsKey(request2));
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_PaavHidden_ShouldFilter() {
    attributeAndValueMap.put(request1, paavHidden);
    attributeAndValueMap.put(request2, paavNotHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);

    assertEquals(1, attributeAndValueMap.size());
    assertTrue(attributeAndValueMap.containsKey(request2));
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_MixedHiddenAndNotHidden_ShouldFilterHidden() {
    attributeAndValueMap.put(request1, attrHidden);
    attributeAndValueMap.put(request2, paavHidden);
    attributeAndValueMap.put(request3, attrNotHidden);
    attributeAndValueMap.put(request4, paavNotHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);

    assertEquals(2, attributeAndValueMap.size());
    assertTrue(attributeAndValueMap.containsKey(request3));
    assertTrue(attributeAndValueMap.containsKey(request4));
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_OtherValueType_ShouldNotFilter() {
    attributeAndValueMap.put(request1, "SomeStringValue"); // Different type
    attributeAndValueMap.put(request2, attrHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);

    assertEquals(1, attributeAndValueMap.size());
    assertTrue(attributeAndValueMap.containsKey(request1)); // String value should remain
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_NullValue_ShouldNotFilter() {
    attributeAndValueMap.put(request1, null);
    attributeAndValueMap.put(request2, attrHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);

    assertEquals(1, attributeAndValueMap.size());
    assertTrue(attributeAndValueMap.containsKey(request1)); // Null value entry should remain
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_PaavWithNullAttribute_ShouldNotFilter() {
    PredefinedAllowedAttributeValue paavWithNullAttr = mock(PredefinedAllowedAttributeValue.class);
    when(paavWithNullAttr.getAttribute()).thenReturn(null);
    attributeAndValueMap.put(request1, paavWithNullAttr);
    attributeAndValueMap.put(request2, attrHidden);

    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);

    assertEquals(1, attributeAndValueMap.size());
    assertTrue(attributeAndValueMap.containsKey(request1)); // Entry with PAAV returning null attribute should remain
  }

  @Test
  void filterSellerHiddenAttributes_FeatureEnabled_EmptyMap_ShouldNotThrowException() {
    service.filterSellerHiddenAttributes(attributeAndValueMap, true, PRODUCT_CODE);
    assertTrue(attributeAndValueMap.isEmpty());
  }

  @Test
  void getAttributeAndValueMap_EmptyInputs_ShouldReturnEmptyMap() {
    Map<AttributeAndValueByTypeRequest, Object> result = service.getAttributeAndValueMap(
        STORE_ID, Collections.emptySet(), Collections.emptySet(), Collections.emptySet(), Collections.emptyList(), true,
      PRODUCT_CODE);

    assertTrue(result.isEmpty());
    verify(repository, times(0)).findByIdInAndStoreIdAndMarkForDeleteFalse(anyList(), anyString());
    verify(allowedAttributeValueService, times(0)).findByStoreIdAndIds(anyString(), anySet());
    verify(predefinedAllowedAttributeValueService, times(0)).findByStoreIdAndIds(anyString(),
      anySet());
    verify(predefinedAllowedAttributeValueService, times(0)).findByStoreIdAndPredefinedAllowedAttributeCodes(anyString(), anySet());
  }

  @Test
  void getAttributeAndValueMap_OnlyAttributes_ShouldReturnAttributeMap() {
    String attrId1 = ATTR_HIDDEN;
    Attribute attr1 = new Attribute();
    attr1.setId(attrId1);
    when(
      repository.findByIdInAndStoreIdAndMarkForDeleteFalse(List.of(attrId1), STORE_ID)).thenReturn(
      List.of(attr1));
    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, Collections.emptySet(), Collections.emptySet(),
        Collections.emptySet(), List.of(attrId1), false, PRODUCT_CODE);

    assertEquals(1, result.size());
    assertTrue(result.containsKey(
      new AttributeAndValueByTypeRequest(attrId1, AttributeAndValueByTypeRequest.type.ATTRIBUTE)));
    assertEquals(attr1, result.get(
      new AttributeAndValueByTypeRequest(attrId1, AttributeAndValueByTypeRequest.type.ATTRIBUTE)));

    verify(repository).findByIdInAndStoreIdAndMarkForDeleteFalse(List.of(attrId1), STORE_ID);
  }

  @Test
  void getAttributeAndValueMap_OnlyAllowedValues_ShouldReturnAllowedValueMap() {
    String allowedId1 = DUPLICATE_ALLOWED_ID;
    AllowedAttributeValue aav1 = new AllowedAttributeValue();
    aav1.setId(allowedId1);
    when(allowedAttributeValueService.findByStoreIdAndIds(STORE_ID, Set.of(allowedId1))).thenReturn(
      List.of(aav1));

    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, Set.of(allowedId1), Collections.emptySet(),
        Collections.emptySet(), Collections.emptyList(), false, PRODUCT_CODE);

    assertEquals(1, result.size());
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(allowedId1,
      AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE)));
    assertEquals(aav1, result.get(new AttributeAndValueByTypeRequest(allowedId1,
      AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE)));

    verify(allowedAttributeValueService).findByStoreIdAndIds(STORE_ID, Set.of(allowedId1));
  }

  @Test
  void getAttributeAndValueMap_OnlyPredefinedById_ShouldReturnPredefinedByIdMap() {
    String predefinedId1 = "predefId1";
    PredefinedAllowedAttributeValue paav1 = new PredefinedAllowedAttributeValue();
    paav1.setId(predefinedId1);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndIds(STORE_ID,
      Set.of(predefinedId1))).thenReturn(List.of(paav1));

    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, Collections.emptySet(), Set.of(predefinedId1),
        Collections.emptySet(), Collections.emptyList(), false, PRODUCT_CODE);

    assertEquals(1, result.size());
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(predefinedId1,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID)));
    assertEquals(paav1, result.get(new AttributeAndValueByTypeRequest(predefinedId1,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID)));

    verify(predefinedAllowedAttributeValueService).findByStoreIdAndIds(STORE_ID,
      Set.of(predefinedId1));
  }

  @Test
  void getAttributeAndValueMap_OnlyPredefinedByCode_ShouldReturnPredefinedByCodeMap() {
    String predefinedCode1 = "PREDEF-CODE-1";
    PredefinedAllowedAttributeValue paav1 = new PredefinedAllowedAttributeValue();
    paav1.setPredefinedAllowedAttributeCode(predefinedCode1);
    when(predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCodes(
      STORE_ID, Set.of(predefinedCode1))).thenReturn(List.of(paav1));

    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, Collections.emptySet(), Collections.emptySet(),
        Set.of(predefinedCode1), Collections.emptyList(), false, PRODUCT_CODE);

    assertEquals(1, result.size());
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(predefinedCode1,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_CODE)));
    assertEquals(paav1, result.get(new AttributeAndValueByTypeRequest(predefinedCode1,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_CODE)));

    verify(predefinedAllowedAttributeValueService).findByStoreIdAndPredefinedAllowedAttributeCodes(
      STORE_ID, Set.of(predefinedCode1));
  }

  @Test
  void getAttributeAndValueMap_MixedInputs_NoFiltering_ShouldReturnCombinedMap() {
    String attrId1 = "attr1";
    Attribute attr1 = new Attribute();
    attr1.setId(attrId1);
    attr1.setHideForSeller(false);

    String allowedId1 = ATTRIBUTE_ID;
    AllowedAttributeValue aav1 = new AllowedAttributeValue();
    aav1.setId(allowedId1);

    String predefinedId1 = ATTRIBUTE_CODE;
    PredefinedAllowedAttributeValue paavById1 = new PredefinedAllowedAttributeValue();
    paavById1.setId(predefinedId1);
    paavById1.setAttribute(attr1); // Link to non-hidden attribute

    String predefinedCode1 = ATTRIBUTE3_NAME;
    PredefinedAllowedAttributeValue paavByCode1 = new PredefinedAllowedAttributeValue();
    paavByCode1.setPredefinedAllowedAttributeCode(predefinedCode1);
    paavByCode1.setAttribute(attr1); // Link to non-hidden attribute

    // --- Mock Service Calls ---
    when(
      repository.findByIdInAndStoreIdAndMarkForDeleteFalse(List.of(attrId1), STORE_ID)).thenReturn(
      List.of(attr1));
    when(allowedAttributeValueService.findByStoreIdAndIds(STORE_ID, Set.of(allowedId1))).thenReturn(
      List.of(aav1));
    when(predefinedAllowedAttributeValueService.findByStoreIdAndIds(STORE_ID,
      Set.of(predefinedId1))).thenReturn(List.of(paavById1));
    when(predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCodes(
      STORE_ID, Set.of(predefinedCode1))).thenReturn(List.of(paavByCode1));
    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, Set.of(allowedId1), Set.of(predefinedId1),
        Set.of(predefinedCode1), List.of(attrId1), false, PRODUCT_CODE); // Filtering disabled
    assertEquals(4, result.size()); // Expect all 4 entries
    assertTrue(result.containsKey(
      new AttributeAndValueByTypeRequest(attrId1, AttributeAndValueByTypeRequest.type.ATTRIBUTE)));
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(allowedId1,
      AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE)));
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(predefinedId1,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID)));
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(predefinedCode1,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_CODE)));
    verify(repository).findByIdInAndStoreIdAndMarkForDeleteFalse(List.of(attrId1), STORE_ID);
    verify(allowedAttributeValueService).findByStoreIdAndIds(STORE_ID, Set.of(allowedId1));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndIds(STORE_ID,
      Set.of(predefinedId1));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndPredefinedAllowedAttributeCodes(
      STORE_ID, Set.of(predefinedCode1));
  }

  @Test
  void getAttributeAndValueMap_MixedInputs_WithFiltering_ShouldReturnFilteredMap() {
    String attrIdHidden = ATTR_HIDDEN;
    Attribute attrHidden = new Attribute();
    attrHidden.setId(attrIdHidden);
    attrHidden.setHideForSeller(true); // Hidden

    String attrIdVisible = ATTR_VISIBLE;
    Attribute attrVisible = new Attribute();
    attrVisible.setId(attrIdVisible);
    attrVisible.setHideForSeller(false); // Visible

    String allowedId1 = ATTRIBUTE_ID;
    AllowedAttributeValue aav1 = new AllowedAttributeValue();
    aav1.setId(allowedId1);

    String predefinedIdHidden = PREDEF_ID_THROWS;
    PredefinedAllowedAttributeValue paavByIdHidden = new PredefinedAllowedAttributeValue();
    paavByIdHidden.setId(predefinedIdHidden);
    paavByIdHidden.setAttribute(attrHidden); // Linked to hidden attribute

    String predefinedCodeVisible = ATTRIBUTE1_NAME;
    PredefinedAllowedAttributeValue paavByCodeVisible = new PredefinedAllowedAttributeValue();
    paavByCodeVisible.setPredefinedAllowedAttributeCode(predefinedCodeVisible);
    paavByCodeVisible.setAttribute(attrVisible); // Linked to visible attribute
    when(repository.findByIdInAndStoreIdAndMarkForDeleteFalse(List.of(attrIdHidden, attrIdVisible),
      STORE_ID)).thenReturn(List.of(attrHidden, attrVisible)); // Return both attributes
    when(allowedAttributeValueService.findByStoreIdAndIds(STORE_ID, Set.of(allowedId1))).thenReturn(
      List.of(aav1));
    when(predefinedAllowedAttributeValueService.findByStoreIdAndIds(STORE_ID,
      Set.of(predefinedIdHidden))).thenReturn(
      List.of(paavByIdHidden)); // Return the one linked to hidden attr
    when(predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCodes(
      STORE_ID, Set.of(predefinedCodeVisible))).thenReturn(
      List.of(paavByCodeVisible)); // Return the one linked to visible attr
    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, Set.of(allowedId1), Set.of(predefinedIdHidden),
        Set.of(predefinedCodeVisible), List.of(attrIdHidden, attrIdVisible), true,
        PRODUCT_CODE); // Filtering ENABLED
    assertEquals(3, result.size()); // Expect 3 entries: attrVisible, aav1, paavByCodeVisible
    assertFalse(result.containsKey(new AttributeAndValueByTypeRequest(attrIdHidden,
      AttributeAndValueByTypeRequest.type.ATTRIBUTE)));
    assertFalse(result.containsKey(new AttributeAndValueByTypeRequest(predefinedIdHidden,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID)));
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(attrIdVisible,
      AttributeAndValueByTypeRequest.type.ATTRIBUTE)));
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(allowedId1,
      AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE)));
    assertTrue(result.containsKey(new AttributeAndValueByTypeRequest(predefinedCodeVisible,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_CODE)));

    // --- Verify Mocks ---
    verify(repository).findByIdInAndStoreIdAndMarkForDeleteFalse(
      List.of(attrIdHidden, attrIdVisible), STORE_ID);
    verify(allowedAttributeValueService).findByStoreIdAndIds(STORE_ID, Set.of(allowedId1));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndIds(STORE_ID,
      Set.of(predefinedIdHidden));
    verify(predefinedAllowedAttributeValueService).findByStoreIdAndPredefinedAllowedAttributeCodes(
      STORE_ID, Set.of(predefinedCodeVisible));
  }

  @Test
  void filterSellerHiddenAttributes_ExceptionOnGetAttribute_ShouldCatchAndContinue() {
    AttributeAndValueByTypeRequest keyThrows = new AttributeAndValueByTypeRequest(PREDEF_ID_THROWS,
      AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID);
    PredefinedAllowedAttributeValue paavThrows = mock(PredefinedAllowedAttributeValue.class);
    when(paavThrows.getAttribute()).thenThrow(new RuntimeException("Error"));
    String attrIdHidden = ATTRIBUTE1_NAME;
    AttributeAndValueByTypeRequest keyHidden = new AttributeAndValueByTypeRequest(attrIdHidden,
      AttributeAndValueByTypeRequest.type.ATTRIBUTE);
    Attribute attrHidden = new Attribute();
    attrHidden.setId(attrIdHidden);
    attrHidden.setHideForSeller(true);
    String predefinedIdVisible = ATTRIBUTE2_NAME;
    AttributeAndValueByTypeRequest keyVisible =
      new AttributeAndValueByTypeRequest(predefinedIdVisible,
        AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID);
    Attribute attrVisible = new Attribute();
    attrVisible.setHideForSeller(false);
    PredefinedAllowedAttributeValue paavVisible = new PredefinedAllowedAttributeValue();
    paavVisible.setId(predefinedIdVisible);
    paavVisible.setAttribute(attrVisible);
    Map<AttributeAndValueByTypeRequest, Object> testMap = new HashMap<>();
    testMap.put(keyThrows, paavThrows);
    testMap.put(keyHidden, attrHidden);
    testMap.put(keyVisible, paavVisible);
    service.filterSellerHiddenAttributes(testMap, true, PRODUCT_CODE);
    assertEquals(2, testMap.size());
    assertTrue(testMap.containsKey(keyThrows));
    assertEquals(paavThrows, testMap.get(keyThrows));
    assertFalse(testMap.containsKey(keyHidden));
    assertTrue(testMap.containsKey(keyVisible));
    assertEquals(paavVisible, testMap.get(keyVisible));
  }

  @Test
  void getAttributeAndValueMap_DuplicateAllowedValueId_ShouldMergeAndKeepFirst() {
    String duplicateAllowedId = DUPLICATE_ALLOWED_ID;
    AllowedAttributeValue aav1 = new AllowedAttributeValue();
    aav1.setId(duplicateAllowedId);
    aav1.setValue(VALUE);

    AllowedAttributeValue aav2 = new AllowedAttributeValue();
    aav2.setId(duplicateAllowedId); // Same ID as aav1
    aav2.setValue(VALUE.concat(VALUE));

    Set<String> allowedIds = Set.of(duplicateAllowedId);

    when(allowedAttributeValueService.findByStoreIdAndIds(STORE_ID, allowedIds)).thenReturn(
      List.of(aav1, aav2));

    Map<AttributeAndValueByTypeRequest, Object> result =
      service.getAttributeAndValueMap(STORE_ID, allowedIds, Collections.emptySet(),
        Collections.emptySet(), Collections.emptyList(), false, PRODUCT_CODE);
    assertEquals(1, result.size());
    AttributeAndValueByTypeRequest expectedKey =
      new AttributeAndValueByTypeRequest(duplicateAllowedId,
        AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE);

    assertTrue(result.containsKey(expectedKey));
    assertEquals(aav1, result.get(expectedKey));
    assertNotEquals(aav2, result.get(expectedKey));
    verify(allowedAttributeValueService).findByStoreIdAndIds(STORE_ID, allowedIds);
    verify(repository, times(0)).findByIdInAndStoreIdAndMarkForDeleteFalse(anyList(), anyString());
    verify(predefinedAllowedAttributeValueService, times(0)).findByStoreIdAndIds(anyString(),
      anySet());
    verify(predefinedAllowedAttributeValueService,
      times(0)).findByStoreIdAndPredefinedAllowedAttributeCodes(anyString(), anySet());
  }

  @Test
  public void findDetailByStoreIdAndIdAndValuePredefinedAttributeFetchIgnoreCaseTest() {
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    when(repository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID)).thenReturn(
      attribute);
    when(predefinedAllowedAttributeValueService.getAttributeIdAndValueLikeIgnoreCase(STORE_ID,
      attribute.getId(), VALUE)).thenReturn(List.of(getDefaultPredefinedAllowedAttributeValue()));

    AttributeResponse attributeResponse =
      service.findDetailByStoreIdAndIdAndValue(STORE_ID, ATTRIBUTE_ID, VALUE, true);

    verify(repository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_ID);
    verify(predefinedAllowedAttributeValueService).getAttributeIdAndValueLikeIgnoreCase(STORE_ID,
      attribute.getId(), VALUE);
    verify(applicationContext).getBean(AttributeService.class);

    assertEquals(VALUE, attributeResponse.getPredefinedAllowedAttributeValues().get(0).getValue());
    assertTrue(attributeResponse.getAllowedAttributeValues().isEmpty());
  }

}
