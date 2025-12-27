package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.service.CategoryServiceWrapper;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
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
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryAndHierarchyDto;
import com.gdn.x.productcategorybase.dto.CategoryAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndNameDTO;
import com.gdn.x.productcategorybase.dto.CategoryDetailDTO;
import com.gdn.x.productcategorybase.dto.CategoryErrorDto;
import com.gdn.x.productcategorybase.dto.CategoryInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.MinWholesaleDiscountDTO;
import com.gdn.x.productcategorybase.dto.WholesaleConfigDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.ShippingRequest;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;
import com.gdn.x.productcategorybase.service.CategoryRestrictedKeywordService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.CategoryWholesaleConfigService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

public class CategoryServiceWrapperImplTest {

  @Mock
  private CategoryService categoryService;

  @Mock
  private CategoryShippingService categoryShippingService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AttributeService attributeService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  @Mock
  private CategoryRestrictedKeywordService categoryRestrictedKeywordService;

  @Mock
  private CategoryWholesaleConfigService categoryWholesaleConfigService;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private OriginalSalesCategoryService oscService;

  @Mock
  private CategoryReferenceService categoryReferenceService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ApplicationContext applicationContext;

  @Captor
  private ArgumentCaptor<CategoryShipping> categoryShippingArgumentCaptor;

  @Captor
  private ArgumentCaptor<Category> categoryArgumentCaptor;

  @Captor
  private ArgumentCaptor<ShippingRequest> shippingRequestArgumentCaptor;

  @InjectMocks
  private CategoryServiceWrapperImpl categoryServiceWrapper;

  private CategoryInfoUpdateDTO categoryInfoUpdateDTO;
  private Category category;
  private Category category1;
  private Category parentCategory;
  private List<CategoryShipping> categoryShippingList;
  private CategoryMappingsUpdateDTO categoryAttributeMappingsUpdateDTO;
  private CategoryMappingsUpdateDTO categoryReferencesUpdateDTO;
  private CategoryMappingsUpdateDTO categoryMappingsCreateDTO;
  private CategoryDetailDTO categoryDetailDTO;
  private Attribute attribute1;
  private Attribute attribute2;
  private Attribute attribute3;
  private Category masterCategory2;
  private Category masterCategory3;
  private Catalog catalog;
  private OriginalSalesCategory originalSalesCategory;
  private Page<Category> categoryPage;
  private List<Category> categoryList;
  private CategoryAttributeUpdateDTO addedAttribute1;
  private CategoryReference categoryReference2;
  private CategoryAttribute categoryAttribute1;
  private RestrictedKeyword restrictedKeyword;
  private List<RestrictedKeyword> restrictedKeywordList;
  private List<RestrictedKeyword> restrictedKeywordList1;
  private RestrictedKeyword restrictedKeyword1;
  private CategoryRestrictedKeyword categoryRestrictedKeyword;
  private CategoryRestrictedKeyword categoryRestrictedKeyword1;
  private CategoryRestrictedKeyword categoryRestrictedKeyword2;
  private CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO;
  private CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO1;
  private CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO;
  private List<CategoryKeywordsUpdateDTO> categoryKeywordsUpdateDTOList;
  private List<CategoryRestrictedKeyword> categoryRestrictedKeywordList;
  private CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest;
  private Pageable pageable;
  private MinWholesaleDiscountDTO minWholesaleDiscountDTO;
  private WholesaleConfigDTO wholesaleConfigDTO;
  private WholesaleMappingDTO wholesaleMappingDTO;
  private List<CategoryTreeDTO> catDTOList;

  private static final String STORE_ID = "storeId";
  private static final String USER_NAME = "userName";
  private static final String ID = "id";
  private static final String ID1 = "id1";
  private static final String OSC_ID = "oscId1";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_CODE1 = "categoryCode1";
  private static final String NAME = "name";
  private static final String NAME_ENGLISH = "nameEnglish";
  private static final byte[] DEFAULT_DESCRIPTION = "default_description".getBytes();
  private static final byte[] DESCRIPTION_ENGLISH = "descriptionEnglish".getBytes();
  private static final Integer INTERNAL_ACTIVATION_INTERVAL = 100;
  private static final Integer LOGISTIC_ADJUSTMENT = 10;
  private static final Integer SEQUENCE = 5;
  private static final String UPDATED_BY = "updatedBy";
  private static final Date UPDATED_DATE = new Date();
  private static final String PARENT_CATEGORY_ID = "parentCategoryId";
  private static final String PREVIOUS_PARENT_CATEGORY_ID = "previousParentCategoryId";
  private static final String SHIPPING_CODE_AS_STRING =
      "{\"deliveredByMerchant\":false,\"specialHandling\":true,\"directFlight\":true, \"ageLimit\":true}";
  private static final String SHIPPING_CODE_AS_STRING_NEW =
      "{\"deliveredByMerchant\":false,\"specialHandling\":false,\"directFlight\":true,\"ageLimit\":true, \"sizeChartRequired\":true}";
  private static final String ATTRIBUTE_ID_1 = "attributeId1";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String ATTRIBUTE_ID_3 = "attributeId3";
  private static final String MASTER_CATEGORY_ID_1 = "masterCategoryId1";
  private static final String MASTER_CATEGORY_ID_2 = "masterCategoryId2";
  private static final String MASTER_CATEGORY_ID_3 = "masterCategoryId3";
  private static final String CATALOG_ID = "CATALOG_ID";
  private static final long COUNT = 1;
  private static final String ACTIVE = "ACTIVE";
  private static final String INACTIVE = "INACTIVE";
  private static final String ALL = "ALL";
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD1 = "keyword1";
  private static final String KEYWORD2 = "keyword2";
  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD_ID1 = "keywordId1";
  private static final String KEYWORD_ID2 = "keywordId2";
  private static final Integer PREVIOUS_ACTIVATION_INTERVAL = 1;
  private static final int DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final Integer QUANTITY = 10;
  private static final Double PERCENTAGE = 10D;
  private static final Double PRICE = 100000D;
  private static final String CONFIGURATION_TYPE = "PERCENTAGE";
  private static final String WHOLESALE_CONFIG =
      "[{\"quantity\":10,\"minWholesaleDiscount\":[{\"price\":100000.0,\"percentage\":10.0}]}]";
  private static final String CATEGORY_ID_1 = "categoryId1";
  private static final String CATEGORY_ID_2 = "categoryId2";
  private static final String CATEGORY_ID_3 = "categoryId3";
  private static final String CATEGORY_ID_4 = "categoryId4";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_CODE_3 = "categoryCode3";
  private static final String CATEGORY_CODE_4 = "categoryCode4";
  private static final String CATALOG_NAME = "catelogName";
  private static final String DOCUMENTS = "Doctor's prescription, Passport, Driving License";
  private List<CategoryRestrictedKeyword> categoryRestrictedKeywords;
  private static final String DESTINATION_CATEGORY = "destinationCategory";
  private static final String MESSAGE = "MESSAGE";
  private static final String TYPE = "3";
  private static final int ACTION = 2;

  @BeforeEach
  public void init() {
    initMocks(this);
    categoryInfoUpdateDTO =
        CategoryInfoUpdateDTO.builder().id(ID).categoryCode(CATEGORY_CODE).name(NAME).nameEnglish(NAME_ENGLISH)
            .defaultDescription(DEFAULT_DESCRIPTION).descriptionEnglish(DESCRIPTION_ENGLISH).display(true)
            .deliveredByMerchant(false).internalActivationInterval(INTERNAL_ACTIVATION_INTERVAL)
            .logisticAdjustment(LOGISTIC_ADJUSTMENT).directFlight(true).specialHandling(true).sequence(SEQUENCE)
            .warranty(true).needIdentity(true).ageLimit(true).updatedBy(UPDATED_BY).updatedDate(UPDATED_DATE)
            .parentCategoryId(PARENT_CATEGORY_ID).activated(true).umkm(true).oscId(OSC_ID).dangerousGoodsLevel(0)
            .build();
    category = new Category();
    category.setId(ID);
    category.setInternalActivationInterval(PREVIOUS_ACTIVATION_INTERVAL);
    category.setCategoryCode(CATEGORY_CODE);
    category.setActivated(true);
    catalog = new Catalog();
    catalog.setName(NAME);
    catalog.setCatalogType(CatalogType.SALES_CATALOG);
    category.setCatalog(catalog);
    Category previousParentCategory = new Category();
    previousParentCategory.setId(PREVIOUS_PARENT_CATEGORY_ID);
    category.setParentCategory(previousParentCategory);
    parentCategory = new Category();
    parentCategory.setId(PARENT_CATEGORY_ID);
    parentCategory.setCategoryCode(CATEGORY_CODE);
    categoryShippingList = new ArrayList<>();
    CategoryShipping categoryShipping = new CategoryShipping();
    categoryShippingList.add(categoryShipping);
    attribute1 = new Attribute();
    attribute1.setId(ATTRIBUTE_ID_1);
    attribute1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    categoryAttribute1 = new CategoryAttribute();
    categoryAttribute1.setCategory(category);
    categoryAttribute1.setAttribute(attribute1);
    categoryAttribute1.setUSP(true);
    attribute2 = new Attribute();
    attribute2.setId(ATTRIBUTE_ID_2);
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute3 = new Attribute();
    attribute3.setId(ATTRIBUTE_ID_3);
    attribute3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    CategoryAttribute categoryAttribute2 = new CategoryAttribute();
    categoryAttribute2.setCategory(category);
    categoryAttribute2.setAttribute(attribute3);
    List<CategoryAttribute> categoryAttributes = new ArrayList<>();
    categoryAttributes.add(categoryAttribute1);
    categoryAttributes.add(categoryAttribute2);
    category.setCategoryAttributes(categoryAttributes);
    addedAttribute1 =
        CategoryAttributeUpdateDTO.builder().attributeId(ATTRIBUTE_ID_2).mainDefiningAttribute(true).usp(false)
            .sequence(10).build();
    CategoryAttributeUpdateDTO deletedAttribute1 =
        CategoryAttributeUpdateDTO.builder().attributeId(ATTRIBUTE_ID_3).mainDefiningAttribute(true).usp(false)
            .sequence(5).build();
    categoryAttributeMappingsUpdateDTO =
        CategoryMappingsUpdateDTO.builder().addedAttributes(Collections.singletonList(addedAttribute1))
            .deletedAttributes(Collections.singletonList(deletedAttribute1)).id(ID).updatedBy(UPDATED_BY)
            .updatedDate(UPDATED_DATE).build();
    Category masterCategory1 = new Category();
    masterCategory1.setId(MASTER_CATEGORY_ID_1);
    CategoryReference categoryReference1 = new CategoryReference();
    categoryReference1.setMasterCategory(masterCategory1);
    categoryAttribute1.setCategory(category);
    masterCategory2 = new Category();
    masterCategory2.setId(MASTER_CATEGORY_ID_2);
    masterCategory3 = new Category();
    masterCategory3.setId(MASTER_CATEGORY_ID_3);
    categoryReference2 = new CategoryReference();
    categoryReference2.setMasterCategory(masterCategory3);
    categoryAttribute2.setCategory(category);
    List<CategoryReference> categoryReferences = new ArrayList<>();
    categoryReferences.add(categoryReference1);
    categoryReferences.add(categoryReference2);
    category.setMasterCategoryReferences(categoryReferences);

    categoryKeywordsUpdateDTO = CategoryKeywordsUpdateDTO.builder().keyword(KEYWORD).keywordId(KEYWORD_ID).build();
    categoryKeywordsUpdateDTO1 = CategoryKeywordsUpdateDTO.builder().keyword(KEYWORD1).keywordId(KEYWORD_ID1).build();

    categoryReferencesUpdateDTO = CategoryMappingsUpdateDTO.builder().id(ID)
        .addedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_2))
        .deletedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_3)).updatedBy(UPDATED_BY)
        .updatedDate(UPDATED_DATE).build();
    categoryMappingsCreateDTO = CategoryMappingsUpdateDTO.builder().id(ID).storeId(STORE_ID)
        .addedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_2))
        .addedAttributes(Collections.singletonList(addedAttribute1))
        .addedKeywords(Collections.singletonList(categoryKeywordsUpdateDTO))
        .deletedKeywords(Collections.singletonList(categoryKeywordsUpdateDTO1)).build();
    categoryDetailDTO = CategoryDetailDTO.builder().categoryInfoDetail(categoryInfoUpdateDTO)
        .categoryMappingsDetail(categoryMappingsCreateDTO).catalogId(CATALOG_ID).id(ID).updatedBy(UPDATED_BY)
        .updatedDate(UPDATED_DATE).createdBy(UPDATED_BY).createdDate(UPDATED_DATE).build();
    catalog = new Catalog(CATALOG_ID);
    categoryList = new ArrayList<>();
    categoryList.add(category);
    categoryPage = new PageImpl<>(categoryList);

    category1 = new Category();
    category1.setId(ID1);
    category1.setCategoryCode(CATEGORY_CODE1);

    restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setId(KEYWORD_ID);

    restrictedKeyword1 = new RestrictedKeyword();
    restrictedKeyword1.setKeyword(KEYWORD1);
    restrictedKeyword1.setId(KEYWORD_ID1);

    categoryRestrictedKeyword = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword.setMarkForDelete(false);
    categoryRestrictedKeyword.setCategory(category);
    categoryRestrictedKeyword.setRestrictedKeyword(restrictedKeyword);
    categoryRestrictedKeyword.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeyword.setRestrictedKeywordId(KEYWORD_ID);

    categoryRestrictedKeyword1 = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword1.setMarkForDelete(true);
    categoryRestrictedKeyword1.setCategory(category1);
    categoryRestrictedKeyword1.setRestrictedKeyword(restrictedKeyword1);
    categoryRestrictedKeyword1.setCategoryCode(CATEGORY_CODE1);
    categoryRestrictedKeyword1.setRestrictedKeywordId(KEYWORD_ID1);

    categoryKeywordsUpdateListDTO =
        CategoryKeywordsUpdateListDTO.builder().storeId(STORE_ID).createdBy(UPDATED_BY).updatedBy(UPDATED_BY)
            .createdDate(UPDATED_DATE).updatedDate(UPDATED_DATE)
            .addedRestrictedKeywords(Collections.singletonList(categoryKeywordsUpdateDTO))
            .deletedRestrictedKeywords(Collections.singletonList(categoryKeywordsUpdateDTO1)).build();
    categoryKeywordsUpdateDTOList = new ArrayList<>();
    categoryKeywordsUpdateDTOList.add(categoryKeywordsUpdateDTO);

    restrictedKeywordList = new ArrayList<>();
    restrictedKeywordList1 = new ArrayList<>();
    restrictedKeywordList.add(restrictedKeyword);
    restrictedKeywordList1.add(restrictedKeyword);
    categoryRestrictedKeywordList = new ArrayList<>();
    categoryRestrictedKeywordList.add(categoryRestrictedKeyword);
    categoryRestrictedKeywordList.add(categoryRestrictedKeyword1);

    categoryRestrictedKeywordsRequest =
        CategoryRestrictedKeywordsRequest.builder().categoryCode(CATEGORY_CODE).keyword(KEYWORD).build();
    pageable = PageRequest.of(PAGE, SIZE);

    minWholesaleDiscountDTO = new MinWholesaleDiscountDTO();
    minWholesaleDiscountDTO.setPercentage(PERCENTAGE);
    minWholesaleDiscountDTO.setPrice(PRICE);
    wholesaleConfigDTO = new WholesaleConfigDTO();
    wholesaleConfigDTO.setQuantity(QUANTITY);
    wholesaleConfigDTO.setMinWholesaleDiscount(Collections.singletonList(minWholesaleDiscountDTO));
    wholesaleMappingDTO = new WholesaleMappingDTO();
    wholesaleMappingDTO.setConfigurationType(CONFIGURATION_TYPE);
    wholesaleMappingDTO.setWholesaleConfig(Collections.singletonList(wholesaleConfigDTO));

    catDTOList = new ArrayList<>();
    catDTOList.add(new CategoryTreeDTO("1", "A-01", "Cat 1", ""));
    catDTOList.add(new CategoryTreeDTO("2", "A-01-01", "Cat 1", "1"));
    catDTOList.add(new CategoryTreeDTO("3", "A-01-02", "Cat 1", "1"));
    catDTOList.add(new CategoryTreeDTO("4", "A-01-02", "Cat 1", "1"));
    catDTOList.add(new CategoryTreeDTO("5", "A-01-01-01", "Cat 1", "2"));
    catDTOList.add(new CategoryTreeDTO("6", "A-01-01-02", "Cat 1", "2"));
    catDTOList.add(new CategoryTreeDTO("21", "B-02-01", "Cat 2", ""));

    originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID1);

    categoryRestrictedKeyword2 = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword2.setMarkForDelete(Boolean.FALSE);
    categoryRestrictedKeyword2.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeyword2.setRestrictedKeyword(restrictedKeyword1);
    categoryRestrictedKeyword2.setRestrictedKeywordId(KEYWORD_ID2);

    categoryRestrictedKeywords = new ArrayList<>();
    categoryRestrictedKeywords.add(categoryRestrictedKeyword);
    categoryRestrictedKeywords.add(categoryRestrictedKeyword1);
    categoryRestrictedKeywords.add(categoryRestrictedKeyword2);

    category.setNameEnglish(NAME);
    category.setName(NAME);
  }

  @Test
  public void updateCategoryInfoTest() throws Exception {
    category.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoTest_noNameChange() throws Exception {
    category.setGenericTemplateEligible(true);
    category.setName(NAME);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setNameEnglish(StringUtils.EMPTY);
    categoryInfoUpdateDTO.setName(StringUtils.EMPTY);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
  }

  @Test
  public void updateCategoryInfoTest_nameEnglishChange() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "addHierarchyChangeTypeInCategoryChangeEvent",false);
    category.setGenericTemplateEligible(true);
    category.setName(NAME_ENGLISH);
    category.setNameEnglish(null);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setNameEnglish(NAME_ENGLISH);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)),
        eq(Arrays.asList(CategoryChangeEventType.CATEGORY_NAME_CHANGE,
            CategoryChangeEventType.DATA_CHANGE)), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
  }

  @Test
  public void updateCategoryInfoTest_noName() throws Exception {
    category.setGenericTemplateEligible(true);
    category.setName(NAME);
    categoryInfoUpdateDTO.setName(StringUtils.EMPTY);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setNameEnglish(NAME);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryIdNull() throws Exception {
    categoryInfoUpdateDTO.setParentCategoryId(null);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertNull(categoryArgumentCaptor.getValue().getParentCategory());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertEquals(INTERNAL_ACTIVATION_INTERVAL, categoryArgumentCaptor.getValue().getInternalActivationInterval(), 0);
    assertEquals(true, categoryArgumentCaptor.getValue().isActivated());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryNullInitially() throws Exception {
    category.setParentCategory(null);
    categoryInfoUpdateDTO.setDisplay(false);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertFalse(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryNullInitiallyB2bTrue() throws Exception {
    categoryInfoUpdateDTO.setB2bExclusive(true);
    categoryInfoUpdateDTO.setHalalCategory(false);
    category.setParentCategory(null);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService).updateB2bExclusiveOrHalalCategoryFlagForChildCategories(eq(STORE_ID), any(Category.class),
        eq(true), eq(false));
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryNullInitiallyHalalFlagTrue() throws Exception {
    categoryInfoUpdateDTO.setHalalCategory(true);
    category.setParentCategory(null);
    parentCategory.setHalalCategory(true);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService).updateB2bExclusiveOrHalalCategoryFlagForChildCategories(eq(STORE_ID), any(Category.class),
        eq(false), eq(true));
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryInitiallyHalalFlagFalse() throws Exception {
    categoryInfoUpdateDTO.setHalalCategory(false);
    category.setHalalCategory(true);
    category.setParentCategory(null);
    parentCategory.setHalalCategory(true);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService).updateB2bExclusiveOrHalalCategoryFlagForChildCategories(eq(STORE_ID), any(Category.class),
        eq(false), eq(false));
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(categoryService).findCountByParentCategoryAndHalalCategory(STORE_ID, category.getParentCategory(), true);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryInitiallyHalalFlagCountMoreThan1False() throws Exception {
    categoryInfoUpdateDTO.setHalalCategory(false);
    category.setHalalCategory(true);
    category.setParentCategory(null);
    parentCategory.setHalalCategory(true);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    when(categoryService.findCountByParentCategoryAndHalalCategory(eq(STORE_ID),
        any(), Mockito.anyBoolean())).thenReturn(2L);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService).updateB2bExclusiveOrHalalCategoryFlagForChildCategories(eq(STORE_ID), any(Category.class),
        eq(false), eq(false));
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(categoryService).findCountByParentCategoryAndHalalCategory(STORE_ID, category.getParentCategory(), true);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryNullInitiallyHalalFlagFalse() throws Exception {
    categoryInfoUpdateDTO.setHalalCategory(true);
    category.setParentCategory(null);
    parentCategory.setHalalCategory(false);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryNullInitiallyHalalCategoryTrue() throws Exception {
    categoryInfoUpdateDTO.setHalalCategory(true);
    categoryInfoUpdateDTO.setId(ID);
    categoryInfoUpdateDTO.setParentCategoryId(StringUtils.EMPTY);
    category.setParentCategory(null);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId())).thenReturn(
        new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService)
        .evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        Mockito.anyList(), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService).updateB2bExclusiveOrHalalCategoryFlagForChildCategories(eq(STORE_ID), any(Category.class),
        eq(false), eq(true));
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }


  @Test
  public void updateCategoryInfoTestParentCategoryIdNullInternalActivationNull() throws Exception {
    categoryInfoUpdateDTO.setParentCategoryId(null);
    categoryInfoUpdateDTO.setInternalActivationInterval(null);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertNull(categoryArgumentCaptor.getValue().getParentCategory());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertEquals(0, categoryArgumentCaptor.getValue().getInternalActivationInterval(), 0);
    assertEquals(true, categoryArgumentCaptor.getValue().isActivated());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryNoChange() throws Exception {
    categoryInfoUpdateDTO.setParentCategoryId(PREVIOUS_PARENT_CATEGORY_ID);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(new ArrayList<>()), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PREVIOUS_PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertEquals(true, categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
  }

  @Test
  public void updateCategoryInfoParentCategoryNoChangeWithNullParentCategoryTest() throws Exception {
    categoryInfoUpdateDTO.setParentCategoryId(null);
    category.setParentCategory(null);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(new ArrayList<>()), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertEquals(true, categoryArgumentCaptor.getValue().isActivated());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
  }

  @Test
  public void updateCategoryInfoTestParentCategoryIdSameAsId() {
    categoryInfoUpdateDTO.setParentCategoryId(ID);
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
    }
  }

  @Test
  public void updateCategoryInfoCategoryNotFound() {
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(null);
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    }
  }

  @Test
  public void updateCategoryInfoParentCategoryNotFound() throws Exception {
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(null);
    category.setParentCategoryId(PREVIOUS_PARENT_CATEGORY_ID);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
      verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
      verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
      verify(categoryService).getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID);
    }
  }

  @Test
  public void updateCategoryAttributesTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2, attribute3));
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3, ATTRIBUTE_ID_2),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
  }

  @Test
  public void updateCategoryAttributesForValidDefiningAttributeTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    category.setGenericTemplateEligible(true);
    attribute2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute2.setName("Warna");
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2, attribute3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3, ATTRIBUTE_ID_2),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
  }

  @Test
  public void updateCategoryAttributesForInValidDefiningAttributeTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    category.setGenericTemplateEligible(true);
    attribute2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute2.setName("attribute");
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2, attribute3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3, ATTRIBUTE_ID_2),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
  }

  @Test
  public void updateCategoryAttributesForMandatoryAttributeTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    category.setGenericTemplateEligible(true);
    attribute2.setName("attribute");
    attribute2.setMandatory(true);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2, attribute3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3, ATTRIBUTE_ID_2),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
  }

  @Test
  public void updateCategoryAttributesExistingAttributeTest() throws Exception {
    categoryAttribute1.setMarkForDelete(true);
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    addedAttribute1.setAttributeId(ATTRIBUTE_ID_1);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute1, attribute3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isUSP());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
  }

  @Test
  public void updateCategoryAttributesExistingAttributeTest_mfdTrue() throws Exception {
    Category category2 = new Category();
    category2.setId(ID);
    category2.setInternalActivationInterval(PREVIOUS_ACTIVATION_INTERVAL);
    category2.setCategoryCode(CATEGORY_CODE);
    category2.setActivated(true);
    categoryAttribute1.setMarkForDelete(true);
    category2.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    addedAttribute1.setAttributeId(ATTRIBUTE_ID_1);
    CategoryAttribute categoryAttribute21 = new CategoryAttribute();
    categoryAttribute21.setCategory(category);
    categoryAttribute21.setAttribute(attribute1);
    categoryAttribute21.setUSP(true);
    categoryAttribute21.setMarkForDelete(false);
    CategoryAttribute categoryAttribute22 = new CategoryAttribute();
    categoryAttribute22.setCategory(category);
    categoryAttribute22.setAttribute(attribute3);
    List<CategoryAttribute> categoryAttributes2 = new ArrayList<>();
    categoryAttributes2.add(categoryAttribute21);
    categoryAttributes2.add(categoryAttribute22);
    category2.setCategoryAttributes(categoryAttributes2);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category2);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute1, attribute3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isUSP());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
  }

  @Test
  public void updateCategoryAttributesAndPublishHistoryForValidDefiningAttributeTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    category.setGenericTemplateEligible(true);
    attribute2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute2.setName("Warna");
    when(applicationContext.getBean(CategoryServiceWrapper.class)).thenReturn(categoryServiceWrapper);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2, attribute3));
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(objectMapper.writeValueAsString(any(List.class))).thenReturn("");
    doNothing().when(domainEventPublisherService).publishCategoryUpdateHistory(any(List.class));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappingsAndPublishHistory(STORE_ID,
        categoryAttributeMappingsUpdateDTO);
    verify(applicationContext).getBean(CategoryServiceWrapper.class);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2, ATTRIBUTE_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    verify(mandatoryParameterHelper).getUsername();
    verify(objectMapper, times(2)).writeValueAsString(any(List.class));
    verify(domainEventPublisherService).publishCategoryUpdateHistory(any(List.class));
    assertEquals(Arrays.asList(ATTRIBUTE_ID_1, ATTRIBUTE_ID_3, ATTRIBUTE_ID_2),
        categoryArgumentCaptor.getValue().getCategoryAttributes().stream().map(CategoryAttribute::getAttribute)
            .map(Attribute::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getCategoryAttributes().stream()
        .filter(categoryAttribute -> StringUtils.equals(categoryAttribute.getAttribute().getId(), ATTRIBUTE_ID_2))
        .findFirst().get().isMarkForDelete());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
  }

  @Test
  public void updateCategoryReferencesAndPublishHistoryTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    when(applicationContext.getBean(CategoryServiceWrapper.class)).thenReturn(categoryServiceWrapper);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(masterCategory2, masterCategory3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappingsAndPublishHistory(STORE_ID, categoryReferencesUpdateDTO);
    verify(applicationContext).getBean(CategoryServiceWrapper.class);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(categoryService).findByCategoryIds(STORE_ID, Arrays.asList(MASTER_CATEGORY_ID_2, MASTER_CATEGORY_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    assertEquals(Arrays.asList(MASTER_CATEGORY_ID_1, MASTER_CATEGORY_ID_3, MASTER_CATEGORY_ID_2),
        categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream()
            .map(CategoryReference::getMasterCategory).map(Category::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
            categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
            categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
            categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_2))
        .findFirst().get().isMarkForDelete());
  }

  @Test
  public void updateCategoryAttributesCategoryNotExistTest() {
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(null);
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    }
  }

  @Test
  public void updateCategoryAttributesAttributeNotFoundTest() {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryAttributeMappingsUpdateDTO);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
      verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2, ATTRIBUTE_ID_3));
    }
  }

  @Test
  public void updateCategoryReferencesTest() throws Exception {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(masterCategory2, masterCategory3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryReferencesUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(categoryService).findByCategoryIds(STORE_ID, Arrays.asList(MASTER_CATEGORY_ID_2, MASTER_CATEGORY_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    assertEquals(Arrays.asList(MASTER_CATEGORY_ID_1, MASTER_CATEGORY_ID_3, MASTER_CATEGORY_ID_2),
        categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream()
            .map(CategoryReference::getMasterCategory).map(Category::getId).collect(Collectors.toList()));
    assertTrue(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
        categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
        categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_1))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
        categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_2))
        .findFirst().get().isMarkForDelete());
  }

  @Test
  public void updateCategoryReferencesAddingExistingDeletedReferenceTest() throws Exception {
    categoryReference2.setMarkForDelete(true);
    categoryReferencesUpdateDTO.setAddedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_3));
    categoryReferencesUpdateDTO.setDeletedMasterCategoryIds(Collections.emptyList());
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory3));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryReferencesUpdateDTO);
    verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    verify(categoryService).findByCategoryIds(STORE_ID, Collections.singletonList(MASTER_CATEGORY_ID_3));
    verify(categoryService)
        .saveUpdatedCategory(categoryArgumentCaptor.capture(), eq(INTERNAL_ACTIVATION_INTERVAL), eq(null),
            Mockito.anyList());
    assertEquals(Arrays.asList(MASTER_CATEGORY_ID_1, MASTER_CATEGORY_ID_3),
        categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream()
            .map(CategoryReference::getMasterCategory).map(Category::getId).collect(Collectors.toList()));
    assertFalse(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
        categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_3))
        .findFirst().get().isMarkForDelete());
    assertFalse(categoryArgumentCaptor.getValue().getMasterCategoryReferences().stream().filter(
        categoryReference -> StringUtils.equals(categoryReference.getMasterCategory().getId(), MASTER_CATEGORY_ID_1))
        .findFirst().get().isMarkForDelete());
  }

  @Test
  public void updateCategoryReferencesCategoryNotExistTest() {
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(null);
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryReferencesUpdateDTO);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
    }
  }

  @Test
  public void updateCategoryReferencesCategoryNotFoundTest() {
    category.setInternalActivationInterval(INTERNAL_ACTIVATION_INTERVAL);
    when(categoryService.findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    Exception exception = null;
    try {
      categoryServiceWrapper.updateCategoryMappings(STORE_ID, categoryReferencesUpdateDTO);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      verify(categoryService).findByStoreIdAndIdInitAllCategoryAttribute(STORE_ID, ID);
      verify(categoryService).findByCategoryIds(STORE_ID, Arrays.asList(MASTER_CATEGORY_ID_2, MASTER_CATEGORY_ID_3));
    }
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(categoryService);
    verifyNoMoreInteractions(restrictedKeywordService);
    verifyNoMoreInteractions(categoryShippingService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(attributeService);
    verifyNoMoreInteractions(catalogService);
    verifyNoMoreInteractions(categoryReferenceService);
    verifyNoMoreInteractions(domainEventPublisherService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
    verifyNoMoreInteractions(applicationContext);

  }

  @Test
  public void createCategoryTest() throws Exception {
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryInfoDetail().setSizeChartRequired(true);
    categoryDetailDTO.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryDetailDTO.getCategoryInfoDetail().setB2bExclusive(true);
    categoryDetailDTO.getCategoryInfoDetail().setHalalCategory(false);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(STORE_ID, false,
      false);
    doNothing().when(categoryService).evictActiveChildCategoryCache(Collections.singletonList(PARENT_CATEGORY_ID));
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING_NEW);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(restrictedKeyword));
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.emptyList());
    Mockito.when(this.oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, OSC_ID))
        .thenReturn(new OriginalSalesCategory());
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Collections.singletonList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    Mockito.verify(this.oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, OSC_ID);
    Assertions.assertEquals(SHIPPING_CODE_AS_STRING_NEW, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void createCategoryWithNoOscTest() throws Exception {
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryInfoDetail().setSizeChartRequired(true);
    categoryDetailDTO.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(STORE_ID, false,
      false);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING_NEW);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(restrictedKeyword));
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.emptyList());
    Mockito.when(this.oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, OSC_ID))
        .thenReturn(null);
    try {
      categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    } catch (Exception e) {
    } finally {
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
      verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
      Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
      Mockito.verify(this.oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, OSC_ID);
    }
  }

  @Test
  public void createCategoryWithNoOscIdPresentTest() throws Exception {
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryInfoDetail().setSizeChartRequired(true);
    categoryDetailDTO.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryDetailDTO.getCategoryInfoDetail().setOscId(null);
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING_NEW);
    try {
      categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    } catch (Exception e) {

    } finally {
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
      verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
      Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    }
  }

  @Test
  public void createCategoryWithNoCatalogPresentTest() throws Exception {
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryInfoDetail().setSizeChartRequired(true);
    categoryDetailDTO.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryDetailDTO.getCategoryInfoDetail().setOscId(null);
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(null);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING_NEW);
    try {
      categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    } catch (Exception e) {
    } finally {
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
      verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
      Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    }
  }

  @Test
  public void createCategoryWithNullParentCategoryIDTest() throws Exception {
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryInfoDetail().setParentCategoryId(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(STORE_ID, false,
      false);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(restrictedKeyword));
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.emptyList());
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Collections.singletonList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
  }

  @Test
  public void createCategory_WhenMasterCategoryReferenceNullTest() throws Exception {
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryMappingsDetail().setAddedMasterCategoryIds(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(restrictedKeyword));
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.emptyList());
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Collections.singletonList(ATTRIBUTE_ID_2));
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryService).save(any(Category.class));
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
  }

  @Test
  public void createCategory_WhenCategoryAttributeNullTest() throws Exception {
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryMappingsDetail().setAddedAttributes(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(this.categoryService.save(any(Category.class))).thenReturn(category);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(restrictedKeyword));
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.emptyList());
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), any(Category.class));
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
  }

  @Test
  public void createCategory_WhenSaveNotSuccessTest() throws Exception {
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    doThrow(new RuntimeException()).when(categoryService)
        .saveAndUpdateProductCategory(eq(STORE_ID), any(Category.class));
    Exception exception = null;
    try {
      categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    } catch (Exception ex) {
      exception = ex;
    } finally {
      verify(attributeService).findByAttributeIds(STORE_ID, Collections.singletonList(ATTRIBUTE_ID_2));
      verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
      verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
      verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
      verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), any(Category.class));
      verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    }
  }

  @Test
  public void getChildCountByFilterType_ALL() throws Exception {
    when(this.categoryService.findOverAllChildCountForParent(STORE_ID, category)).thenReturn(COUNT);
    this.categoryServiceWrapper.setChildCountByFilterType(STORE_ID, categoryPage, ALL);
    verify(this.categoryService).findOverAllChildCountForParent(STORE_ID, category);
  }

  @Test
  public void getChildCountByFilterType_ACTIVE() throws Exception {
    when(this.categoryService.findActiveChildCountForParent(STORE_ID, category)).thenReturn(COUNT);
    this.categoryServiceWrapper.setChildCountByFilterType(STORE_ID, categoryPage, ACTIVE);
    verify(this.categoryService).findActiveChildCountForParent(STORE_ID, category);
  }

  @Test
  public void getChildCountByFilterType_INACTIVE() throws Exception {
    when(this.categoryService.findInActiveChildCountForParent(STORE_ID, category)).thenReturn(COUNT);
    this.categoryServiceWrapper.setChildCountByFilterType(STORE_ID, categoryPage, INACTIVE);
    verify(this.categoryService).findInActiveChildCountForParent(STORE_ID, category);
  }

  @Test
  public void updateCategoryInfo_ActivationTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "addHierarchyChangeTypeInCategoryChangeEvent",true);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)),
        eq(Arrays.asList(CategoryChangeEventType.CATEGORY_HIERARCHY_CHANGE, CategoryChangeEventType.CATEGORY_NAME_CHANGE,
            CategoryChangeEventType.DATA_CHANGE)), eq(new HashSet<>(Arrays.asList(CategoryChangeEventType
            .CATEGORY_HIERARCHY_CHANGE.name(), CategoryChangeEventType.DISPLAY_CHANGE.name()))), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
  }

  @Test
  public void updateCategoryInfoWhenStatusChangeFlagTrue_ActivationTest() throws Exception {
    categoryInfoUpdateDTO.setActivated(false);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "addHierarchyChangeTypeInCategoryChangeEvent",true);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, true);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), eq(Arrays
            .asList(CategoryChangeEventType.CATEGORY_HIERARCHY_CHANGE, CategoryChangeEventType.CATEGORY_NAME_CHANGE,
                CategoryChangeEventType.ACTIVATE_DEACTIVATE_CHANGE, CategoryChangeEventType.DATA_CHANGE)),
        Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(categoryService).getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertFalse(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
  }

  @Test
  public void publishAllCategoriesTest() throws Exception {
    Mockito.when(this.categoryService.publishAllCategories(CATALOG_ID, STORE_ID)).thenReturn(Boolean.TRUE);
    this.categoryServiceWrapper.publishAllCategories(CATALOG_ID, STORE_ID);
    Mockito.verify(this.categoryService).publishAllCategories(CATALOG_ID, STORE_ID);
  }

  @Test
  public void publishAllCategoriesTest_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.categoryService).publishAllCategories(CATALOG_ID, STORE_ID);
    this.categoryServiceWrapper.publishAllCategories(CATALOG_ID, STORE_ID);
    Mockito.verify(this.categoryService).publishAllCategories(CATALOG_ID, STORE_ID);
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "restrictedKeywordListChange", true);
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().get(0).setKeywordId(null);
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito
        .when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD))
        .thenReturn(null);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID, Collections.singletonList(restrictedKeyword),
            category, Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
            Collections.singletonList(restrictedKeyword), category, Collections.singletonList(categoryKeywordsUpdateDTO1),
            new HashMap<>());
    this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE, categoryKeywordsUpdateListDTO);
    Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(Mockito.anyList());
    Mockito.verify(this.restrictedKeywordService)
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService).getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsDuplicateTest() throws Exception {
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().get(0).setKeywordId(null);
    categoryKeywordsUpdateListDTO.setAddedRestrictedKeywords(Arrays.asList(categoryKeywordsUpdateDTO,categoryKeywordsUpdateDTO));
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito
        .when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD))
        .thenReturn(null);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID, Collections.singletonList(restrictedKeyword),
            category, Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
            Collections.singletonList(restrictedKeyword), category, Collections.singletonList(categoryKeywordsUpdateDTO1),
            new HashMap<>());
    this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE, categoryKeywordsUpdateListDTO);
    Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(Mockito.anyList());
    Mockito.verify(this.restrictedKeywordService)
      .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService).getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
  }

  @Test
  public void updateCategoriesWithRestrictedKeywords_CnCategoryTest() throws Exception {
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().get(0).setKeywordId(null);
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID, Collections.singletonList(restrictedKeyword),
            category, Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
            Collections.singletonList(restrictedKeyword), category, Collections.singletonList(categoryKeywordsUpdateDTO1),
            new HashMap<>());
    Mockito
        .when(this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD))
        .thenReturn(null);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE, categoryKeywordsUpdateListDTO);
    Mockito.verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(Mockito.anyList());
    Mockito.verify(this.restrictedKeywordService)
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(restrictedKeywordService).getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsTest_NullTest() throws Exception {
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE, categoryKeywordsUpdateListDTO));
    } finally {
      Mockito.verify(this.categoryService)
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    }
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsTest_SalesCategoryTest() throws Exception {
    category.getCatalog().setCatalogType(CatalogType.SALES_CATALOG);
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE, categoryKeywordsUpdateListDTO));
    } finally {
      Mockito.verify(this.categoryService)
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    }
  }

  @Test
  public void findCategoryRestrictedKeywordsTest() throws Exception {
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(Arrays.asList(restrictedKeyword), pageable, Arrays.asList(restrictedKeyword).size());
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordPage);
    this.categoryServiceWrapper.findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable);
    assertNotNull(restrictedKeyword);
    assertEquals(Arrays.asList(restrictedKeyword), restrictedKeywordPage.getContent());
    assertEquals(KEYWORD, restrictedKeyword.getKeyword());
    assertEquals(KEYWORD_ID, restrictedKeyword.getId());
    assertEquals(KEYWORD, categoryRestrictedKeywordsRequest.getKeyword());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeywordsRequest.getCategoryCode());
    assertEquals(KEYWORD_ID, categoryRestrictedKeyword.getRestrictedKeywordId());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeyword.getCategoryCode());
    assertNull(categoryRestrictedKeyword.getRestrictedKeyword().getValidateByDs());
  }

  @Test
  public void findCategoryRestrictedKeywordsRestrictedKeywordTest() {
    restrictedKeyword.setValidateByDs(true);
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(Collections.singletonList(restrictedKeyword), pageable,
            Collections.singletonList(restrictedKeyword).size());
    categoryRestrictedKeyword.setDestinationCategory(DESTINATION_CATEGORY);
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordPage);
    CategoryCodeAndNameDTO categoryCodeAndNameDTO = new CategoryCodeAndNameDTO();
    categoryCodeAndNameDTO.setCategoryCode(DESTINATION_CATEGORY);
    categoryCodeAndNameDTO.setDestinationCategoryName(NAME);
    categoryCodeAndNameDTO.setDestinationCategoryEnglishName(NAME_ENGLISH);
    Mockito.when(
        categoryService.findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(DESTINATION_CATEGORY)))
        .thenReturn(Arrays.asList(categoryCodeAndNameDTO, categoryCodeAndNameDTO));
    Page<RestrictedKeywordsResponse> categoryRestrictedKeywords = this.categoryServiceWrapper
        .findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable);
    verify(categoryService)
        .findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(DESTINATION_CATEGORY));
    assertNotNull(restrictedKeyword);
    assertEquals(Collections.singletonList(restrictedKeyword), restrictedKeywordPage.getContent());
    assertEquals(KEYWORD, restrictedKeyword.getKeyword());
    assertEquals(KEYWORD_ID, restrictedKeyword.getId());
    assertEquals(KEYWORD, categoryRestrictedKeywordsRequest.getKeyword());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeywordsRequest.getCategoryCode());
    assertEquals(KEYWORD_ID, categoryRestrictedKeyword.getRestrictedKeywordId());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeyword.getCategoryCode());
    assertEquals(NAME, categoryRestrictedKeywords.getContent().get(0).getDestinationCategoryName());
    assertEquals(NAME_ENGLISH, categoryRestrictedKeywords.getContent().get(0).getDestinationCategoryEnglishName());
    assertTrue(categoryRestrictedKeywords.getContent().get(0).getValidateByDs());
  }

  @Test
  public void findCategoryRestrictedKeywordsRestrictedKeywordNullTest() {
    categoryRestrictedKeyword.setRestrictedKeyword(null);
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(Collections.singletonList(restrictedKeyword), pageable,
            Collections.singletonList(restrictedKeyword).size());
    categoryRestrictedKeyword.setDestinationCategory(DESTINATION_CATEGORY);
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordPage);
    CategoryCodeAndNameDTO categoryCodeAndNameDTO = new CategoryCodeAndNameDTO();
    categoryCodeAndNameDTO.setCategoryCode(DESTINATION_CATEGORY);
    categoryCodeAndNameDTO.setDestinationCategoryName(NAME);
    categoryCodeAndNameDTO.setDestinationCategoryEnglishName(NAME_ENGLISH);
    Mockito.when(
            categoryService.findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(DESTINATION_CATEGORY)))
        .thenReturn(Arrays.asList(categoryCodeAndNameDTO, categoryCodeAndNameDTO));
    Page<RestrictedKeywordsResponse> categoryRestrictedKeywords = this.categoryServiceWrapper
        .findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable);
    verify(categoryService)
        .findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(DESTINATION_CATEGORY));
    assertNotNull(restrictedKeyword);
    assertEquals(Collections.singletonList(restrictedKeyword), restrictedKeywordPage.getContent());
    assertEquals(KEYWORD, restrictedKeyword.getKeyword());
    assertEquals(KEYWORD_ID, restrictedKeyword.getId());
    assertEquals(KEYWORD, categoryRestrictedKeywordsRequest.getKeyword());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeywordsRequest.getCategoryCode());
    assertEquals(KEYWORD_ID, categoryRestrictedKeyword.getRestrictedKeywordId());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeyword.getCategoryCode());
    assertEquals(NAME, categoryRestrictedKeywords.getContent().get(0).getDestinationCategoryName());
    assertEquals(NAME_ENGLISH, categoryRestrictedKeywords.getContent().get(0).getDestinationCategoryEnglishName());
    assertNull(categoryRestrictedKeywords.getContent().get(0).getValidateByDs());
  }

  @Test
  public void findCategoryRestrictedKeywordsRestrictedKeyword2Test() {
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(Collections.singletonList(restrictedKeyword), pageable,
            Collections.singletonList(restrictedKeyword).size());
    categoryRestrictedKeyword.setDestinationCategory(DESTINATION_CATEGORY);
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryRestrictedKeyword));
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordPage);
    CategoryCodeAndNameDTO categoryCodeAndNameDTO = new CategoryCodeAndNameDTO();
    categoryCodeAndNameDTO.setCategoryCode(NAME);
    categoryCodeAndNameDTO.setDestinationCategoryName(NAME);
    categoryCodeAndNameDTO.setDestinationCategoryEnglishName(NAME_ENGLISH);
    Mockito.when(
        categoryService.findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(DESTINATION_CATEGORY)))
        .thenReturn(Arrays.asList(categoryCodeAndNameDTO, categoryCodeAndNameDTO));
    this.categoryServiceWrapper
        .findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable);
    verify(categoryService)
        .findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(DESTINATION_CATEGORY));
    assertNotNull(restrictedKeyword);
    assertEquals(Collections.singletonList(restrictedKeyword), restrictedKeywordPage.getContent());
    assertEquals(KEYWORD, restrictedKeyword.getKeyword());
    assertEquals(KEYWORD_ID, restrictedKeyword.getId());
    assertEquals(KEYWORD, categoryRestrictedKeywordsRequest.getKeyword());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeywordsRequest.getCategoryCode());
    assertEquals(KEYWORD_ID, categoryRestrictedKeyword.getRestrictedKeywordId());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeyword.getCategoryCode());
  }

  @Test
  public void findCategoryRestrictedKeywords_keywordNullTest() throws Exception {
    Page<CategoryRestrictedKeyword> categoryRestrictedKeywordPage =
        new PageImpl<>(Collections.singletonList(categoryRestrictedKeyword), pageable,
            Collections.singletonList(categoryRestrictedKeyword).size());
    categoryRestrictedKeywordsRequest.setKeyword(null);
    Mockito.when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE, pageable))
        .thenReturn(categoryRestrictedKeywordPage);
    this.categoryServiceWrapper.findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE, pageable);
    assertNotNull(categoryRestrictedKeyword);
    assertEquals(Arrays.asList(categoryRestrictedKeyword), categoryRestrictedKeywordPage.getContent());
    assertEquals(restrictedKeyword, categoryRestrictedKeyword.getRestrictedKeyword());
    assertEquals(KEYWORD_ID, categoryRestrictedKeyword.getRestrictedKeywordId());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeyword.getCategoryCode());
    assertEquals(null, categoryRestrictedKeywordsRequest.getKeyword());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeywordsRequest.getCategoryCode());
  }

  @Test
  public void findCategoryRestrictedKeywords_CategoryCodeNullTest() throws Exception {
    Page<RestrictedKeyword> restrictedKeywordPage =
        new PageImpl<>(Arrays.asList(restrictedKeyword), pageable, Arrays.asList(restrictedKeyword).size());
    categoryRestrictedKeywordsRequest.setCategoryCode(null);
    Mockito.when(this.restrictedKeywordService.getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable))
        .thenReturn(restrictedKeywordPage);
    this.categoryServiceWrapper.findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable);
    verify(this.restrictedKeywordService).getRestrictedKeywordSuggestions(STORE_ID, KEYWORD, pageable);
    assertNotNull(restrictedKeyword);
    assertEquals(Arrays.asList(restrictedKeyword), restrictedKeywordPage.getContent());
    assertEquals(KEYWORD, restrictedKeyword.getKeyword());
    assertEquals(KEYWORD_ID, restrictedKeyword.getId());
    assertEquals(KEYWORD, categoryRestrictedKeywordsRequest.getKeyword());
    assertEquals(null, categoryRestrictedKeywordsRequest.getCategoryCode());
    assertEquals(KEYWORD_ID, categoryRestrictedKeyword.getRestrictedKeywordId());
    assertEquals(CATEGORY_CODE, categoryRestrictedKeyword.getCategoryCode());
  }

  @Test
  public void createNewCategoryRestrictedKeywordMappingTest() throws Exception {
    categoryDetailDTO.getCategoryMappingsDetail().setDeletedKeywords(new ArrayList<>());
    categoryDetailDTO.getCategoryMappingsDetail().setAddedKeywords(new ArrayList<>());
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywordList);
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Assertions.assertEquals(1, categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().size());
    Assertions.assertEquals(KEYWORD,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getKeyword());
    Assertions.assertEquals(KEYWORD_ID,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getId());
  }

  @Test
  public void createNewCategoryDuplicateRestrictedKeywordMappingTest() throws Exception {
    categoryDetailDTO.getCategoryMappingsDetail().setDeletedKeywords(new ArrayList<>());
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setKeyword(KEYWORD);
    categoryDetailDTO.getCategoryMappingsDetail()
        .setAddedKeywords(Arrays.asList(categoryKeywordsUpdateDTO, categoryKeywordsUpdateDTO));
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywordList);
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Collections.singletonList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(this.restrictedKeywordService)
      .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService).saveRestrictedKeywords(Mockito.anyList());
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Assertions.assertEquals(1, categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().size());
    Assertions.assertEquals(KEYWORD,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getKeyword());
    Assertions.assertEquals(KEYWORD_ID,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getId());
  }

  @Test
  public void createNewCategoryAddParentKeywordAndAActionTest() throws Exception {
    categoryDetailDTO.getCategoryMappingsDetail().setDeletedKeywords(new ArrayList<>());
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setKeyword(KEYWORD);
    categoryDetailDTO.getCategoryMappingsDetail()
        .setAddedKeywords(Arrays.asList(categoryKeywordsUpdateDTO, categoryKeywordsUpdateDTO));
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Collections.singletonList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryRestrictedKeywordList.get(0).setMessage(MESSAGE);
    categoryRestrictedKeywordList.get(0).setType(TYPE);
    categoryRestrictedKeywordList.get(0).setAction(ACTION);
    categoryRestrictedKeywordList.get(1).setMessage(MESSAGE);
    categoryRestrictedKeywordList.get(1).setType(TYPE);
    categoryRestrictedKeywordList.get(1).setAction(ACTION);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywordList);
    categoryDetailDTO.getCategoryMappingsDetail().setAddedKeywords(new ArrayList<>());
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Collections.singletonList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Assertions.assertEquals(1, categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().size());
    Assertions.assertEquals(KEYWORD,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getKeyword());
    Assertions.assertEquals(KEYWORD_ID,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getId());
  }

  @Test
  public void createNewCategoryWholesaleConfigTest() throws Exception {
    categoryDetailDTO.getCategoryMappingsDetail().setDeletedKeywords(new ArrayList<>());
    categoryDetailDTO.getCategoryMappingsDetail().setAddedKeywords(new ArrayList<>());
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryMappingsDetail().setWholesaleMapping(wholesaleMappingDTO);
    categoryDetailDTO.getCategoryInfoDetail().setWholesalePriceConfigEnabled(true);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywordList);
    when(categoryService.saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture()))
        .thenReturn(parentCategory);
    doNothing().when(categoryWholesaleConfigService).updateCategoryWholesaleConfiguration(STORE_ID,
        ConverterUtil.getWholesalePriceConfiguration(STORE_ID, wholesaleMappingDTO, parentCategory));
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    verify(categoryWholesaleConfigService).updateCategoryWholesaleConfiguration(STORE_ID,
        ConverterUtil.getWholesalePriceConfiguration(STORE_ID, wholesaleMappingDTO, parentCategory));
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isWholesalePriceConfigEnabled());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Assertions.assertEquals(1, categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().size());
    Assertions.assertEquals(KEYWORD,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getKeyword());
    Assertions.assertEquals(KEYWORD_ID,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getId());
  }

  @Test
  public void publishHistoryEventForWholesaleConfigUpdateTest() {
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    categoryServiceWrapper.publishHistoryEventForWholesaleConfigUpdate(STORE_ID, CategoryUpdateHistoryDTO.builder()
        .category(category).build());
    Mockito.verify(mandatoryParameterHelper).getUsername();
    Mockito.verify(domainEventPublisherService).publishCategoryUpdateHistory(any(List.class));
  }

  @Test
  public void updateCategoriesWithWholesaleConfigTest() throws Exception {
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, ID)).thenReturn(category);
    Mockito.when(categoryWholesaleConfigService
        .updateCategoryWholesaleMappings(eq(STORE_ID), any(WholesalePriceConfiguration.class), any(Category.class),
            eq(true))).thenReturn(null);
    Mockito.doNothing().when(categoryWholesaleConfigService)
        .updateWholesaleConfigForChildCategories(eq(STORE_ID), any(WholesalePriceConfiguration.class),
            any(Category.class));
    this.categoryServiceWrapper.updateCategoriesWithWholesaleConfig(STORE_ID, ID, wholesaleMappingDTO);
    verify(categoryService).findByStoreIdAndId(STORE_ID, ID);
    verify(categoryWholesaleConfigService)
        .updateCategoryWholesaleMappings(eq(STORE_ID), any(WholesalePriceConfiguration.class), any(Category.class),
            eq(true));
    verify(categoryWholesaleConfigService)
        .updateWholesaleConfigForChildCategories(eq(STORE_ID), any(WholesalePriceConfiguration.class),
            any(Category.class));
  }

  @Test
  public void updateCategoriesWithWholesaleConfigApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, ID)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceWrapper.updateCategoriesWithWholesaleConfig(STORE_ID, ID, wholesaleMappingDTO));
    } finally {
      verify(categoryService).findByStoreIdAndId(STORE_ID, ID);
    }
  }

  @Test
  public void updateCategoryInfo_genericTemplateTest() throws Exception {
    attribute1.setMandatory(true);
    category.setGenericTemplateEligible(true);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.findByStoreIdAndIdInitCategoryAttribute(STORE_ID, PARENT_CATEGORY_ID))
        .thenReturn(parentCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(categoryService).getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID);
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
  }

  @Test
  public void updateCategoryInfo_WholesalePriceConfigEnabledTest() throws Exception {
    attribute1.setMandatory(true);
    categoryInfoUpdateDTO.setWholesalePriceConfigEnabled(true);
    when(categoryService.findByStoreIdAndIdInitCategoryAttribute(STORE_ID, ID)).thenReturn(category);
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(categoryService).getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID);
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryArgumentCaptor.getValue().isWholesalePriceConfigEnabled());
  }

  @Test
  public void createNewCategoryWholesaleConfigEmptyObjectTest() throws Exception {
    categoryDetailDTO.getCategoryMappingsDetail().setDeletedKeywords(new ArrayList<>());
    categoryDetailDTO.getCategoryMappingsDetail().setAddedKeywords(new ArrayList<>());
    categoryDetailDTO.getCategoryInfoDetail().setInternalActivationInterval(null);
    categoryDetailDTO.getCategoryMappingsDetail().setWholesaleMapping(new WholesaleMappingDTO());
    categoryDetailDTO.getCategoryInfoDetail().setWholesalePriceConfigEnabled(true);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(attributeService.findByAttributeIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(attribute2));
    when(categoryService.findByCategoryIds(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(masterCategory2));
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(catalogService.findByStoreIdAndId(STORE_ID, CATALOG_ID)).thenReturn(catalog);
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywordList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(parentCategory.getId())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(categoryService).evictChildCategoryCache(Mockito.anyList());
    categoryServiceWrapper.createCategoryWithoutEventPublish(STORE_ID, categoryDetailDTO);
    verify(attributeService).findByAttributeIds(STORE_ID, Arrays.asList(ATTRIBUTE_ID_2));
    verify(categoryService).findByCategoryIds(eq(STORE_ID), Mockito.anyList());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(catalogService).findByStoreIdAndId(STORE_ID, CATALOG_ID);
    verify(this.categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(this.categoryService).saveAndUpdateProductCategory(eq(STORE_ID), categoryArgumentCaptor.capture());
    verify(categoryShippingService).save(categoryShippingArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    Mockito.verify(categoryService).getParentCategoryHierarchyByCategoryId(parentCategory.getId());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    Assertions.assertEquals(1, categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().size());
    Assertions.assertEquals(KEYWORD,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getKeyword());
    Assertions.assertEquals(KEYWORD_ID,
        categoryArgumentCaptor.getValue().getCategoryRestrictedKeywords().get(0).getRestrictedKeyword().getId());
  }

  @Test
  public void getFinalParentCategoryCachedTest() throws Exception {
    Category c1 = new Category();
    c1.setStoreId(STORE_ID);
    c1.setCategoryCode(CATEGORY_CODE_1);
    c1.setId(CATEGORY_ID_1);
    Category c2 = new Category();
    c2.setStoreId(STORE_ID);
    c2.setCategoryCode(CATEGORY_CODE_2);
    c2.setId(CATEGORY_ID_2);
    c2.setParentCategory(c1);
    Category c3 = new Category();
    c3.setStoreId(STORE_ID);
    c3.setCategoryCode(CATEGORY_CODE_3);
    c3.setId(CATEGORY_ID_3);
    c3.setParentCategory(c2);
    Category c4 = new Category();
    c4.setStoreId(STORE_ID);
    c4.setCategoryCode(CATEGORY_CODE_4);
    c4.setId(CATEGORY_ID_4);
    c4.setParentCategory(c3);
    when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_4)).thenReturn(c4);
    List<Category> categoryHierarchy = Arrays.asList(c4, c3, c2, c1);
    when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_4)).thenReturn(categoryHierarchy);
    Assertions.assertEquals(CATEGORY_CODE_1, categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_4));
    Mockito.verify(categoryService).findByStoreIdAndId(STORE_ID, CATEGORY_ID_4);
    Mockito.verify(categoryService).findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_CODE_4);
  }

  @Test
  public void getFinalParentCategoryCachedExceptionTest() throws Exception {
    when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_4)).thenReturn(null);
    Exception exception = null;
    try {
      categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_4);
    } catch (Exception e) {
      exception = e;
    } finally {
      verify(categoryService).findByStoreIdAndId(STORE_ID, CATEGORY_ID_4);
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
    }
  }

  @Test
  public void getAllActiveCategoryTreeTest() throws Exception {
    Mockito.when(categoryService.getActiveCategoryTree(anyString(), anyString())).thenReturn(this.catDTOList);
    Mockito.when(categoryService.buildCategoryTree(catDTOList, StringUtils.EMPTY)).thenReturn(catDTOList);
    List<CategoryTreeDTO> allCategoryTree =
        categoryServiceWrapper.getActiveCategoryTree(CATALOG_NAME, STORE_ID, CATEGORY_CODE);
    verify(categoryService).getActiveCategoryTree(CATALOG_NAME, STORE_ID);
    verify(categoryService).buildCategoryTree(catDTOList, StringUtils.EMPTY);
    Assertions.assertEquals(7, allCategoryTree.size());
  }

  @Test
  public void getCategoryDetailByCategoryIdTest() throws Exception {
    category.setActivated(true);
    Mockito.when(categoryService.findByStoreIdAndIdInitCategoryAttribute(STORE_ID, CATEGORY_ID_1))
        .thenReturn(this.category);
    CategoryErrorDto categoryErrorDto = categoryServiceWrapper.getCategoryDetailByCategoryId(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findByStoreIdAndIdInitCategoryAttribute(STORE_ID, CATEGORY_ID_1);
    Assertions.assertEquals(category, categoryErrorDto.getCategory());
    Assertions.assertEquals(StringUtils.EMPTY, categoryErrorDto.getErrorCode());
    Assertions.assertEquals(StringUtils.EMPTY, categoryErrorDto.getErrorMessage());
  }

  @Test
  public void validateCategoryMfdTrueTest() throws Exception {
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_1)).thenReturn(null);
    CategoryErrorDto categoryErrorDto = categoryServiceWrapper.validateCategory(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findByStoreIdAndId(STORE_ID, CATEGORY_ID_1);
    Assertions.assertEquals(ErrorMessage.INVALID_CATEGORY_ERROR_CODE.getMessage(), categoryErrorDto.getErrorCode());
    Assertions.assertEquals(ErrorMessage.INVALID_CATEGORY_ERROR_MESSAGE.getMessage(), categoryErrorDto.getErrorMessage());
  }

  @Test
  public void validateCategoryActivationFalseTest() throws Exception {
    category.setActivated(false);
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_1)).thenReturn(category);
    Mockito.when(categoryService.findActiveChildCountForParent(STORE_ID, category)).thenReturn(0L);
    CategoryErrorDto categoryErrorDto = categoryServiceWrapper.validateCategory(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findByStoreIdAndId(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category);
    Assertions.assertEquals(ErrorMessage.INVALID_CATEGORY_ERROR_CODE.getMessage(), categoryErrorDto.getErrorCode());
    Assertions.assertEquals(ErrorMessage.INVALID_CATEGORY_ERROR_MESSAGE.getMessage(), categoryErrorDto.getErrorMessage());
  }

  @Test
  public void validateCategoryNotCnCategoryTest() throws Exception {
    category.setActivated(true);
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_1)).thenReturn(category);
    Mockito.when(categoryService.findActiveChildCountForParent(STORE_ID, category)).thenReturn(1L);
    CategoryErrorDto categoryErrorDto = categoryServiceWrapper.validateCategory(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findByStoreIdAndId(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category);
    Assertions.assertEquals(ErrorMessage.INVALID_CATEGORY_ERROR_CODE.getMessage(), categoryErrorDto.getErrorCode());
    Assertions.assertEquals(ErrorMessage.INVALID_CATEGORY_ERROR_MESSAGE.getMessage(), categoryErrorDto.getErrorMessage());
  }

  @Test
  public void validateCategoryTest() throws Exception {
    category.setActivated(true);
    Mockito.when(categoryService.findByStoreIdAndId(STORE_ID, CATEGORY_ID_1)).thenReturn(category);
    Mockito.when(categoryService.findActiveChildCountForParent(STORE_ID, category)).thenReturn(0L);
    CategoryErrorDto categoryErrorDto = categoryServiceWrapper.validateCategory(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findByStoreIdAndId(STORE_ID, CATEGORY_ID_1);
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category);
    Assertions.assertEquals(StringUtils.EMPTY, categoryErrorDto.getErrorCode());
    Assertions.assertEquals(StringUtils.EMPTY, categoryErrorDto.getErrorMessage());
  }

  @Test
  public void updateCategoryInfoOSCIDNullTest() throws Exception {
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    try {
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    } catch (Exception e) {

    } finally {
      verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    }
  }

  @Test
  public void updateCategoryInfoOSCNotActiveTest() throws Exception {
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID)).thenReturn(null);
    try {
      categoryInfoUpdateDTO.setOscId(ID1);
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    } catch (Exception e) {

    } finally {
      verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
      verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    }
  }

  @Test
  public void updateCategoryInfoOSCTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING);
    category.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoOSCDataChangeTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "oscChangeCheckSwitch" , true);
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID);
    originalSalesCategory.setOscLongText(MESSAGE);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(2);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(Mockito.anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(2, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoOSCDataChangeSwitchFalseTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "oscChangeCheckSwitch" , false);
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID);
    originalSalesCategory.setOscLongText(MESSAGE);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(2);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(Mockito.anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(2, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoOSCDataLongTextNoChangeTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "oscChangeCheckSwitch" , true);
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID);
    originalSalesCategory.setOscLongText(MESSAGE);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(Mockito.anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoOSCLongTextDataChangeTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "oscChangeCheckSwitch" , true);
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID1);
    originalSalesCategory.setOscLongText(MESSAGE);
    OriginalSalesCategory originalSalesCategory1 = new OriginalSalesCategory();
    originalSalesCategory1.setId(ID);
    originalSalesCategory1.setOscLongText(OriginalSalesCategory.OSC_LONG_TEXT);
    category.setOriginalSalesCategory(originalSalesCategory1);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(2);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(Mockito.anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(2, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoOSCExistingOSCTest() throws Exception {
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID);
    category.setGenericTemplateEligible(true);
    category.setB2bExclusive(true);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    categoryInfoUpdateDTO.setB2bExclusive(true);
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoOSCNoChangeTest() throws Exception {
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID1);
    category.setGenericTemplateEligible(true);
    category.setDisplay(true);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryInfoUpdateDTO.setDisplay(false);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(ID1);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
        .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
            Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(categoryShippingArgumentCaptor.capture());
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(oscService).findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1);
    assertEquals(SHIPPING_CODE_AS_STRING, categoryShippingArgumentCaptor.getValue().getShippingCode());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertFalse(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(shippingRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(shippingRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(shippingRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(shippingRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }


  @Test
  public void updateCategoryInfoOSCNoChangeExceptionTest() throws Exception {
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID1);
    category.setGenericTemplateEligible(true);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(null);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(STORE_ID, ID1))
        .thenReturn(originalSalesCategory);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService).saveUpdatedCategory(any(Category.class), anyInt(), eq(null), Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false));
    } finally {
      verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    }
  }

  @Test
  public void updateCategoryInfoWhenStatusChangeFlagTrue_ActivationOSCTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "addHierarchyChangeTypeInCategoryChangeEvent",true);
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID1);
    category.setGenericTemplateEligible(true);
    category.setOriginalSalesCategory(originalSalesCategory);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(null);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
        .thenReturn(new ArrayList<>());
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    Mockito.when(this.oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(anyString(), anyString()))
        .thenReturn(new OriginalSalesCategory());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, true);
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getSalesCategoryIdByMasterCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
        eq(Arrays.asList(PARENT_CATEGORY_ID)), eq(Arrays.asList(CategoryChangeEventType.CATEGORY_HIERARCHY_CHANGE,
            CategoryChangeEventType.CATEGORY_NAME_CHANGE, CategoryChangeEventType.ACTIVATE_DEACTIVATE_CHANGE,
            CategoryChangeEventType.DATA_CHANGE)), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(categoryService).getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
  }

  @Test
  public void updateCategoryInfoWhenStatusChangeFlagTrueOriginalSalesCategoryNullTest() throws Exception {
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(ID1);
    category.setGenericTemplateEligible(true);
    category.setOriginalSalesCategory(null);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setOscId(null);
    catalog = new Catalog();
    catalog.setCatalogType(CatalogType.SALES_CATALOG);
    category.setCatalog(catalog);
    when(categoryService.findByStoreIdAndId(STORE_ID, ID))
        .thenReturn(category);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    Mockito.when(this.oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(anyString(), anyString()))
        .thenReturn(new OriginalSalesCategory());
    categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, true);
    verify(categoryService).findByStoreIdAndId(STORE_ID, ID);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService)
        .saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), any(), any(), any(),
            Mockito.anySet(), any(), any());
    verify(categoryService).getParentCategoryHierarchyByCategoryId(ID);
    verify(categoryService).getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID);
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(anyString(), anyString());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(DEFAULT_DESCRIPTION, categoryArgumentCaptor.getValue().getDefaultDescription());
    assertEquals(DESCRIPTION_ENGLISH, categoryArgumentCaptor.getValue().getDescriptionEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
  }

  @Test
  public void findByStoreIdAndCategoryCodeMFDFalseTest() {
    categoryRestrictedKeyword.setMarkForDelete(Boolean.TRUE);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywords);
    List<RestrictedKeywordsMappedToCategoryResponse>
        result = this.categoryServiceWrapper.getRestrictedKeywordMappedToCategory(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Assertions.assertEquals(1, result.size());
    Assertions.assertFalse(result.contains(KEYWORD));
    Assertions.assertFalse(result.get(0).isValidateByDs());
  }

  @Test
  public void findByStoreIdAndCategoryCodeMFDFalseTest2() {
    categoryRestrictedKeyword.setMarkForDelete(Boolean.TRUE);
    categoryRestrictedKeywords.get(1).getRestrictedKeyword().setValidateByDs(true);
    when(this.categoryRestrictedKeywordService.findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryRestrictedKeywords);
    List<RestrictedKeywordsMappedToCategoryResponse>
        result = this.categoryServiceWrapper.getRestrictedKeywordMappedToCategory(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRestrictedKeywordService).findByStoreIdAndCategoryCode(STORE_ID, CATEGORY_CODE);
    Assertions.assertEquals(1, result.size());
    Assertions.assertFalse(result.contains(KEYWORD));
    Assertions.assertTrue(result.get(0).isValidateByDs());
    Assertions.assertEquals(KEYWORD_ID2, result.get(0).getKeywordId());
  }

  @Test
  public void validateCategoryForRestrictedKeywordCategoryChangeTest() {
    Catalog catalog1 = new Catalog();
    catalog1.setCatalogType(CatalogType.MASTER_CATALOG);
    Catalog catalog2 = new Catalog();
    catalog2.setCatalogType(CatalogType.SALES_CATALOG);
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = new Category();
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setActivated(true);
    category2.setCatalog(catalog2);
    Category category3 = new Category();
    category3.setCategoryCode(CATEGORY_CODE_3);
    category3.setActivated(true);
    category3.setCatalog(catalog1);
    Category category4 = new Category();
    category4.setCategoryCode(CATEGORY_CODE_4);
    category4.setActivated(true);
    category4.setCatalog(catalog1);

    when(categoryService.findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4))).thenReturn(
        Arrays.asList(category1, category2, category3));
    when(categoryService.findActiveChildCountForParent(STORE_ID, category3)).thenReturn(1L);

    categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(STORE_ID,
        new ArrayList<>(Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4)));

    verify(categoryService).findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4));
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category3);
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category4);
  }

  @Test
  public void validateCategoryForRestrictedKeywordCategoryChangeNoChildCategoryTest() {
    Catalog catalog1 = new Catalog();
    catalog1.setCatalogType(CatalogType.MASTER_CATALOG);
    Catalog catalog2 = new Catalog();
    catalog2.setCatalogType(CatalogType.SALES_CATALOG);
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = new Category();
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setActivated(true);
    category2.setCatalog(catalog2);
    Category category3 = new Category();
    category3.setCategoryCode(CATEGORY_CODE_3);
    category3.setActivated(true);
    category3.setCatalog(catalog1);
    Category category4 = new Category();
    category4.setCategoryCode(CATEGORY_CODE_4);
    category4.setActivated(true);
    category4.setCatalog(catalog1);

    when(categoryService.findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4))).thenReturn(
        Arrays.asList(category1, category2, category3));
    when(categoryService.findActiveChildCountForParent(STORE_ID, category3)).thenReturn(0L);

    categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(STORE_ID,
        new ArrayList<>(Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4)));

    verify(categoryService).findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4));
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category3);
    verify(categoryService).findActiveChildCountForParent(STORE_ID, category4);
  }

  @Test
  public void validateCategoryForRestrictedKeywordCategoryChangeAllCategoryFoundTest() {
    Catalog catalog1 = new Catalog();
    catalog1.setCatalogType(CatalogType.MASTER_CATALOG);
    Catalog catalog2 = new Catalog();
    catalog2.setCatalogType(CatalogType.SALES_CATALOG);
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = new Category();
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setActivated(true);
    category2.setCatalog(catalog2);
    Category category3 = new Category();
    category3.setCategoryCode(CATEGORY_CODE_3);
    category3.setActivated(true);
    category3.setCatalog(catalog1);
    Category category4 = new Category();
    category4.setCategoryCode(CATEGORY_CODE_4);
    category4.setActivated(true);
    category4.setCatalog(catalog1);

    when(categoryService.findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4))).thenReturn(
        Arrays.asList(category1, category2, category3, category4));
    when(categoryService.findActiveChildCountForParent(STORE_ID, category3)).thenReturn(0L);

    categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(STORE_ID,
        new ArrayList<>(Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4)));

    verify(categoryService).findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4));
    verify(categoryService, times(2)).findActiveChildCountForParent(STORE_ID, category3);
  }

  @Test
  public void validateCategoryForRestrictedKeywordCategoryChangeNoCategoryFoundTest() {
    Catalog catalog1 = new Catalog();
    catalog1.setCatalogType(CatalogType.MASTER_CATALOG);
    Catalog catalog2 = new Catalog();
    catalog2.setCatalogType(CatalogType.SALES_CATALOG);
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = new Category();
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setActivated(true);
    category2.setCatalog(catalog2);
    Category category3 = new Category();
    category3.setCategoryCode(CATEGORY_CODE_3);
    category3.setActivated(true);
    category3.setCatalog(catalog1);
    Category category4 = new Category();
    category4.setCategoryCode(CATEGORY_CODE_4);
    category4.setActivated(true);
    category4.setCatalog(catalog1);

    when(categoryService.findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4))).thenReturn(new ArrayList<>());

    categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(STORE_ID,
        new ArrayList<>(Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4)));

    verify(categoryService).findCategoriesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE_1, CATEGORY_CODE_2, CATEGORY_CODE_3, CATEGORY_CODE_4));
  }

  @Test
  public void updateB2bExclusiveFlag() {
    category1.setB2bExclusive(true);
    categoryServiceWrapper.updateB2bExclusiveFlag(category, category1, true);
  }

  @Test
  public void publishCategoryChangeAndClearCacheTest() throws Exception {
    categoryServiceWrapper.publishCategoryChangeAndClearCache(new CategoryAndHierarchyDto(category,
        Arrays.asList(PARENT_CATEGORY_ID)));
    Mockito.verify(categoryService).evictChildCategoryCache(Arrays.asList(PARENT_CATEGORY_ID));
    Mockito.verify(categoryService).evictActiveChildCategoryCache(Arrays.asList(PARENT_CATEGORY_ID));
    Mockito.verify(categoryService, times(4))
        .evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(domainEventPublisherService).publishCategory(category, null, new HashSet<>(), true);
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsNullFirstTimeTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "markRestrictedKeywordEnable",true);
    List<RestrictedKeyword> list = new ArrayList<>();
    list.add(restrictedKeyword);
    restrictedKeyword.setValidateByDs(Boolean.TRUE);
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().get(0).setKeywordId(null);
    Mockito.when(
      this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_CODE)).thenReturn(category);
    Mockito.when(
        this.restrictedKeywordService.saveRestrictedKeywords(Mockito.anyList()))
      .thenReturn(list);
    Mockito.when(
      this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID,
        KEYWORD)).thenReturn(restrictedKeyword);
    Mockito.when(
      this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID,
        KEYWORD1)).thenReturn(restrictedKeyword);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
      .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito.doNothing().when(categoryRestrictedKeywordService)
      .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID,
        Collections.singletonList(restrictedKeyword), category,
        Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    Mockito.doNothing().when(categoryRestrictedKeywordService)
      .addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
        Collections.singletonList(restrictedKeyword), category,
        Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE,
      categoryKeywordsUpdateListDTO);
    Mockito.verify(this.restrictedKeywordService)
      .saveRestrictedKeywords(Mockito.anyList());
    Mockito.verify(this.restrictedKeywordService)
      .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService)
      .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD1);
    Mockito.verify(categoryService)
      .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService)
      .getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
  }

  @Test
  public void updateCategoriesWithNoRestrictedKeywordsSaveTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "markRestrictedKeywordEnable",true);
    List<RestrictedKeyword> list = new ArrayList<>();
    list.add(restrictedKeyword);
    restrictedKeyword.setValidateByDs(null);
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().get(0).setKeywordId(null);
    Mockito.when(
      this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_CODE)).thenReturn(category);
    Mockito.when(
        this.restrictedKeywordService.saveRestrictedKeywords(Mockito.anyList()))
      .thenReturn(list);
    Mockito.when(
      this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID,
        KEYWORD)).thenReturn(restrictedKeyword);
    Mockito.when(
      this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID,
        KEYWORD1)).thenReturn(restrictedKeyword1);
    Mockito.when(
        this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
      .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito.doNothing().when(categoryRestrictedKeywordService)
      .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID,
        Collections.singletonList(restrictedKeyword), category,
        Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    Mockito.doNothing().when(categoryRestrictedKeywordService)
      .addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
        Collections.singletonList(restrictedKeyword), category,
        Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE,
      categoryKeywordsUpdateListDTO);
    Mockito.verify(this.restrictedKeywordService)
      .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService)
      .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD1);
    Mockito.verify(categoryService)
      .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService)
      .getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
  }

  @Test
  public void updateCategoriesWithNoRestrictedKeywordsSaveTest3() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "markRestrictedKeywordEnable", true);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "removeDeletedKeywordsFromAddList",
        true);
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "restrictedKeywordListChange", true);
    List<RestrictedKeyword> list = new ArrayList<>();
    list.add(restrictedKeyword);
    restrictedKeyword.setValidateByDs(null);
    category.getCatalog().setCatalogType(CatalogType.MASTER_CATALOG);
    categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().get(0).setKeywordId(null);
    Mockito.when(
        this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID,
            CATEGORY_CODE)).thenReturn(category);
    Mockito.when(this.restrictedKeywordService.saveRestrictedKeywords(Mockito.anyList()))
        .thenReturn(list);
    Mockito.when(
        this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(
            STORE_ID, KEYWORD)).thenReturn(restrictedKeyword);
    Mockito.when(
        this.restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(
            STORE_ID, KEYWORD1)).thenReturn(restrictedKeyword1);
    Mockito.when(this.restrictedKeywordService.findByKeywordId(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(Arrays.asList(restrictedKeyword));
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsMappings(STORE_ID,
            Collections.singletonList(restrictedKeyword), category,
            Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    Mockito.doNothing().when(categoryRestrictedKeywordService)
        .addAndDeleteCategoryRestrictedKeywordsForChildCategories(STORE_ID,
            Collections.singletonList(restrictedKeyword), category,
            Collections.singletonList(categoryKeywordsUpdateDTO1), new HashMap<>());
    this.categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(CATEGORY_CODE,
        categoryKeywordsUpdateListDTO);
    Mockito.verify(this.restrictedKeywordService)
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD);
    Mockito.verify(this.restrictedKeywordService)
        .findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(STORE_ID, KEYWORD1);
    Mockito.verify(categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(restrictedKeywordService)
        .getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
    Mockito.verify(restrictedKeywordService).findByKeywordId(eq(STORE_ID), Mockito.anyList());
  }

  @Test
  public void updateCategoryInfoHistoryTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    category.setActivated(false);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(1);
    categoryInfoUpdateDTO.setActivated(true);
    categoryInfoUpdateDTO.setDefaultDescription(null);
    categoryInfoUpdateDTO.setDescriptionEnglish(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
      .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
      .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
        Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    List<CategoryHistoryEventModel> historyEventModelList =
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, true);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
      eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertTrue(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(1, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertEquals(6, historyEventModelList.size());
  }

  @Test
  public void updateCategoryInfoChangeShippingHistoryTest() throws Exception {
    ShippingRequest shippingRequest = new ShippingRequest();
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    category.setActivated(false);
    category.setBopisEligible(false);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(true);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(1);
    categoryInfoUpdateDTO.setActivated(true);
    categoryInfoUpdateDTO.setDefaultDescription(null);
    categoryInfoUpdateDTO.setDescriptionEnglish(null);
    categoryInfoUpdateDTO.setBopisEligible(true);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
      .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    when(objectMapper.readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class)).thenReturn(
      shippingRequest);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
      .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
        Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING);

    List<CategoryHistoryEventModel> historyEventModelList =
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
      eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(any(CategoryShipping.class));
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING, ShippingRequest.class);
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertFalse(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(1, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertEquals(10, historyEventModelList.size());
  }

  @Test
  public void updateCategoryInfoChangeHistoryTest() throws Exception {
    ShippingRequest shippingRequest = new ShippingRequest();
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING_NEW);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    category.setActivated(false);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(false);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(1);
    categoryInfoUpdateDTO.setActivated(true);
    categoryInfoUpdateDTO.setDefaultDescription(null);
    categoryInfoUpdateDTO.setDescriptionEnglish(null);
    categoryInfoUpdateDTO.setSpecialHandling(false);
    categoryInfoUpdateDTO.setDirectFlight(false);
    categoryInfoUpdateDTO.setAgeLimit(null);
    categoryInfoUpdateDTO.setLogisticAdjustment(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
      .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    when(objectMapper.readValue(SHIPPING_CODE_AS_STRING_NEW, ShippingRequest.class)).thenReturn(
      shippingRequest);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
      .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
        Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING_NEW);

    List<CategoryHistoryEventModel> historyEventModelList =
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
      eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(any(CategoryShipping.class));
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING_NEW, ShippingRequest.class);
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertFalse(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(1, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertEquals(4, historyEventModelList.size());
  }

  @Test
  public void updateCategoryInfoShippingExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.categoryServiceWrapper, "categoryHistoryEnable", true);
    categoryShippingList.get(0).setShippingCode(SHIPPING_CODE_AS_STRING_NEW);
    category.setGenericTemplateEligible(true);
    category.setDangerousGoodsLevel(0);
    category.setActivated(false);
    category.setDefaultDescription(DESCRIPTION_ENGLISH);
    category.setDescriptionEnglish(DEFAULT_DESCRIPTION);
    categoryInfoUpdateDTO.setGenericTemplateEligible(true);
    categoryInfoUpdateDTO.setSizeChartRequired(false);
    categoryInfoUpdateDTO.setDocumentType(DOCUMENTS);
    categoryInfoUpdateDTO.setDangerousGoodsLevel(1);
    categoryInfoUpdateDTO.setActivated(true);
    categoryInfoUpdateDTO.setDefaultDescription(DEFAULT_DESCRIPTION);
    categoryInfoUpdateDTO.setDescriptionEnglish(DESCRIPTION_ENGLISH);
    categoryInfoUpdateDTO.setSpecialHandling(false);
    categoryInfoUpdateDTO.setDirectFlight(false);
    categoryInfoUpdateDTO.setAgeLimit(null);
    categoryInfoUpdateDTO.setLogisticAdjustment(null);
    doNothing().when(categoryService).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    when(categoryService.findByStoreIdAndId(STORE_ID, category.getId())).thenReturn(category);
    when(categoryReferenceService.getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId()))
      .thenReturn(new ArrayList<>());
    when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID)).thenReturn(parentCategory);
    when(categoryShippingService.findByCategoryCode(STORE_ID, CATEGORY_CODE)).thenReturn(categoryShippingList);
    when(categoryService.getParentCategoryHierarchyByCategoryId(ID)).thenReturn(Arrays.asList(PARENT_CATEGORY_ID));
    when(categoryService.getParentCategoryHierarchyByCategoryId(PARENT_CATEGORY_ID)).thenReturn(new ArrayList<>());
    doThrow(new RuntimeException()).when(objectMapper).readValue(SHIPPING_CODE_AS_STRING_NEW,
      ShippingRequest.class);
    doNothing().when(categoryShippingService).update(any(CategoryShipping.class));
    doNothing().when(categoryService)
      .saveUpdatedCategoryInfo(any(Category.class), anyInt(), eq(null), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(),
        Mockito.anyList());
    when(objectMapper.writeValueAsString(any(ShippingRequest.class))).thenReturn(SHIPPING_CODE_AS_STRING_NEW);

    List<CategoryHistoryEventModel> historyEventModelList =
      categoryServiceWrapper.updateCategoryInfo(STORE_ID, categoryInfoUpdateDTO, false);
    verify(categoryService, times(2)).getParentCategoryHierarchyByCategoryId(anyString());
    verify(categoryService).findByStoreIdAndId(STORE_ID, category.getId());
    verify(categoryReferenceService).getMasterCategoryIdBySalesCategoryId(STORE_ID, category.getId());
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, PARENT_CATEGORY_ID);
    verify(categoryService).saveUpdatedCategoryInfo(categoryArgumentCaptor.capture(), eq(PREVIOUS_ACTIVATION_INTERVAL),
      eq(Arrays.asList(PARENT_CATEGORY_ID)), Mockito.anyList(), Mockito.anySet(), Mockito.anyList(), Mockito.anyList());
    verify(categoryService, times(4)).evictGenericCategoryTreeCache(anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean());
    verify(categoryShippingService).findByCategoryCode(STORE_ID, CATEGORY_CODE);
    verify(categoryShippingService).update(any(CategoryShipping.class));
    verify(objectMapper).readValue(SHIPPING_CODE_AS_STRING_NEW, ShippingRequest.class);
    verify(objectMapper).writeValueAsString(shippingRequestArgumentCaptor.capture());
    assertEquals(NAME, categoryArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryArgumentCaptor.getValue().getNameEnglish());
    assertEquals(SEQUENCE, categoryArgumentCaptor.getValue().getSequence());
    assertEquals(PARENT_CATEGORY_ID, categoryArgumentCaptor.getValue().getParentCategory().getId());
    assertEquals(UPDATED_BY, categoryArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryArgumentCaptor.getValue().getUpdatedDate());
    assertFalse(categoryArgumentCaptor.getValue().isActivated());
    assertNull(categoryArgumentCaptor.getValue().getInternalActivationInterval());
    assertTrue(categoryArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
    assertEquals(1, categoryArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertEquals(6, historyEventModelList.size());
  }

  @Test
  public void getCategoryAndDefiningAttributesTest() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    Mockito.when(categoryService.getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE1))).thenReturn(Arrays.asList(category));
    List<Category> categories =
        this.categoryServiceWrapper.getCategoryAndDefiningAttributes(STORE_ID,
            Arrays.asList(CATEGORY_CODE1));
    verify(categoryService).getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE1));
    assertFalse(categories.isEmpty());
  }

  @Test
  public void getCategoryAndDefiningAttributesTest2() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    Mockito.when(categoryService.getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE1))).thenReturn(new ArrayList<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          this.categoryServiceWrapper.getCategoryAndDefiningAttributes(STORE_ID,
              Arrays.asList(CATEGORY_CODE1)));
    } finally {
      verify(categoryService).getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
          Arrays.asList(CATEGORY_CODE1));
    }
  }

  @Test
  public void getCategoryAndDefiningAttributesTestWithEmptyCategoryCodes() {
    Mockito.when(
            categoryService.getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
                new ArrayList<>()))
        .thenReturn(new ArrayList<>());

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceWrapper.getCategoryAndDefiningAttributes(STORE_ID, new ArrayList<>()));
  }

}
