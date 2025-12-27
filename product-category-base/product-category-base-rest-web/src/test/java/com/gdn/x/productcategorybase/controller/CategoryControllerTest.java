package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.service.CategoryHistoryService;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.util.MapperUtil;
import com.gdn.x.productcategorybase.dto.CategoryAndHierarchyDto;
import com.gdn.x.productcategorybase.dto.CategoryDetailDTO;
import com.gdn.x.productcategorybase.dto.CategoryErrorDto;
import com.gdn.x.productcategorybase.dto.CategoryInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryServiceDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeAndKeywordListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryDetailRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryReferenceRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.MinWholesaleDiscountRequest;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleConfigRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.helper.CategoryServiceHelper;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryRestrictedKeywordService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryServiceWrapper;
import com.gdn.x.productcategorybase.service.CategoryWholesaleConfigService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

public class CategoryControllerTest {
  private static final String USERNAME = "username";
  private static final String ATTRIBUTE_NAME = "attribute-name";
  private static final String CATALOG_ID = "CATALOG_ID";
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String CATALOG_CODE = "Catalog-code";
  private static final String CATALOG_NAME = "Catalog-name";
  private static final String CATEGORY_ID = "3";
  private static final String ATTRIBUTE_ID = "34";
  private static final String MASTER_CATEGORY_REFERENCE_ID = "4";
  private static final String MASTER_CATEGORY_ID = "5";
  private static final String SHORT_DESCRIPTION = "short-description";
  private static final int SEQUENCE = 3;
  private static final String CODE = "code";
  private static final String NAME = "name";
  private static final String REQUEST_ID = "request-id";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String CREATE = "/create";
  private static final String REFERENCE_CREATE = "/reference/create";
  private static final String BASE_PATH = "/api/category";
  private static final String DETAIL = "/api/category/{id}";
  private static final String DETAIL_BY_CATEGORY_CODE = "/api/category/categoryCode/{categoryCode}";
  private static final String BASIC_CATEGORY_AND_CATALOG_INFO = "/api/category/basicInfo/{categoryCode}";
  private static final String DETAIL_BY_CATEGORY_ID = "/api/category/categoryId/{categoryId}";
  private static final String DELETE = "/delete";
  private static final String FILTER_NAME_PAGEABLE = "/api/category/filter/name/pageable";
  private static final String GET_ALL_CATEGORY_TREE = "/getAllCategoryTree";
  private static final String GET_ACTIVE_CATEGORY_TREE = "/getActiveCategoryTree";
  private static final String GET_CATEGORY_TREE = "/getCategoryTree";
  private static final String VALIDATE_IS_CN_CATEGORY = "/api/category/validateIsCnCategory";
  private static final String PUBLISH_ALL_CATEGORY = "/api/category/publishAllCategory";
  private static final String UPDATE_CATEGORY_INFO = "/api/category/updateInfo";
  private static final String UPDATE_CATEGORY_MAPPINGS = "/api/category/updateMappings";
  private static final String CREATE_CATEGORY = "/api/category/createCategory";
  private static final String GET_CATEGORY_TREE_WITH_REVIEW_CONFIG = "/api/category/getCategoryTreeWithReviewConfig";
  private static final String GET_CATEGORY_RESTRICTED_KEYWORDS = "/api/category/get-restricted-keywords";
  private static final String VALIDATE_CATEGORY = "/api/category/{categoryId}/validate-category";
  private static final String SUMMARY = "/summary";
  private static final String GET_RESTRICTED_KEYWORD_MAPPED_TO_CATEGORY =
      "/api/category/{categoryCode}/restricted-keywords";
  private static final String UPDATE_CATEGORIES_WITH_RESTRICTED_KEYWORDS =
      "/api/category/{categoryCode}/restricted-keywords";
  private static final String PARENT_CATEGORY_ID = "1";
  private static final String CATEGORY1_CODE = "CATEGORY_1_CODE";
  private static final String SHIPPING_CODE = "{\"deliveredByMerchant\":false,\"specialHandling\":false,\"directFlight\":true,\"ageLimit\":true, \"sizeChartRequired\":true}";
  private static final String CATEGORY2_CODE = "CATEGORY_2_CODE";
  private static final String CATEGORY1_NAME = "CATEGORY_1_NAME";
  private static final String CATEGORY2_NAME = "CATEGORY_2_NAME";
  private static final String STATE = "ALL";
  private static final String ID = "id";
  private static final String NAME_ENGLISH = "nameEnglish";
  private static final byte[] DESCRIPTION = "description".getBytes();
  private static final byte[] DESCRIPTION_ENGLISH = "descriptionEnglish".getBytes();
  private static final Integer INTERNAL_ACTIVATION_INTERVAL = 100;
  private static final Integer LOGISTIC_ADJUSTMENT = 10;
  private static final String UPDATED_BY = "updatedBy";
  private static final Date UPDATED_DATE = new Date();
  private static final int DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD_ID = "keywordId";
  private static final String GENERIC_TEMPLATE_ELIGIBLE = "true";
  private static final String GET_CATEGORY_TREE_FOR_GENERIC_TEMPLATE = "/api/category/getGenericTemplateCategories";
  private static final Integer QUANTITY = 10;
  private static final Double PERCENTAGE = 10D;
  private static final Double PRICE = 100000D;
  private static final String CONFIGURATION_TYPE = "PERCENTAGE";
  private static final boolean WHOLESALE_CONFIG_ENABLE = true;
  private static final String OSC_CODE = "OSC_CODE";
  private static final String OSC_SHORT_TEXT = "OSC_SHORT_TEXT";
  private static final String OSC_LONG_TEXT = "OSC_LONG_TEXT";
  private static final Boolean ACTIVATED = true;
  private static final int ACTION = 1;
  private static final String TYPE = "type";
  private static final String MESSAGE = "message";
  private static final String DESTINATION_CATEGORY = "destination_category";

  private static final String CHILD_PARENT_PAGEABLE =
      "/api/category/filter/childParent/pageable/{id}";
  private static final String CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT =
      "/api/category/filter/childParent/catalog-type/pageablecount";
  private static final String CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE =
      "/api/category/filter/childParent/catalog-type/pageable";
  private static final String HIERARCHY_FILTER_CATEGORY_CODE =
      "/api/category/hierarchy/filter/category-code/";
  private static final String FILTER_BULK_CATEGORY_CODE =
      "/api/category/filter/bulk/category-codes";
  private static final String CHILD_PARENT_WITH_CATALOG_ID_PAGEABLE_COUNT =
      "/filter/childParent/catalog/pageablecount";
  private static final String UPDATE_DISPLAY_FLAG = "/update/displayFlag/1234";
  public static final String CHILD_PARENT_PAGEABLE_COUNT = "/filter/childParent/pageablecount/{id}";
  private static final String GET_PARENT_CATEGORY = "/filter/parent";
  private static final String GET_FINAL_PARENT_CATEGORY = "/final-parent-category";
  private static final String GET_FINAL_PARENT_CATEGORY_CACHED = "/final-parent-category-cached";
  private static final String GET_CATEGORY_NAMES = "/getCategoryNames";
  private static final String GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE = "/getAllChildCategoriesFromC1CategoryCode";
  private static final String HIERARCHY_FILTER_CATEGORY_CODES = "/hierarchy/filter/category-codes";
  private static final String BLANK = "";
  private static final String NON_INVENTORY_CODE = "nonInventoryCode";
  private static final String GET_WHOLESALE_CONFIG_TO_CATEGORY = "/api/category/get-wholesale-config";
  private static final String UPDATE_CATEGORIES_WITH_WHOLESALE_CONFIG = "/api/category/{categoryId}/wholesale-config";
  private static final String DOCUMENTS = "Doctor's prescription, Passport, Driving License";
  private static final String ALL = "ALL";
  private static final String CREATE_OSC = "/api/category/create-original-sales-category";
  private static final String GET_OSC_BY_ID = "/api/category/{id}/original-sales-category";
  private static final String GET_CATEGORY_RESTRICTED_KEYWORD_LIST =
      "/api/category/keywords/getCategoryRestrictedKeywordByCategoryCodeAndIds";
  public static final String GET_CATEGORY_RESTRICTED_KEYWORD = "/api/category/keywords/getCategoryRestrictedKeyword";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String SIZE_CHART_ATTRIBUTE_CODE = "sizeChartAttributeCode";

  @InjectMocks
  private CategoryController categoryController;

  @Mock
  private CategoryService service;

  @Mock
  private Page<Category> categoryPage;

  @Mock
  private CatalogService catalogService;

  @Mock
  private AttributeService attributeService;

  @Mock
  private CategoryServiceHelper categoryServiceHelper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private MapperUtil mapperUtil;

  @Mock
  private CategoryServiceWrapper categoryServiceWrapper;

  @Mock
  private CategoryRestrictedKeywordService categoryRestrictedKeywordService;

  @Mock
  private CategoryWholesaleConfigService categoryWholesaleConfigService;

  @Mock
  private OriginalSalesCategoryService originalSalesCategoryService;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private CategoryHistoryService categoryHistoryService;

  @Captor
  private ArgumentCaptor<CategoryInfoUpdateDTO> categoryInfoUpdateDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryMappingsUpdateDTO> categoryMappingsUpdateDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryDetailDTO> categoryDetailDTOArgumentCaptor;

  private MockMvc mockMvc;

  private String sampleJsonReq;
  private String sampleJsonReqEmpty;
  private String sampleJsonSaveReq;
  private String sampleJsonSaveReqTemp;

  private String updateReqJsonTemp;
  private String updateReqJson;

  private SimpleRequestHolder requestJson;
  private SimpleRequestHolder requestJsonEmpty;
  private CategoryRequest categoryReqJson;
  private CategoryRequest updateCategoryRequestJson;
  private CategoryHistoryResponse categoryHistoryResponse;

  private final Pageable pageable = PageRequest.of(0, 10);
  private int sequence = 1;
  private Category category;
  private Category masterCategoryReference;
  private List<CategoryTreeDTO> catDTOList;
  private Map<String, String> categoryMap;
  private CategoryMultipleIdRequest defaultBulkCategoryCodeRequest;
  private CategoryCodeRequest categoryCodeRequest;
  private List<String> categoryCodes;
  private CategoryCodeResponse categoryCodeResponse;
  private List<String> categoryCodesResult;
  private List<CategoryShipping> categoryShippings;
  private CategoryShipping categoryShipping;
  private List<ShippingResponse> shippingResponses;
  private ShippingResponse shippingResponse;
  private Page<Category> pageOfSimplifiedCategory;
  private CategoryInfoUpdateRequest categoryInfoUpdateRequest;
  private CategoryMappingsUpdateRequest categoryMappingsUpdateRequest;
  private CategoryDetailRequest categoryDetailRequest;
  private List<CategoryServiceDTO> categoryServiceDTOS;
  private CategoryServiceDTO categoryServiceDTO;
  private CategoryDetailDTO categoryDetailDTO;
  private CategoryMappingsUpdateDTO categoryMappingsUpdateDTO;
  private CategoryInfoUpdateDTO categoryInfoUpdateDTO;
  private CategoryKeywordsUpdateRequest categoryKeywordsUpdateRequest;
  private CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList;
  private CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest;
  private CategoryRestrictedKeywordsResponse categoryRestrictedKeywordsResponse;
  private RestrictedKeywordsResponse restrictedKeywordsResponse;
  private List<RestrictedKeywordsResponse> restrictedKeywordsResponseList;
  private Page<RestrictedKeywordsResponse> restrictedKeywordsResponsePage;
  private CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse;
  private RestrictedKeyword restrictedKeyword;
  private WholesaleMappingRequest wholesaleMapping;
  private WholesaleConfigRequest wholesaleConfig;
  private MinWholesaleDiscountRequest minWholesaleDiscount;
  private WholesaleMappingResponse wholesaleMappingResponse;
  private WholesaleConfigResponse wholesaleConfigResponse;
  private MinWholesaleDiscountResponse minWholesaleDiscountResponse;
  private OriginalSalesCategoryRequest originalSalesCategoryRequest;
  private OriginalSalesCategoryResponse originalSalesCategoryResponse;
  private OscInfoUpdateDTO oscInfoUpdateDTO;
  private CategoryCodeAndKeywordListRequest categoryCodeAndKeywordListRequest;

  @Test
  public void deleteCategoryTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder =
        new SimpleRequestHolder(CategoryControllerTest.CATEGORY_ID);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(simpleRequestHolder);
    this.mockMvc
        .perform(
            post(CategoryController.BASE_PATH + CategoryControllerTest.DELETE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(request).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).markForDeleteCategory(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATEGORY_ID);

  }

  @Test
  public void deleteCategoryTestJson() throws Exception {
    this.mockMvc
        .perform(
            post(CategoryController.BASE_PATH + CategoryControllerTest.DELETE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(this.sampleJsonReq).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).markForDeleteCategory(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void deleteCategoryWithEmptyIdTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder = new SimpleRequestHolder("");
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(simpleRequestHolder);
    try {
      this.mockMvc
          .perform(
              post(CategoryController.BASE_PATH + CategoryControllerTest.DELETE)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(request).param("storeId", CategoryControllerTest.STORE_ID)
                  .param("channelId", CategoryControllerTest.CHANNEL_ID)
                  .param("clientId", CategoryControllerTest.CLIENT_ID)
                  .param("requestId", CategoryControllerTest.REQUEST_ID)
                  .param("username", CategoryControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void deleteCategoryWithEmptyIdTestJson() throws Exception {
    try {
      this.mockMvc
          .perform(
              post(CategoryController.BASE_PATH + CategoryControllerTest.DELETE)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                  .content(this.sampleJsonReqEmpty)
                  .param("storeId", CategoryControllerTest.STORE_ID)
                  .param("channelId", CategoryControllerTest.CHANNEL_ID)
                  .param("clientId", CategoryControllerTest.CLIENT_ID)
                  .param("requestId", CategoryControllerTest.REQUEST_ID)
                  .param("username", CategoryControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void getCategoriesByCategoryCodesWithoutActivated1() throws Exception {
    List<Category> content = getDefaultCategories();
    List<String> categoryCodes = defaultBulkCategoryCodeRequest.getCategoryCode();
    Page<Category> pageOfCategories = new PageImpl<Category>(content, pageable, content.size());
    Page<Category> pageOfChildrenCategories =
        new PageImpl<Category>(new ArrayList<Category>(), pageable, 0);
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.objectMapper.readValue(requestString, CategoryMultipleIdRequest.class)).thenReturn(
        defaultBulkCategoryCodeRequest);
    when(
        this.service.findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID, categoryCodes,
            null, pageable)).thenReturn(pageOfCategories);
    when(
        this.service.findChildForParent(eq(CategoryControllerTest.STORE_ID),
            (Category) any(), eq(pageable))).thenReturn(
        pageOfChildrenCategories);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID,
        categoryCodes, null, pageable);
    verify(this.service, Mockito.times(content.size())).findChildForParent(
        eq(CategoryControllerTest.STORE_ID), (Category) any(),
        eq(pageable));
  }

  @Test
  public void getCategoriesByCategoryCodesWithoutActivated2() throws Exception {
    String activated = "";
    List<Category> content = getDefaultCategories();
    List<String> categoryCodes = defaultBulkCategoryCodeRequest.getCategoryCode();
    Page<Category> pageOfCategories = new PageImpl<Category>(content, pageable, content.size());
    Page<Category> pageOfChildrenCategories =
        new PageImpl<Category>(new ArrayList<Category>(), pageable, 0);
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.objectMapper.readValue(requestString, CategoryMultipleIdRequest.class)).thenReturn(
        defaultBulkCategoryCodeRequest);
    when(
        this.service.findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID, categoryCodes,
            null, pageable)).thenReturn(pageOfCategories);
    when(
        this.service.findChildForParent(eq(CategoryControllerTest.STORE_ID),
            (Category) any(), eq(pageable))).thenReturn(
        pageOfChildrenCategories);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
                .param("activated", activated)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID,
        categoryCodes, null, pageable);
    verify(this.service, Mockito.times(content.size())).findChildForParent(
        eq(CategoryControllerTest.STORE_ID), (Category) any(),
        eq(pageable));
  }

  @Test
  public void getCategoriesByCategoryCodesWithActivatedTrue() throws Exception {
    String activated = "true";
    List<Category> content = getDefaultCategories();
    List<String> categoryCodes = defaultBulkCategoryCodeRequest.getCategoryCode();
    Page<Category> pageOfCategories = new PageImpl<Category>(content, pageable, content.size());
    Page<Category> pageOfChildrenCategories =
        new PageImpl<Category>(new ArrayList<Category>(), pageable, 0);
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.objectMapper.readValue(requestString, CategoryMultipleIdRequest.class)).thenReturn(
        defaultBulkCategoryCodeRequest);
    when(
        this.service.findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID, categoryCodes,
            Boolean.valueOf(activated), pageable)).thenReturn(pageOfCategories);
    when(
        this.service.findChildForParent(eq(CategoryControllerTest.STORE_ID),
            (Category) any(), eq(pageable))).thenReturn(
        pageOfChildrenCategories);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
                .param("activated", activated)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID,
        categoryCodes, Boolean.valueOf(activated), pageable);
    verify(this.service, Mockito.times(content.size())).findChildForParent(
        eq(CategoryControllerTest.STORE_ID), (Category) any(),
        eq(pageable));
  }

  @Test
  public void getCategoriesByCategoryCodesWithActivatedFalse() throws Exception {
    String activated = "false";
    List<Category> content = getDefaultCategories();
    List<String> categoryCodes = defaultBulkCategoryCodeRequest.getCategoryCode();
    // for (Category category : content)
    // categoryCodes.add(category.getCategoryCode());
    Page<Category> pageOfCategories = new PageImpl<Category>(content, pageable, content.size());
    Page<Category> pageOfChildrenCategories =
        new PageImpl<Category>(new ArrayList<Category>(), pageable, 0);
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.objectMapper.readValue(requestString, CategoryMultipleIdRequest.class)).thenReturn(
        defaultBulkCategoryCodeRequest);
    when(
        this.service.findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID, categoryCodes,
            Boolean.valueOf(activated), pageable)).thenReturn(pageOfCategories);
    when(
        this.service.findChildForParent(eq(CategoryControllerTest.STORE_ID),
            (Category) any(), eq(pageable))).thenReturn(
        pageOfChildrenCategories);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
                .param("activated", activated)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID,
        categoryCodes, Boolean.valueOf(activated), pageable);
    verify(this.service, Mockito.times(content.size())).findChildForParent(
        eq(CategoryControllerTest.STORE_ID), (Category) any(),
        eq(pageable));
  }

  @Test
  public void getCategoriesByCategoryCodesWithoutActivated1AndListRequestBody() throws Exception {
    List<Category> content = getDefaultCategories();
    List<String> categoryCodes = defaultBulkCategoryCodeRequest.getCategoryCode();
    String[] mapperReturn = categoryCodes.toArray(new String[0]);
    Page<Category> pageOfCategories = new PageImpl<Category>(content, pageable, content.size());
    Page<Category> pageOfChildrenCategories =
        new PageImpl<Category>(new ArrayList<Category>(), pageable, 0);
    String requestString = OBJECT_MAPPER.writeValueAsString(categoryCodes);
    doThrow(RuntimeException.class).when(this.objectMapper).readValue(requestString,
        CategoryMultipleIdRequest.class);
    when(this.objectMapper.readValue(requestString, String[].class)).thenReturn(mapperReturn);
    when(
        this.service.findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID, categoryCodes,
            null, pageable)).thenReturn(pageOfCategories);
    when(
        this.service.findChildForParent(eq(CategoryControllerTest.STORE_ID),
            (Category) any(), eq(pageable))).thenReturn(
        pageOfChildrenCategories);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID,
        categoryCodes, null, pageable);
    verify(this.service, Mockito.times(content.size())).findChildForParent(
        eq(CategoryControllerTest.STORE_ID), (Category) any(),
        eq(pageable));
  }

  @Test
  public void getCategoriesByCategoryCodesWithNullRequestDto() throws Exception {
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(null))
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void getCategoriesByCategoryCodesWithoutRequestDto() throws Exception {
    categoryController.getCategoriesByCategoryCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, pageable.getPageNumber(), pageable.getPageSize(), "TRUE", null);
  }

  @Test
  public void getCategoriesByCategoryCodesWithActivatedTrueException() throws Exception {
    String activated = "true";
    List<Category> content = getDefaultCategories();
    List<String> categoryCodes = defaultBulkCategoryCodeRequest.getCategoryCode();
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.objectMapper.readValue(requestString, CategoryMultipleIdRequest.class)).thenReturn(
        defaultBulkCategoryCodeRequest);
    when(
        this.service.findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID, categoryCodes,
            Boolean.valueOf(activated), pageable)).thenReturn(null);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
                .param("activated", activated)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findByStoreIdAndCategoryCodes(CategoryControllerTest.STORE_ID,
        categoryCodes, Boolean.valueOf(activated), pageable);
  }

  @Test
  public void getCategoriesByCategoryCodesWithNullCategoryCodes() throws Exception {
    defaultBulkCategoryCodeRequest.setCategoryCode(null);
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest))
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void getCategoriesByCategoryCodesWithEmptyCategoryCodes() throws Exception {
    defaultBulkCategoryCodeRequest.setCategoryCode(new ArrayList<String>());
    this.mockMvc
        .perform(
            post(CategoryControllerTest.FILTER_BULK_CATEGORY_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest))
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void getCategoryByNamePageable() throws Exception {
    CategoryResponse response = new CategoryResponse();
    response.setName(this.category.getName());
    response.setSequence(SEQUENCE);
    response.setShortDescription(SHORT_DESCRIPTION);
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponseList.add(response);
    GdnRestListResponse<CategoryResponse> categoryResponseGdnRestListResponse =
        new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, categoryResponseList,
            new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), categoryPage.getTotalElements()),
            REQUEST_ID);
    when(categoryServiceHelper.findByNameWithChildCount(anyString(), anyString(), anyString(), any(), anyString(), anyString()))
        .thenReturn(categoryResponseGdnRestListResponse);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.FILTER_NAME_PAGEABLE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("name", CategoryControllerTest.NAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryServiceHelper)
        .findByNameWithChildCount(CategoryControllerTest.REQUEST_ID, CategoryControllerTest.STORE_ID,
            CategoryControllerTest.NAME, this.pageable, CategoryControllerTest.STATE, STATE);
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void getCategoryDetail() throws Exception {
    this.category.getCategoryAttributes().add(
        new CategoryAttribute(this.category, new Attribute("name",
            AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(
            1), false, false, CategoryControllerTest.STORE_ID));
    Mockito.when(
        this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATEGORY_ID)).thenReturn(this.category);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.DETAIL, CategoryControllerTest.CATEGORY_ID)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.value.name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.value.sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(jsonPath("$.value.shortDescription", equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.value.umkm", equalTo(true)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATEGORY_ID);
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, category.getDangerousGoodsLevel());
  }

  @Test
  public void getCategoryDetailWithNullCategoryId() throws Exception {
    Mockito.when(this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID, null))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.service)
        .findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailWithNullStoreIdAndCategoryId() throws Exception {
    Mockito.when(this.service.findByStoreIdAndIdInitCategoryAttribute(null, null)).thenThrow(RuntimeException.class);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.service)
        .findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailWithShippingData() throws Exception {
    category.setActivated(false);
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(1),
        false, false, CategoryControllerTest.STORE_ID));
    Mockito.doReturn(new CategoryErrorDto(category, StringUtils.EMPTY, StringUtils.EMPTY))
        .when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.doReturn(this.categoryShippings).when(this.service).findShippingInfoByStoreIdCategoryCode(
        CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
    Mockito.when(mapperUtil.getShippingResponseList(categoryShippings)).thenReturn(shippingResponses);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.value.name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.value.sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(jsonPath("$.value.umkm", equalTo(true)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.shippingResponses[0].sizeChartRequired", equalTo(true)));
    verify(this.mapperUtil).getShippingResponseList(categoryShippings);
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    verify(this.service)
        .findShippingInfoByStoreIdCategoryCode(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
  }

  @Test
  public void getCategoryDetailWithShippingDataValidCategoryTest() throws Exception {
    category.setActivated(true);
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(1),
        false, false, CategoryControllerTest.STORE_ID));
    Mockito.doReturn(new CategoryErrorDto(category, StringUtils.EMPTY, StringUtils.EMPTY))
        .when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.doReturn(this.categoryShippings).when(this.service).findShippingInfoByStoreIdCategoryCode(
        CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
    Mockito.when(mapperUtil.getShippingResponseList(categoryShippings)).thenReturn(shippingResponses);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.value.name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.value.sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(jsonPath("$.value.umkm", equalTo(true)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.shippingResponses[0].sizeChartRequired", equalTo(true)));
    verify(this.mapperUtil).getShippingResponseList(categoryShippings);
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    verify(this.service)
        .findShippingInfoByStoreIdCategoryCode(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
  }

  @Test
  public void getCategoryDetailWithNullShippingData() throws Exception {
    category.setActivated(false);
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(1),
        false, false, CategoryControllerTest.STORE_ID));
    Mockito.doReturn(new CategoryErrorDto(category, StringUtils.EMPTY, StringUtils.EMPTY))
        .when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.doReturn(null).when(this.service)
        .findShippingInfoByStoreIdCategoryCode(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
    Mockito.when(mapperUtil.getShippingResponseList(null)).thenReturn(shippingResponses);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    verify(this.service)
        .findShippingInfoByStoreIdCategoryCode(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
    verify(this.mapperUtil).getShippingResponseList(null);
  }

  @Test
  public void getCategoryDetail_WhenMasterCatalogTypeTest() throws Exception {
    category.setActivated(true);
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false,
            CategoryControllerTest.STORE_ID), Integer.valueOf(1), false, false,
        CategoryControllerTest.STORE_ID));
    this.category.getCatalog().setCatalogType(CatalogType.SALES_CATALOG);
    Mockito.doReturn(new CategoryErrorDto(category, StringUtils.EMPTY, StringUtils.EMPTY))
        .when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    this.mockMvc.perform(
        get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailWithShippingDataWithApplicationException() throws Exception {
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(1),
        false, false, CategoryControllerTest.STORE_ID));
    Mockito.doThrow(RuntimeException.class).when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailWithApplicationRuntimeException() throws Exception {
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(1),
        false, false, CategoryControllerTest.STORE_ID));
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailWithShippingDataWithException() throws Exception {
    this.category.getCategoryAttributes().add(new CategoryAttribute(this.category,
        new Attribute("name", AttributeType.DEFINING_ATTRIBUTE, false, CategoryControllerTest.STORE_ID), Integer.valueOf(1),
        false, false, CategoryControllerTest.STORE_ID));
    Mockito.doThrow(RuntimeException.class).when(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    this.mockMvc.perform(get(CategoryControllerTest.DETAIL_BY_CATEGORY_ID, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.categoryServiceWrapper)
        .getCategoryDetailByCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailByCode() throws Exception {
    Mockito.when(
        this.service.findByStoreIdAndCategoryCode(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CODE)).thenReturn(this.category);

    this.mockMvc
        .perform(
            get(CategoryControllerTest.DETAIL_BY_CATEGORY_CODE, CategoryControllerTest.CODE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CategoryControllerTest.CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.value.shortDescription", equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.umkm", equalTo(true)));


    verify(this.service).findByStoreIdAndCategoryCode(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CODE);
  }

  @Test
  public void getBasicCategoryInfoAndCatalogInfo() throws Exception {
    Mockito.when(
        this.service.findBasicInfoByStoreIdAndCategoryCode(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CODE)).thenReturn(this.category);

    this.mockMvc
        .perform(
            get(CategoryControllerTest.BASIC_CATEGORY_AND_CATALOG_INFO, CategoryControllerTest.CODE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CategoryControllerTest.CODE)))
        .andExpect(jsonPath("$.value.name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.value.shortDescription", equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.umkm", equalTo(true)));


    verify(this.service).findBasicInfoByStoreIdAndCategoryCode(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CODE);
  }

  @Test
  public void getBasicCategoryInfoAndCatalogInfoExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationException.class).when(this.service)
        .findBasicInfoByStoreIdAndCategoryCode(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);

    this.mockMvc
        .perform(
            get(CategoryControllerTest.BASIC_CATEGORY_AND_CATALOG_INFO, CategoryControllerTest.CODE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.service).findBasicInfoByStoreIdAndCategoryCode(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CODE);
  }

  @Test
  public void getChildFromParent() throws Exception {
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_PAGEABLE, CategoryControllerTest.CATEGORY_ID)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParent(CategoryControllerTest.STORE_ID, this.category,
        this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void testGetChildFromParentCount() throws Exception {
    Category parentCategory = new Category();
    parentCategory.setId("12345");
    this.category.setParentCategory(parentCategory);
    this.mockMvc
        .perform(
            get(
                CategoryControllerTest.BASE_PATH
                    + CategoryControllerTest.CHILD_PARENT_PAGEABLE_COUNT,
                CategoryControllerTest.CATEGORY_ID)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service, Mockito.times(2)).findChildForParent(CategoryControllerTest.STORE_ID,
        this.category, this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage, Mockito.times(2)).getTotalElements();
  }

  @Test
  public void getChildFromParentWithCatalogId() throws Exception {
    Mockito.when(this.categoryServiceWrapper.setChildCountByFilterType(STORE_ID, categoryPage, STATE))
        .thenReturn(categoryServiceDTOS);
    this.mockMvc
        .perform(
            get(
                CategoryControllerTest.BASE_PATH
                    + CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_ID_PAGEABLE_COUNT)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("catalogId", CategoryControllerTest.CATALOG_ID)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("filterType", CategoryControllerTest.STATE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findChildForParentByCatalogId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATEGORY_ID, CategoryControllerTest.CATALOG_ID, ALL, true, false, this.pageable);
    verify(this.categoryServiceWrapper).setChildCountByFilterType(STORE_ID, categoryPage, STATE);
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeWithChildCountTest() throws Exception {
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, null, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, null, this.pageable);
    verify(this.service).findChildForParent(CategoryControllerTest.STORE_ID, this.category,
        this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage, Mockito.times(2)).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeAndActivatedWithChildCountTest() throws Exception {
    Boolean activated = true;
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, activated, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, activated, this.pageable);
    verify(this.service).findChildForParent(CategoryControllerTest.STORE_ID, this.category,
        this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage, Mockito.times(2)).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeWithFalseActivatedWithChildCountTest()
      throws Exception {
    Boolean activated = false;
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, activated, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, activated, this.pageable);
    verify(this.service).findChildForParent(CategoryControllerTest.STORE_ID, this.category,
        this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage, Mockito.times(2)).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeWithDifferentActivatedWithChildCountTest()
      throws Exception {
    String activated = "xyz";
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, null, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, null, this.pageable);
    verify(this.service).findChildForParent(CategoryControllerTest.STORE_ID, this.category,
        this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage, Mockito.times(2)).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeActivatedWithChildCountTest() throws Exception {
    String activated = "";
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, null, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, null, this.pageable);
    verify(this.service).findChildForParent(CategoryControllerTest.STORE_ID, this.category,
        this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage, Mockito.times(2)).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeTest() throws Exception {
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, null, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, null, this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeAndActivatedTest() throws Exception {
    Boolean activated = true;
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, activated, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, activated, this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeWithActivatedTest() throws Exception {
    String activated = "";
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, null, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, null, this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeWithFalseActivatedTest() throws Exception {
    boolean activated = false;
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, activated, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, activated, this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void getChildFromParentByCatalogTypeWithDifferentActivatedTest() throws Exception {
    String activated = "xyz";
    when(
        this.service.findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
            this.category, CatalogType.MASTER_CATALOG, null, this.pageable)).thenReturn(
        this.categoryPage);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryId", CategoryControllerTest.CATEGORY_ID)
                .param("catalogType", CatalogType.MASTER_CATALOG.toString())
                .param("activated", String.valueOf(activated)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(
            jsonPath("$.content[0].catalog.catalogType",
                equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).findById(CategoryControllerTest.CATEGORY_ID);
    verify(this.service).findChildForParentWithCatalogType(CategoryControllerTest.STORE_ID,
        this.category, CatalogType.MASTER_CATALOG, null, this.pageable);
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  private Category getDefaultCategory(String name) {
    Category category = new Category(CategoryControllerTest.STORE_ID, name, this.sequence);
    category.setDescription("description".getBytes());
    category.setCatalog(new Catalog());
    category.setShortDescription("shortDescription");
    this.sequence++;
    return category;
  }

  private List<Category> getDefaultCategories() {
    List<Category> result = new ArrayList<Category>();
    Catalog catalog =
        new Catalog("TEST", "TEST", CatalogType.MASTER_CATALOG, CategoryControllerTest.STORE_ID);
    Category category1 = new Category(CategoryControllerTest.STORE_ID, "CAT-001", "Kategori 1");
    category1.setCatalog(catalog);
    Category category2 = new Category(CategoryControllerTest.STORE_ID, "CAT-002", "Kategori 2");
    category2.setCatalog(catalog);
    result.add(category1);
    result.add(category2);
    return result;
  }

  @Test
  public void getMasterWarehouseSummary_storeIdIsNotDefault_callDbAndReturn() throws Exception {
    List<Category> categoryList = new ArrayList<>();
    categoryList.add(this.category);
    String categoryStr = OBJECT_MAPPER.writeValueAsString(categoryList);
    when(this.categoryServiceHelper.getCategoryList(anyString(), anyInt(), anyInt(), any(AtomicLong.class))).thenReturn(
        categoryStr);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.BASE_PATH).param("storeId", "11001")
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].sequence", equalTo(CategoryControllerTest.SEQUENCE)))
        .andExpect(
            jsonPath("$.content[0].shortDescription",
                equalTo(CategoryControllerTest.SHORT_DESCRIPTION)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(0)));
    verify(this.categoryServiceHelper).getCategoryList(anyString(), anyInt(), anyInt(), any(AtomicLong.class));
  }

  @Test
  public void getMasterWarehouseSummary_defaultStoreId_getFromLocalList() throws Exception {
    CategoryResponse response = new CategoryResponse();
    response.setName(this.category.getName());
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponseList.add(response);
    String categoryStr = OBJECT_MAPPER.writeValueAsString(categoryResponseList);
    when(this.categoryServiceHelper.getCategoryList(anyString(), anyInt(), anyInt(), any(AtomicLong.class))).thenReturn(
        categoryStr);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.BASE_PATH).param("storeId", STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(10)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(0)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CategoryControllerTest.NAME)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryServiceHelper).getCategoryList(anyString(), anyInt(), anyInt(), any(AtomicLong.class));

  }

  @Test
  public void saveCategoryTest() throws Exception {
    when(
        this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATALOG_ID)).thenReturn(this.category.getCatalog());
    List<CategoryAttributeRequest> categoryAttributes = new ArrayList<CategoryAttributeRequest>();
    CategoryAttributeRequest categoryAttributeRequest = new CategoryAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    Attribute attribute = new Attribute();
    attribute.setId(CategoryControllerTest.ATTRIBUTE_ID);
    attributeRequest.setId(CategoryControllerTest.ATTRIBUTE_ID);
    categoryAttributeRequest.setAttribute(attributeRequest);
    categoryAttributes.add(categoryAttributeRequest);
    CategoryRequest parentCategory = new CategoryRequest();
    parentCategory.setId("1223");
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setCreatedBy("system");
    categoryRequest.setCreatedDate(new Date());
    categoryRequest.setParentCategory(parentCategory);
    categoryRequest.setStoreId(CategoryControllerTest.STORE_ID);
    List<CategoryReferenceRequest> masterCategoryReferences =
        new ArrayList<CategoryReferenceRequest>();
    CategoryReferenceRequest categoryReference = new CategoryReferenceRequest();
    categoryReference.setMasterCategoryId(CategoryControllerTest.MASTER_CATEGORY_ID);
    categoryReference.setStoreId(CategoryControllerTest.STORE_ID);
    masterCategoryReferences.add(categoryReference);
    categoryRequest.setMasterCategoryReferences(masterCategoryReferences);
    categoryRequest.setCategoryAttributes(categoryAttributes);
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    when(this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeRequest.getId()))
        .thenReturn(attribute);
    when(
        this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.MASTER_CATEGORY_ID)).thenReturn(this.category);
    when(
        this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID, categoryRequest
            .getParentCategory().getId())).thenReturn(this.category);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).saveAndUpdateProductCategory(anyString(), any(Category.class));
    Mockito.verify(this.catalogService).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATALOG_ID);
    Mockito.verify(this.attributeService).findById(CategoryControllerTest.STORE_ID,
        attributeRequest.getId());
    Mockito.verify(this.service).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.MASTER_CATEGORY_ID);
    Mockito.verify(this.service).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        categoryRequest.getParentCategory().getId());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, category.getDangerousGoodsLevel());
  }

  @Test
  public void testSaveCategoryWithEmptyAttribute() throws Exception {
    CategoryAttributeRequest categoryAttributeRequest = new CategoryAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    Attribute attribute = new Attribute();
    attribute.setId(CategoryControllerTest.ATTRIBUTE_ID);
    attributeRequest.setId(CategoryControllerTest.ATTRIBUTE_ID);
    try {
      when(
          this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
              CategoryControllerTest.CATALOG_ID)).thenReturn(this.category.getCatalog());
      List<CategoryAttributeRequest> categoryAttributes = new ArrayList<CategoryAttributeRequest>();
      categoryAttributeRequest.setAttribute(attributeRequest);
      categoryAttributes.add(categoryAttributeRequest);
      CategoryRequest categoryRequest = new CategoryRequest();
      categoryRequest.setName(CategoryControllerTest.NAME);
      categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
      categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
      categoryRequest.setCreatedBy("system");
      categoryRequest.setCreatedDate(new Date());
      categoryRequest.setStoreId(CategoryControllerTest.STORE_ID);

      categoryRequest.setCategoryAttributes(categoryAttributes);
      CatalogRequest catalogRequest =
          new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
              CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG",
              CategoryControllerTest.STORE_ID);
      catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
      categoryRequest.setCatalog(catalogRequest);
      String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
      when(
          this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeRequest.getId()))
          .thenReturn(null);
      this.mockMvc.perform(
          post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(request).param("storeId", CategoryControllerTest.STORE_ID)
              .param("channelId", CategoryControllerTest.CHANNEL_ID)
              .param("clientId", CategoryControllerTest.CLIENT_ID)
              .param("requestId", CategoryControllerTest.REQUEST_ID)
              .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(
          status().isOk());

    } catch (Exception e) {
      Mockito.verify(this.attributeService).findById(CategoryControllerTest.STORE_ID,
          attributeRequest.getId());
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      Mockito.verify(this.catalogService).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
          CategoryControllerTest.CATALOG_ID);

    }
  }

  @Test
  public void saveCategoryTestJson() throws Exception {
    Mockito.when(
        this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATALOG_ID)).thenReturn(this.category.getCatalog());

    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.sampleJsonSaveReq).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).save(any(Category.class));
    Mockito.verify(this.catalogService).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATALOG_ID);
  }

  @Test
  public void saveCategoryTest2() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    CategoryRequest parentCategory = new CategoryRequest();
    parentCategory.setId("1223");
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setCreatedBy("system");
    categoryRequest.setCreatedDate(new Date());
    categoryRequest.setParentCategory(parentCategory);
    categoryRequest.setCategoryAttributes(null);
    categoryRequest.setMasterCategoryReferences(null);
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    Mockito.when(
        this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATALOG_ID)).thenReturn(this.category.getCatalog());
    when(
        this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID, categoryRequest
            .getParentCategory().getId())).thenReturn(this.category);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        categoryRequest.getParentCategory().getId());
    Mockito.verify(this.service).save(any(Category.class));
    Mockito.verify(this.catalogService).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATALOG_ID);

  }

  @Test
  public void saveCategoryWithEmptyCatalogIdTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setCreatedBy("system");
    categoryRequest.setCreatedDate(new Date());
    categoryRequest.setCatalog(new CatalogRequest());
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    try {
      this.mockMvc.perform(
          post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(request).param("storeId", CategoryControllerTest.STORE_ID)
              .param("channelId", CategoryControllerTest.CHANNEL_ID)
              .param("clientId", CategoryControllerTest.CLIENT_ID)
              .param("requestId", CategoryControllerTest.REQUEST_ID)
              .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(
          status().isOk());
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.CATALOG_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void saveCategoryWithEmptyCreatedByTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setCreatedDate(new Date());
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    try {
      this.mockMvc.perform(post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(request).param("storeId", CategoryControllerTest.STORE_ID)
          .param("channelId", CategoryControllerTest.CHANNEL_ID)
          .param("clientId", CategoryControllerTest.CLIENT_ID)
          .param("requestId", CategoryControllerTest.REQUEST_ID));
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void saveCategoryWithEmptyCreatedDateTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setCreatedBy("system");
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    try {
      this.mockMvc.perform(post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(request).param("storeId", CategoryControllerTest.STORE_ID)
          .param("channelId", CategoryControllerTest.CHANNEL_ID)
          .param("clientId", CategoryControllerTest.CLIENT_ID)
          .param("requestId", CategoryControllerTest.REQUEST_ID));
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  private CategoryMultipleIdRequest createDefaultBulkCategoryCodeRequest() {
    CategoryMultipleIdRequest request = new CategoryMultipleIdRequest();
    List<Category> categories = getDefaultCategories();
    List<String> categoryCodes = new ArrayList<>();
    for (Category category : categories) {
      categoryCodes.add(category.getCategoryCode());
    }
    request.setCategoryCode(categoryCodes);
    return request;
  }

  private CategoryMultipleIdRequest createOver500BulkCategoryCodeRequest() {
    CategoryMultipleIdRequest request = new CategoryMultipleIdRequest();
    List<Category> result = new ArrayList<>();
    Catalog catalog =
        new Catalog("TEST", "TEST", CatalogType.MASTER_CATALOG, CategoryControllerTest.STORE_ID);
    Category category1 = new Category(CategoryControllerTest.STORE_ID, "CAT-001", "Kategori 1");
    category1.setCatalog(catalog);
    for (int i = 0; i < 550; i++) {
      result.add(category1);
    }
    List<String> categoryCodes = new ArrayList<>();
    for (Category category : result) {
      categoryCodes.add(category.getCategoryCode());
    }
    request.setCategoryCode(categoryCodes);
    return request;
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.categoryController).build();
    ReflectionTestUtils.setField(this.categoryController, "defaultInternalActivationPeriod", 72);
    ReflectionTestUtils.setField(this.categoryController, "attributeConfiguration", "[\"1\"]");

    Catalog catalog =
        new Catalog(CategoryControllerTest.CATALOG_NAME, CategoryControllerTest.CATALOG_CODE,
            CatalogType.MASTER_CATALOG, CategoryControllerTest.STORE_ID);
    catalog.setId(CategoryControllerTest.CATALOG_ID);

    this.category = new Category();
    this.category.setCategoryCode(CategoryControllerTest.CODE);
    this.category.setName(CategoryControllerTest.NAME);
    this.category.setSequence(CategoryControllerTest.SEQUENCE);
    this.category.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    this.category.setCatalog(catalog);
    this.category.setStoreId(CategoryControllerTest.STORE_ID);
    this.category.setUmkm(true);

    this.masterCategoryReference = new Category();
    this.masterCategoryReference.setCategoryCode(CategoryControllerTest.CODE + "-ref");
    this.masterCategoryReference.setName(CategoryControllerTest.NAME + "-ref");
    this.masterCategoryReference.setSequence(CategoryControllerTest.SEQUENCE);
    this.masterCategoryReference.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    this.masterCategoryReference.setCatalog(catalog);
    this.masterCategoryReference.setStoreId(CategoryControllerTest.STORE_ID);

    catDTOList = new ArrayList<CategoryTreeDTO>();
    CategoryTreeDTO cat = new CategoryTreeDTO(REQUEST_ID, CATEGORY_ID, CATEGORY_ID, "", null, true, DOCUMENTS);
    catDTOList.add(cat);

    categoryServiceDTOS = new ArrayList<>();
    categoryServiceDTO = new CategoryServiceDTO();
    categoryServiceDTO.setCategory(category);
    categoryServiceDTO.getCategory().setCatalog(catalog);
    categoryServiceDTOS.add(categoryServiceDTO);


    categoryHistoryResponse = new CategoryHistoryResponse();
    categoryHistoryResponse.setCategoryCode(CATEGORY_CODE);

    Attribute attribute =
        new Attribute(CategoryControllerTest.ATTRIBUTE_NAME, AttributeType.DEFINING_ATTRIBUTE,
            true, CategoryControllerTest.STORE_ID);

    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    this.category.getCategoryAttributes().add(categoryAttribute);

    this.defaultBulkCategoryCodeRequest = createDefaultBulkCategoryCodeRequest();

    ObjectMapper mapper = new ObjectMapper();

    this.sampleJsonReqEmpty =
        FileUtils.readFileToString(new File("src/test/resources/deleteCategoryEmpty.json"));
    this.mockMvc = standaloneSetup(this.categoryController).build();

    this.requestJsonEmpty =
        mapper.readValue(this.sampleJsonReqEmpty,
            mapper.getTypeFactory().constructType(SimpleRequestHolder.class));
    Assertions.assertNotNull(this.requestJsonEmpty);

    this.sampleJsonReq =
        FileUtils.readFileToString(new File("src/test/resources/deleteCategory" + ".json"));
    this.mockMvc = standaloneSetup(this.categoryController).build();

    this.requestJson =
        mapper.readValue(this.sampleJsonReq,
            mapper.getTypeFactory().constructType(SimpleRequestHolder.class));
    Assertions.assertNotNull(this.requestJson);

    this.sampleJsonSaveReqTemp =
        FileUtils.readFileToString(new File("src/test/resources/saveCategory.json"));
    this.sampleJsonSaveReq =
        this.sampleJsonSaveReqTemp.replace("@{currentdate}", ""
            + Calendar.getInstance().getTime().getTime());
    this.mockMvc = standaloneSetup(this.categoryController).build();

    this.categoryReqJson =
        mapper.readValue(this.sampleJsonSaveReq,
            mapper.getTypeFactory().constructType(CategoryRequest.class));
    Assertions.assertNotNull(this.categoryReqJson);

    this.updateReqJsonTemp =
        FileUtils.readFileToString(new File("src/test/resources/updateCategory.json"));
    this.mockMvc = standaloneSetup(this.categoryController).build();

    this.updateReqJson =
        this.updateReqJsonTemp.replace("@{catalogid}", CategoryControllerTest.CATALOG_ID);
    this.updateReqJson =
        this.updateReqJson.replace("@{currentdate}", ""
            + Calendar.getInstance().getTime().getTime());

    this.mockMvc = standaloneSetup(this.categoryController).build();

    this.updateCategoryRequestJson =
        mapper.readValue(this.updateReqJson,
            mapper.getTypeFactory().constructType(CategoryRequest.class));
    Assertions.assertNotNull(this.updateCategoryRequestJson);

    when(this.service.findById(CategoryControllerTest.CATEGORY_ID)).thenReturn(this.category);
    when(this.service.findById(CategoryControllerTest.MASTER_CATEGORY_REFERENCE_ID)).thenReturn(
        this.masterCategoryReference);

    when(
        this.service.findByName(CategoryControllerTest.STORE_ID, CategoryControllerTest.NAME,
            this.pageable, CategoryControllerTest.STATE, STATE)).thenReturn(this.categoryPage);

    when(this.service.findByStoreId(CategoryControllerTest.STORE_ID, this.pageable)).thenReturn(
        this.categoryPage);

    when(
        this.service.findChildForParent(CategoryControllerTest.STORE_ID, this.category,
            this.pageable)).thenReturn(this.categoryPage);
    when(
        this.service.findChildForParentByCatalogId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATEGORY_ID, CategoryControllerTest.CATALOG_ID, ALL, true, false, this.pageable))
        .thenReturn(this.categoryPage);

    List<Category> categories = new ArrayList<>();
    categories.add(this.category);

    when(this.categoryPage.getContent()).thenReturn(categories);

    when(
        this.service.findByCategoryNameAndCatalogType(anyString(), anyString(),
            (CatalogType) any(), (Pageable) any())).thenReturn(
        this.categoryPage);

    List<Category> simplifiedCategories = new ArrayList<>();
    Category simplifiedCategory1 = new Category();
    Category simplifiedCategory2 = new Category();

    Category simplifiedCategoryParent = new Category();
    simplifiedCategory1.setParentCategory(simplifiedCategoryParent);

    simplifiedCategories.add(simplifiedCategory1);
    simplifiedCategories.add(simplifiedCategory2);

    pageOfSimplifiedCategory = new PageImpl<>(simplifiedCategories, this.pageable, 1000L);

    this.categoryMap = new HashMap<>();
    categoryMap.put(CATEGORY1_CODE, CATEGORY1_NAME);
    categoryMap.put(CATEGORY2_CODE, CATEGORY2_NAME);

    categoryCodes = Arrays.asList(CATEGORY1_CODE, CATEGORY2_CODE);
    categoryCodeRequest = new CategoryCodeRequest(categoryCodes);
    categoryCodesResult = Arrays.asList("10101", "10102");
    categoryCodeResponse = new CategoryCodeResponse(categoryCodesResult);
    categoryShipping = new CategoryShipping();
    categoryShipping.setCategoryCode(CategoryControllerTest.CATEGORY1_CODE);
    categoryShipping.setShippingCode(CategoryControllerTest.SHIPPING_CODE);
    categoryShippings = new ArrayList<>();
    categoryShippings.add(categoryShipping);
    shippingResponse = new ShippingResponse();
    shippingResponse.setDeliveredByMerchant(true);
    shippingResponse.setSizeChartRequired(true);
    shippingResponses = new ArrayList<>();
    shippingResponses.add(shippingResponse);

    categoryInfoUpdateRequest = CategoryInfoUpdateRequest.builder().categoryCode(CODE)
        .name(NAME).nameEnglish(NAME_ENGLISH).defaultDescription(DESCRIPTION)
        .descriptionEnglish(DESCRIPTION_ENGLISH).display(true).deliveredByMerchant(false)
        .internalActivationInterval(INTERNAL_ACTIVATION_INTERVAL).logisticAdjustment(LOGISTIC_ADJUSTMENT)
        .directFlight(true).specialHandling(true).sequence(SEQUENCE).warranty(true).needIdentity(true)
        .ageLimit(true).parentCategoryId(PARENT_CATEGORY_ID).activated(true).build();
    categoryInfoUpdateRequest.setId(ID);
    categoryInfoUpdateRequest.setUpdatedBy(UPDATED_BY);
    categoryInfoUpdateRequest.setUpdatedDate(UPDATED_DATE);
    categoryInfoUpdateRequest.setUmkm(true);
    categoryInfoUpdateRequest.setSizeChartRequired(true);
    categoryInfoUpdateDTO = new CategoryInfoUpdateDTO();
    categoryInfoUpdateDTO = CategoryInfoUpdateDTO.builder().categoryCode(CODE).name(NAME).nameEnglish(NAME_ENGLISH)
        .defaultDescription(DESCRIPTION).descriptionEnglish(DESCRIPTION_ENGLISH).display(true)
        .deliveredByMerchant(false).internalActivationInterval(INTERNAL_ACTIVATION_INTERVAL)
        .logisticAdjustment(LOGISTIC_ADJUSTMENT).directFlight(true).specialHandling(true).sequence(SEQUENCE)
        .warranty(true).needIdentity(true).ageLimit(true).parentCategoryId(PARENT_CATEGORY_ID).activated(true)
        .umkm(true).build();
    categoryInfoUpdateDTO.setId(ID);
    categoryInfoUpdateDTO.setUpdatedBy(UPDATED_BY);
    categoryInfoUpdateDTO.setUpdatedDate(UPDATED_DATE);

    categoryMappingsUpdateRequest = new CategoryMappingsUpdateRequest();
    categoryMappingsUpdateRequest.setAddedAttributes(Collections.emptyList());
    categoryMappingsUpdateRequest.setAddedMasterCategoryIds(Collections.emptyList());
    categoryMappingsUpdateRequest.setDeletedAttributes(Collections.emptyList());
    categoryMappingsUpdateRequest.setDeletedMasterCategoryIds(Collections.emptyList());


    categoryMappingsUpdateRequest.setId(ID);
    categoryMappingsUpdateRequest.setUpdatedBy(UPDATED_BY);
    categoryMappingsUpdateRequest.setUpdatedDate(UPDATED_DATE);
    categoryMappingsUpdateRequest.setCreatedBy(UPDATED_BY);
    categoryMappingsUpdateRequest.setCreatedDate(UPDATED_DATE);

    categoryDetailRequest =
        CategoryDetailRequest.builder().categoryInfoDetail(categoryInfoUpdateRequest)
            .categoryMappingsDetail(categoryMappingsUpdateRequest).catalogId(CATALOG_ID).build();
    categoryDetailRequest.setId(ID);
    categoryDetailRequest.setUpdatedBy(UPDATED_BY);
    categoryDetailRequest.setUpdatedDate(UPDATED_DATE);
    categoryDetailRequest.setCreatedBy(UPDATED_BY);
    categoryDetailRequest.setCreatedDate(UPDATED_DATE);
    categoryDetailDTO = new CategoryDetailDTO();
    categoryDetailDTO.setId(ID);
    categoryDetailDTO.setUpdatedBy(UPDATED_BY);
    categoryDetailDTO.setUpdatedDate(UPDATED_DATE);
    categoryDetailDTO.setCreatedBy(UPDATED_BY);
    categoryDetailDTO.setCreatedDate(UPDATED_DATE);
    categoryDetailDTO.setCatalogId(CATALOG_ID);
    categoryMappingsUpdateDTO = new CategoryMappingsUpdateDTO();
    categoryMappingsUpdateDTO.setId(ID);
    categoryMappingsUpdateDTO.setUpdatedBy(UPDATED_BY);
    categoryMappingsUpdateDTO.setUpdatedDate(UPDATED_DATE);
    categoryMappingsUpdateDTO.setCreatedBy(UPDATED_BY);
    categoryMappingsUpdateDTO.setCreatedDate(UPDATED_DATE);
    categoryDetailDTO.setCategoryMappingsDetail(categoryMappingsUpdateDTO);
    categoryDetailDTO.setCategoryInfoDetail(categoryInfoUpdateDTO);

    categoryRestrictedKeywordsRequest = new CategoryRestrictedKeywordsRequest();
    categoryRestrictedKeywordsRequest.setCategoryCode(CATEGORY1_CODE);
    categoryRestrictedKeywordsRequest.setKeyword(KEYWORD);
    restrictedKeywordsResponse = new RestrictedKeywordsResponse();
    restrictedKeywordsResponse.setSelected(true);
    restrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsResponse.setKeyword(KEYWORD);
    restrictedKeywordsResponse.setAction(ACTION);
    restrictedKeywordsResponse.setType(TYPE);
    restrictedKeywordsResponse.setMessage(MESSAGE);
    restrictedKeywordsResponse.setDestinationCategory(DESTINATION_CATEGORY);
    restrictedKeywordsResponseList = new ArrayList<>();
    restrictedKeywordsResponseList.add(restrictedKeywordsResponse);
    categoryRestrictedKeywordsResponse = new CategoryRestrictedKeywordsResponse();
    categoryRestrictedKeywordsResponse.setCategoryCode(CATEGORY1_CODE);
    categoryRestrictedKeywordsResponse.setRestrictedKeywords(restrictedKeywordsResponseList);
    restrictedKeywordsResponsePage = new PageImpl<>(restrictedKeywordsResponseList, this.pageable, 1000L);

    categoryKeywordsUpdateRequest = new CategoryKeywordsUpdateRequest();
    categoryKeywordsUpdateRequest.setKeyword(KEYWORD);
    categoryKeywordsUpdateRequest.setKeywordId(KEYWORD_ID);
    categoryKeywordsUpdateRequest.setAction(ACTION);
    categoryKeywordsUpdateRequest.setType(TYPE);
    categoryKeywordsUpdateRequest.setMessage(MESSAGE);
    categoryKeywordsUpdateRequest.setDestinationCategory(DESTINATION_CATEGORY);

    categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
    categoryKeywordUpdateRequestList.setAddedKeywords(Collections.singletonList(categoryKeywordsUpdateRequest));

    restrictedKeyword = new RestrictedKeyword();
    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setId(KEYWORD_ID);
    categoryRestrictedKeywordResponse = new CategoryRestrictedKeywordResponse();
    categoryRestrictedKeywordResponse.setRestrictedKeywordId(KEYWORD_ID);
    categoryRestrictedKeywordResponse.setAction(ACTION);
    categoryRestrictedKeywordResponse.setType(TYPE);
    categoryRestrictedKeywordResponse.setMessage(MESSAGE);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATION_CATEGORY);

    minWholesaleDiscount = new MinWholesaleDiscountRequest();
    minWholesaleDiscount.setPrice(PRICE);
    minWholesaleDiscount.setPercentage(PERCENTAGE);

    wholesaleConfig = new WholesaleConfigRequest();
    wholesaleConfig.setQuantity(QUANTITY);
    wholesaleConfig.setMinWholesaleDiscount(Collections.singletonList(minWholesaleDiscount));

    wholesaleMapping = new WholesaleMappingRequest();
    wholesaleMapping.setWholesaleConfig(Collections.singletonList(wholesaleConfig));
    wholesaleMapping.setConfigurationType(CONFIGURATION_TYPE);

    minWholesaleDiscountResponse = new MinWholesaleDiscountResponse();
    minWholesaleDiscountResponse.setPrice(PRICE);
    minWholesaleDiscountResponse.setPercentage(PERCENTAGE);
    wholesaleConfigResponse = new WholesaleConfigResponse();
    wholesaleConfigResponse.setQuantity(QUANTITY);
    wholesaleConfigResponse.setMinWholesaleDiscount(Collections.singletonList(minWholesaleDiscountResponse));
    wholesaleMappingResponse = new WholesaleMappingResponse();
    wholesaleMappingResponse.setConfigurationType(CONFIGURATION_TYPE);
    wholesaleMappingResponse.setWholesaleConfig(Collections.singletonList(wholesaleConfigResponse));
    wholesaleMappingResponse.setWholesalePriceConfigEnabled(WHOLESALE_CONFIG_ENABLE);

    originalSalesCategoryRequest = new OriginalSalesCategoryRequest();
    originalSalesCategoryRequest.setOscCode(OSC_CODE);
    originalSalesCategoryRequest.setOscLongText(OSC_LONG_TEXT);
    originalSalesCategoryRequest.setOscShortText(OSC_SHORT_TEXT);

    oscInfoUpdateDTO = OscInfoUpdateDTO.builder().activated(true).oscCode(OSC_CODE).oscLongText(OSC_LONG_TEXT)
        .oscShortText(OSC_SHORT_TEXT).build();

    originalSalesCategoryResponse = new OriginalSalesCategoryResponse();
    originalSalesCategoryResponse.setOscCode(OSC_CODE);
    originalSalesCategoryResponse.setOscLongText(OSC_LONG_TEXT);
    originalSalesCategoryResponse.setOscShortText(OSC_SHORT_TEXT);

    categoryCodeAndKeywordListRequest = new CategoryCodeAndKeywordListRequest();
    categoryCodeAndKeywordListRequest.setCategoryCode(CATEGORY1_CODE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.service);
    verifyNoMoreInteractions(this.categoryPage);
    verifyNoMoreInteractions(this.catalogService);
    verifyNoMoreInteractions(this.attributeService);
    verifyNoMoreInteractions(this.categoryServiceHelper);
    verifyNoMoreInteractions(this.mapperUtil);
    verifyNoMoreInteractions(this.categoryServiceWrapper);
    verifyNoMoreInteractions(this.categoryRestrictedKeywordService);
    verifyNoMoreInteractions(this.categoryHistoryService);
  }

  @Test
  public void filterSummaryByCategoryNameAndCatalogTypeTest() throws Exception {
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryController.FILTER_NAME_CATALOG_TYPE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("categoryName", "OMA").param("catalogType", "MASTER_CATALOG")).andExpect(
        status().isOk());
    verify(this.service).findByCategoryNameAndCatalogType(anyString(), anyString(),
        (CatalogType) any(), (Pageable) any());
    verify(this.categoryPage).getContent();
    verify(this.categoryPage).getTotalElements();
  }

  @Test
  public void updateCategoryTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setId(CategoryControllerTest.CATEGORY_ID);
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setUpdatedBy("system");
    categoryRequest.setUpdatedDate(new Date());
    CategoryRequest parentCategory = new CategoryRequest();
    parentCategory.setId(CategoryControllerTest.CATEGORY_ID);
    parentCategory.setName(CategoryControllerTest.NAME);
    parentCategory.setSequence(CategoryControllerTest.SEQUENCE);
    parentCategory.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    parentCategory.setUpdatedBy("system");
    parentCategory.setUpdatedDate(new Date());
    categoryRequest.setParentCategory(parentCategory);
    List<CategoryAttributeRequest> categoryAttributes = new ArrayList<CategoryAttributeRequest>();
    CategoryAttributeRequest categoryAttributeRequest = new CategoryAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    Attribute attribute = new Attribute();
    attribute.setId(CategoryControllerTest.ATTRIBUTE_ID);
    attributeRequest.setId(CategoryControllerTest.ATTRIBUTE_ID);
    categoryAttributeRequest.setAttribute(attributeRequest);
    categoryAttributes.add(categoryAttributeRequest);
    List<CategoryReferenceRequest> masterCategoryReferences =
        new ArrayList<CategoryReferenceRequest>();
    CategoryReferenceRequest categoryReference = new CategoryReferenceRequest();
    categoryReference.setMasterCategoryId(CategoryControllerTest.MASTER_CATEGORY_ID);
    categoryReference.setStoreId(CategoryControllerTest.STORE_ID);
    masterCategoryReferences.add(categoryReference);
    categoryRequest.setMasterCategoryReferences(masterCategoryReferences);
    categoryRequest.setCategoryAttributes(categoryAttributes);
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    List<CategoryAttribute> categoryAttributeList = new ArrayList<CategoryAttribute>();
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    categoryAttributeList.add(categoryAttribute);
    this.category.setCategoryAttributes(categoryAttributeList);
    Category parent = new Category();
    parent.setId(CategoryControllerTest.CATEGORY_ID);
    this.category.setParentCategory(parent);

    Mockito.when(
        this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATEGORY_ID)).thenReturn(this.category);
    when(this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeRequest.getId()))
        .thenReturn(attribute);
    when(
        this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.MASTER_CATEGORY_ID)).thenReturn(this.category);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).findByStoreIdAndIdInitCategoryAttribute(
        CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.verify(this.service).adjustCategory(any(Category.class), any(Category.class),
        anyString(), (Integer) eq(null));
    Mockito.verify(this.attributeService).findById(CategoryControllerTest.STORE_ID,
        attributeRequest.getId());
    Mockito.verify(this.service).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.MASTER_CATEGORY_ID);

  }

  @Test
  public void updateCategoryTestJson() throws Exception {
    Mockito.when(
        this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATEGORY_ID)).thenReturn(this.category);
    Category newCategory =
        CategoryControllerTest.OBJECT_MAPPER.readValue(this.updateReqJson, Category.class);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.updateReqJson)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).findByStoreIdAndIdInitCategoryAttribute(
        CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.verify(this.service).adjustCategory(this.category, newCategory, "", null);
  }

  @Test
  public void updateCategoryWithEmptyCatalogIdTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setId(CategoryControllerTest.CATEGORY_ID);
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setUpdatedBy("system");
    categoryRequest.setUpdatedDate(new Date());
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    try {
      this.mockMvc.perform(
          post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(request)
              .param("storeId", CategoryControllerTest.STORE_ID)
              .param("channelId", CategoryControllerTest.CHANNEL_ID)
              .param("clientId", CategoryControllerTest.CLIENT_ID)
              .param("requestId", CategoryControllerTest.REQUEST_ID)
              .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(
          status().isOk());
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.CATALOG_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void updateCategoryWithEmptyCategoryIdTest() throws Exception {
    Category category = new Category();
    category.setUpdatedBy("system");
    category.setUpdatedDate(new Date());
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(category);
    try {
      this.mockMvc.perform(
          post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(request)
              .param("storeId", CategoryControllerTest.STORE_ID)
              .param("channelId", CategoryControllerTest.CHANNEL_ID)
              .param("clientId", CategoryControllerTest.CLIENT_ID)
              .param("requestId", CategoryControllerTest.REQUEST_ID)
              .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(
          status().isOk());
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @Test
  public void updateCategoryWithEmptyUpdatedByTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setId(CategoryControllerTest.CATEGORY_ID);
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setUpdatedDate(new Date());
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    try {
      this.mockMvc.perform(
          post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(request)
              .param("storeId", CategoryControllerTest.STORE_ID)
              .param("channelId", CategoryControllerTest.CHANNEL_ID)
              .param("clientId", CategoryControllerTest.CLIENT_ID)
              .param("requestId", CategoryControllerTest.REQUEST_ID)
              .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(
          status().isOk());
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void updateCategoryWithEmptyUpdatedDateTest() throws Exception {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setId(CategoryControllerTest.CATEGORY_ID);
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setUpdatedBy("system");
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    try {
      this.mockMvc.perform(
          post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(request)
              .param("storeId", CategoryControllerTest.STORE_ID)
              .param("channelId", CategoryControllerTest.CHANNEL_ID)
              .param("clientId", CategoryControllerTest.CLIENT_ID)
              .param("requestId", CategoryControllerTest.REQUEST_ID)
              .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(
          status().isOk());
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      assertTrue(applicationException.getErrorMessage().contains(
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void uploadCategoryTest() throws Exception {
    Category category1 = this.getDefaultCategory("Elektronik");
    Category categoryX = this.getDefaultCategory("Peralatan Rumah Tangga");
    String categoryId1 = "CATE1";
    String categoryIdX = "CATEX";
    String catalogId = "CATA123";
    Catalog catalog = new Catalog();
    catalog.setId(catalogId);
    category1.setCatalog(catalog);
    category1.setParentCategory(categoryX);
    Attribute attribute12 = new Attribute();
    Attribute attribute56 = new Attribute();
    String attributeId12 = "AT12";
    String attributeId56 = "AT56";
    attribute12.setId(attributeId12);
    attribute56.setId(attributeId56);
    Mockito.when(this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeId12))
        .thenReturn(attribute12);
    Mockito.when(this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeId56))
        .thenReturn(attribute56);
    category1.getCategoryAttributes().add(
        new CategoryAttribute(category1, attribute12, 1, true, true,
            CategoryControllerTest.STORE_ID));
    Mockito
        .when(this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID, catalogId))
        .thenReturn(catalog);
    Mockito.when(this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID, categoryIdX))
        .thenReturn(categoryX);
    category.setCategoryCode(CATEGORY1_CODE);
    category.setId(categoryId1);
    Mockito.when(this.service.save(category1)).thenReturn(category);
    Mockito.when(
        this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
            categoryId1)).thenReturn(category1);
    InputStream in = this.getClass().getResourceAsStream("/Category.csv");
    MockMultipartFile file = new MockMultipartFile("categoryCsvFile", in);

    this.mockMvc.perform(MockMvcRequestBuilders.multipart(CategoryController.BASE_PATH + "/upload").file(file)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service, Mockito.times(1)).save(category1);
    Mockito.verify(this.attributeService, Mockito.times(1)).findById(
        CategoryControllerTest.STORE_ID, "AT12");
    Mockito.verify(this.attributeService, Mockito.times(1)).findById(
        CategoryControllerTest.STORE_ID, "AT56");
    Mockito.verify(this.service, Mockito.times(1)).findByStoreIdAndId(
        CategoryControllerTest.STORE_ID, categoryIdX);
    Mockito.verify(this.service, Mockito.times(1)).findByStoreIdAndIdInitCategoryAttribute(
        CategoryControllerTest.STORE_ID, categoryId1);
    Mockito.verify(this.catalogService, Mockito.times(1)).findByStoreIdAndId(
        CategoryControllerTest.STORE_ID, catalogId);

  }

  @Test
  public void testUpdateCategoryDisplayFlag() throws Exception {

    this.mockMvc.perform(get(CategoryController.BASE_PATH + UPDATE_DISPLAY_FLAG)
        .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
        .param("storeId", CategoryControllerTest.STORE_ID)
        .param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID)
        .param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME).param("flag", "true"));
    Mockito.verify(this.service).setCategoryDisplayable("1234", true);
  }

  @Test
  public void getCategoriesAndFinalCategoryMapping_test() throws Exception {
    Map<String, String> categoryMap = new HashMap<String, String>();
    categoryMap.put(DEFAULT_USERNAME, DEFAULT_USERNAME);
    when(this.service.getCategoryToFinalParent()).thenReturn(categoryMap);
    this.mockMvc.perform(get(
        CategoryController.BASE_PATH + CategoryController.GET_ALL_CATEGORIES_PARENT_MAPPING)
        .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
        .param("storeId", CategoryControllerTest.STORE_ID)
        .param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID)
        .param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME));
    verify(this.service).getCategoryToFinalParent();
  }

  @Test
  public void getParentCategories_test() throws Exception {
    when(this.service.getParentCategories()).thenReturn(new ArrayList<String>());
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_PARENT_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)).andExpect(status().isOk());
    verify(this.service).getParentCategories();
  }

  @Test
  public void getParentCategoriesNotEmpty_test() throws Exception {
    List<String> parent = new ArrayList<String>();
    parent.add(ATTRIBUTE_ID);
    when(this.service.getParentCategories()).thenReturn(parent);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_PARENT_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)).andExpect(status().isOk());
    verify(this.service).getParentCategories();
  }

  @Test
  public void getFinalParentCategoryCached() throws Exception {
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID)).thenReturn(CATEGORY_ID);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_FINAL_PARENT_CATEGORY_CACHED)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)).andExpect(status().isOk());
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategoryCachedWhenError() throws Exception {
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID)).thenThrow(Exception.class);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_FINAL_PARENT_CATEGORY_CACHED)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)).andExpect(status().isOk());
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategoryCachedWhenErrorEmpty() throws Exception {
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID)).thenThrow(
        EmptyResultDataAccessException.class);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_FINAL_PARENT_CATEGORY_CACHED)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)).andExpect(status().isOk());
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategory() throws Exception {
    when(this.service.getFinalParentCategory(CATEGORY_ID)).thenReturn(CATEGORY_ID);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_FINAL_PARENT_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)).andExpect(status().isOk());
    verify(this.service).getFinalParentCategory(CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategoryWhenError() throws Exception {
    when(this.service.getFinalParentCategory(CATEGORY_ID)).thenThrow(Exception.class);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_FINAL_PARENT_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)).andExpect(status().isOk());
    verify(this.service).getFinalParentCategory(CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategoryWhenErrorEmpty() throws Exception {
    when(this.service.getFinalParentCategory(CATEGORY_ID)).thenThrow(
        EmptyResultDataAccessException.class);
    this.mockMvc.perform(
        get(CategoryController.BASE_PATH + CategoryControllerTest.GET_FINAL_PARENT_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content("").accept(MediaType.APPLICATION_JSON)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)).andExpect(status().isOk());
    verify(this.service).getFinalParentCategory(CATEGORY_ID);
  }

  @Test
  public void getAllCategoryTree() throws Exception {
    when(this.service.getAllCategoryTree(anyString(), anyString(), anyBoolean()))
        .thenReturn(catDTOList);
    this.mockMvc
        .perform(
            get(CategoryController.BASE_PATH + CategoryControllerTest.GET_ALL_CATEGORY_TREE)
                .contentType(MediaType.APPLICATION_JSON).content("")
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).getAllCategoryTree(any(), any(), anyBoolean());
  }

  @Test
  public void getActiveCategoryTree() throws Exception {
    when(categoryServiceWrapper.getActiveCategoryTree(anyString(), eq(STORE_ID), eq(NON_INVENTORY_CODE)))
        .thenReturn(catDTOList);
    this.mockMvc
        .perform(
            get(CategoryController.BASE_PATH + CategoryControllerTest.GET_ACTIVE_CATEGORY_TREE)
                .contentType(MediaType.APPLICATION_JSON).content("")
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME)
                .param("nonInventoryCode", NON_INVENTORY_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(categoryServiceWrapper).getActiveCategoryTree(any(), eq(STORE_ID), eq(NON_INVENTORY_CODE));
  }

  @Test
  public void getAllCategoryTreeWhenError() throws Exception {
    when(this.service.getAllCategoryTree(any(), any(), anyBoolean())).thenThrow(
        Exception.class);
    this.mockMvc
        .perform(
            get(CategoryController.BASE_PATH + CategoryControllerTest.GET_ALL_CATEGORY_TREE)
                .contentType(MediaType.APPLICATION_JSON).content("")
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.service).getAllCategoryTree(any(), any(), anyBoolean());
  }

  @Test
  public void getActiveCategoryTreeException() throws Exception {
    when(categoryServiceWrapper.getActiveCategoryTree(any(), eq(STORE_ID), eq(NON_INVENTORY_CODE)))
        .thenThrow(RuntimeException.class);
    this.mockMvc
        .perform(
            get(CategoryController.BASE_PATH + CategoryControllerTest.GET_ACTIVE_CATEGORY_TREE)
                .contentType(MediaType.APPLICATION_JSON).content("")
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME)
                .param("nonInventoryCode", NON_INVENTORY_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(categoryServiceWrapper).getActiveCategoryTree(any(), eq(STORE_ID), eq(NON_INVENTORY_CODE));
  }

  @Test
  public void postConstructorTest() throws Exception{
    Method method = categoryController.getClass().getDeclaredMethod("init");
    method.setAccessible(true);
    method.invoke(categoryController);
  }

  @Test
  public void postConstructorExceptionTest() throws Exception{
    ReflectionTestUtils.setField(categoryController, "attributeConfiguration", "[test]");
    Method method = categoryController.getClass().getDeclaredMethod("init");
    method.setAccessible(true);
    method.invoke(categoryController);
  }

  @Test
  public void getCategoryTree() throws Exception {
    List<String> catCodeList = new ArrayList<>();
    catCodeList.add(CATEGORY_ID);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(catCodeList);

    when(this.service.getCategoryTree(Mockito.anyList(), anyString(), anyString()))
        .thenReturn(catDTOList);
    this.mockMvc
        .perform(
            post(CategoryController.BASE_PATH + CategoryControllerTest.GET_CATEGORY_TREE)
                .contentType(MediaType.APPLICATION_JSON).content("")
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME).content(json))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.service).getCategoryTree(Mockito.anyList(), any(),
        anyString());
  }

  @Test
  public void getCategoryTreeWhenError() throws Exception {
    List<String> catCodeList = new ArrayList<>();
    catCodeList.add(CATEGORY_ID);

    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(catCodeList);

    when(this.service.getCategoryTree(Mockito.any(), any(), any()))
        .thenThrow(RuntimeException.class);

    this.mockMvc
        .perform(
            post(CategoryController.BASE_PATH + CategoryControllerTest.GET_CATEGORY_TREE)
                .contentType(MediaType.APPLICATION_JSON).content("")
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME).content(json))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.service).getCategoryTree(Mockito.any(), any(),
        any());
  }

  @Test
  public void testGetCategorySummarySimplified() throws Exception {
    when(
        this.service.findCategorySummaryByStoreIdAndCatalogIdAndDisplay(anyString(),
            anyString(), anyBoolean(), any(Pageable.class))).thenReturn(
        pageOfSimplifiedCategory);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc
        .perform(
            get(CategoryController.BASE_PATH + CategoryControllerTest.SUMMARY + "/" + CATALOG_ID)
                .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))));

    verify(this.service).findCategorySummaryByStoreIdAndCatalogIdAndDisplay(any(),
        any(), any(), any(Pageable.class));
  }

  @Test
  public void getCategoryNamesTest() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.service.findCategoryNamesByCategoryCodes(STORE_ID,
        defaultBulkCategoryCodeRequest.getCategoryCode(), pageable)).thenReturn(categoryMap);
    this.mockMvc.perform(
            post(CategoryController.BASE_PATH + CategoryControllerTest.GET_CATEGORY_NAMES)
                .accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestString)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findCategoryNamesByCategoryCodes(eq(STORE_ID),
        eq(defaultBulkCategoryCodeRequest.getCategoryCode()), any(Pageable.class));
  }

  @Test
  public void getCategoryNamesOver500_Test() throws Exception {
    this.defaultBulkCategoryCodeRequest = createOver500BulkCategoryCodeRequest();
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.service.findCategoryNamesByCategoryCodes(STORE_ID,
        defaultBulkCategoryCodeRequest.getCategoryCode(), pageable)).thenReturn(categoryMap);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.GET_CATEGORY_NAMES)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("page", String.valueOf(pageable.getPageNumber()))
            .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void getCategoryNamesTest_Exception() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(defaultBulkCategoryCodeRequest);
    when(this.service.findCategoryNamesByCategoryCodes(BLANK,
        defaultBulkCategoryCodeRequest.getCategoryCode(), pageable)).thenThrow(Exception.class);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.GET_CATEGORY_NAMES)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .param("storeId", CategoryControllerTest.BLANK)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("page", String.valueOf(pageable.getPageNumber()))
            .param("size", String.valueOf(pageable.getPageSize()))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).findCategoryNamesByCategoryCodes(eq(BLANK),
        eq(defaultBulkCategoryCodeRequest.getCategoryCode()), any(Pageable.class));
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCode_Test() throws Exception{
    String requestString = OBJECT_MAPPER.writeValueAsString(categoryCodeRequest);
    Mockito.when(this.service
        .findAllChildForC1CategoryCodes(anyString(), Mockito.anyList(),
          anyBoolean()))
        .thenReturn(categoryCodes);
     this.mockMvc.perform(post(CategoryController.BASE_PATH
         + CategoryControllerTest.GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE)
         .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
         .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
         .param("channelId", CategoryControllerTest.CHANNEL_ID)
         .param("clientId", CategoryControllerTest.CLIENT_ID)
         .param("requestId", CategoryControllerTest.REQUEST_ID)
         .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
         .andExpect(jsonPath("$.success", equalTo(true)))
         .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    Mockito.verify(this.service)
        .findAllChildForC1CategoryCodes(anyString(), Mockito.anyList(),
          anyBoolean());
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCodeTest_Exception_Test() throws Exception{
    String requestString = OBJECT_MAPPER.writeValueAsString(categoryCodeRequest);
    Mockito.when(this.service
        .findAllChildForC1CategoryCodes(anyString(), Mockito.anyList(),
          anyBoolean()))
        .thenThrow(Exception.class);
    this.mockMvc.perform(post(
        CategoryController.BASE_PATH + CategoryControllerTest.GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
        .param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID)
        .param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    Mockito.verify(this.service)
        .findAllChildForC1CategoryCodes(anyString(), Mockito.anyList(),
          anyBoolean());
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCode_EmptyRequest_Test() throws Exception{
    String requestString = OBJECT_MAPPER.writeValueAsString(new CategoryCodeRequest());
    Mockito.when(this.service
        .findAllChildForC1CategoryCodes(anyString(), Mockito.anyList(),
          anyBoolean()))
        .thenThrow(Exception.class);
    this.mockMvc.perform(post(
        CategoryController.BASE_PATH + CategoryControllerTest.GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE)
        .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void filterCategoryHierarchyByCategoryCodesTest() throws Exception {
    CategoryCodeRequest request = new CategoryCodeRequest();
    request.setCategoryCodes(new ArrayList<String>());
    request.getCategoryCodes().add("12345");
    String requestString = OBJECT_MAPPER.writeValueAsString(request);
    Mockito
        .when(service.findCategoryHierarchyByCategoryCode(anyString(), anyString()))
        .thenReturn(getDefaultCategories());
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.HIERARCHY_FILTER_CATEGORY_CODES)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestString).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(service)
        .findCategoryHierarchyByCategoryCode(anyString(), anyString());
  }
  
  @Test
  public void validateCnCategory_Valid_SuccessTrue() throws Exception {
    Mockito.when(service.validateIsCategoryCn(STORE_ID, CATEGORY_ID)).thenReturn(true);
    Mockito.when(service.findBasicInfoByStoreIdAndCategoryCode(STORE_ID, CATEGORY_ID)).thenReturn(category);
    this.mockMvc
        .perform(
            get(CategoryControllerTest.VALIDATE_IS_CN_CATEGORY)
                .param("storeId", CategoryControllerTest.STORE_ID)
                .param("channelId", CategoryControllerTest.CHANNEL_ID)
                .param("clientId", CategoryControllerTest.CLIENT_ID)
                .param("requestId", CategoryControllerTest.REQUEST_ID)
                .param("username", CategoryControllerTest.DEFAULT_USERNAME)
                .param("categoryCode", CategoryControllerTest.CATEGORY_ID))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    
    Mockito.verify(service).validateIsCategoryCn(STORE_ID, CATEGORY_ID);
    Mockito.verify(service).findBasicInfoByStoreIdAndCategoryCode(STORE_ID, CATEGORY_ID);
  }
  
  @Test
  public void validateCnCategory_ExceptionHappen_SuccessFalse() throws Exception {
    Mockito.when(service.validateIsCategoryCn(STORE_ID, CATEGORY_ID)).thenThrow(Exception.class);
    this.mockMvc
    .perform(
        get(CategoryControllerTest.VALIDATE_IS_CN_CATEGORY)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("categoryCode", CategoryControllerTest.CATEGORY_ID))
    .andExpect(status().isOk())
    .andExpect(jsonPath("$.success", equalTo(false)));
    
    Mockito.verify(service).validateIsCategoryCn(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void saveCategoryTestWithParentCategoryNull() throws Exception {
    CategoryRequest categoryRequest = getCategoryRequest();
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    Mockito.when(
        this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATALOG_ID)).thenReturn(this.category.getCatalog());
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).save(any(Category.class));
    Mockito.verify(this.catalogService).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATALOG_ID);
  }

  @Test
  public void saveCategoryTestWithInternalActivationIntervalIsNotNull() throws Exception {
    CategoryRequest categoryRequest = getCategoryRequest();
    categoryRequest.setInternalActivationInterval(72);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    Mockito.when(
        this.catalogService.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATALOG_ID)).thenReturn(this.category.getCatalog());
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + CategoryControllerTest.CREATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(request).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).save(any(Category.class));
    Mockito.verify(this.catalogService).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.CATALOG_ID);
  }

  private CategoryRequest getCategoryRequest() {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setName(CategoryControllerTest.NAME);
    categoryRequest.setSequence(CategoryControllerTest.SEQUENCE);
    categoryRequest.setShortDescription(CategoryControllerTest.SHORT_DESCRIPTION);
    categoryRequest.setCreatedBy("system");
    categoryRequest.setUpdatedBy("system");
    categoryRequest.setUpdatedDate(new Date());
    categoryRequest.setCreatedDate(new Date());
    categoryRequest.setCategoryAttributes(null);
    categoryRequest.setMasterCategoryReferences(null);
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    return categoryRequest;
  }

  @Test
  public void updateCategoryTestWithNullParentCategory() throws Exception {
    CategoryRequest categoryRequest = getCategoryRequest();
    categoryRequest.setId(CategoryControllerTest.CATEGORY_ID);
    List<CategoryAttributeRequest> categoryAttributes = new ArrayList<CategoryAttributeRequest>();
    CategoryAttributeRequest categoryAttributeRequest = new CategoryAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    Attribute attribute = new Attribute();
    attribute.setId(CategoryControllerTest.ATTRIBUTE_ID);
    attributeRequest.setId(CategoryControllerTest.ATTRIBUTE_ID);
    categoryAttributeRequest.setAttribute(attributeRequest);
    categoryAttributes.add(categoryAttributeRequest);
    List<CategoryReferenceRequest> masterCategoryReferences =
        new ArrayList<CategoryReferenceRequest>();
    CategoryReferenceRequest categoryReference = new CategoryReferenceRequest();
    categoryReference.setMasterCategoryId(CategoryControllerTest.MASTER_CATEGORY_ID);
    categoryReference.setStoreId(CategoryControllerTest.STORE_ID);
    masterCategoryReferences.add(categoryReference);
    categoryRequest.setMasterCategoryReferences(masterCategoryReferences);
    categoryRequest.setCategoryAttributes(categoryAttributes);
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME, CategoryControllerTest.CATALOG_CODE,
            "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    List<CategoryAttribute> categoryAttributeList = new ArrayList<CategoryAttribute>();
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    categoryAttributeList.add(categoryAttribute);
    this.category.setCategoryAttributes(categoryAttributeList);
    Category parent = new Category();
    parent.setId(CategoryControllerTest.CATEGORY_ID);
    this.category.setParentCategory(parent);

    Mockito.when(
        this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATEGORY_ID)).thenReturn(this.category);
    when(this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeRequest.getId()))
        .thenReturn(attribute);
    when(
        this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.MASTER_CATEGORY_ID)).thenReturn(this.category);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).findByStoreIdAndIdInitCategoryAttribute(
        CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.verify(this.service).adjustCategory(any(Category.class), any(Category.class),
        anyString(), (Integer) eq(null));
    Mockito.verify(this.attributeService).findById(CategoryControllerTest.STORE_ID,
        attributeRequest.getId());
    Mockito.verify(this.service).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.MASTER_CATEGORY_ID);
  }

  @Test
  public void updateCategoryTestWithInternalActivationIntervalIsNotNull() throws Exception {
    CategoryRequest categoryRequest = getCategoryRequest();
    categoryRequest.setId(CategoryControllerTest.CATEGORY_ID);
    List<CategoryAttributeRequest> categoryAttributes = new ArrayList<CategoryAttributeRequest>();
    CategoryAttributeRequest categoryAttributeRequest = new CategoryAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    Attribute attribute = new Attribute();
    attribute.setId(CategoryControllerTest.ATTRIBUTE_ID);
    attributeRequest.setId(CategoryControllerTest.ATTRIBUTE_ID);
    categoryAttributeRequest.setAttribute(attributeRequest);
    categoryAttributes.add(categoryAttributeRequest);
    List<CategoryReferenceRequest> masterCategoryReferences =
        new ArrayList<CategoryReferenceRequest>();
    CategoryReferenceRequest categoryReference = new CategoryReferenceRequest();
    categoryReference.setMasterCategoryId(CategoryControllerTest.MASTER_CATEGORY_ID);
    categoryReference.setStoreId(CategoryControllerTest.STORE_ID);
    masterCategoryReferences.add(categoryReference);
    categoryRequest.setMasterCategoryReferences(masterCategoryReferences);
    categoryRequest.setCategoryAttributes(categoryAttributes);
    CatalogRequest catalogRequest =
        new CatalogRequest(CategoryControllerTest.CATALOG_NAME,
            CategoryControllerTest.CATALOG_CODE, "MASTER_CATALOG", CategoryControllerTest.STORE_ID);
    catalogRequest.setId(CategoryControllerTest.CATALOG_ID);
    categoryRequest.setCatalog(catalogRequest);
    List<CategoryAttribute> categoryAttributeList = new ArrayList<CategoryAttribute>();
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    categoryAttributeList.add(categoryAttribute);
    this.category.setCategoryAttributes(categoryAttributeList);
    Category parent = new Category();
    parent.setId(CategoryControllerTest.CATEGORY_ID);
    this.category.setParentCategory(parent);
    categoryRequest.setInternalActivationInterval(72);
    Mockito.when(
        this.service.findByStoreIdAndIdInitCategoryAttribute(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.CATEGORY_ID)).thenReturn(this.category);
    when(this.attributeService.findById(CategoryControllerTest.STORE_ID, attributeRequest.getId()))
        .thenReturn(attribute);
    when(
        this.service.findByStoreIdAndId(CategoryControllerTest.STORE_ID,
            CategoryControllerTest.MASTER_CATEGORY_ID)).thenReturn(this.category);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRequest);
    this.mockMvc.perform(
        post(CategoryController.BASE_PATH + "/update").accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).findByStoreIdAndIdInitCategoryAttribute(
        CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
    Mockito.verify(this.service).adjustCategory(any(Category.class), any(Category.class),
        anyString(), (Integer) eq(null));
    Mockito.verify(this.attributeService).findById(CategoryControllerTest.STORE_ID,
        attributeRequest.getId());
    Mockito.verify(this.service).findByStoreIdAndId(CategoryControllerTest.STORE_ID,
        CategoryControllerTest.MASTER_CATEGORY_ID);
  }

  @Test
  public void testPublishAllCategoriesSuccess() throws Exception {
    Mockito.doNothing().when(categoryServiceWrapper)
        .publishAllCategories(CategoryControllerTest.CATALOG_NAME, STORE_ID);
    this.mockMvc.perform(
        post(CategoryControllerTest.PUBLISH_ALL_CATEGORY).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("catalogName",CategoryControllerTest.CATALOG_NAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(categoryServiceWrapper)
        .publishAllCategories(CategoryControllerTest.CATALOG_NAME, STORE_ID);
  }

  @Test
  public void updateCategoryInfoSuccess() throws Exception {
    categoryInfoUpdateRequest.setGenericTemplateEligible(true);
    categoryInfoUpdateRequest.setDocumentType(DOCUMENTS);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryInfoUpdateRequest);
    Mockito.when(
      categoryServiceWrapper.updateCategoryInfo(eq(STORE_ID), any(CategoryInfoUpdateDTO.class),
        anyBoolean())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORY_INFO)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(categoryServiceWrapper).updateCategoryInfo(
        eq(STORE_ID), categoryInfoUpdateDTOArgumentCaptor.capture(), anyBoolean());
    assertEquals(CODE, categoryInfoUpdateDTOArgumentCaptor.getValue().getCategoryCode());
    assertEquals(NAME, categoryInfoUpdateDTOArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryInfoUpdateDTOArgumentCaptor.getValue().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION),
        Arrays.toString(categoryInfoUpdateDTOArgumentCaptor.getValue().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH),
        Arrays.toString(categoryInfoUpdateDTOArgumentCaptor.getValue().getDescriptionEnglish()));
    assertEquals(SEQUENCE, categoryInfoUpdateDTOArgumentCaptor.getValue().getSequence(), 0);
    assertEquals(PARENT_CATEGORY_ID, categoryInfoUpdateDTOArgumentCaptor.getValue().getParentCategoryId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryInfoUpdateDTOArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(UPDATED_BY, categoryInfoUpdateDTOArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(UPDATED_DATE, categoryInfoUpdateDTOArgumentCaptor.getValue().getUpdatedDate());
    assertEquals(100, categoryInfoUpdateDTOArgumentCaptor.getValue().getInternalActivationInterval(), 0);
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isNeedIdentity());
    assertFalse(categoryInfoUpdateDTOArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isDirectFlight());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isActivated());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL,
        categoryInfoUpdateDTOArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isGenericTemplateEligible());
    assertTrue(categoryInfoUpdateDTOArgumentCaptor.getValue().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryInfoUpdateDTOArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void updateCategoryInfoApplicationRunTimeException() throws Exception {
    categoryInfoUpdateRequest.setUpdatedBy(null);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryInfoUpdateRequest);
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORY_INFO)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void updateCategoryInfoApplicationException() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryInfoUpdateRequest);
    doThrow(new ApplicationException()).when(categoryServiceWrapper).updateCategoryInfo(eq(STORE_ID),
        any(CategoryInfoUpdateDTO.class), anyBoolean());
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORY_INFO)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper).updateCategoryInfo(
        eq(STORE_ID), any(CategoryInfoUpdateDTO.class), anyBoolean());
  }

  @Test
  public void updateCategoryReferencesSuccess() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryMappingsUpdateRequest);
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORY_MAPPINGS)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(categoryServiceWrapper).updateCategoryMappingsAndPublishHistory(
        eq(STORE_ID), categoryMappingsUpdateDTOArgumentCaptor.capture());
    assertEquals(ID, categoryMappingsUpdateDTOArgumentCaptor.getValue().getId());
  }

  @Test
  public void updateCategoryMappingsApplicationRunTimeException() throws Exception {
    categoryMappingsUpdateRequest.setUpdatedBy(null);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryMappingsUpdateRequest);
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORY_MAPPINGS)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void updateCategoryMappingsApplicationException() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryMappingsUpdateRequest);
    doThrow(new ApplicationException()).when(categoryServiceWrapper)
        .updateCategoryMappingsAndPublishHistory(eq(STORE_ID),
        any(CategoryMappingsUpdateDTO.class));
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORY_MAPPINGS)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper).updateCategoryMappingsAndPublishHistory(
        eq(STORE_ID), any(CategoryMappingsUpdateDTO.class));
  }

  @Test
  public void getCategoryRestrictedKeywordsTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRestrictedKeywordsRequest);
    Mockito.when(categoryServiceWrapper.findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable))
        .thenReturn(restrictedKeywordsResponsePage);
    this.mockMvc.perform(
        post(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORDS).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME).content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(categoryServiceWrapper)
        .findCategoryRestrictedKeywords(eq(STORE_ID), any(CategoryRestrictedKeywordsRequest.class), any(Pageable.class));
  }

  @Test
  public void getCategoryRestrictedKeywordsExceptionTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRestrictedKeywordsRequest);
    Mockito.when(categoryServiceWrapper.findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable))
        .thenThrow(RuntimeException.class);
    this.mockMvc.perform(
        post(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORDS).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME).content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper)
        .findCategoryRestrictedKeywords(eq(STORE_ID), any(CategoryRestrictedKeywordsRequest.class), any(Pageable.class));
  }

  @Test
  public void getCategoryRestrictedKeywordsApplicationExceptionTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryRestrictedKeywordsRequest);
    Mockito.when(categoryServiceWrapper.findCategoryRestrictedKeywords(STORE_ID, categoryRestrictedKeywordsRequest, pageable))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
        post(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORDS).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME).content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper)
        .findCategoryRestrictedKeywords(eq(STORE_ID), any(CategoryRestrictedKeywordsRequest.class), any(Pageable.class));
  }

  @Test
  public void createCategorySuccess() throws Exception {
    categoryDetailDTO.getCategoryInfoDetail().setGenericTemplateEligible(true);
    categoryDetailRequest.getCategoryInfoDetail().setGenericTemplateEligible(true);
    categoryDetailRequest.getCategoryInfoDetail().setSizeChartRequired(true);
    categoryDetailRequest.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryDetailRequest);
    category.setCategoryCode(CATEGORY1_CODE);
    Mockito.when(categoryServiceWrapper.createCategoryWithoutEventPublish(eq(STORE_ID),
      any(CategoryDetailDTO.class))).thenReturn(
      Pair.of(new CategoryAndHierarchyDto(category, new ArrayList<>()), new ArrayList<>()));
    this.mockMvc.perform(post(CategoryControllerTest.CREATE_CATEGORY).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME).content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CATEGORY1_CODE)));
    Mockito.verify(categoryServiceWrapper)
        .createCategoryWithoutEventPublish(eq(STORE_ID), categoryDetailDTOArgumentCaptor.capture());
    Mockito.verify(categoryServiceWrapper).publishCategoryChangeAndClearCache(any());
    assertEquals(ID, categoryDetailDTOArgumentCaptor.getValue().getId());
    Assertions.assertNotNull(categoryDetailDTOArgumentCaptor.getValue().getCategoryInfoDetail());
    assertTrue(categoryDetailDTOArgumentCaptor.getValue().getCategoryInfoDetail().isUmkm());
    assertTrue(categoryDetailDTOArgumentCaptor.getValue().getCategoryInfoDetail().isSizeChartRequired());
    assertTrue(DOCUMENTS.equals(categoryDetailDTOArgumentCaptor.getValue().getCategoryInfoDetail().getDocumentType()));
  }

  @Test
  public void createCategoryApplicationRunTimeException() throws Exception {
    categoryDetailRequest.setCreatedBy(null);
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryDetailRequest);
    this.mockMvc.perform(
        post(CategoryControllerTest.CREATE_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }

  @Test
  public void createCategoryApplicationException() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(
        categoryDetailRequest);
    doThrow(new ApplicationException()).when(categoryServiceWrapper).createCategoryWithoutEventPublish(eq(STORE_ID),
        any(CategoryDetailDTO.class));
    this.mockMvc.perform(
        post(CategoryControllerTest.CREATE_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper).createCategoryWithoutEventPublish(
        eq(STORE_ID), any(CategoryDetailDTO.class));
  }

  @Test
  public void getCategoryTreeTest() throws Exception {
    when(this.service.getCategoryTree(STORE_ID)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(get(CategoryControllerTest.GET_CATEGORY_TREE_WITH_REVIEW_CONFIG)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME).param("name", CategoryControllerTest.NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.service).getCategoryTree(STORE_ID);
  }

  @Test
  public void getCategoryTreeTest_expectException() throws Exception {
    when(this.service.getCategoryTree(STORE_ID)).thenThrow(RuntimeException.class);
    this.mockMvc.perform(get(CategoryControllerTest.GET_CATEGORY_TREE_WITH_REVIEW_CONFIG)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME).param("name", CategoryControllerTest.NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.service).getCategoryTree(STORE_ID);
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryKeywordUpdateRequestList);
    Mockito.when(categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(eq(CATEGORY_ID),
      any(CategoryKeywordsUpdateListDTO.class))).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORIES_WITH_RESTRICTED_KEYWORDS, CategoryControllerTest.CATEGORY1_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(categoryServiceWrapper)
        .updateCategoriesWithRestrictedKeywords(eq(CATEGORY1_CODE), any(CategoryKeywordsUpdateListDTO.class));
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsExceptionTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryKeywordUpdateRequestList);
    doThrow(new Exception()).when(categoryServiceWrapper)
        .updateCategoriesWithRestrictedKeywords(eq(CATEGORY1_CODE), any(CategoryKeywordsUpdateListDTO.class));
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORIES_WITH_RESTRICTED_KEYWORDS, CategoryControllerTest.CATEGORY1_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper)
        .updateCategoriesWithRestrictedKeywords(eq(CATEGORY1_CODE), any(CategoryKeywordsUpdateListDTO.class));
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsApplicationExceptionTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryKeywordUpdateRequestList);
    doThrow(new ApplicationRuntimeException()).when(categoryServiceWrapper)
        .updateCategoriesWithRestrictedKeywords(eq(CATEGORY1_CODE), any(CategoryKeywordsUpdateListDTO.class));
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORIES_WITH_RESTRICTED_KEYWORDS, CategoryControllerTest.CATEGORY1_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper)
        .updateCategoriesWithRestrictedKeywords(eq(CATEGORY1_CODE), any(CategoryKeywordsUpdateListDTO.class));
  }

  @Test
  public void getRestrictedKeywordsMappedToCategoryByCodeTest() throws Exception {
    Mockito.when(this.categoryServiceWrapper
        .getRestrictedKeywordMappedToCategory(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE))
        .thenReturn(Collections.emptyList());

    this.mockMvc.perform(
        get(CategoryControllerTest.GET_RESTRICTED_KEYWORD_MAPPED_TO_CATEGORY, CategoryControllerTest.CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID).param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.categoryServiceWrapper)
        .getRestrictedKeywordMappedToCategory(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
  }

  @Test
  public void getRestrictedKeywordsMappedToCategoryByCodeExceptionTest() throws Exception {
    when(this.categoryServiceWrapper
        .getRestrictedKeywordMappedToCategory(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE))
        .thenThrow(RuntimeException.class);

    this.mockMvc.perform(
        get(CategoryControllerTest.GET_RESTRICTED_KEYWORD_MAPPED_TO_CATEGORY, CategoryControllerTest.CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID).param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.categoryServiceWrapper)
        .getRestrictedKeywordMappedToCategory(CategoryControllerTest.STORE_ID, CategoryControllerTest.CODE);
  }

  @Test
  public void getWholesaleConfigToCategoryTest() throws Exception {
    Mockito.when(this.categoryWholesaleConfigService
        .findByStoreIdAndCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID,
            CategoryControllerTest.CATEGORY1_CODE)).thenReturn(wholesaleMappingResponse);
    this.mockMvc.perform(
        get(CategoryControllerTest.GET_WHOLESALE_CONFIG_TO_CATEGORY).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)
            .param("categoryCode", CategoryControllerTest.CATEGORY1_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryWholesaleConfigService)
        .findByStoreIdAndCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID,
            CategoryControllerTest.CATEGORY1_CODE);
  }

  @Test
  public void getWholesaleConfigToCategoryExceptionTest() throws Exception {
    when(this.categoryWholesaleConfigService
        .findByStoreIdAndCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID,
            CategoryControllerTest.CATEGORY1_CODE)).thenThrow(Exception.class);
    this.mockMvc.perform(
        get(CategoryControllerTest.GET_WHOLESALE_CONFIG_TO_CATEGORY).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)
            .param("categoryId", CategoryControllerTest.CATEGORY_ID)
            .param("categoryCode", CategoryControllerTest.CATEGORY1_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.categoryWholesaleConfigService)
        .findByStoreIdAndCategoryId(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID,
            CategoryControllerTest.CATEGORY1_CODE);
  }

  @Test
  public void updateCategoriesWithWholesaleConfigTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryKeywordUpdateRequestList);
    when(categoryServiceWrapper.updateCategoriesWithWholesaleConfig(eq(STORE_ID), eq(CATEGORY_ID),
        any(WholesaleMappingDTO.class))).thenReturn(new CategoryUpdateHistoryDTO());
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORIES_WITH_WHOLESALE_CONFIG, CategoryControllerTest.CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID).param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME).content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(categoryServiceWrapper)
        .updateCategoriesWithWholesaleConfig(eq(STORE_ID), eq(CATEGORY_ID), any(WholesaleMappingDTO.class));
    Mockito.verify(categoryServiceWrapper)
        .publishHistoryEventForWholesaleConfigUpdate(eq(STORE_ID), any(CategoryUpdateHistoryDTO.class));
  }

  @Test
  public void updateCategoriesWithWholesaleConfigExceptionTest() throws Exception {
    String request = CategoryControllerTest.OBJECT_MAPPER.writeValueAsString(categoryKeywordUpdateRequestList);
    doThrow(Exception.class).when(categoryServiceWrapper)
        .updateCategoriesWithWholesaleConfig(eq(STORE_ID), eq(CATEGORY_ID), any(WholesaleMappingDTO.class));
    this.mockMvc.perform(
        put(CategoryControllerTest.UPDATE_CATEGORIES_WITH_WHOLESALE_CONFIG, CategoryControllerTest.CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID).param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME).content(request)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(categoryServiceWrapper)
        .updateCategoriesWithWholesaleConfig(eq(STORE_ID), eq(CATEGORY_ID), any(WholesaleMappingDTO.class));
  }

  @Test
  public void getGenericTemplateCategoriesTest() throws Exception {
    when(this.service.getAllCategoryTreeforGenericTemplate(STORE_ID, true, true)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(get(CategoryControllerTest.GET_CATEGORY_TREE_FOR_GENERIC_TEMPLATE)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME).param("name", CategoryControllerTest.NAME)
        .param("genericTemplateEligible", CategoryControllerTest.GENERIC_TEMPLATE_ELIGIBLE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.service).getAllCategoryTreeforGenericTemplate(STORE_ID, true, true);
  }

  @Test
  public void getGenericTemplateCategoriesExceptionTest() throws Exception {
    when(this.service.getAllCategoryTreeforGenericTemplate(STORE_ID, true, true)).thenThrow(Exception.class);
    this.mockMvc.perform(get(CategoryControllerTest.GET_CATEGORY_TREE_FOR_GENERIC_TEMPLATE)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME).param("name", CategoryControllerTest.NAME)
        .param("genericTemplateEligible", CategoryControllerTest.GENERIC_TEMPLATE_ELIGIBLE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.service).getAllCategoryTreeforGenericTemplate(STORE_ID, true, true);
  }

  @Test
  public void validateCategory() throws Exception {
    Mockito.when(categoryServiceWrapper.validateCategory(STORE_ID, CATEGORY_ID)).thenReturn(new CategoryErrorDto());
    this.mockMvc.perform(get(CategoryControllerTest.VALIDATE_CATEGORY, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryServiceWrapper)
        .validateCategory(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void validateCategoryExceptionTest() throws Exception {
    Mockito.when(categoryServiceWrapper.validateCategory(STORE_ID, CATEGORY_ID)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(get(CategoryControllerTest.VALIDATE_CATEGORY, CategoryControllerTest.CATEGORY_ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.categoryServiceWrapper)
        .validateCategory(CategoryControllerTest.STORE_ID, CategoryControllerTest.CATEGORY_ID);
  }

  @Test
  public void createOriginalSalesCategoryTest() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(originalSalesCategoryRequest);
    when(this.objectMapper.readValue(requestString, OriginalSalesCategoryRequest.class))
        .thenReturn(originalSalesCategoryRequest);
    when(this.service.saveOSC(originalSalesCategoryRequest)).thenReturn(ID);
    this.mockMvc.perform(post(CategoryControllerTest.CREATE_OSC).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.value", equalTo(CategoryControllerTest.ID)));
    verify(this.service).saveOSC(originalSalesCategoryRequest);
  }

  @Test
  public void createOriginalSalesCategoryExceptionTest() throws Exception {
    originalSalesCategoryRequest.setOscCode(null);
    String requestString = OBJECT_MAPPER.writeValueAsString(originalSalesCategoryRequest);
    when(this.objectMapper.readValue(requestString, OriginalSalesCategoryRequest.class))
        .thenReturn(originalSalesCategoryRequest);
    this.mockMvc.perform(post(CategoryControllerTest.CREATE_OSC).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void getCategoryRestrictedKeywordListTest() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(categoryCodeAndKeywordListRequest);
    when(this.categoryRestrictedKeywordService.getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(STORE_ID,
        CATEGORY1_CODE, new ArrayList<>())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORD_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.categoryRestrictedKeywordService).getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(STORE_ID,
        CATEGORY1_CODE, new ArrayList<>());
  }

  @Test
  public void getCategoryRestrictedKeywordListExceptionTest() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(categoryCodeAndKeywordListRequest);
    doThrow(RuntimeException.class).when(categoryRestrictedKeywordService).getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(STORE_ID,
        CATEGORY1_CODE, new ArrayList<>());
    this.mockMvc.perform(post(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORD_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.categoryRestrictedKeywordService).getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(STORE_ID,
        CATEGORY1_CODE, new ArrayList<>());
  }

  @Test
  public void createOriginalSalesCategoryException1Test() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(originalSalesCategoryRequest);
    when(this.objectMapper.readValue(requestString, OriginalSalesCategoryRequest.class))
        .thenReturn(originalSalesCategoryRequest);
    Mockito.doThrow(Exception.class).when(this.service).saveOSC(originalSalesCategoryRequest);
    this.mockMvc.perform(post(CategoryControllerTest.CREATE_OSC).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.service).saveOSC(originalSalesCategoryRequest);
  }

  @Test
  public void filterOscSummaryResponseTest() throws Exception {
    Mockito.when(this.originalSalesCategoryService.filterSummaryOSC(OSC_CODE, KEYWORD, ACTIVATED))
        .thenReturn(new ArrayList<>());
    this.mockMvc.perform(get(CategoryController.BASE_PATH + CategoryController.FETCH_OSC_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
        .param("oscCode", OSC_CODE).param("keyword", KEYWORD).param("activated", String.valueOf(ACTIVATED)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.originalSalesCategoryService).filterSummaryOSC(OSC_CODE, KEYWORD, ACTIVATED);
  }

  @Test
  public void filterOscSummaryResponseExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.originalSalesCategoryService)
        .filterSummaryOSC(OSC_CODE, KEYWORD, ACTIVATED);
    this.mockMvc.perform(get(CategoryController.BASE_PATH + CategoryController.FETCH_OSC_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).param("storeId", STORE_ID)
        .param("channelId", CategoryControllerTest.CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", CategoryControllerTest.REQUEST_ID).param("username", DEFAULT_USERNAME)
        .param("oscCode", OSC_CODE).param("keyword", KEYWORD).param("activated", String.valueOf(ACTIVATED)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.originalSalesCategoryService).filterSummaryOSC(OSC_CODE, KEYWORD, ACTIVATED);
  }

  @Test
  public void updateOriginalSalesCategoryTest() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(oscInfoUpdateDTO);
    when(this.objectMapper.readValue(requestString, OscInfoUpdateDTO.class)).thenReturn(oscInfoUpdateDTO);
    this.mockMvc.perform(
        put(CategoryController.BASE_PATH + CategoryController.UPDATE_OSC_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.originalSalesCategoryService).updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
  }

  @Test
  public void updateOriginalSalesCategoryExceptionTest() throws Exception {
    oscInfoUpdateDTO.setOscCode(null);
    String requestString = OBJECT_MAPPER.writeValueAsString(oscInfoUpdateDTO);
    when(this.objectMapper.readValue(requestString, OscInfoUpdateDTO.class)).thenReturn(oscInfoUpdateDTO);
    this.mockMvc.perform(
        put(CategoryController.BASE_PATH + CategoryController.UPDATE_OSC_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
  }

  @Test
  public void updateOriginalSalesCategoryException1Test() throws Exception {
    String requestString = OBJECT_MAPPER.writeValueAsString(oscInfoUpdateDTO);
    when(this.objectMapper.readValue(requestString, OscInfoUpdateDTO.class)).thenReturn(oscInfoUpdateDTO);
    Mockito.doThrow(Exception.class).when(this.originalSalesCategoryService)
        .updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
    this.mockMvc.perform(
        put(CategoryController.BASE_PATH + CategoryController.UPDATE_OSC_LIST).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    verify(this.originalSalesCategoryService).updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
  }

  @Test
  public void getOSCByIdTest() throws Exception {
    Mockito.doReturn(this.originalSalesCategoryResponse).when(this.originalSalesCategoryService)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
    this.mockMvc.perform(get(CategoryControllerTest.GET_OSC_BY_ID, CategoryControllerTest.ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.value.oscCode", equalTo(CategoryControllerTest.OSC_CODE)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.originalSalesCategoryService)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
  }

  @Test
  public void getOSCByIdExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.originalSalesCategoryService)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
    this.mockMvc.perform(get(CategoryControllerTest.GET_OSC_BY_ID, CategoryControllerTest.ID)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("username", CategoryControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.originalSalesCategoryService)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
  }

  @Test
  public void getCategoryRestrictedKeywordByIdTest() throws Exception {
    Mockito.doReturn(this.categoryRestrictedKeywordResponse).when(this.categoryRestrictedKeywordService)
        .getCategoryRestrictedKeywordById(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
    this.mockMvc.perform(
        get(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORD).param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID).param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID).param("id", CategoryControllerTest.ID))
        .andExpect(status().isOk()).andExpect(
        jsonPath("$.value.categoryCode", equalTo(categoryRestrictedKeywordResponse.getCategoryCode())))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.categoryRestrictedKeywordService)
        .getCategoryRestrictedKeywordById(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
  }

  @Test
  public void getCategoryRestrictedKeywordByIdExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.categoryRestrictedKeywordService)
        .getCategoryRestrictedKeywordById(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
    this.mockMvc.perform(get(CategoryControllerTest.GET_CATEGORY_RESTRICTED_KEYWORD)
        .param("storeId", CategoryControllerTest.STORE_ID).param("channelId", CategoryControllerTest.CHANNEL_ID)
        .param("clientId", CategoryControllerTest.CLIENT_ID).param("requestId", CategoryControllerTest.REQUEST_ID)
        .param("id", CategoryControllerTest.ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.categoryRestrictedKeywordService)
        .getCategoryRestrictedKeywordById(CategoryControllerTest.STORE_ID, CategoryControllerTest.ID);
  }

  @Test
  public void validateCategoryForRestrictedKeywordCategoryChangeTest() throws Exception {
    Mockito.when(categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(eq(STORE_ID),
        Mockito.anyList())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(CategoryController.BASE_PATH
            + CategoryController.VALIDATE_CATEGORY_FOR_RESTRICTED_KEYWORD_CATEGORY_CHANGE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(Arrays.asList(CATEGORY1_CODE)))
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    Mockito.verify(categoryServiceWrapper)
        .validateCategoryForRestrictedKeywordCategoryChange(eq(STORE_ID), Mockito.anyList());
  }

  @Test
  public void validateCategoryForRestrictedKeywordCategoryChangeErrorTest() throws Exception {
    Mockito.when(categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(eq(STORE_ID),
        Mockito.anyList())).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(CategoryController.BASE_PATH
            + CategoryController.VALIDATE_CATEGORY_FOR_RESTRICTED_KEYWORD_CATEGORY_CHANGE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(Arrays.asList(CATEGORY1_CODE)))
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    Mockito.verify(categoryServiceWrapper)
        .validateCategoryForRestrictedKeywordCategoryChange(eq(STORE_ID), Mockito.anyList());
  }

  @Test
  public void fetchCategoryHistoryTest() throws Exception {
    Mockito.when(this.categoryHistoryService.fetchCategoryHistory(STORE_ID, CATEGORY_CODE, pageable.getPageNumber(), pageable.getPageSize()))
        .thenReturn(categoryHistoryResponses(categoryHistoryResponse));
    this.mockMvc.perform(
            get(CategoryController.BASE_PATH + CategoryController.FETCH_HISTORY_BY_CATEGORY_CODE,
                CATEGORY_CODE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
        ).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.categoryHistoryService, Mockito.times(1))
        .fetchCategoryHistory(STORE_ID, CATEGORY_CODE, pageable.getPageNumber(), pageable.getPageSize());
  }

  @Test
  public void fetchCategoryHistoryApplicationExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.categoryHistoryService)
        .fetchCategoryHistory(STORE_ID, CATEGORY_CODE, pageable.getPageNumber(), pageable.getPageSize());
    this.mockMvc.perform(
            get(CategoryController.BASE_PATH + CategoryController.FETCH_HISTORY_BY_CATEGORY_CODE,
                CATEGORY_CODE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
        ).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.categoryHistoryService, Mockito.times(1))
        .fetchCategoryHistory(STORE_ID, CATEGORY_CODE, pageable.getPageNumber(), pageable.getPageSize());
  }

  @Test
  public void fetchCategoryHistoryExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.categoryHistoryService)
        .fetchCategoryHistory(STORE_ID, CATEGORY_CODE, pageable.getPageNumber(), pageable.getPageSize());
    this.mockMvc.perform(
            get(CategoryController.BASE_PATH + CategoryController.FETCH_HISTORY_BY_CATEGORY_CODE,
                CATEGORY_CODE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)
                .param("page", String.valueOf(pageable.getPageNumber()))
                .param("size", String.valueOf(pageable.getPageSize()))
        ).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.categoryHistoryService, Mockito.times(1))
        .fetchCategoryHistory(STORE_ID, CATEGORY_CODE, pageable.getPageNumber(), pageable.getPageSize());
  }

  private Page<CategoryHistoryResponse> categoryHistoryResponses(
      CategoryHistoryResponse categoryHistoryResponse) {
    return new PageImpl<>(Collections.singletonList(categoryHistoryResponse),
        PageRequest.of(0, 25), 25);
  }

  @Test
  public void getCategoryEligibleForSizeChartAdditionTest() throws Exception {
    Mockito.when(categoryServiceWrapper.getCategoryAndDefiningAttributes(Mockito.eq(STORE_ID),
        Mockito.anyList())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(CategoryController.BASE_PATH
            + CategoryController.CHECK_CATEGORY_ELIGIBLE_FOR_SIZE_CHART_ADDITION)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(Arrays.asList(CATEGORY1_CODE)))
            .param("storeId", CategoryControllerTest.STORE_ID)
            .param("channelId", CategoryControllerTest.CHANNEL_ID)
            .param("clientId", CategoryControllerTest.CLIENT_ID)
            .param("requestId", CategoryControllerTest.REQUEST_ID)
            .param("username", CategoryControllerTest.USERNAME)
            .param("sizeChartAttributeCode", CategoryControllerTest.SIZE_CHART_ATTRIBUTE_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CategoryControllerTest.REQUEST_ID)));
    Mockito.verify(categoryServiceWrapper)
        .getCategoryAndDefiningAttributes(Mockito.eq(STORE_ID), Mockito.anyList());
  }


}
