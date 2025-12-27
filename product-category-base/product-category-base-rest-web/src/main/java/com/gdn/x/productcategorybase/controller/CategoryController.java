package com.gdn.x.productcategorybase.controller;

import static com.google.common.base.Preconditions.checkNotNull;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import com.gdn.x.productcategorybase.Constants;
import jakarta.annotation.PostConstruct;

import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryEligibleForSizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.service.CategoryHistoryService;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.util.CommonUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.io.ICsvBeanReader;
import org.supercsv.io.ICsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.notification.dto.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.categorytree.CategoryTreeControllerErrorMessage;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.controller.util.MapperUtil;
import com.gdn.x.productcategorybase.csv.CategoryCsv;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.CategoryAndHierarchyDto;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.CategoryDetailDTO;
import com.gdn.x.productcategorybase.dto.CategoryErrorDto;
import com.gdn.x.productcategorybase.dto.CategoryInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryServiceDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeAndKeywordListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryDetailRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryReferenceRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryParentResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.CreateCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.helper.CategoryServiceHelper;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryHistoryService;
import com.gdn.x.productcategorybase.service.CategoryRestrictedKeywordService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryServiceWrapper;
import com.gdn.x.productcategorybase.service.CategoryWholesaleConfigService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.PostConstruct;

@RestController
@RequestMapping(value = CategoryController.BASE_PATH)
@Tag(name = "CategoryController", description = "Master Category Service API")
public class CategoryController {
  public static final String BASE_PATH = "/api/category";
  @SuppressWarnings("unused")
  private static final Logger LOG = LoggerFactory.getLogger(CategoryController.class);
  private static final String DETAIL = "/{id}";
  private static final String DETAIL_BY_CATEGORY_CODE = "/categoryCode/{categoryCode}";
  private static final String DETAIL_BY_CATEGORY_ID = "/categoryId/{categoryId}";
  private static final String CREATE = "/create";
  private static final String BASIC_CATEGORY_AND_CATALOG_INFO = "/basicInfo/{categoryCode}";
  private static final String DELETE = "/delete";
  private static final String UPDATE = "/update";
  private static final String UPDATE_CATEGORY_INFO = "/updateInfo";
  private static final String CREATE_CATEGORY = "/createCategory";
  private static final String UPDATE_CATEGORY_MAPPINGS = "/updateMappings";
  private static final String UPDATE_DISPLAY_FLAG = "/update/displayFlag/{id}";
  private static final String UPLOAD = "/upload";
  private static final String FILTER_BULK_CATEGORY_CODE = "/filter/bulk/category-codes";
  private static final String FILTER_NAME_PAGEABLE = "/filter/name/pageable";
  private static final String CHILD_PARENT_PAGEABLE = "/filter/childParent/pageable/{id}";
  private static final String CHILD_PARENT_PAGEABLE_COUNT =
      "/filter/childParent/pageablecount/{id}";
  private static final String CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT =
      "/filter/childParent/catalog-type/pageablecount";
  private static final String CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE =
      "/filter/childParent/catalog-type/pageable";
  private static final String CHILD_PARENT_WITH_CATALOG_ID_PAGEABLE_COUNT =
      "/filter/childParent/catalog/pageablecount";
  private static final String HIERARCHY_FILTER_CATEGORY_CODE = "/hierarchy/filter/category-code";
  private static final String HIERARCHY_FILTER_CATEGORY_CODES = "/hierarchy/filter/category-codes";
  private static final String CATEGORY_EXPORT_CSV_FILENAME = "category-export.csv";
  protected static final String GET_ALL_CATEGORIES_PARENT_MAPPING = "/parent-category-mapping";
  public static final String FILTER_NAME_CATALOG_TYPE = "/filter/name/catalog-type";
  private static final String GET_FINAL_PARENT_CATEGORY = "/final-parent-category";
  private static final String GET_FINAL_PARENT_CATEGORY_CACHED = "/final-parent-category-cached";
  private static final String GET_ALL_CATEGORY_TREE = "/getAllCategoryTree";
  private static final String GET_ACTIVE_CATEGORY_TREE = "/getActiveCategoryTree";
  private static final String PUBLISH_ALL_CATEGORY = "/publishAllCategory";
  private static final String GET_CATEGORY_TREE_WITH_REVIEW_CONFIG = "/getCategoryTreeWithReviewConfig";
  private static final String UPDATE_CATEGORIES_WITH_RESTRICTED_KEYWORDS = "/{categoryCode}/restricted-keywords";
  private static final String GET_CATEGORY_TREE = "/getCategoryTree";
  private static final String GET_CATEGORY_NAMES = "/getCategoryNames";
  private static final String GET_PARENT_CATEGORY = "/filter/parent";
  private static final String SUMMARY = "/summary";
  private static final String PATH_CATALOG_ID = "/{catalogId}";
  private static final int REQUEST_LIMIT = 500;
  private static final String GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE = "/getAllChildCategoriesFromC1CategoryCode";
  private static final String VALIDATE_IS_CN_CATEGORY = "/validateIsCnCategory";
  public static final String FETCH_HISTORY_BY_CATEGORY_CODE = "/fetchHistoryByCategoryCode/{categoryCode}";
  private static final int DEFAULT_PAGE_SIZE = 10;
  private static final String CATEGORY_INFO_UPDATE_ERROR_MESSAGE = "Error while updating category info";
  private static final String CATEGORY_MAPPINGS_UPDATE_ERROR_MESSAGE = "Error while updating category mappings";
  private static final String CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE = "Error while updating category keywords";
  private static final String OSC_FETCH_ERROR_MESSAGE = "Error while fetching osc filter";
  private static final String CATEGORY_CRATE_ERROR_MESSAGE = "Error while creating category";
  private static final String GET_CATEGORY_RESTRICTED_KEYWORDS = "/get-restricted-keywords";
  private static final String GET_RESTRICTED_KEYWORD_MAPPED_TO_CATEGORY = "/{categoryCode}/restricted-keywords";
  private static final String GET_CATEGORY_TREE_FOR_GENERIC_TEMPLATE = "/getGenericTemplateCategories";
  private static final String GET_WHOLESALE_CONFIG_TO_CATEGORY = "/get-wholesale-config";
  private static final String UPDATE_CATEGORIES_WITH_WHOLESALE_CONFIG = "/{categoryId}/wholesale-config";
  private static final String VALIDATE_CATEGORY = "/{categoryId}/validate-category";
  private static final String CREATE_OSC = "/create-original-sales-category";
  public static final String FETCH_OSC_LIST ="/fetch-osc-list";
  public static final String UPDATE_OSC_LIST ="/update-osc-list";
  private static final String GET_OSC_BY_ID = "/{id}/original-sales-category";
  private static final String GET_CATEGORY_RESTRICTED_KEYWORD= "/keywords/getCategoryRestrictedKeyword";
  private static final String GET_CATEGORY_RESTRICTED_KEYWORD_LIST =
      "/keywords/getCategoryRestrictedKeywordByCategoryCodeAndIds";
  public static final String VALIDATE_CATEGORY_FOR_RESTRICTED_KEYWORD_CATEGORY_CHANGE = "/validateCategoryForRestrictedKeywordCategoryChange";
  public static final String CHECK_CATEGORY_ELIGIBLE_FOR_SIZE_CHART_ADDITION =
      "/check-category-eligible-for-size-chart-addition";

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private CategoryServiceHelper categoryServiceHelper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private CategoryRestrictedKeywordService categoryRestrictedKeywordService;

  @Autowired
  private CategoryWholesaleConfigService categoryWholesaleConfigService;

  @Autowired
  private MapperUtil mapperUtil;

  @Autowired
  private CategoryServiceWrapper categoryServiceWrapper;

  @Autowired
  private OriginalSalesCategoryService originalSalesCategoryService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private CategoryHistoryService categoryHistoryService;

  private static Set<String> attributeConfigurationSet = new HashSet<>();

  private static final Logger LOGGER = LoggerFactory.getLogger(CategoryController.class);

  @Value("${default.internal.activation.period}")
  private int defaultInternalActivationPeriod;

  @Value("${product.attribute.configuration}")
  private String attributeConfiguration;

  @PostConstruct
  private void init() {
    try {
      attributeConfigurationSet =
          new ObjectMapper().readValue(attributeConfiguration, new TypeReference<Set<String>>() {
          });
    } catch (IOException ex) {
      LOGGER.error("error: while setting attributeConfiguration", ex);
    }
  }

  private Category convertRequestToCategory(String storeId, CategoryRequest request)
      throws Exception {
    Category category = new Category();
    BeanUtils.copyProperties(request, category, "catalog", "parentCategory", "categoryAttributes",
        "productCategories", "version", "createdBy", "createdDate", "masterCategoryReferences",
        "salesCategoryReferences", "b2bSalesCategoryReferences");
    for (CategoryAttributeRequest categoryAttributeRequest : request.getCategoryAttributes()) {
      category.getCategoryAttributes().add(
          this.convertRequestToCategoryAttribute(storeId, category, categoryAttributeRequest));
    }
    if ((request.getMasterCategoryReferences() != null)
        && !request.getMasterCategoryReferences().isEmpty()) {
      for (CategoryReferenceRequest masterCategoryReference : request.getMasterCategoryReferences()) {
        category.getMasterCategoryReferences().add(
            this.convertRequestToCategoryReference(storeId, category, masterCategoryReference));
      }
    }
    category.setParentCategory(null);
    return category;
  }

  private CategoryAttribute convertRequestToCategoryAttribute(String storeId, Category category,
      CategoryAttributeRequest categoryAttributeRequest) throws Exception {
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    BeanUtils.copyProperties(categoryAttributeRequest, categoryAttribute, "attribute", "category", Constants.ID);
    categoryAttribute.setCategory(category);
    Attribute attribute =
        this.attributeService.findById(storeId, categoryAttributeRequest.getAttribute().getId());
    if (attribute == null) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Cannot find attribute with ID" + " = " + categoryAttributeRequest.getAttribute().getId());
    }
    categoryAttribute.setAttribute(attribute);
    return categoryAttribute;
  }

  private CategoryReference convertRequestToCategoryReference(String storeId, Category category,
      CategoryReferenceRequest request) throws Exception {
    CategoryReference categoryReference = new CategoryReference();
    BeanUtils.copyProperties(request, categoryReference, Constants.ID);
    categoryReference.setSalesCategory(category);
    if (categoryReference.getStoreId() == null) {
      categoryReference.setStoreId(storeId);
    }
    Category masterCategory =
        this.categoryService.findByStoreIdAndId(storeId, request.getMasterCategoryId());
    if (category == null) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Cannot find category with ID = " + request.getMasterCategoryId());
    }
    categoryReference.setMasterCategory(masterCategory);
    return categoryReference;
  }

  private CategorySummaryResponse convertCategoryToCategorySumaryResponse(Category category) {
    CategorySummaryResponse response = new CategorySummaryResponse();
    response.setId(category.getId());
    response.setCreatedBy(category.getCreatedBy());
    response.setCreatedDate(category.getCreatedDate());
    response.setUpdatedBy(category.getUpdatedBy());
    response.setUpdatedDate(category.getUpdatedDate());
    response.setStoreId(category.getStoreId());
    if (category.getParentCategory() != null) {
      response.setParentId(category.getParentCategory().getId());
    }
    response.setCode(category.getCategoryCode());
    response.setDisplay(category.isDisplay());
    response.setName(category.getName());
    response.setMarkForDelete(category.isMarkForDelete());
    response.setSequence(category.getSequence());
    return response;
  }

  @SuppressWarnings("unused")
  private Category convertRequestToParentCategory(String storeId, CategoryRequest request)
      throws Exception {
    return this.categoryService.findByStoreIdAndId(storeId, request.getId());
  }

  @PostMapping(value = CategoryController.DELETE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "delete category", description = "delete Category by store id and category id")
  
  public GdnBaseRestResponse deleteCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleRequestHolder request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage());
    this.categoryService.markForDeleteCategory(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = CategoryController.HIERARCHY_FILTER_CATEGORY_CODE
      + CategoryController.DETAIL, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "filter category hierarchy by categoryCode",
      description = "filter category hierarchy by categoryCode")
  
  public GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("id") String categoryCode) throws Exception {
    List<Category> categories =
        this.categoryService.findCategoryHierarchyByCategoryCode(storeId, categoryCode);
    int pageSize = categories.size();
    if(CollectionUtils.isEmpty(categories)){
      pageSize = DEFAULT_PAGE_SIZE;
      LOGGER.warn("there is zero Category found for categoryCode : {}", categoryCode);
    }
    Pageable pageable = PageRequest.of(0, pageSize);
    Page<Category> categoriesPage = new PageImpl<Category>(categories);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        categoriesPage);
  }

  @PostMapping(value = CategoryController.HIERARCHY_FILTER_CATEGORY_CODES, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "filter category hierarchy by categoryCodes",
      description = "filter category hierarchy by categoryCodes")
  
  public GdnRestListResponse<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryCodeRequest categoryCodeRequest) throws Exception {
    List<CategoryHierarchyResponse> categoryHierarchyResponses = new ArrayList<>();
    for (String categoryCode : categoryCodeRequest.getCategoryCodes()) {
      List<Category> categories =
          this.categoryService.findCategoryHierarchyByCategoryCode(storeId, categoryCode);
      CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
      categoryHierarchyResponse.setCategoryCode(categoryCode);
      for (Category category : categories) {
        if (category.getCategoryCode().equals(categoryCode)) {
          categoryHierarchyResponse.setCategoryId(category.getId());
        }
      }
      categoryHierarchyResponse.setCategoryHierarchy(populateListResponse(categories));
      categoryHierarchyResponses.add(categoryHierarchyResponse);
    }
    Pageable pageable = PageRequest.of(0, categoryHierarchyResponses.size());
    return new GdnRestListResponse<>(null, null, true, categoryHierarchyResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            categoryHierarchyResponses.size()), requestId);
  }

  @GetMapping(value = CategoryController.FILTER_NAME_PAGEABLE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Category by name pageable", description = "get Category by store id and "
      + "name and pageable")
  
  public GdnRestListResponse<CategoryResponse> getCategoryByNamePageable(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name,
      @RequestParam(defaultValue = "ALL") String state, @RequestParam(defaultValue = "ALL") String documentFilterType)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return categoryServiceHelper
        .findByNameWithChildCount(requestId, storeId, name, pageable, state, documentFilterType);
  }

  @RequestMapping(value = CategoryController.DETAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Category by id", description = "get category by store id and id and "
      + "pageable")
  
  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("id") String categoryId) throws Exception {
    try {
      Category category = this.categoryService.findByStoreIdAndIdInitCategoryAttribute(storeId, categoryId);
      CategoryDetailResponse response =
          ConverterUtil.convertCategoryToCategoryDetailResponse(category, attributeConfigurationSet);
      return new GdnRestSingleResponse<CategoryDetailResponse>("", "", true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      LOGGER.error(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(),
          ErrorCategory.UNSPECIFIED.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = CategoryController.DETAIL_BY_CATEGORY_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Category by Code", description = "get Category by Store Id and Code")
  
  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetailByCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("categoryCode") String categoryCode) throws Exception {
    Category category = this.categoryService.findByStoreIdAndCategoryCode(storeId, categoryCode);
    CategoryDetailResponse response =
        ConverterUtil.convertCategoryToCategoryDetailResponse(category, attributeConfigurationSet);
    return new GdnRestSingleResponse<CategoryDetailResponse>("", "", true, response, requestId);
  }

  @Operation(summary = "API To check category eligible for size chart based on size chart "
      + "attribute", description =
                 "API To check category eligible for size chart based on size chart" + " attribute")
  
  @PostMapping(value = CategoryController.CHECK_CATEGORY_ELIGIBLE_FOR_SIZE_CHART_ADDITION,
               produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<CategoryEligibleForSizeChartResponse> getCategoryEligibleForSizeChartAddition(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<String> categoryCodes,
      @RequestParam("sizeChartAttributeCode") String sizeChartAttributeCode) throws Exception {
    List<Category> categories =
        categoryServiceWrapper.getCategoryAndDefiningAttributes(storeId, categoryCodes);
    Map<String, Boolean> map =
        CommonUtil.getCategoryEligibleForSizeChartAdditionMap(sizeChartAttributeCode, categories);
    return new GdnRestSingleResponse<>(null, null, true,
        new CategoryEligibleForSizeChartResponse(map), requestId);
  }


  @RequestMapping(value = CategoryController.BASIC_CATEGORY_AND_CATALOG_INFO, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Basic Category Info and Catalog Info", description = "get Basic Category Info and Catalog Info")
  
  public GdnRestSingleResponse<CategoryResponse> getBasicCategoryInfoAndCatalogInfo(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("categoryCode") String categoryCode) {
    try {
      Category category = this.categoryService.findBasicInfoByStoreIdAndCategoryCode(storeId, categoryCode);
      LOG.info("Category value for category-code : {} {}", category, categoryCode);
      CategoryResponse response =
          ConverterUtil.getCategoryDetailResponse(category);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (Exception e) {
      LOGGER.error("Error when trying to fetch basic category info for code {} ", categoryCode, e);
      return new GdnRestSingleResponse<>(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(),
          ErrorCategory.UNSPECIFIED.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = CategoryController.DETAIL_BY_CATEGORY_ID, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Category by Id", description = "get Category by Store Id and category Id")
  
  public GdnBaseRestResponse getCategoryDetailById(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false) boolean fetchHideForSellerAttributes,
      @PathVariable("categoryId") String categoryId) {
    CategoryDetailAndShippingResponse response = new CategoryDetailAndShippingResponse();
    List<CategoryShipping> categoryShippingList;
    List<ShippingResponse> shippingResponses = null;
    try {
      LOGGER.info("Fetch category info based on category Id, category ID :{}", categoryId);
      CategoryErrorDto categoryErrorDto =
          this.categoryServiceWrapper.getCategoryDetailByCategoryId(storeId, categoryId);
      LOGGER.info("cat: {}", categoryErrorDto.getCategory().getDocumentType());
      if (CatalogType.MASTER_CATALOG.equals(categoryErrorDto.getCategory().getCatalog().getCatalogType())) {
        categoryShippingList = this.categoryService
            .findShippingInfoByStoreIdCategoryCode(storeId, categoryErrorDto.getCategory().getCategoryCode());
        shippingResponses = mapperUtil.getShippingResponseList(categoryShippingList);
      }
      response = ConverterUtil.convertCategoryToCategoryDetailAndShippingResponse(categoryErrorDto.getCategory(),
          attributeConfigurationSet, shippingResponses, fetchHideForSellerAttributes);
      return new GdnRestSingleResponse<>(categoryErrorDto.getErrorMessage(), categoryErrorDto.getErrorCode(), true,
          response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), ErrorMessage.CATEGORY_NOT_FOUND.getMessage(), false,
          response, requestId);
    } catch (Exception e) {
      LOGGER.error(ErrorCategory.UNSPECIFIED.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(),
          ErrorCategory.UNSPECIFIED.getMessage(), false, response, requestId);
    }
  }

  @RequestMapping(value = CategoryController.VALIDATE_CATEGORY_FOR_RESTRICTED_KEYWORD_CATEGORY_CHANGE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "validate category", description = "validate category")
  
  public GdnRestListResponse<CategoryErrorResponse> validateCategoryForRestrictedKeywordCategoryChange(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<String> categoryCodes) {
    LOGGER.info("Validate categories : {} ", categoryCodes);
    try {
      List<CategoryErrorResponse> categoryErrorResponseList =
          categoryServiceWrapper.validateCategoryForRestrictedKeywordCategoryChange(storeId, categoryCodes);
      return new GdnRestListResponse<>(null, null, true, categoryErrorResponseList, new PageMetaData(), requestId);
    } catch (Exception e) {
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, null, new PageMetaData(), requestId);
    }
  }

  @RequestMapping(value = CategoryController.VALIDATE_CATEGORY, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Category by Id", description = "get Category by Store Id and category Id")
  
  public GdnBaseRestResponse validateCategory(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("categoryId") String categoryId) {
    try {
      CategoryErrorDto categoryErrorDto = categoryServiceWrapper.validateCategory(storeId, categoryId);
      return new GdnBaseRestResponse(categoryErrorDto.getErrorMessage(), categoryErrorDto.getErrorCode(), true,
          requestId);
    } catch (Exception e) {
      LOGGER.error(ErrorCategory.UNSPECIFIED.getMessage(), e);
      return new GdnBaseRestResponse(ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage(),
          ErrorCategory.UNSPECIFIED.getMessage(), false, requestId);
    }
  }

  @RequestMapping(method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of Category", description = "get list of Category by store id and "
      + "" + "pageable")
  
  public String getCategorySummary(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    LOG.info("Getting categories summary for page num {} and size {}", page, size);
    AtomicLong totalCategoryCount = new AtomicLong();
    String categoryResponseStr = categoryServiceHelper.getCategoryList(storeId, page, size, totalCategoryCount);
    // manually building response as its better not to convert to huge list then serialize it again
    // DO NOT use this approach anywhere else without clear requirement
    StringBuilder categoryResponseBuilder = new StringBuilder();
    categoryResponseBuilder.append("{\"requestId\":\"").append(requestId).append("\",")
        .append("\"success\":").append(true).append(",").append("\"content\":")
        .append(categoryResponseStr).append(",").append("\"pageMetaData\":{")
        .append("\"pageSize\":").append(size).append(",").append("\"pageNumber\":").append(page)
        .append(",").append("\"totalRecords\":").append(totalCategoryCount.get()).append("}}");
    return categoryResponseBuilder.toString();
  }

  @RequestMapping(value = CategoryController.CHILD_PARENT_PAGEABLE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of Category", description = "get list of Category by store id and "
      + "" + "pageable")
  
  public GdnRestListResponse<CategoryResponse> getChildFromParent(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("id") String categoryId)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Category category = this.categoryService.findById(categoryId);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.categoryService.findChildForParent(storeId, category, pageable));
  }

  @GetMapping(value = CategoryController.CHILD_PARENT_PAGEABLE_COUNT, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of Category", description = "get list of Category by store id and "
      + "" + "pageable")
  
  public GdnRestListResponse<CategoryDTO> getChildFromParentCount(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("id") String categoryId)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Category category = this.categoryService.findById(categoryId);
    return this.populateListResponseDTO(storeId, channelId, clientId, requestId, pageable,
        this.categoryService.findChildForParent(storeId, category, pageable));
  }

  @GetMapping(value = CategoryController.CHILD_PARENT_WITH_CATALOG_ID_PAGEABLE_COUNT, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get child Categories by catalog id", description =
      "get list of child count Category by store id and catalog id and " + "pageable")
  
  public GdnRestListResponse<CategoryDTO> getChildFromParentWithCatalogId(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String catalogId,
      @RequestParam(required = false) String categoryId, @RequestParam(defaultValue = "ALL") String filterType,
      @RequestParam(defaultValue = "ALL") String documentFilterType,
      @RequestParam(defaultValue = "true") boolean ignoreB2bExclusive,
      @RequestParam(defaultValue = "false") boolean filterHalalCategory) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<Category> categoryPage =
        this.categoryService.findChildForParentByCatalogId(storeId, categoryId, catalogId, documentFilterType,
            ignoreB2bExclusive, filterHalalCategory, pageable);
    List<CategoryServiceDTO> categoryServiceDTOS =
        this.categoryServiceWrapper.setChildCountByFilterType(storeId, categoryPage, filterType);
    return new GdnRestListResponse<CategoryDTO>(null, null, true, ConverterUtil.toCategoryDTOList(categoryServiceDTOS),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), categoryPage.getTotalElements()), requestId);
  }

  @GetMapping(value = CategoryController.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get child Categories by parent Category and Catalog type",
      description = "get list of child Category by store id and parent Category and Catalog type with "
          + "child countd")
  
  public GdnRestListResponse<CategoryDTO> getChildFromParentByCatalogTypeWithChildCount(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String categoryId,
      @RequestParam CatalogType catalogType, @RequestParam(required = false) String activated)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Category category = this.categoryService.findById(categoryId);
    if (activated != null && !"".equals(activated)
        && ("true".equalsIgnoreCase(activated) || "false".equalsIgnoreCase(activated)))
      return this.populateListResponseDTO(storeId, channelId, clientId, requestId, pageable,
          this.categoryService.findChildForParentWithCatalogType(storeId, category, catalogType,
              Boolean.valueOf(activated), pageable));
    else
      return this.populateListResponseDTO(storeId, channelId, clientId, requestId, pageable,
          this.categoryService.findChildForParentWithCatalogType(storeId, category, catalogType,
              null, pageable));
  }

  @GetMapping(value = CategoryController.FILTER_NAME_CATALOG_TYPE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "filter summary category by categoryName and catalogType",
      description = "filter summary category by categoryName and catalogType")
  
  public GdnRestListResponse<CategoryResponse> filterSummaryByCategoryNameAndCatalogType(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String categoryName,
      @RequestParam CatalogType catalogType) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<Category> categories =
        this.categoryService.findByCategoryNameAndCatalogType(storeId, categoryName, catalogType,
            pageable);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable, categories);
  }

  @GetMapping(value = CategoryController.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get child Categories by parent Category and Catalog type",
      description = "get list of child Category by store id and parent Category and Catalog type")
  
  public GdnRestListResponse<CategoryResponse> getChildFromParentByCatalogType(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String categoryId,
      @RequestParam CatalogType catalogType, @RequestParam(required = false) String activated)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Category category = this.categoryService.findById(categoryId);
    if (activated != null && !"".equals(activated)
        && ("true".equalsIgnoreCase(activated) || "false".equalsIgnoreCase(activated)))
      return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
          this.categoryService.findChildForParentWithCatalogType(storeId, category, catalogType,
              Boolean.valueOf(activated), pageable));
    else
      return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
          this.categoryService.findChildForParentWithCatalogType(storeId, category, catalogType,
              null, pageable));
  }

  @PostMapping(value = CategoryController.FILTER_BULK_CATEGORY_CODE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get categories by category codes and pageable",
      description = "category codes is mandatory, return error if it were null or empty")
  
  public GdnRestListResponse<CategoryDTO> getCategoriesByCategoryCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(
          defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(required = false) String activated, @RequestBody String requestBody) {
    CategoryMultipleIdRequest bulkRequest = null;
    try {
      if (requestBody == null)
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            "Request obj cannot be null");
      else {
        try {
          bulkRequest = objectMapper.readValue(requestBody, CategoryMultipleIdRequest.class);
        } catch (Exception e) {
          LOG.info("Request id :  {}, non CategoryMultipleIdRequest, trying with String []",
              requestId, e);
          try {
            String[] tempCategoryList = objectMapper.readValue(requestBody, String[].class);
            bulkRequest = new CategoryMultipleIdRequest();
            bulkRequest.setCategoryCode(Arrays.asList(tempCategoryList));
          } catch (Exception ex) {
            LOG.error("Request id {} , non CategoryMultipleIdRequest and non String[] ", requestId,
                ex);
          }
        }
        if (bulkRequest == null || bulkRequest.getCategoryCode() == null
            || bulkRequest.getCategoryCode().isEmpty())
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              "Category codes cannot be empty");
      }
      Pageable pageable = PageRequest.of(page, size);
      Page<Category> categories;
      if (StringUtils.isNotBlank(activated)
          && ("true".equalsIgnoreCase(activated) || "false".equalsIgnoreCase(activated))) {
        categories =
            this.categoryService.findByStoreIdAndCategoryCodes(storeId,
                bulkRequest.getCategoryCode(), Boolean.valueOf(activated), pageable);
      } else {
        categories =
            this.categoryService.findByStoreIdAndCategoryCodes(storeId,
                bulkRequest.getCategoryCode(), null, pageable);
      }

      return this.populateListResponseDTO(storeId, channelId, clientId, requestId, pageable,
          categories);
    } catch (ApplicationException e) {
      LOG.error(
          "Error invoking getCategoriesByCategoryCodes at CategoryController\nchannelId :{}, clientId:{}, requestId :{}, username :{}",
          new Object[] {channelId, clientId, requestId, username}, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false,
          requestId);
    } catch (Exception e) {
      LOG.error(
          "Error invoking getCategoriesByCategoryCodes at CategoryController\nchannelId :{}, clientId:{}, requestId :{}, username :{}",
          new Object[] {channelId, clientId, requestId, username}, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @GetMapping(value = CategoryController.GET_ALL_CATEGORIES_PARENT_MAPPING, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get categories and final parent categories mapping")
  
  public GdnRestListResponse<CategoryParentResponse> getCategoriesAndFinalCategoryMapping(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username)
      throws Exception {
    Map<String, String> categoryMapping = this.categoryService.getCategoryToFinalParent();
    List<CategoryParentResponse> categoryParentResponses = covertToListOfModels(categoryMapping);
    return new GdnRestListResponse<>(categoryParentResponses, new PageMetaData(
        categoryParentResponses.size(), 0, categoryParentResponses.size()), requestId);
  }

  private List<CategoryParentResponse> covertToListOfModels(Map<String, String> categoryMapping) {
    List<CategoryParentResponse> categoryParentResponseList = new ArrayList<>();
    for (String key : categoryMapping.keySet()) {
      categoryParentResponseList.add(new CategoryParentResponse(key, categoryMapping.get(key)));
    }
    return categoryParentResponseList;
  }

  private GdnRestListResponse<CategoryResponse> populateListResponse(String storeId,
      String channelId, String clientId, String requestId, Pageable pageable,
      Page<Category> categoryPage) throws Exception {
    List<CategoryResponse> categoryResponses = new ArrayList<CategoryResponse>();
    for (Category category : categoryPage.getContent()) {
      categoryResponses.add(CategoryServiceHelper.parseCategoryResponse(category));
    }
    return new GdnRestListResponse<CategoryResponse>(StringUtils.EMPTY, StringUtils.EMPTY, true, categoryResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            categoryPage.getTotalElements()), requestId);
  }

  private List<CategoryResponse> populateListResponse(List<Category> categories) {
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    for(Category category : categories) {
      categoryResponses.add(CategoryServiceHelper.parseCategoryResponse(category));
    }
    return categoryResponses;
  }

  private GdnRestListResponse<CategoryDTO> populateListResponseDTO(String storeId,
      String channelId, String clientId, String requestId, Pageable pageable,
      Page<Category> categoryPage) throws Exception {
    GdnRestListResponse<CategoryDTO> wrapper = null;
    List<CategoryDTO> categoryDTOs = new ArrayList<CategoryDTO>();
    // categoryService.findChildForParent(storeId, category, pageable)
    for (Category category : categoryPage.getContent()) {
      CategoryResponse response = new CategoryResponse();
      CategoryDTO responseDTO = new CategoryDTO();
      BeanUtils.copyProperties(category, response);
      CatalogResponse catalog = new CatalogResponse();
      BeanUtils.copyProperties(category.getCatalog(), catalog);
      catalog.setCatalogType(category.getCatalog().getCatalogType().toString());
      response.setCatalog(catalog);
      responseDTO.setNameEnglish(category.getNameEnglish());
      responseDTO.setDescriptionEnglish(category.getDescriptionEnglish());
      if (category.getParentCategory() != null) {
        response.setParentCategoryId(category.getParentCategory().getId());
      }
      // getTotalElementChild
      Page<Category> catCount =
          this.categoryService.findChildForParent(storeId, category, pageable);
      BeanUtils.copyProperties(response, responseDTO);
      responseDTO.setChildCount(catCount.getTotalElements());
      categoryDTOs.add(responseDTO);
    }
    wrapper =
        new GdnRestListResponse<CategoryDTO>(null, null, true, categoryDTOs, new PageMetaData(
            pageable.getPageSize(), pageable.getPageNumber(), categoryPage.getTotalElements()),
            requestId);
    return wrapper;
  }


  @PostMapping(value = CategoryController.CREATE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "save category", description = "get Category by store id and name and pageable")
  
  public GdnBaseRestResponse saveCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || request.getCreatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCatalog().getId()),
        ErrorMessage.CATALOG_ID_MUST_NOT_BE_BLANK.getMessage());

    Category category = new Category();
    if(request.getParentCategory() == null && request.getInternalActivationInterval() == null){
      request.setInternalActivationInterval(defaultInternalActivationPeriod);
    } else if(request.getParentCategory() != null) {
      request.setInternalActivationInterval(null);
    }
    BeanUtils.copyProperties(request, category, "catalog", "parentCategory", "categoryAttributes",
        "productCategories", "masterCategoryReferences", "salesCategoryReferences",
        "b2bSalesCategoryReferences", Constants.ID);
    category.setCatalog(this.catalogService.findByStoreIdAndId(storeId, request.getCatalog()
        .getId()));
    if (request.getParentCategory() != null) {
      category.setParentCategory(this.categoryService.findByStoreIdAndId(storeId, request
          .getParentCategory().getId()));
    }
    if ((request.getCategoryAttributes() != null) && !request.getCategoryAttributes().isEmpty()) {
      for (CategoryAttributeRequest categoryAttributeRequest : request.getCategoryAttributes()) {
        category.getCategoryAttributes().add(
            this.convertRequestToCategoryAttribute(storeId, category, categoryAttributeRequest));
      }
    }
    if ((request.getMasterCategoryReferences() != null)
        && !request.getMasterCategoryReferences().isEmpty()) {
      for (CategoryReferenceRequest masterCategoryReference : request.getMasterCategoryReferences()) {
        category.getMasterCategoryReferences().add(
            this.convertRequestToCategoryReference(storeId, category, masterCategoryReference));
      }
    }

    if (StringUtils.isEmpty(category.getStoreId())) {
      category.setStoreId(storeId);
    }
    if (!category.getMasterCategoryReferences().isEmpty()) {
      category = this.categoryService.saveAndUpdateProductCategory(storeId, category);
    } else {
      category = this.categoryService.save(category);
    }
    domainEventPublisherService.publishCategory(category, null, new HashSet<>(), false);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = CategoryController.UPDATE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "Update Category", description = "Update Category")
  public GdnBaseRestResponse updateCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || request.getUpdatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCatalog().getId()),
        ErrorMessage.CATALOG_ID_MUST_NOT_BE_BLANK.getMessage());
    Category savedCategory =
        this.categoryService.findByStoreIdAndIdInitCategoryAttribute(storeId, request.getId());
    Integer savedInternalActivationInterval = savedCategory.getInternalActivationInterval();
    if(request.getParentCategory() == null && request.getInternalActivationInterval() == null){
      request.setInternalActivationInterval(defaultInternalActivationPeriod);
    } else if(request.getParentCategory() != null) {
      request.setInternalActivationInterval(null);
    }
    BeanUtils.copyProperties(request, savedCategory, "categoryCode", "catalog", "parentCategory",
        "categoryAttributes", "productCategories", "version", "createdBy", "createdDate",
        "masterCategoryReferences", "salesCategoryReferences", "b2bSalesCategoryReferences");

    String parentCategoryIdOneLevelAbove = "";

    if (StringUtils.isEmpty(savedCategory.getStoreId())) {
      savedCategory.setStoreId(storeId);
    }

    if (request.getParentCategory() != null) {
      parentCategoryIdOneLevelAbove = request.getParentCategory().getId();
    }
    savedCategory.setParentCategory(null);
    this.categoryService
        .adjustCategory(savedCategory, this.convertRequestToCategory(storeId, request),
            parentCategoryIdOneLevelAbove, savedInternalActivationInterval);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = CategoryController.UPDATE_DISPLAY_FLAG, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "Update Category display", description = "Update Category display")
  
  public GdnBaseRestResponse updateCategoryDisplayFlag(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("id") String categoryId, @RequestParam Boolean flag) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(categoryId),
        ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage());
    this.categoryService.setCategoryDisplayable(categoryId, flag);
    return new GdnBaseRestResponse(null, null, true, categoryId);
  }

  @PostMapping(value = CategoryController.UPLOAD, produces = {"text/csv"}, consumes = {
      MediaType.MULTIPART_FORM_DATA_VALUE})
  
  @Operation(summary = "upload category", description = "upload category")
  public byte[] uploadCategory(HttpServletRequest request, HttpServletResponse response,
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestPart("categoryCsvFile") MultipartFile categoryCsvFile) throws Exception {
    List<String> categoryIds = new ArrayList<String>();
    ICsvBeanReader beanReader = null;
    try {
      beanReader =
          new CsvBeanReader(new InputStreamReader(categoryCsvFile.getInputStream(), "UTF-8"),
              CsvPreference.STANDARD_PREFERENCE);
      beanReader.getHeader(true);
      CategoryCsv categoryCsv, currentCategoryCsv = null;
      Category currentCategory = null;

      while ((categoryCsv =
          beanReader
              .read(CategoryCsv.class, CategoryCsv.INPUT_HEADER, CategoryCsv.INPUT_PROCESSORS)) != null) {
        storeId = categoryCsv.getStoreId();
        Attribute attribute =
            this.attributeService.findById(categoryCsv.getStoreId(), categoryCsv.getAttributeId());
        checkNotNull(attribute, "not found attribute with id " + categoryCsv.getAttributeId());

        if (!categoryCsv.equals(currentCategoryCsv)) {
          currentCategoryCsv = categoryCsv;
          currentCategory = new Category();
          BeanUtils.copyProperties(categoryCsv, currentCategory, "parentCategory", "description");
          currentCategory.setDescription(categoryCsv.getDescription().getBytes());
          Catalog catalog =
              this.catalogService.findByStoreIdAndId(categoryCsv.getStoreId(),
                  categoryCsv.getCatalogId());
          checkNotNull(catalog, "not found catalog with id " + categoryCsv.getCatalogId());
          currentCategory.setCatalog(catalog);

          if (categoryCsv.getParentCategory() != null) {
            Category parentCategory =
                this.categoryService.findByStoreIdAndId(storeId, categoryCsv.getParentCategory());
            checkNotNull(parentCategory,
                "not found category with id " + categoryCsv.getParentCategory());
            currentCategory.setParentCategory(parentCategory);
          }
        }
        currentCategory.getCategoryAttributes().add(
            new CategoryAttribute(currentCategory, attribute, categoryCsv.getAttributeSequence(),
                categoryCsv.isMainDefiningAttribute(), categoryCsv.isUSP(), categoryCsv
                    .getStoreId()));
      }
      if (currentCategory != null) {
        Category category = categoryService.save(currentCategory);
        domainEventPublisherService.publishCategory(category, null, new HashSet<>(), false);
        categoryIds.add(category.getId());
      }
    } finally {
      if (beanReader != null) {
        beanReader.close();
      }
    }

    ICsvBeanWriter beanWriter = null;
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    PrintWriter writer = new PrintWriter(new BufferedOutputStream(out, 1024));
    try {
      beanWriter = new CsvBeanWriter(writer, CsvPreference.STANDARD_PREFERENCE);
      beanWriter.writeHeader(CategoryCsv.ACTUAL_HEADER);

      for (String categoryId : categoryIds) {
        Category category =
            this.categoryService.findByStoreIdAndIdInitCategoryAttribute(storeId, categoryId);
        CategoryCsv categoryCsv = new CategoryCsv();
        BeanUtils.copyProperties(category, categoryCsv, "description");
        categoryCsv.setDescription(new String(category.getDescription()));
        categoryCsv.setId(categoryId);
        if (category.getParentCategory() != null) {
          categoryCsv.setParentCategory(category.getParentCategory().getId());
        }
        categoryCsv.setCatalogId(category.getCatalog().getId());
        for (CategoryAttribute categoryAttribute : category.getCategoryAttributes()) {
          categoryCsv.setAttributeId(categoryAttribute.getAttribute().getId());
          categoryCsv.setAttributeSequence(categoryAttribute.getSequence());
          categoryCsv.setMainDefiningAttribute(categoryAttribute.isMainDefiningAttribute());
          categoryCsv.setUSP(categoryAttribute.isUSP());
          beanWriter.write(categoryCsv, CategoryCsv.OUTPUT_HEADER, CategoryCsv.OUTPUT_PROCESSORS);
        }
      }
    } finally {
      if (beanWriter != null) {
        beanWriter.close();
      }
    }
    out.close();
    byte[] result = out.toByteArray();
    response.setHeader("Content-Disposition", "attachment; filename=\""
        + CategoryController.CATEGORY_EXPORT_CSV_FILENAME + "\"");
    return result;
  }

  @GetMapping(value = CategoryController.GET_FINAL_PARENT_CATEGORY, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get categories and final parent categories mapping")
  
  public GdnRestSingleResponse<SingleObjectResponse<String>> getFinalParentCategory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String categoryId) throws Exception {
    String finalParentCategory = StringUtils.EMPTY;
    boolean isSuccess = false;
    String errorMsg = StringUtils.EMPTY;
    try {
      finalParentCategory = this.categoryService.getFinalParentCategory(categoryId);
      isSuccess = true;
    } catch (EmptyResultDataAccessException dataException) {
      LOG.error("Not able to find final parent category for category {} and requestId {}",
          categoryId, requestId, dataException);
      // setting success is true in this case as there is no actual exception and
      // final parent category is coming as empty because of some data inconsistency
      isSuccess = true;
    } catch (Exception e) {
      LOG.error("Exception occurred while fetching final parent category from given category {} ",
          categoryId, e);
      errorMsg = e.getMessage();
    }
    return new GdnRestSingleResponse<SingleObjectResponse<String>>(errorMsg, StringUtils.EMPTY,
        isSuccess, new SingleObjectResponse(finalParentCategory), requestId);
  }

  /**
   * get final parent category cached function used for validation of new product's category
   * especially via MTA-API
   */
  @GetMapping(value = CategoryController.GET_FINAL_PARENT_CATEGORY_CACHED, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get categories and final parent categories mapping (cached)")
  
  public GdnRestSingleResponse<SingleObjectResponse<String>> getFinalParentCategoryCached(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String categoryId) throws Exception {
    String finalParentCategory = StringUtils.EMPTY;
    boolean isSuccess = false;
    String errorMsg = StringUtils.EMPTY;
    try {
      finalParentCategory = this.categoryServiceWrapper.getFinalParentCategoryCached(storeId, categoryId);
      isSuccess = true;
    } catch (EmptyResultDataAccessException dataException) {
      LOG.error("Not able to find final parent category for category {} and requestId {}",
          categoryId, requestId, dataException);

      isSuccess = true;
    } catch (Exception e) {
      LOG.error("Exception occurred while fetching final parent category from given category {} ",
          categoryId, e);
      errorMsg = e.getMessage();
    }
    return new GdnRestSingleResponse<SingleObjectResponse<String>>(errorMsg, StringUtils.EMPTY,
        isSuccess, new SingleObjectResponse(finalParentCategory), requestId);
  }

  @GetMapping(value = CategoryController.GET_ALL_CATEGORY_TREE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get categories")
  
  public GdnRestListResponse<CategoryTreeResponse> getAllCategoryTree(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String catalogName, @RequestParam(required = false) boolean ignoreB2bExclusive)
      throws Exception {

    try {
      List<CategoryTreeResponse> catResponse = new ArrayList<>();
      List<CategoryTreeDTO> catDTOList =
          this.categoryService.getAllCategoryTree(catalogName, storeId, ignoreB2bExclusive);
      for (CategoryTreeDTO data : catDTOList) {
        CategoryTreeResponse cat = new CategoryTreeResponse();
        BeanUtils.copyProperties(data, cat);
        catResponse.add(cat);
      }

      return new GdnRestListResponse<>(catResponse, null, requestId);
    } catch (Exception e) {
      LOGGER.error("[PCB] Get all category error: " + e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @PostMapping(value = CategoryController.PUBLISH_ALL_CATEGORY, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get active category tree")
  
  public GdnBaseRestResponse publishAllCategories(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String catalogName) {
    this.categoryServiceWrapper.publishAllCategories(catalogName, storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = CategoryController.GET_ACTIVE_CATEGORY_TREE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get active category tree")
  
  public GdnRestListResponse<CategoryTreeResponse> getActiveCategoryTree(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String catalogName, @RequestParam String nonInventoryCode) throws Exception {

    try {
      List<CategoryTreeResponse> catResponse = new ArrayList<>();
      List<CategoryTreeDTO> catDTOList =
          categoryServiceWrapper.getActiveCategoryTree(catalogName, storeId, nonInventoryCode);
      for (CategoryTreeDTO data : catDTOList) {
        CategoryTreeResponse cat = new CategoryTreeResponse();
        BeanUtils.copyProperties(data, cat);
        catResponse.add(cat);
      }
      return new GdnRestListResponse<>(catResponse, null, requestId);
    } catch (Exception e) {
      LOGGER.error("[PCB] Get all active category error: " + e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), StringUtils.EMPTY, Boolean.FALSE, requestId);
    }
  }

  @PostMapping(value = CategoryController.GET_CATEGORY_TREE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get categories")
  
  public GdnRestListResponse<CategoryTreeResponse> getCategoryTree(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String catalogName, @RequestBody List<String> categoryCodes)
      throws Exception {

    try {
      List<CategoryTreeResponse> catResponse = new ArrayList<>();
      List<CategoryTreeDTO> catDTOList =
          this.categoryService.getCategoryTree(categoryCodes, catalogName, storeId);
      for (CategoryTreeDTO data : catDTOList) {
        CategoryTreeResponse cat = new CategoryTreeResponse();
        BeanUtils.copyProperties(data, cat);
        catResponse.add(cat);
      }
      return new GdnRestListResponse<>(catResponse, null, requestId);
    } catch (Exception e) {
      LOGGER.error("[PCB] Get category error: " + e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @GetMapping(value = CategoryController.GET_PARENT_CATEGORY, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get all parent categories")
  
  public GdnRestSingleResponse<SingleObjectResponse<List<String>>> getParentCategories(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId) throws Exception {
    List<String> parentCategories = this.categoryService.getParentCategories();
    if (CollectionUtils.isNotEmpty(parentCategories)) {
      return new GdnRestSingleResponse<SingleObjectResponse<List<String>>>(
          new SingleObjectResponse<List<String>>(parentCategories), requestId);
    } else {
      return new GdnRestSingleResponse<SingleObjectResponse<List<String>>>("No data found", null,
          true, null, requestId);
    }
  }

  @GetMapping(value = CategoryController.SUMMARY + CategoryController.PATH_CATALOG_ID, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get category summary with simplified response",
      description = "this api is used for blibli.com category list")
  
  public GdnRestListResponse<CategorySummaryResponse> getCategorySummarySimplifiedByCatalogId(
      @PathVariable("catalogId") String catalogId, @RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) Boolean display,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    Page<Category> pageOfCategory =
        this.categoryService.findCategorySummaryByStoreIdAndCatalogIdAndDisplay(storeId, catalogId,
            display, PageRequest.of(page, size));
    List<CategorySummaryResponse> responseContents = new ArrayList<>();
    for (Category category : pageOfCategory.getContent()) {
      CategorySummaryResponse responseContent = convertCategoryToCategorySumaryResponse(category);
      responseContents.add(responseContent);
    }
    return new GdnRestListResponse<>(responseContents, new PageMetaData(pageOfCategory.getSize(),
        pageOfCategory.getNumber(), pageOfCategory.getTotalElements()), requestId);
  }

  @PostMapping(value = CategoryController.GET_CATEGORY_NAMES, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get category names by category codes")
  
  public GdnRestSingleResponse<CategoryNamesResponse> getCategoryNames(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "50") Integer size,
      @RequestBody CategoryMultipleIdRequest categoryCodes) {
    LOG.info("{} Invoking getBrandNames at controller", requestId);
    Map<String, String> categoryMap;
    CategoryNamesResponse response = new CategoryNamesResponse();
    Pageable pageable = PageRequest.of(page, size);
    String errorCode = null;
    String errorMessage = null;
    boolean success = false;
    try {
      if (CollectionUtils.isNotEmpty(categoryCodes.getCategoryCode())) {
        if (categoryCodes.getCategoryCode().size() < REQUEST_LIMIT) {
          categoryMap = this.categoryService
              .findCategoryNamesByCategoryCodes(storeId, categoryCodes.getCategoryCode(), pageable);
          response.setCategoryMap(categoryMap);
          success = true;
        } else {
          errorMessage =
              "Request should not exceed 500 categoryCodes" + ErrorCategory.VALIDATION.getMessage()
                  + "";
          errorCode = ErrorCategory.VALIDATION.getCode();
        }
      }
    } catch (Exception e) {
      LOGGER.error("[PCB] Error fetching category names " + e.getMessage(), e);
      errorCode = ErrorCategory.DATA_ACCESS.getCode();
      errorMessage = e.getMessage();
    }
    return new GdnRestSingleResponse<>(errorMessage, errorCode, success, response, requestId);
  }

  @PostMapping(value = CategoryController.GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get child categories from C1 categoryCodes", description = "get all child "
      + "category codes from C1 categoryCode ")
  
  public GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(defaultValue = "false", required = false) boolean filterOutInactiveCn,
    @RequestBody CategoryCodeRequest request)  {
    String errorMessage = null;
    boolean success = false;
    CategoryCodeResponse categoryCodeResponse = null;
    try {
      LOG.info(" Invoking getAllChildCategoriesFromC1CategoryCode at CategoryController - {}", requestId);
      GdnPreconditions.checkArgument(
          (request.getCategoryCodes() != null && !request.getCategoryCodes().isEmpty()),
          CategoryTreeControllerErrorMessage.CATEGORY_CODES_MUST_NOT_BE_BLANK);
      List<String> categoryCodes = request.getCategoryCodes();
      List<String> childCategoryCodes =
          this.categoryService.findAllChildForC1CategoryCodes(storeId, categoryCodes, filterOutInactiveCn);
      categoryCodeResponse = new CategoryCodeResponse(childCategoryCodes);
      success = true;
    }catch(Exception e){
      LOGGER.error("Error while getting child categories -{} ",requestId, e);
      errorMessage = e.getMessage();
    }
    return new GdnRestSingleResponse<>(errorMessage, null, success,
        categoryCodeResponse, requestId);
  }

  @GetMapping(value = CategoryController.VALIDATE_IS_CN_CATEGORY, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "validate is Cn category", description = "validate whether specified category is Cn or not")
  
  public GdnRestSingleResponse<CategoryResponse> validateCnCategory(
      @RequestParam String storeId,
      @RequestParam String channelId,
      @RequestParam String clientId,
      @RequestParam String requestId,
      @RequestParam String username,
      @RequestParam String categoryCode)  {
    try{
      boolean validateIsCategoryCn = this.categoryService.validateIsCategoryCn(storeId, categoryCode);
      Category category = categoryService.findBasicInfoByStoreIdAndCategoryCode(storeId, categoryCode);
      CategoryResponse categoryResponse = ConverterUtil.getCategoryDetailResponse(category);
      return new GdnRestSingleResponse<>(null, null, validateIsCategoryCn, categoryResponse, requestId);
    } catch(Exception e){
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @PutMapping(value = CategoryController.UPDATE_CATEGORY_INFO, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "Update Category Info", description = "Update Category Info")
  
  public GdnBaseRestResponse updateCategoryInfo(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryInfoUpdateRequest request,
      @RequestParam(defaultValue = "false") boolean statusChangeFlag)
      throws Exception {
    try {
      LOG.info("Update category info api called. RequestId: {}", requestId);
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getUpdatedBy()),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getId()),
          ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage());
      CategoryInfoUpdateDTO categoryInfoUpdateDTO = ConverterUtil.toCategoryInfoUpdateDTO(request);
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      List<CategoryHistoryEventModel> historyEventModelList =
        categoryServiceWrapper.updateCategoryInfo(storeId, categoryInfoUpdateDTO,
          statusChangeFlag);
      domainEventPublisherService.publishCategoryUpdateHistory(historyEventModelList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("Exception occurred while updating category info : {}", request, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(),
          Boolean.FALSE, requestId);
    } catch (Exception e) {
      LOG.error("Error occurred while updating category info : {}", request, e);
      return new GdnBaseRestResponse(CATEGORY_INFO_UPDATE_ERROR_MESSAGE,
          ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, requestId);
    }
  }

  @PostMapping(value = CategoryController.CREATE_CATEGORY, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Create Category ", description = "Create Category ")
  
  public GdnRestSingleResponse<CreateCategoryResponse> createCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryDetailRequest request) {
    try {
      LOG.info("Creating new category. CategoryDetailRequest :{}", request);
      GdnPreconditions.checkArgument(
          !(StringUtils.isEmpty(request.getCreatedBy()) || Objects.isNull(request.getCreatedDate())),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCatalogId()),
          ErrorMessage.CATALOG_ID_MUST_NOT_BE_BLANK.getMessage());
      CategoryDetailDTO categoryDetailDTO = ConverterUtil.toCategoryDetailDTO(request);
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      Pair<CategoryAndHierarchyDto, List<RestrictedKeywordHistoryEventModel>>
        categoryAndHierarchyHistoryPair =
        categoryServiceWrapper.createCategoryWithoutEventPublish(storeId, categoryDetailDTO);
      categoryServiceWrapper.publishCategoryChangeAndClearCache(categoryAndHierarchyHistoryPair.getLeft());
      domainEventPublisherService.publishRestrictedKeywordHistory(
        categoryAndHierarchyHistoryPair.getRight());
      return new GdnRestSingleResponse<CreateCategoryResponse>(null, null, true,
          new CreateCategoryResponse(categoryAndHierarchyHistoryPair.getLeft().getCategory().getCategoryCode()), requestId);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      LOG.error("Exception occurred while creating category  : {}", request,
          applicationRuntimeException);
      return new GdnRestSingleResponse<CreateCategoryResponse>(applicationRuntimeException.getErrorMessage(),
          applicationRuntimeException.getErrorCodes().getCode(), Boolean.FALSE, null, requestId);
    } catch (Exception e) {
      LOG.error("Error occurred while creating category : {}", request, e);
      return new GdnRestSingleResponse<CreateCategoryResponse>(CATEGORY_CRATE_ERROR_MESSAGE,
          ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, null, requestId);
    }
  }

  @PutMapping(value = CategoryController.UPDATE_CATEGORY_MAPPINGS, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "Update Category References", description = "Update Category References")
  
  public GdnBaseRestResponse updateCategoryMappings(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryMappingsUpdateRequest request) throws Exception {
    try {
      LOG.info("Update category mappings api called. RequestId: {}", requestId);
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getUpdatedBy()),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getId()),
          ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage());
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO =
          ConverterUtil.toCategoryMappingsUpdateDTO(request);
      categoryServiceWrapper.updateCategoryMappingsAndPublishHistory(storeId,
          categoryMappingsUpdateDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("Exception occurred while updating category mappings : {}", request, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(),
          Boolean.FALSE, requestId);
    } catch (Exception e) {
      LOG.error("Error occurred while updating category info : {}", request, e);
      return new GdnBaseRestResponse(CATEGORY_MAPPINGS_UPDATE_ERROR_MESSAGE,
          ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, requestId);
    }
  }

  @GetMapping(value = CategoryController.GET_CATEGORY_TREE_WITH_REVIEW_CONFIG, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "fetch all category tree with respective review configs",
      description = "fetch all category tree with respective review configs")
  
  public GdnRestListResponse<CategoryTreeNodeResponse> getCategoryTreeWithReviewConfig(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) {
    try {
      List<CategoryTreeNodeResponse> categoryTreeNodeResponseList = this.categoryService.getCategoryTree(storeId);
      return new GdnRestListResponse<>(null, null, true, categoryTreeNodeResponseList,
          new PageMetaData(0, 0, categoryTreeNodeResponseList.size()), requestId);
    } catch (Exception e) {
      LOG.error("Caught exception while returning category tree with review configs", e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @PutMapping(value = CategoryController.UPDATE_CATEGORIES_WITH_RESTRICTED_KEYWORDS, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update the categories till Cn level with the restricted keywords",
      description = "Update the categories till Cn level with the restricted keywords")
  
  public GdnBaseRestResponse updateCategoriesWithRestrictedKeywords(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("categoryCode") String categoryCode,
      @RequestBody CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList) throws Exception {
    try {
      LOG.info("Update categories with restricted keywords with request : {} for category : {}",
          categoryKeywordUpdateRequestList, categoryCode);
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO =
          ConverterUtil.toCategoryKeywordsUpdateListDTO(categoryKeywordUpdateRequestList,storeId);
      List<RestrictedKeywordHistoryEventModel> keywordHistoryEventModelList =
        categoryServiceWrapper.updateCategoriesWithRestrictedKeywords(categoryCode,
          categoryKeywordsUpdateListDTO);
      domainEventPublisherService.publishRestrictedKeywordHistory(keywordHistoryEventModelList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("Runtime Exception occurred while updating category mappings with restricted keywords : {}",
          categoryKeywordUpdateRequestList, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      LOG.error("Exception occurred while updating category with restricted keywords : {}",
          categoryKeywordUpdateRequestList, e);
      return new GdnBaseRestResponse(CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(),
          Boolean.FALSE, requestId);
    }
  }

  @PostMapping(value = CategoryController.GET_CATEGORY_RESTRICTED_KEYWORDS, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "fetch the keywords associated with a category")
  
  public GdnRestListResponse<RestrictedKeywordsResponse> getCategoryRestrictedKeywords(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody CategoryRestrictedKeywordsRequest request) {
    Pageable pageable = PageRequest.of(page, size);
    try {
      LOG.info("Fetch Restricted keyword called with request : {} requestId: {}", request, requestId);
      GdnPreconditions.checkArgument(
          (StringUtils.isNotBlank(request.getKeyword()) || StringUtils.isNoneBlank(request.getCategoryCode())),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_RESTRICTED_KEYWORD.getMessage());
      Page<RestrictedKeywordsResponse> restrictedKeywordsResponsePage =
          this.categoryServiceWrapper.findCategoryRestrictedKeywords(storeId, request, pageable);
      return new GdnRestListResponse<>(null, null, true,
          restrictedKeywordsResponsePage.getContent(), new PageMetaData(pageable.getPageSize(),
          pageable.getPageNumber(), restrictedKeywordsResponsePage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("Exception occurred while fetching keywords : {}", request, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    } catch (Exception e) {
      LOG.error("Error occurred while fetching keywords : {}", request, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @GetMapping(value = CategoryController.GET_CATEGORY_TREE_FOR_GENERIC_TEMPLATE, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get categories for generic template", description = "get categories for generic template")
  
  public GdnRestListResponse<CategoryTreeResponse> getCategpryTreeForGenericTemplate(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "false") boolean genericTemplateEligible, @RequestParam(defaultValue = "true") boolean ignoreB2bExclusive) throws Exception {
    try {
      List<CategoryTreeResponse> responseList =
          this.categoryService.getAllCategoryTreeforGenericTemplate(storeId, genericTemplateEligible, ignoreB2bExclusive);
      return new GdnRestListResponse<>(null, null, true, responseList, new PageMetaData(0, 0, responseList.size()),
          requestId);
    } catch (Exception e) {
      LOG.error("Caught exception while returning category tree for generic template", e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @GetMapping(value = CategoryController.GET_RESTRICTED_KEYWORD_MAPPED_TO_CATEGORY, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  
  @Operation(summary = "get restricted keyword mapped to category", description = "get restricted keyword mapped to category")
  public GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> getRestrictedKeywordMappedToCategory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @PathVariable("categoryCode") String categoryCode)
      throws Exception {
    try {
      List<RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordList =
          this.categoryServiceWrapper.getRestrictedKeywordMappedToCategory(storeId, categoryCode);
      return new GdnRestListResponse<>(restrictedKeywordList,
          new PageMetaData(restrictedKeywordList.size(), 0, restrictedKeywordList.size()), requestId);
    } catch (Exception e) {
      LOG.error("error getting restricted keyword mapped to category : {}", categoryCode, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @GetMapping(value = CategoryController.GET_WHOLESALE_CONFIG_TO_CATEGORY, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  
  @Operation(summary = "get wholesale config mapped to category", description = "get wholesale config mapped to category")
  public GdnRestSingleResponse<WholesaleMappingResponse> getWholesaleConfigToCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false) String categoryCode,
      @RequestParam(required = false) String categoryId) throws Exception {
    try {
      LOG.info("Get Wholesale Config called with categoryCode : {} categoryId: {}", categoryCode, categoryId);
      GdnPreconditions.checkArgument((StringUtils.isNotBlank(categoryCode) || StringUtils.isNotBlank(categoryId)),
          ErrorMessage.ERROR_GETTING_WHOLESALE_CONFIG.getMessage());
      WholesaleMappingResponse wholesaleMappingResponse =
          this.categoryWholesaleConfigService.findByStoreIdAndCategoryId(storeId, categoryId, categoryCode);
      return new GdnRestSingleResponse<>(null, null, true, wholesaleMappingResponse, requestId);
    } catch (Exception e) {
      LOG.error("error getting wholesale configuration mapped to category : {} {}", categoryId, categoryCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @PutMapping(value = CategoryController.UPDATE_CATEGORIES_WITH_WHOLESALE_CONFIG, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update categories with wholesale configuration", description = "Update the categories till Cn level with the wholesale configuration")
  
  public GdnBaseRestResponse updateCategoriesWithWholesaleConfig(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("categoryId") String categoryId,
      @RequestBody WholesaleMappingRequest wholesaleMapping) throws Exception {
    try {
      LOG.info("Update categories with wholesale configuration with request : {} for category : {}", wholesaleMapping,
          categoryId);
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      WholesaleMappingDTO wholesaleMappingDTO = ConverterUtil.toCategoryWholesaleDTO(wholesaleMapping);
      CategoryUpdateHistoryDTO categoryUpdateHistoryDTO =
          categoryServiceWrapper.updateCategoriesWithWholesaleConfig(storeId, categoryId,
              wholesaleMappingDTO);
      categoryServiceWrapper.publishHistoryEventForWholesaleConfigUpdate(storeId,
          categoryUpdateHistoryDTO);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("Exception occurred while updating category with wholesale config : {}", wholesaleMapping, e);
      return new GdnBaseRestResponse(CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(),
          Boolean.FALSE, requestId);
    }
  }

  @RequestMapping(value = CategoryController.CREATE_OSC, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "create new original sales category", description = "create new original sales category ")
  
  public GdnRestSimpleResponse<String> createOriginalSalesCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody OriginalSalesCategoryRequest request)
      throws Exception {
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getOscCode()),
          ErrorMessage.OSC_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getOscLongText()),
          ErrorMessage.OSC_LONG_TEXT_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getOscShortText()),
          ErrorMessage.OSC_SHORT_TEXT_MUST_NOT_BE_BLANK.getMessage());
      String response = this.categoryService.saveOSC(request);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, response);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("Exception while saving osc with request : {}", request, e);
      return new GdnRestSimpleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId, null);
    } catch (Exception e) {
      LOGGER.error("Exception while saving osc with request : {}", request, e);
      return new GdnRestSimpleResponse<>(ErrorMessage.ERROR_IN_SAVING_OSC.getMessage(),
          ErrorCategory.UNSPECIFIED.getMessage(), false, requestId, null);
    }
  }

  @RequestMapping(value = CategoryController.FETCH_OSC_LIST, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter osc list", description = "filter osc list")
  
  public GdnRestListResponse<OscSummaryResponse> filterOscSummaryResponse(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false) String oscCode,
      @RequestParam(required = false) String keyword, @RequestParam(required = false) Boolean activated)
      throws Exception {
    try {
      LOG.info("Fetch summary based on oscCode: {} keyword : {} activated : {}", oscCode, keyword, activated);
      List<OscSummaryResponse> responses = originalSalesCategoryService.filterSummaryOSC(oscCode, keyword, activated);
      return new GdnRestListResponse<>(null, null, true, responses,
          new PageMetaData(Integer.MAX_VALUE, 0, responses.size()), requestId);
    } catch (Exception e) {
      LOG.error("Exception occurred while fetching osc list : ", e);
      return new GdnRestListResponse<>(OSC_FETCH_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = CategoryController.UPDATE_OSC_LIST, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update original sales category", description = "update original sales category ")
  
  public GdnBaseRestResponse updateOriginalSalesCategory(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody OscInfoUpdateDTO request) throws Exception {
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getOscCode()),
          ErrorMessage.OSC_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getOscLongText()),
          ErrorMessage.OSC_LONG_TEXT_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getOscShortText()),
          ErrorMessage.OSC_SHORT_TEXT_MUST_NOT_BE_BLANK.getMessage());
      this.originalSalesCategoryService.updateOsc(storeId, username, request);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("Exception while updating osc with request : {} ", request, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception while updating osc with request : {} ", request, e);
      return new GdnBaseRestResponse(ErrorMessage.ERROR_IN_UPDATING_OSC.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = CategoryController.GET_OSC_BY_ID, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  
  @Operation(summary = "get original sales category by ID", description = "get original sales category by ID")
  public GdnRestSingleResponse<OriginalSalesCategoryResponse> getOSCById(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("id") String id) throws Exception {
    try {
      OriginalSalesCategoryResponse response =
          this.originalSalesCategoryService.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      LOG.error("error getting original sales category by ID : {} ", id, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = CategoryController.GET_CATEGORY_RESTRICTED_KEYWORD, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Entity from pcc_category_restricted_keyword by id", description = "get Entity from pcc_category_restricted_keyword by id")
  
  public GdnRestSimpleResponse<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordById(@RequestParam String id,
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId) throws Exception {
    try {
      LOG.info("Invoking getCategoryRestrictedKeywordById at controller for id : {} and requestId : {}", id,
          requestId);
      CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse =
          this.categoryRestrictedKeywordService.getCategoryRestrictedKeywordById(storeId, id);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, categoryRestrictedKeywordResponse);
    } catch (Exception e) {
      LOG.error("Caught exception while getting CategoryRestrictedKeyword by id : {} ",id, e);
      return new GdnRestSimpleResponse<>(ErrorMessage.INVALID_ID.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(),
          false, requestId, null);
    }
  }

  @RequestMapping(value = CategoryController.GET_CATEGORY_RESTRICTED_KEYWORD_LIST,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get category restricted keyword list", description = "Get category restricted keyword list")
  
  public GdnRestListResponse<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordList(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryCodeAndKeywordListRequest categoryCodeAndKeywordListRequest) throws Exception {
    try {
      LOG.info("Get category restricted keyword list for request : {} ", categoryCodeAndKeywordListRequest);
      List<CategoryRestrictedKeywordResponse> response =
          categoryRestrictedKeywordService.getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(storeId,
              categoryCodeAndKeywordListRequest.getCategoryCode(), categoryCodeAndKeywordListRequest.getKeywordIds());
      return new GdnRestListResponse<>(null, null, true, response, new PageMetaData(0, 1, response.size()), requestId);
    } catch (Exception e) {
      LOG.error("Error when trying to fetch category restricted keyword list for request : {} , error - ",
          categoryCodeAndKeywordListRequest, e);
      return new GdnRestListResponse<>(null, null, false, null, new PageMetaData(0, 1, 0), requestId);
    }
  }

  @GetMapping(value = CategoryController.FETCH_HISTORY_BY_CATEGORY_CODE, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get Category History Info", description = "Get Category History Info")
  
  public GdnRestListResponse<CategoryHistoryResponse> getCategoryHistory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "20") Integer size,
      @PathVariable("categoryCode") String categoryCode) {
    try {
      Page<CategoryHistoryResponse> categoryHistoryResponse =
          this.categoryHistoryService.fetchCategoryHistory(storeId, categoryCode, page, size);
      return new GdnRestListResponse<>(categoryHistoryResponse.getContent(),
          new PageMetaData(size, page, categoryHistoryResponse.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("Error when trying to fetch category history info for code {} ", categoryCode, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, null, null,
          requestId);
    } catch (Exception e) {
      LOG.error("Error when trying to fetch category history info for code {} ", categoryCode, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }


}
