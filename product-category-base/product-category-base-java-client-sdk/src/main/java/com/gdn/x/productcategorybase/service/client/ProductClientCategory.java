package com.gdn.x.productcategorybase.service.client;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.ProductCategoryApiPath;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryParentResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;

public class ProductClientCategory extends GdnBaseRestCrudClient {

  public static final String BASE_PATH = "/api/category";
  private static final String CREATE = ProductClientCategory.BASE_PATH + "/create";
  private static final String DELETE = ProductClientCategory.BASE_PATH + "/delete";
  private static final String UPDATE = ProductClientCategory.BASE_PATH + "/update";
  private static final String DETAIL_BY_CATEGORY_CODE = ProductClientCategory.BASE_PATH
      + "/categoryCode/";
  private static final String FILTER_NAME_PAGEABLE = ProductClientCategory.BASE_PATH
      + "/filter/name/pageable";
  private static final String CHILD_PARENT_PAGEABLE = ProductClientCategory.BASE_PATH
      + "/filter/childParent/pageable";
  private static final String CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE =
      ProductClientCategory.BASE_PATH + "/filter/childParent/catalog-type/pageable";
  private static final String CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT =
      ProductClientCategory.BASE_PATH + "/filter/childParent/catalog-type/pageablecount";
  private static final String CHILD_PARENT_PAGEABLE_COUNT = ProductClientCategory.BASE_PATH
      + "/filter/childParent/pageablecount";
  private static final String CHILD_PARENT_WITH_CATALOG_ID_PAGEABLE_COUNT =
      ProductClientCategory.BASE_PATH + "/filter/childParent/catalog/pageablecount";
  private static final String HIERARCHY_FILTER_CATEGORY_CODE = ProductClientCategory.BASE_PATH
      + "/hierarchy/filter/category-code/";
  private static final String HIERARCHY_FILTER_CATEGORY_CODES = ProductClientCategory.BASE_PATH
      + "/hierarchy/filter/category-codes";
  private static final String FILTER_BULK_CATEGORY_CODE = ProductClientCategory.BASE_PATH
      + "/filter/bulk/category-codes";
  private static final String GET_ALL_CATEGORIES_PARENT_MAPPING = "/parent-category-mapping";
  private static final String FILTER_NAME_CATALOG_TYPE = ProductClientCategory.BASE_PATH
      + "/filter/name/catalog-type";
  private static final String GET_FINAL_PARENT_CATEGORY = "/final-parent-category";
  private static final String GET_FINAL_PARENT_CATEGORY_CACHED = "/final-parent-category-cached";
  private static final String GET_PARENT_CATEGORY = "/filter/parent";
  private static final String GET_ALL_CATEGORY_TREE = BASE_PATH + "/getAllCategoryTree";
  private static final String GET_ACTIVE_CATEGORY_TREE = BASE_PATH + "/getActiveCategoryTree";
  private static final String GET_CATEGORY_TREE = BASE_PATH + "/getCategoryTree";
  private static final String GET_CATEGORY_NAMES = BASE_PATH + "/getCategoryNames";
  public static final String SUMMARY = "/summary";
  private static final String GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE =
      ProductClientCategory.BASE_PATH + "/getAllChildCategoriesFromC1CategoryCode";
  private static final String VALIDATE_IS_CN_CATEGORY = "/api/category/validateIsCnCategory";


  public ProductClientCategory(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    this.setContextPath(contextPath);
  }

  public ProductClientCategory(String username, String password, String host, Integer port,
      String storeId, String clientId, String channelId, String contextPath) {
    super(username, password, host, port, clientId, channelId, storeId, contextPath);
  }

  public GdnRestListResponse<CategoryTreeResponse> getAllCategoryTree(String requestId,
      String username, String catalogName) throws Exception {

    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("catalogName", catalogName);

    URI uri =
        this.generateURI(ProductClientCategory.GET_ALL_CATEGORY_TREE, requestId, username,
            additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryTreeResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryTreeResponse> getActiveCategoryTree(String requestId,
      String username, String catalogName, String nonInventoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("catalogName", catalogName);
    additionalParameterMap.put("nonInventoryCode", nonInventoryCode);
    URI uri =
        this.generateURI(ProductClientCategory.GET_ACTIVE_CATEGORY_TREE, requestId, username,
            additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryTreeResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryTreeResponse> getCategoryTree(String requestId,
      String username, String catalogName, List<String> categoryCodes) throws Exception {

    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("catalogName", catalogName);

    URI uri =
        this.generateURI(ProductClientCategory.GET_CATEGORY_TREE, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, categoryCodes, CategoryTreeResponse.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<CategoryTreeResponse>>() {});
  };

  public GdnBaseRestResponse deleteCategory(String requestId, String username,
      SimpleRequestHolder holder) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.DELETE, requestId, username, additionalParameterMap);
    return this
        .invokePost(uri, CategoryRequest.class, holder, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
      String requestId, String username, String categoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.HIERARCHY_FILTER_CATEGORY_CODE + categoryCode,
            requestId, username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(
      String requestId, String username, CategoryCodeRequest request) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.HIERARCHY_FILTER_CATEGORY_CODES, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, request, CategoryCodeRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<CategoryHierarchyResponse>>() {
        });
  }

  public GdnRestSingleResponse<CategoryNamesResponse> getCategoryNamesByCategoryCodes(String requestId,
      String username, CategoryMultipleIdRequest categoryCodes) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.GET_CATEGORY_NAMES, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, categoryCodes, CategoryNamesResponse.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<CategoryNamesResponse>>() {});
  }

  private URI generateURI(String path, String requestId, String username,
      Map<String, String> additionalParameterMap) throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(),
        this.getClientConfig().getPort(), location,
        this.getMandatoryParameter(this.getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  public GdnRestListResponse<CategoryResponse> getCategoryByNamePageable(
      GdnRestListRequest listRequest, String username, String name) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("name", name);
    URI uri =
        this.generateURI(ProductClientCategory.FILTER_NAME_PAGEABLE, listRequest.getRequestId(),
            username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(String requestId,
      String username, String categoryId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH + "/" + categoryId, requestId, username,
            additionalParameterMap);
    return this.invokeGetSingle(uri, CategoryDetailResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetailByCategoryCode(
      String requestId, String username, String categoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.DETAIL_BY_CATEGORY_CODE + categoryCode, requestId,
            username, additionalParameterMap);
    return this.invokeGetSingle(uri, CategoryDetailResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryResponse> getCategorySummary(GdnRestListRequest listRequest,
      String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH, listRequest.getRequestId(), username,
            additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryResponse> getChildFromParent(GdnRestListRequest listRequest,
      String username, String categoryId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri =
        this.generateURI(ProductClientCategory.CHILD_PARENT_PAGEABLE + "/" + categoryId,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryDTO> getChildFromParentByCatalogTypeAndActivatedWithChildCount(
      GdnRestListRequest listRequest, String username, String parentCategoryId,
      CatalogType catalogType, Boolean activated) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("categoryId", parentCategoryId);
    additionalParameterMap.put("catalogType", catalogType.toString());
    additionalParameterMap.put("activated", String.valueOf(activated));
    URI uri =
        this.generateURI(ProductClientCategory.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE_COUNT,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryDTO.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryResponse> getChildFromParentByCatalogTypeAndActivated(
      GdnRestListRequest listRequest, String username, String parentCategoryId,
      CatalogType catalogType, Boolean activated) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("categoryId", parentCategoryId);
    additionalParameterMap.put("catalogType", catalogType.toString());
    additionalParameterMap.put("activated", String.valueOf(activated));
    URI uri =
        this.generateURI(ProductClientCategory.CHILD_PARENT_WITH_CATALOG_TYPE_PAGEABLE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryResponse> filterSummaryByCategoryNameAndCatalogType(
      GdnRestListRequest listRequest, String username, String categoryName, CatalogType catalogType)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    additionalParameterMap.put("categoryName", categoryName);
    additionalParameterMap.put("catalogType", catalogType.name());
    URI uri =
        this.generateURI(ProductClientCategory.FILTER_NAME_CATALOG_TYPE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryDTO> getChildFromParentByCatalogIdWithChildCount(
      GdnRestListRequest listRequest, String username, String parentCategoryId, String catalogId)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    if ((parentCategoryId != null) && (parentCategoryId.trim().length() > 0)) {
      additionalParameterMap.put("categoryId", "" + parentCategoryId);
    }
    additionalParameterMap.put("catalogId", "" + catalogId);
    URI uri =
        this.generateURI(ProductClientCategory.CHILD_PARENT_WITH_CATALOG_ID_PAGEABLE_COUNT,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryDTO.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryDTO> getChildFromParentWithChildCount(
      GdnRestListRequest listRequest, String username, String categoryId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + listRequest.getPage());
    additionalParameterMap.put("size", "" + listRequest.getSize());
    URI uri =
        this.generateURI(ProductClientCategory.CHILD_PARENT_PAGEABLE_COUNT + "/" + categoryId,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryDTO.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryDTO> getCategoriesByCategoryCodes(
      GdnRestListRequest listRequest, String username, Boolean activated, List<String> categoryCodes)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", "" + Long.toString(listRequest.getPage()));
    additionalParameterMap.put("size", "" + Long.toString(listRequest.getSize()));
    additionalParameterMap.put("activated", String.valueOf(activated));
    CategoryMultipleIdRequest bulkRequest = new CategoryMultipleIdRequest();
    bulkRequest.setCategoryCode(categoryCodes);
    URI uri =
        this.generateURI(ProductClientCategory.FILTER_BULK_CATEGORY_CODE,
            listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokePostType(uri, bulkRequest, CategoryMultipleIdRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestListResponse<CategoryDTO>>() {});
  }

  public GdnRestListResponse<CategoryParentResponse> getCategoriesAndFinalCategoryMapping(
      String requestId, String username) throws Exception {
    final Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH
            + ProductClientCategory.GET_ALL_CATEGORIES_PARENT_MAPPING, requestId, username,
            additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryParentResponse.class);
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  public GdnBaseRestResponse saveCategory(String requestId, String username,
      CategoryRequest categoryRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.CREATE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, CategoryRequest.class, categoryRequest,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnBaseRestResponse updateCategory(String requestId, String username,
      CategoryRequest categoryRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.UPDATE, requestId, username, additionalParameterMap);
    return this.invokePost(uri, CategoryRequest.class, categoryRequest,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategory(String requestId,
      String username, String categoryId) throws Exception {
    final Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("categoryId", categoryId);
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH
            + ProductClientCategory.GET_FINAL_PARENT_CATEGORY, requestId, username,
            additionalParameterMap);
    return this.invokeGetSingle(uri, SingleObjectResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);

  }

  public GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategoryCached(String requestId,
      String username, String categoryId) throws Exception {
    final Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("categoryId", categoryId);
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH
            + ProductClientCategory.GET_FINAL_PARENT_CATEGORY_CACHED, requestId, username,
            additionalParameterMap);

    return this.invokeGetSingle(uri, SingleObjectResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<SingleObjectResponse> getParentCategories(String requestId)
      throws Exception {
    final Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH
            + ProductClientCategory.GET_PARENT_CATEGORY, requestId, null, additionalParameterMap);
    return this.invokeGetSingle(uri, SingleObjectResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  /**
   * Get simplified categories by catalog id and displayable flag in a simplified response
   * @param listRequest paging request
   * @param username username for auditing
   * @param catalogId catalog id (Mandatory)
   * @param display displayable flag (Optional, pass null to ignore this filter)
   * @return GdnRestListResponse<CategorySummaryResponse> paging response
   * @throws Exception ApplicationRuntimeException
   */
  public GdnRestListResponse<CategorySummaryResponse> getCategorySummarySimplified(
      GdnRestListRequest listRequest, String username, String catalogId, Boolean display) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("page", String.valueOf(listRequest.getPage()));
    additionalParameterMap.put("size", String.valueOf(listRequest.getSize()));
    additionalParameterMap.put("display", String.valueOf(display));
    URI uri =
        this.generateURI(ProductClientCategory.BASE_PATH + ProductClientCategory.SUMMARY + "/"
            + catalogId, listRequest.getRequestId(), username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategorySummaryResponse.class,
        ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(String requestId,
      String username, CategoryCodeRequest request)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.GET_ALL_CHILDS_FROM_C1_CATERGORY_CODE, requestId, username,
            additionalParameterMap);
    return this.invokePostType(uri, request, CategoryCodeRequest.class,
        ProductClient.APPLICATION_JSON_VALUE,
        new TypeReference<GdnRestSingleResponse<CategoryCodeResponse>>() {});
  }
  
  public GdnRestSingleResponse<CategoryResponse> validateIsCnCategory(String requestId, String username,
      String parentCategoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductClientCategory.VALIDATE_IS_CN_CATEGORY, requestId, username, additionalParameterMap);
    
    return this.invokeGetSingle(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE);
  }

  public GdnRestListResponse<CategoryResponse> getMasterParentCategoriesByProductCode(
      String requestId, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("productCode", productCode);
    URI uri =
        this.generateURI(ProductCategoryApiPath.GET_MASTER_PARENT_CATEGORY_RESPONSE_BY_PRODUCT_CODE,
            requestId, username, additionalParameterMap);
    return this.invokeGetSummary(uri, CategoryResponse.class, ProductClient.APPLICATION_JSON_VALUE,
        Collections.EMPTY_MAP);
  }
}
