package com.gdn.partners.pcu.master.service.impl;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.master.client.model.CategoryDetailAndShippingResponse;
import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.margin.webmodel.MarginOrderResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.feign.MarginFeign;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.client.feign.ProductCategorySuggestionFeign;
import com.gdn.partners.pcu.master.client.feign.XBPFeign;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategoryCodesListRequest;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.model.request.CategoryCreateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryInfoUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryMappingsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
import com.gdn.partners.pcu.master.service.CategoryService;
import com.gdn.partners.pcu.master.service.impl.exception.ActivationValidationException;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.master.web.model.request.CategoryRestrictedKeywordsWebRequest;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ProfitMarginWebRequest;
import com.gdn.partners.pcu.master.web.model.response.BaseMarginHierarchyWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CatalogTreeWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryInfoWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryMappingResponse;
import com.gdn.partners.pcu.master.web.model.response.CreateCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.DocumentWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MarginCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscDetailsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscSummaryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.ProfitMarginWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordAction;
import com.gdn.partners.pcu.master.web.model.response.SuggestedCategoriesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleMappingWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.productcategorybase.MandatoryParameterConstants;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.CreateCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryServiceImpl implements CategoryService {
  private static final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
  public static final String SPECIAL_MARGIN = "SPECIAL-MARGIN";
  public static final String BASE = "BASE";
  public static final String BASE_ADDON = "BASE-ADDON";
  public static final String BASE_MARGIN = "BASE-MARGIN";
  public static final String ORDER_ITEM_ID = "-";
  public static final String INTERNAL = "INTERNAL";
  private static Integer PAGE = 0;
  private static Integer SIZE = Integer.MAX_VALUE;


  @Autowired
  private MarginFeign marginFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private XBPFeign xbpFeign;

  @Autowired
  private ProductCategorySuggestionFeign productCategorySuggestionFeign;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Value("${default.sales.catalog.code}")
  private String defaultSalesCatalogCode;

  @Value("${category.suggestion.max.limit}")
  private Integer categorySuggestionMaxLimit;

  @Value("${category.suggestion.threshold}")
  private Double categorySuggestionThreshold;

  @Value("${category.sequence.page.size}")
  private Integer categorySequencePageSize;

  @Value("${category.restrictedKeywordTypeList.value}")
  private String restrictedKeywordTypeList ;

  @Value("${category.restrictedKeywordActionLists.value}")
  private String restrictedKeywordActionLists ;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${margin.new.changes.enabled}")
  private boolean marginNewChangesEnabled ;

  @Value("${set.default.orderType.margin}")
  private boolean setDefaultOrderTypeForMargin;

  @Value("${default.orderType.margin}")
  private String defaultOrderTypeForMargin;

  @Override
  public void activate(String requestId, CategoryStatusChangeServiceRequest request) {
    request.setActivated(true);
    if (Constants.MASTER_CATALOG.equals(request.getCatalogType())) {
      validateActivation(requestId, request);
    }
      update(request);
  }

  /**
   * Validate category activation
   * Conditions :
   * Category's margin has been set
   * Category has been handled by internal user
   * Category has product defaultDescription
   * Category has shipping code
   *
   * @param requestId
   * @param request
   * @throws Exception
   */
  private void validateActivation(String requestId, CategoryStatusChangeServiceRequest request) {
    boolean isCategoryMarginSet;
    if (Boolean.TRUE.equals(request.getB2bExclusive())) {
      isCategoryMarginSet = true;
    } else {
      isCategoryMarginSet = isCategoryMarginSet(request.getCategoryCode());
    }
    boolean isCategoryDescriptionSet = isCategoryDescriptionSet(request.getId());
    boolean isCategoryShippingCodeSet = isCategoryShippingCodeSet(request.getCategoryCode());
    throwActivationException(request.getName(), isCategoryMarginSet, isCategoryDescriptionSet, isCategoryShippingCodeSet);
  }

  /**
   * Throws category activation exception based on params accordingly
   *
   * @param categoryName
   * @param isCategoryMarginSet
   * @param isCategoryDescriptionSet
   * @param isCategoryShippingCodeSet
   */
  private void throwActivationException(String categoryName, boolean isCategoryMarginSet,
      boolean isCategoryDescriptionSet, boolean isCategoryShippingCodeSet) {
    StringBuilder reason = new StringBuilder();
    boolean isValidated = true;
    reason.append(ErrorMessages.ACTIVATION_FAILURE_ERR_MESSAGE + categoryName);

    if (!isCategoryMarginSet) {
      reason.append(ErrorMessages.MARGIN_NOT_SET_ERR_MESSAGE).append(",");
      isValidated = false;
    }
    if (!isCategoryDescriptionSet) {
      reason.append(ErrorMessages.DESCRIPTION_EMPTY_ERR_MESSAGE).append(",");
      isValidated = false;
    }
    if (!isCategoryShippingCodeSet) {
      reason.append(ErrorMessages.CATEGORY_SHIPPING_CODE_NOT_SET_ERR_MESSAGE).append(",");
      isValidated = false;
    }
    if (!isValidated) {
      throw new ActivationValidationException(reason.toString().replaceAll(",$", ""));
    }
  }

  /**
   * Check is margin with certain category is already set in margin service
   *
   * @param categoryCode
   * @return boolean
   * @throws Exception
   */
  private boolean isCategoryMarginSet(String categoryCode) {
    if(!marginNewChangesEnabled) {
      GdnRestSingleResponse<MarginCategoryResponse> margin =
          marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(categoryCode, simpleDateFormat.format(new Date()));
      return ResponseHelper.marginResponseValidation(margin);
    }
    else {
      try {
        getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(INTERNAL,categoryCode);
        return true;
      } catch (Exception e) {
        log.error("error while invoking margin feign ", e);
        return false;
      }
    }
  }

  /**
   * Check whether categoryCode has shipping code
   * Check in master data
   *
   * @param categoryCode
   * @return boolean
   * @throws Exception
   */
  private boolean isCategoryShippingCodeSet(String categoryCode) {
    GdnRestListResponse<CategoryShippingResponse> categoryShippingResponse =
        pcbFeign.getCategoryShippingByCategoryCode(categoryCode);
    return ResponseHelper.categoryShippingValidation(categoryShippingResponse);
  }

  /**
   * Check whether Category's product defaultDescription is already set
   * Check in master data
   *
   * @param categoryId
   * @return boolean
   * @throws Exception
   */
  private boolean isCategoryDescriptionSet(String categoryId) {
    GdnRestSingleResponse<CategoryDetailResponse> category = pcbFeign.getCategoryDetail(categoryId);
    ResponseHelper.validateResponse(category);
    if (ArrayUtils.isEmpty(category.getValue().getDefaultDescription())) {
      return false;
    }
    return true;
  }

  @Override
  public void update(CategoryStatusChangeServiceRequest request) {
    GdnBaseRestResponse updateCategory =
        pcbFeign.updateCategoryInfo(RequestHelper.toCategoryInfoUpdateRequestForStatus(request), true);
    ResponseHelper.validateResponse(updateCategory);
  }

  @Override
  public void deactivate(String requestId, CategoryStatusChangeServiceRequest request) throws Exception {
    request.setActivated(false);
    update(request);
  }

  @Override
  public GdnBaseRestResponse updateCategoryInfo(CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest) {
    GdnBaseRestResponse response = pcbFeign
        .updateCategoryInfo(RequestHelper.toCategoryInfoUpdateRequest(categoryInfoUpdateServiceRequest), Boolean.FALSE);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse updateCategoryMappings(
      CategoryMappingsUpdateServiceRequest categoryMappingsUpdateServiceRequest) {
    GdnBaseRestResponse response = pcbFeign.updateCategoryMappings(
        RequestHelper.toCategoryMappingsUpdateRequest(categoryMappingsUpdateServiceRequest));
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public List<CategoryMappingResponse> getSalesCategoryMappingByCategoryCodes(List<String> categoryCodes,
      String catalogId) {
    List<CategoryDTO> response = getChildCategories(catalogId, null);
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest(new ArrayList<>());
    List<CategoryMappingResponse> categoryMappingResponses =
        response.stream().map(ResponseHelper::toCategoryMappingResponse)
            .sorted(Comparator.comparing(CategoryMappingResponse::getName, String.CASE_INSENSITIVE_ORDER))
            .collect(Collectors.toList());
    Map<String, CategoryMappingResponse> categoryMappingResponseMap = categoryMappingResponses.stream().
        collect(Collectors.toMap(CategoryMappingResponse::getCategoryCode, Function.identity(),
            (categoryCode1, categoryCode2) -> categoryCode1, LinkedHashMap::new));
    for (String categoryCode : categoryCodes) {
      if (Objects.nonNull(categoryMappingResponseMap.get(categoryCode))) {
        categoryMappingResponseMap.get(categoryCode).setMapped(Boolean.TRUE);
      } else {
        categoryCodeRequest.getCategoryCodes().add(categoryCode);
      }
    }
    if (CollectionUtils.isNotEmpty(categoryCodeRequest.getCategoryCodes())) {
      getHierarchyByCategoryCodes(categoryCodeRequest, categoryMappingResponseMap);
    }
    return new ArrayList<>(categoryMappingResponseMap.values());
  }

  /**
   * Get mapping by category codes
   *
   * @param categoryCodeRequest
   * @param categoryMappingResponseMap
   */
  private void getHierarchyByCategoryCodes(CategoryCodeRequest categoryCodeRequest,
      Map<String, CategoryMappingResponse> categoryMappingResponseMap) {
    GdnRestListResponse<CategoryHierarchyResponse> categoryHierarchyResponse =
        pcbFeign.getHierarchyByCategoryCodes(categoryCodeRequest);
    ResponseHelper.validateResponse(categoryHierarchyResponse);
    List<CategoryHierarchyResponse> categoryHierarchyResponses = categoryHierarchyResponse.getContent();
    for (CategoryHierarchyResponse hierarchyResponse : categoryHierarchyResponses) {
      Map<String, CategoryMappingResponse> responseMap = categoryMappingResponseMap;
      List<CategoryResponse> hierarchyResponses = Lists.reverse(hierarchyResponse.getCategoryHierarchy());
      int count = hierarchyResponses.size() - 1;
      for (int i = 0; i < count; i++) {
        responseMap = setChildren(responseMap, hierarchyResponses.get(i));
      }
      responseMap.get(hierarchyResponses.get(count).getCategoryCode()).setMapped(Boolean.TRUE);
    }
  }

  /**
   * Set children based on hierarchy
   *
   * @param responseMap
   * @param categoryResponse
   */
  private Map<String, CategoryMappingResponse> setChildren(Map<String, CategoryMappingResponse> responseMap,
      CategoryResponse categoryResponse) {
    List<CategoryDTO> childCategories = getChildCategories(categoryResponse.
        getCatalog().getId(), categoryResponse.getId());
    List<CategoryMappingResponse> responseList = childCategories.stream().map(ResponseHelper::toCategoryMappingResponse)
        .sorted(Comparator.comparing(CategoryMappingResponse::getName, String.CASE_INSENSITIVE_ORDER))
        .collect(Collectors.toList());
    responseMap.get(categoryResponse.getCategoryCode()).setChildren(responseList);
    return responseList.stream().
        collect(Collectors.toMap(CategoryMappingResponse::getCategoryCode, Function.identity()));
  }

  /**
   * Get sub-categories by parent category id
   *
   * @param catalogId
   * @param categoryId
   * @return
   */
  private List<CategoryDTO> getChildCategories(String catalogId, String categoryId) {
    GdnRestListResponse<CategoryDTO> categoryDTOGdnRestListResponse =
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(catalogId, categoryId, PAGE, SIZE, Constants.ACTIVE,
            Constants.ALL, false, false);
    ResponseHelper.validateResponse(categoryDTOGdnRestListResponse);
    return categoryDTOGdnRestListResponse.getContent();
  }

  @Override
  public SingleBaseResponse<CategoryInfoWebResponse> getCategoryInfoWithShippingDetail(String categoryId,
      String requestId, boolean fetchHideForSellerAttributes) {
    GdnRestSingleResponse<CategoryDetailAndShippingResponse> response =
        pcbFeign.getCategoryInfoByCategoryId(fetchHideForSellerAttributes,categoryId);
    ResponseHelper.validateResponse(response);
    return new SingleBaseResponse<>(response.getErrorMessage(), response.getErrorCode(), true, requestId,
        ResponseHelper.toCategoryInfoWebResponse(response.getValue()));
  }

  @Override
  public CreateCategoryWebResponse createCategory(
      CategoryCreateServiceRequest categoryCreateServiceRequest) {
    GdnRestSingleResponse<CreateCategoryResponse> response = pcbFeign
        .createCategory(RequestHelper.toCategoryDetailRequest(categoryCreateServiceRequest));
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toCreateCategoryWebResponse(response.getValue());
  }

  @Override
  public MarginCategoryWebResponse getMarginByCategoryCode(String categoryCode) {
    if(!marginNewChangesEnabled) {
      GdnRestSingleResponse<MarginCategoryResponse> response =
          marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(categoryCode, simpleDateFormat.format(new Date()));
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toMarginCategoryWebResponse(response.getValue());
    }
    else {
      return getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(INTERNAL,categoryCode);
    }
  }

  @Override
  public List<CategoryResponse> findCategorySummaryByParentId(String parentId) {
    GdnRestListResponse<CategoryResponse> response =
        pcbFeign.findCategorySummaryByParentId(categorySequencePageSize.intValue(), parentId);
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public MarginCategoryWebResponse getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(
      String businessPartnerCode, String catergoryCode) {
    MarginCategoryWebResponse marginCategoryWebResponse = new MarginCategoryWebResponse();
    if (!marginNewChangesEnabled) {
      GdnRestSingleResponse<MarginOrderResponse> response =
          marginFeign.filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(businessPartnerCode,
              catergoryCode, simpleDateFormat.format(new Date()), "GDN-SKU");
      ResponseHelper.validateResponse(response);
      marginCategoryWebResponse = ResponseHelper.getMarginBusinessPartnerWebResponse(response.getValue());
      marginCategoryWebResponse.setCategoryCode(catergoryCode);
    } else {
      marginCategoryWebResponse = profitMarginByBusinessPartnerCodeAndCategoryCode(businessPartnerCode,
          catergoryCode);
    }
    return marginCategoryWebResponse;
  }

  private MarginCategoryWebResponse profitMarginByBusinessPartnerCodeAndCategoryCode(String businessPartnerCode,
      String categoryCode) {
    boolean officialStore = false;
    if (!INTERNAL.equalsIgnoreCase(businessPartnerCode)) {
      GdnRestSingleResponse<ProfileResponse> profileResponse =
          xbpFeign.filterByBusinessPartnerCode(businessPartnerCode);
      ResponseHelper.validateResponse(profileResponse);
      officialStore = profileResponse.getValue().isOfficial();
    }
    FilterMarginsByOrderItemsRequest marginsRequest = ResponseHelper.getFilterMarginsByOrderItemsRequest
        (businessPartnerCode, categoryCode, officialStore, setDefaultOrderTypeForMargin, defaultOrderTypeForMargin);
    Optional<OrderItemMarginsResponse> orderItemMarginsResponse =
        getOrderItemMarginsResponse(marginsRequest);
    return ResponseHelper.getMarginBusinessPartnerWebResponse(orderItemMarginsResponse,
        categoryCode);
  }

  private Optional<OrderItemMarginsResponse> getOrderItemMarginsResponse(
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequest) {
    ListBaseResponse<OrderItemMarginsResponse> orderItemMarginsResponseListBaseResponse =
        marginFeign.filterMargin(clientParameterHelper.getStoreId(), clientParameterHelper.getChannelId(),
            clientParameterHelper.getClientId(), clientParameterHelper.getRequestId(),
            MandatoryParameterConstants.USERNAME, filterMarginsByOrderItemsRequest);
    ResponseHelper.validateResponse(orderItemMarginsResponseListBaseResponse);
    return orderItemMarginsResponseListBaseResponse.getContent().stream().filter(
            orderItemMarginsResponse1 -> Optional.ofNullable(orderItemMarginsResponse1.getMargins())
                .orElse(new ArrayList<>()).stream().filter(Objects::nonNull).anyMatch(margin ->
                    (SPECIAL_MARGIN.equals(margin.getMarginType()) && (BASE.equals(margin.getReplacementType())
                        || BASE_ADDON.equals(margin.getReplacementType())))
                        || BASE_MARGIN.equals(margin.getMarginType())))
        .findFirst();
  }

  @Override
  public GdnBaseRestResponse updateCategoryRestrictedKeywordMappings(String categoryCode,
      CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList) {
    GdnBaseRestResponse baseRestResponse =
        pcbFeign.updateCategoriesWithRestrictedKeywords(categoryCode, categoryKeywordUpdateRequestList);
    ResponseHelper.validateResponse(baseRestResponse);
    return baseRestResponse;
  }

  @Override
  public Page<RestrictedKeywordsWebResponse> findRestrictedKeywords(
      CategoryRestrictedKeywordsWebRequest categoryRestrictedKeywordsWebRequest, Pageable pageable) {
    GdnRestListResponse<RestrictedKeywordsResponse> restrictedKeywordsResponseList = pcbFeign
        .getCategoryRestrictedKeywords(pageable.getPageNumber(), pageable.getPageSize(),
            RequestHelper.toCategoryRestrictedKeywordsRequest(categoryRestrictedKeywordsWebRequest));
    ResponseHelper.validateResponse(restrictedKeywordsResponseList);
    List<RestrictedKeywordsWebResponse> restrictedKeywordsWebResponseList =
        restrictedKeywordsResponseList.getContent().stream().map(ResponseHelper::getRestrictedKeywordsWebResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(restrictedKeywordsWebResponseList, pageable,
        restrictedKeywordsResponseList.getPageMetaData().getTotalRecords());
  }

  @Override
  public WholesaleMappingWebResponse findWholesaleConfig(String categoryId, String categoryCode) {
    GdnRestSingleResponse<WholesaleMappingResponse> wholesaleMappingResponseGdnRestSingleResponse =
        pcbFeign.getCategoryWholesaleConfig(categoryId, categoryCode);
    ResponseHelper.validateResponse(wholesaleMappingResponseGdnRestSingleResponse);
    WholesaleMappingWebResponse wholesaleMappingWebResponse =
        ResponseHelper.getWholesaleMappingWebResponse(wholesaleMappingResponseGdnRestSingleResponse.getValue());
    return wholesaleMappingWebResponse;
  }

  @Override
  public GdnBaseRestResponse updateCategoryWholesaleConfigMapping(String categoryId,
      WholesaleMappingRequest wholesaleMappingRequest) {
    GdnBaseRestResponse wholesaleConfigGdnBaseRestResponse =
        pcbFeign.updateCategoriesWithWholesaleConfig(categoryId, wholesaleMappingRequest);
    ResponseHelper.validateResponse(wholesaleConfigGdnBaseRestResponse);
    return wholesaleConfigGdnBaseRestResponse;
  }

  @Override
  public List<CategoryTreeResponse> getCategoryListForGenericTemplate(boolean genericTemplateEligible) {
    GdnRestListResponse<CategoryTreeResponse> response = pcbFeign.getCategoryTree(genericTemplateEligible);
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public List<CatalogTreeWebResponse> getCategoryTreeForCategoryCodes(CategoryCodeRequest request) {
    GdnRestListResponse<CategoryHierarchyResponse> response = pcbFeign.getHierarchyByCategoryCodes(request);
    ResponseHelper.validateResponse(response);
    List<List<CategoryResponse>> listOfCategoryResponses = new ArrayList<>();
    for (CategoryHierarchyResponse categoryHierarchyResponse : response.getContent()) {
      listOfCategoryResponses.add(categoryHierarchyResponse.getCategoryHierarchy());
    }
    return ResponseHelper.getCatalogTreeWebResponse(listOfCategoryResponses);
  }

  @Override
  public DocumentWebResponse getDocumentList() {
    return getDocumentWebResponse(Constants.DOCUMENT_TYPE);
  }

  @Override
  public DocumentWebResponse getSystemParamValue(String variable) {
    return getDocumentWebResponse(variable);
  }

  private DocumentWebResponse getDocumentWebResponse(String variable) {
    GdnRestSingleResponse<SystemParameterResponse> response = pcbFeign.getDocumentList(variable);
    List<String> documentList = new ArrayList<>();
    ResponseHelper.validateResponse(response);
    if (StringUtils.isNotEmpty(response.getValue().getValue())) {
      documentList = Arrays.stream(response.getValue().getValue().split(",")).collect(Collectors.toList());
    }
    return DocumentWebResponse.builder().documentList(documentList).build();
  }

  @Override
  public List<SuggestedCategoriesWebResponse> getCategorySuggestionByProductName(String productName) {
    ProductCategorySuggestionResponse productCategoryPredictionResponse = productCategorySuggestionFeign
        .predictProductCategoriesByProductName(productName, categorySuggestionMaxLimit.intValue(),
            categorySuggestionThreshold.doubleValue());
    return ResponseHelper.toPredictedCategoriesWebResponse(productCategoryPredictionResponse);
  }

  @Override
  public BaseResponse validateCategory(String requestId, String categoryId) {
    GdnBaseRestResponse response = pcbFeign.validateCategory(categoryId);
    ResponseHelper.validateResponse(response);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), true, requestId);
  }

  @Override
  public String addOriginalSalesCategory(String storeId,
      OriginalSalesCategoryWebRequest originalSalesCategoryWebRequest) {
    log.info("Adding new original sales category : {}", originalSalesCategoryWebRequest);
    OriginalSalesCategoryRequest originalSalesCategoryRequest = new OriginalSalesCategoryRequest();
    BeanUtils.copyProperties(originalSalesCategoryWebRequest, originalSalesCategoryRequest);
    originalSalesCategoryRequest.setStoreId(storeId);
    GdnRestSimpleResponse<String> response = pcbFeign.createOriginalSalesCategory(originalSalesCategoryRequest);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public List<OscSummaryWebResponse> getOscListing(String oscCode, String keyword, Boolean activated) {
    GdnRestListResponse<OscSummaryResponse> response = pcbFeign.filterOscSummaryResponse(oscCode, keyword, activated);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toOscSummaryWebResponse(response.getContent());
  }

  @Override
  public GdnBaseRestResponse updateOriginalSalesCategory(OscInfoUpdateDTO oscInfoUpdateDTO) {
    GdnBaseRestResponse response = pcbFeign.updateOriginalSalesCategory(oscInfoUpdateDTO);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public OscDetailsWebResponse getOriginalSalesCategory(String id){
    GdnRestSingleResponse<OriginalSalesCategoryResponse> response = pcbFeign.getOSCById(id);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toOscDetailsWebResponse(response.getValue());
  }

  @Override
  public Map<String, Object> getValueFromProperties() throws IOException {
    Map<String, Object> restrictedKeywordMap = new HashMap<>();
    List<RestrictedKeywordAction> restrictedKeywordActionList =
        objectMapper.readValue(restrictedKeywordActionLists, new TypeReference<List<RestrictedKeywordAction>>() {
        });
    List<RestrictedKeywordAction> keywordTypeList =
        objectMapper.readValue(restrictedKeywordTypeList, new TypeReference<List<RestrictedKeywordAction>>() {
        });
    restrictedKeywordMap.put(Constants.RESTRICTED_KEYWORD_TYPE_LIST, keywordTypeList);
    restrictedKeywordMap.put(Constants.RESTRICTED_KEYWORD_ACTION_LIST, restrictedKeywordActionList);
    return restrictedKeywordMap;
  }

  @Override
  public List<BaseMarginHierarchyWebResponse> getCategoryMarginHierarchy(String categoryCode) {
    GdnRestListResponse<CategoryResponse> categoryResponses =
        pcbFeign.filterCategoryHierarchyByCategoryCode(categoryCode);
    ResponseHelper.validateResponse(categoryResponses);
    List<String> categoryCodes = categoryResponses.getContent().stream()
        .map(CategoryResponse::getCategoryCode).collect(Collectors.toList());
    ListBaseResponse<BaseMarginResponse> baseMarginResponse =
        marginFeign.filterActiveBasicMargin(new CategoryCodesListRequest(categoryCodes));
    ResponseHelper.validateResponse(baseMarginResponse);
    return ResponseHelper.toBaseMarginHierarchyWebResponse(categoryResponses.getContent(),
        baseMarginResponse.getContent());
  }

  @Override
  public ProfitMarginWebResponse getProfitMargin(ProfitMarginWebRequest profitMarginWebRequest) {
    MarginCategoryWebResponse marginCategoryWebResponse =
        profitMarginByBusinessPartnerCodeAndCategoryCode(profitMarginWebRequest.getBusinessPartnerCode(),
            profitMarginWebRequest.getCategoryCode());
    return ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest,
        marginCategoryWebResponse);
  }

}
