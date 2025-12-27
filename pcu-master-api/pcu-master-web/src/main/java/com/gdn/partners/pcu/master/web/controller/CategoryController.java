package com.gdn.partners.pcu.master.web.controller;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.CategoryApiPath;
import com.gdn.partners.pcu.master.service.CategoryService;
import com.gdn.partners.pcu.master.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.master.web.model.request.CategoryCreateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryInfoUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryKeywordUpdateWebRequestList;
import com.gdn.partners.pcu.master.web.model.request.CategoryMappingsUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryRestrictedKeywordsWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryStatusChangeWebRequest;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ProfitMarginWebRequest;
import com.gdn.partners.pcu.master.web.model.request.WholesaleMappingWebRequest;
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
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.SuggestedCategoriesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleMappingWebResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;


@Slf4j
@Tag(name = "Category API")
@RestController
@RequestMapping(value = CategoryApiPath.BASE_PATH)
public class CategoryController {

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Value("${html.sanitization.enabled}")
  private boolean htmlSanitizationEnabled;

  @Operation(summary = "Activate Category")
  @PutMapping(value = CategoryApiPath.ACTIVATE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateCategory(@PathVariable String categoryCode,
      @RequestBody CategoryStatusChangeWebRequest request) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Method : Activate for category code : {}", categoryCode);
    categoryService.activate(requestId,
        ConverterUtil.toCategoryStatusChangeServiceRequest(request, categoryCode, clientParameterHelper));
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Deactivate Category")
  @PutMapping(value = CategoryApiPath.DEACTIVATE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse deactivateCategory(@PathVariable String categoryCode,
      @RequestBody CategoryStatusChangeWebRequest request) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Method : Deactivate for category code : {}", categoryCode);
    categoryService.deactivate(requestId,
        ConverterUtil.toCategoryStatusChangeServiceRequest(request, categoryCode, clientParameterHelper));
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Update Category Info")
  @PutMapping(value = CategoryApiPath.UPDATE_CATEGORY_INFO, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public BaseResponse updateCategoryInfo(@PathVariable String categoryCode,
      @RequestBody CategoryInfoUpdateWebRequest categoryInfoUpdateWebRequest) {
    GdnBaseRestResponse response = categoryService.updateCategoryInfo(
        ConverterUtil.toCategoryInfoUpdateServiceRequest(categoryCode, categoryInfoUpdateWebRequest,
            clientParameterHelper, htmlSanitizationEnabled));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Update Category Mappings")
  @PutMapping(value = CategoryApiPath.UPDATE_CATEGORY_MAPPINGS, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public BaseResponse updateCategoryMappings(@PathVariable String categoryId,
      @RequestBody CategoryMappingsUpdateWebRequest categoryMappingsUpdateWebRequest) {
    GdnBaseRestResponse response = categoryService.updateCategoryMappings(
        ConverterUtil.toCategoryMappingsUpdateServiceRequest(categoryId, categoryMappingsUpdateWebRequest,
            clientParameterHelper));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get category Details")
  @GetMapping(value = CategoryApiPath.INFO, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public SingleBaseResponse<CategoryInfoWebResponse> getCategoryInfoWithShippingDetails(
      @RequestParam(required = false) boolean fetchHideForSellerAttributes,
      @PathVariable("categoryId") String categoryId) {
    String requestId = this.clientParameterHelper.getRequestId();
    if (StringUtils.isNotBlank(categoryId)) {
      return categoryService.getCategoryInfoWithShippingDetail(categoryId, requestId,
          fetchHideForSellerAttributes);
    } else {
      return new SingleBaseResponse<>(ErrorMessage.CATEGORY_ID_MUST_NOT_BE_BLANK.getMessage(),
          ErrorCategory.INVALID_FORMAT.getMessage(), false, requestId, null);
    }
  }

  @Operation(summary = "Create Category")
  @PostMapping(value = CategoryApiPath.CREATE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public SingleBaseResponse<CreateCategoryWebResponse> createCategory(
      @RequestBody CategoryCreateWebRequest categoryCreateWebRequest) {
    CreateCategoryWebResponse response = categoryService.createCategory(
        ConverterUtil.toCategoryCreateServiceRequest(categoryCreateWebRequest,
            clientParameterHelper, htmlSanitizationEnabled));
    return new SingleBaseResponse(null, null, true, clientParameterHelper.getRequestId(), response);
  }

  @Operation(summary = "To get master category mappings")
  @PostMapping(value = CategoryApiPath.GET_CATEGORY_MAPPINGS_BY_CATEGORY_CODES, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<  CategoryMappingResponse> getCategoryMappingsByCategoryCodes(@RequestParam String catalogId,
      @RequestBody List<String> categoryCodes) {
    List<CategoryMappingResponse> categoryMappingResponse = categoryService.getSalesCategoryMappingByCategoryCodes(categoryCodes, catalogId);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        categoryMappingResponse, new Metadata());
  }

  @Operation(summary = "To get margin by categoryCode")
  @GetMapping(value = CategoryApiPath.GET_MARGIN_BY_CATEGORY_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MarginCategoryWebResponse> getMarginByCategoryCode(@PathVariable String categoryCode) {
    MarginCategoryWebResponse response = categoryService.getMarginByCategoryCode(categoryCode);
    return new SingleBaseResponse<MarginCategoryWebResponse>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Get category summary with used sequence by category Id")
  @GetMapping(value = CategoryApiPath.GET_USED_SEQUENCE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<Integer> getUsedSequence(
      @PathVariable("parentId") String parentId) {
    String requestId = this.clientParameterHelper.getRequestId();
    List<CategoryResponse> summaryByParentId = categoryService.findCategorySummaryByParentId(parentId);
    List<Integer> usedSequence =
        summaryByParentId.stream().map(CategoryResponse::getSequence).collect(Collectors.toList());
    long totalItems = usedSequence.size();
    return new ListBaseResponse<>(null, null, true, requestId,
        usedSequence, new Metadata(0, 0, totalItems));
  }

  @Operation(summary = "To get margin by categoryCode and business partner code")
  @GetMapping(value = CategoryApiPath.GET_MARGIN_BY_BUSINESS_PARTNER_CODE_AND_CATEGORY_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MarginCategoryWebResponse> getMarginByBusinessPartnerCodeAndCategoryCode(
      @PathVariable String businessPartnerCode, @PathVariable String categoryCode) {
    MarginCategoryWebResponse response =
        categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(businessPartnerCode, categoryCode);
    return new SingleBaseResponse<MarginCategoryWebResponse>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Update Category Restricted Keywords Mappings")
  @PutMapping(value = CategoryApiPath.UPDATE_CATEGORY_RESTRICTED_KEYWORD_MAPPINGS, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateCategoryRestrictedKeywordMapping(@PathVariable String categoryCode,
      @RequestBody CategoryKeywordUpdateWebRequestList categoryKeywordUpdateWebRequestList) {
    log.info("updating the category with restricted keyword mappings with request : {}",
        categoryKeywordUpdateWebRequestList);
    GdnBaseRestResponse response = categoryService.updateCategoryRestrictedKeywordMappings(categoryCode,
        ConverterUtil.toCategoryKeywordRequestList(categoryKeywordUpdateWebRequestList, clientParameterHelper));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Fetch category restricted keywords by categoryCode or keyword")
  @PostMapping(value = CategoryApiPath.GET_CATEGORY_RESTRICTED_KEYWORD, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RestrictedKeywordsWebResponse> getCategoryRestrictedKeywords(
      @RequestBody CategoryRestrictedKeywordsWebRequest request, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    log.info("Method : Fetch restricted keyword for request: {}", request);
    Pageable pageable = PageRequest.of(page, size);
    Page<RestrictedKeywordsWebResponse> restrictedKeywordsWebResponseList =
        categoryService.findRestrictedKeywords(request, pageable);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        restrictedKeywordsWebResponseList.getContent(),
        new Metadata(page, size, restrictedKeywordsWebResponseList.getTotalElements()));
  }

  @Operation(summary = "Get category wholesale configuration by categoryCode or categoryId")
  @GetMapping(value = CategoryApiPath.GET_CATEGORY_WHOLESALE_CONFIG, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<WholesaleMappingWebResponse> getCategoryWholesaleConfig(
      @RequestParam(required = false) String categoryId, @RequestParam(required = false) String categoryCode) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Get wholesale configuration for categoryCode: {}  categoryId: {}", categoryCode, categoryId);
    WholesaleMappingWebResponse wholesaleMappingWebResponse =
        categoryService.findWholesaleConfig(categoryId, categoryCode);
    return new SingleBaseResponse<>(null, null, true, requestId, wholesaleMappingWebResponse);
  }

  @Operation(summary = "Update Category Wholesale Configuration")
  @PutMapping(value = CategoryApiPath.UPDATE_CATEGORY_WHOLESALE_CONFIG_MAPPING, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateCategoryWholesaleConfigMapping(@PathVariable String categoryId,
      @RequestBody WholesaleMappingWebRequest wholesaleMappingWebRequest) {
    log.info("updating the category with Wholesale Config mappings with request : {}", wholesaleMappingWebRequest);
    GdnBaseRestResponse response = categoryService.updateCategoryWholesaleConfigMapping(categoryId,
        ConverterUtil.toCategoryWholesaleConfig(wholesaleMappingWebRequest));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Fetch category list for generic template")
  @GetMapping(value = CategoryApiPath.GET_CATEGORY_LIST_FOR_GENERIC_TEMPLATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<CategoryTreeResponse> getCategoryListForGenericTemplate(
      @RequestParam(defaultValue = "false") boolean genericTemplateEligible) {
    log.info("Method : Fetch category tree for generic template");
    List<CategoryTreeResponse> response = categoryService.getCategoryListForGenericTemplate(genericTemplateEligible);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(), response, new Metadata());
  }

  @Operation(summary = "Get category tree for category codes")
  @PostMapping(value = CategoryApiPath.GET_CATEGORY_TREE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<CatalogTreeWebResponse> getCategoryTree(@RequestBody CategoryCodeRequest request) {
    log.info("Method: To fetch category tree for list of category codes : {}", request);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        categoryService.getCategoryTreeForCategoryCodes(request), new Metadata());
  }

  @Operation(summary = "Get list of document for category")
  @GetMapping(value = CategoryApiPath.GET_DOCUMENT_LIST, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<DocumentWebResponse> getSupportingDocuments() {
    log.info("Method : Fetch the document list for category");
    DocumentWebResponse response = categoryService.getDocumentList();
    return new SingleBaseResponse(null, null, true, clientParameterHelper.getRequestId(), response);
  }

  @Operation(summary = "Get system parameter value by variable")
  @GetMapping(value = CategoryApiPath.GET_SYSTEM_PARAM_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<DocumentWebResponse> getSystemParamValue(
    @RequestParam(required = true) String variable) {
    log.info("Method : Fetch the document list for category");
    DocumentWebResponse response = categoryService.getSystemParamValue(variable);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
      response);
  }

  @Operation(summary = "Get category suggestion by product name")
  @GetMapping(value = CategoryApiPath.GET_CATEGORY_SUGGESTIONS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<SuggestedCategoriesWebResponse> getCategoryPredictions(@RequestParam String productName) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Get category predictions by productName: {}, requestId {} ", productName, requestId);
    List<SuggestedCategoriesWebResponse> suggestedCategoriesWebRespons =
        categoryService.getCategorySuggestionByProductName(productName);
    return new ListBaseResponse<>(null, null, true, requestId, suggestedCategoriesWebRespons,
        new Metadata(0, suggestedCategoriesWebRespons.size(), Long.valueOf(suggestedCategoriesWebRespons.size())));
  }

  @Operation(summary = "Validate category")
  @GetMapping(value = CategoryApiPath.VALIDATE_CATEGORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse validateCategory(@PathVariable("categoryId") String categoryId) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Validate category categoryId: {}, requestId {} ", categoryId, requestId);
    return categoryService.validateCategory(requestId, categoryId);
  }

  @Operation(summary = "API to save original sales category")
  @PostMapping(value = CategoryApiPath.ADD_ORIGINAL_SALES_CATEGORY, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<String> createOriginalSalesCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody OriginalSalesCategoryWebRequest request) {
    log.info("API to save original sales category: {}", request);
    return new GdnRestSimpleResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        categoryService.addOriginalSalesCategory(storeId, request));
  }

  @Operation(summary = "Fetch osc listing")
  @GetMapping(value = CategoryApiPath.FETCH_OSC_LIST, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<OscSummaryWebResponse> getOscListing(@RequestParam(required = false) String oscCode,
      @RequestParam(required = false) String keyword, @RequestParam(required = false) Boolean activated) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Get OSC Listing for code : {}, keyword:{}, activated: {} requestId {} ", oscCode, keyword, activated,
        requestId);
    List<OscSummaryWebResponse> responses = categoryService.getOscListing(oscCode, keyword, activated);
    return new ListBaseResponse<>(null, null, true, requestId, responses,
        new Metadata(0, responses.size(), (long) responses.size()));
  }

  @Operation(summary = "Update Original Sales Category")
  @PutMapping(value = CategoryApiPath.UPDATE_OSC_LIST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateOriginalSalesCategory(@RequestBody OriginalSalesCategoryUpdateWebRequest request) {
    log.info("updating original Sales Category with request : {}", request);
    GdnBaseRestResponse response =
        categoryService.updateOriginalSalesCategory(ConverterUtil.toOscInfoUpdateDTO(request));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Fetch Original Sales Category")
  @GetMapping(value = CategoryApiPath.GET_ORIGINAL_SALES_CATEGORY_DETAILS_BY_ID, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<OscDetailsWebResponse> getOscDetailsById(@PathVariable("id") String id) {
    OscDetailsWebResponse response = categoryService.getOriginalSalesCategory(id);
    return new GdnRestSimpleResponse<>(null, null, true, clientParameterHelper.getRequestId(), response);
  }

  @Operation(summary = "Get Value from application property")
  @GetMapping(value = CategoryApiPath.GET_VALUE_FROM_PROPERTIES, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<Map<String, Object>> getValueFromProperties() throws IOException {
    log.info("Get Value from application property");
    return new GdnRestSimpleResponse<>(null, null, true, null, categoryService.getValueFromProperties());
  }

  @Operation(summary = "Fetch base margin hierarchy")
  @GetMapping(value = CategoryApiPath.FETCH_BASE_MARGIN_HIERARCHY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BaseMarginHierarchyWebResponse> fetchBaseMarginHierarchy(
      @PathVariable("categoryCode") String categoryCode) {
    log.info("Fetching base margin hierarchy for category code : {} ", categoryCode);
    List<BaseMarginHierarchyWebResponse> response = categoryService.getCategoryMarginHierarchy(categoryCode);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response, new Metadata(0, response.size(), (long) response.size()));
  }

  @Operation(summary = "Calculation of profit hierarchy")
  @PostMapping(value = CategoryApiPath.PROFIT_MARGIN, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProfitMarginWebResponse> profitMargin(@RequestBody ProfitMarginWebRequest request) {
    log.info("Calculation of profit margin for request : {} ", request);
    ProfitMarginWebResponse response = categoryService.getProfitMargin(request);
    return new SingleBaseResponse<>(null, null, true,
        clientParameterHelper.getRequestId(), response);
  }

}
