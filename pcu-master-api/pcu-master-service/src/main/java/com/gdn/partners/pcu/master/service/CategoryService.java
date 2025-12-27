package com.gdn.partners.pcu.master.service;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.model.request.CategoryCreateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryInfoUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryMappingsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
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
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.SuggestedCategoriesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleMappingWebResponse;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;

public interface CategoryService {

  /**
   * To activate a category
   *
   * @param requestId
   * @param request
   */
  void activate(String requestId, CategoryStatusChangeServiceRequest request) throws Exception;

  /**
   * To deactivate a category
   *
   * @param requestId
   * @param request
   */
  void deactivate(String requestId, CategoryStatusChangeServiceRequest request) throws Exception;

  /**
   * To update category
   *
   * @param request
   */
  void update(CategoryStatusChangeServiceRequest request) throws Exception;

  /**
   * Update category info
   * @param categoryInfoUpdateServiceRequest
   * @return
   */
  GdnBaseRestResponse updateCategoryInfo(CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest);

  /**
   * Update category mappings - attribute for master category and master categories for sales category
   * @param categoryMappingsUpdateServiceRequest
   * @return
   */
  GdnBaseRestResponse updateCategoryMappings(
      CategoryMappingsUpdateServiceRequest categoryMappingsUpdateServiceRequest);

  /**
   * Create New Category
   * @param categoryCreateServiceRequest
   * @return CreateCategoryWebResponse
   */
  CreateCategoryWebResponse createCategory(CategoryCreateServiceRequest categoryCreateServiceRequest);

  /**
   * Get category information along with shipping details
   *
   * @param categoryId
   * @param requestId
   * @param fetchHideForSellerAttributes
   * @return
   */
  SingleBaseResponse<CategoryInfoWebResponse> getCategoryInfoWithShippingDetail(String categoryId,
      String requestId, boolean fetchHideForSellerAttributes);


  /**
   *
   * Get Sales Category Mapping by Category Codes
   *
   * @param categoryCodes
   * @param catalogId
   * @return List of CategoryMappingResponse
   */
  List<CategoryMappingResponse> getSalesCategoryMappingByCategoryCodes(List<String> categoryCodes, String catalogId);

  /**
   *
   * @param categoryCode
   */
  MarginCategoryWebResponse getMarginByCategoryCode(String categoryCode);

  /**
   * Get category summary with used sequence by category Id
   *
   * @param parentId
   * @return
   */
  List<CategoryResponse> findCategorySummaryByParentId(String parentId);

  /**
   * Get margin business partner by Business Partner Code and category code
   * @param businessPartnerCode
   * @param catergoryCode
   * @return
   */
  MarginCategoryWebResponse getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(String businessPartnerCode, String catergoryCode);


  /**
   * updating the restricted keyword for category
   * @param categoryCode
   * @param toCategoryKeywordRequestList
   * @return
   */
  GdnBaseRestResponse updateCategoryRestrictedKeywordMappings(String categoryCode,
      CategoryKeywordUpdateRequestList toCategoryKeywordRequestList);
  /**
   * Get restricted keywords by category code or keyword
   *
   * @param categoryRestrictedKeywordsWebRequest
   * @return
   */
  Page<RestrictedKeywordsWebResponse> findRestrictedKeywords(
      CategoryRestrictedKeywordsWebRequest categoryRestrictedKeywordsWebRequest, Pageable pageable);

  /**
   * Get Wholesale Configuration by category code or category Id
   *
   * @param categoryId
   * @param categoryCode
   * @return
   */
  WholesaleMappingWebResponse findWholesaleConfig(String categoryId, String categoryCode);

  /**
   * Update category Wholesale Config mapping
   *
   * @param categoryId
   * @param wholesaleMappingRequest
   * @return
   */
  GdnBaseRestResponse updateCategoryWholesaleConfigMapping(String categoryId,
       WholesaleMappingRequest wholesaleMappingRequest);

  /**
   * Get category tree for generic template
   *
   * @Param genericTemplateEligible
   * @return
   */
  List<CategoryTreeResponse> getCategoryListForGenericTemplate(boolean genericTemplateEligible);

  /**
   * Get category tree for list of category codes
   *
   * @return
   * @Param request
   */
  List<CatalogTreeWebResponse> getCategoryTreeForCategoryCodes(CategoryCodeRequest request);

  /**
   * Get list of document for category
   *
   * @return
   */
  DocumentWebResponse getDocumentList();

  /**
   * Get system param value
   *
   * @param variable
   * @return
   */
  DocumentWebResponse getSystemParamValue(String variable);

  /**
   * get category suggestion by product name
   * @param productName
   * @return
   */
  List<SuggestedCategoriesWebResponse> getCategorySuggestionByProductName(String productName);

  /**
   * validate category
   * @param requestId
   * @param categoryId
   * @return
   */
  BaseResponse validateCategory(String requestId, String categoryId);


  /**
   * API to add new original sales category
   *
   * @param storeId
   * @param originalSalesCategoryWebRequest
   * @return
   */
  String addOriginalSalesCategory(String storeId, OriginalSalesCategoryWebRequest originalSalesCategoryWebRequest);

  /**
   * get osc filtered listing
   *
   * @param oscCode
   * @param activated
   * @param keyword
   * @return
   */
  List<OscSummaryWebResponse> getOscListing(String oscCode, String keyword, Boolean activated);

  /**
   * updating Original Sales Category
   *
   * @param oscInfoUpdateDTO
   * @return
   */
  GdnBaseRestResponse updateOriginalSalesCategory(OscInfoUpdateDTO oscInfoUpdateDTO);

  /**
   *
   * @param id
   * @return
   */
  OscDetailsWebResponse getOriginalSalesCategory(String id);

  /**
   * Getting value from application properties for restrictedKeywordActionList and restrictedKeywordType
   *
   * @return
   * @throws IOException
   */
  Map<String,Object> getValueFromProperties() throws IOException;

  /**
   * Get category margin hierarchy by cn
   * @param categoryCode
   * @return
   */
  List<BaseMarginHierarchyWebResponse> getCategoryMarginHierarchy(String categoryCode);

  /**
   * calculation of profit margin
   * @param profitMarginWebRequest
   * @return
   */
  ProfitMarginWebResponse getProfitMargin(ProfitMarginWebRequest profitMarginWebRequest);
}
