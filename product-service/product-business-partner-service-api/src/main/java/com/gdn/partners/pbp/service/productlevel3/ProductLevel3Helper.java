package com.gdn.partners.pbp.service.productlevel3;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;

public interface ProductLevel3Helper {

  /**
   * Check restricted keyword in product detail
   *
   * @param productDetailResponse
   * @param categoryCode
   * @return
   */
  List<RestrictedKeywordsByField> getRestrictedKeywordsInProductDetails(ProductDetailResponse productDetailResponse, String categoryCode);

  /**
   * Check restricted keyword in product detail along with actionType
   *
   * @param productDetailResponse
   * @param categoryCode
   * @return
   */
  RestrictedKeywordsByFieldAndActionType getRestrictedKeywordsWithActionTypeInProductDetails(
      ProductDetailResponse productDetailResponse, String categoryCode);

  /**
   * Get top restricted keyword action
   *
   * @param restrictedKeywordToActionTypeMap
   * @param restrictedKeywordsByFieldList
   * @param restrictedKeywordsByFieldAndActionType
   * @return
   */
  RestrictedKeywordsByFieldAndActionType getResultantActionType(Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType);

  /**
   * returns true if price, status, sellerSku, off2On, pickupPointCode, wholesale is updated;
   * @param quickEditRequest
   * @param itemSummaryResponse
   * @param wholesalePriceActivated
   * @return
   */
  boolean isProductItemDetailChangedForListingUpdate(QuickEditRequest quickEditRequest,
      ItemSummaryResponse itemSummaryResponse, Boolean wholesalePriceActivated);

  /**
   * set product l3 details
   * @param productBusinessPartner
   * @param productLevel3
   */
  void setProductLevel3DetailsFromSummaryResponse(ProductBusinessPartner productBusinessPartner, ProductLevel3 productLevel3);

  /**
   * Returns true if fields updated on listing update request
   *
   * @param quickEditV2Request
   * @param itemSummaryListResponse
   * @param wholesaleActivated
   * @return
   */
  boolean isProductItemDetailChangedForL5ListingUpdate(QuickEditV2Request quickEditV2Request,
    ItemSummaryListResponse itemSummaryListResponse, Boolean wholesaleActivated);

  /**
   * set ProductLevel3 request
   * @param request
   * @return
   */
   ProductLevel3 generateProductLevel3(ProductL3UpdateRequest request);

  /**
   *
   * @param productAndItemActivationRequest
   */
  void addBundleRecipeInWMS(ProductAndItemActivationRequest productAndItemActivationRequest);


  /**
   * auto fill family colour
   * @param productL3UpdateRequest
   */
  void autoFillFamilyColourAttribute(ProductL3UpdateRequest productL3UpdateRequest);

  /**
   * validate shared product bundle recipe
   * @param request
   * @param itemSkuAndItemCodeMap
   * @return
   */
  List<ProductBundleRecipeRequest> validateShareProductRecipe(ProductL3UpdateRequest request,
      Map<String, String> itemSkuAndItemCodeMap);
}
