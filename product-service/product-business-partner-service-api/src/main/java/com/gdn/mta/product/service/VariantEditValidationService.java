package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

public interface VariantEditValidationService {

  /**
   *Validate the price and stock update and add add them to success or failure list
   * @param requests
   * @param successRequests
   * @param failedRequests
   */
  void validateListOfVariants(List<ProductPriceStockAndImagesRequest> requests,
      List<ProductPriceStockAndImagesRequest> successRequests, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemSummaryResponse> itemSummaryResponseMap) throws Exception;

  /**
   *
   * @param request
   * @param categoryCode
   * @param itemSku
   * @param wholesalePriceSkuResponse
   * @param wholesaleMappingResponse
   * @return
   */
  Boolean isSameThreshold(ProductPriceStockAndImagesRequest request, String categoryCode, String itemSku,
      WholesalePriceSkuResponse wholesalePriceSkuResponse, WholesaleMappingResponse wholesaleMappingResponse);

  /**
   *
   * @param request
   * @param categoryCode
   * @param itemSku
   * @param wholesalePriceSkuResponse
   * @return
   */
  Boolean isSameThresholdL5(ItemPickupPointRequest request, String categoryCode, String itemSku,
      WholesalePriceSkuResponse wholesalePriceSkuResponse);

  /**
   * @param requests
   * @param failedRequests
   * @param itemSummaryResponseList
   * @return
   * @throws Exception
   */
  List<ProductPriceStockAndImagesRequest> validateListOfVariantsWithSuccess(
      List<ProductPriceStockAndImagesRequest> requests, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemSummaryResponse> itemSummaryResponseList)
      throws Exception;

  /**
   * Validate L5 request
   *
   * @param productVariantUpdateRequest
   * @param errorCode
   * @param profileResponse
   * @throws Exception
   */
  void validateL5UpdateRequest(ProductVariantUpdateRequest productVariantUpdateRequest, SimpleStringResponse errorCode,
      ProfileResponse profileResponse) throws Exception;

  /**
   *
   * @param productVariantUpdateRequest
   * @return
   * @throws Exception
   */
  boolean validateL5UpdateRequestWithErrorCode(ProductVariantUpdateRequest productVariantUpdateRequest) throws Exception;

  /**
   * @param request
   * @param failedRequests
   * @param itemSummaryResponseMap
   * @return
   * @throws Exception
   */
  List<ProductPriceStockAndImagesRequest> validateListOfVariantsForMultiUsedProduct(
      List<ProductPriceStockAndImagesRequest> request, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemSummaryResponse> itemSummaryResponseMap) throws Exception;

  void validateListOfVariantsL5ForMultiUsedProduct(
    List<ProductVariantPriceStockAndImagesRequest> requests,
    Map<String, ItemPickupPointListingResponse> itemSummaryResponseMap,
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
    List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
    boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse)
    throws Exception;

  /**
   *For adding price and stock update request to failure list
   * @param request
   * @param failedRequests
   * @param code
   * @param message
   */
  void addToFailureList(ProductPriceStockAndImagesRequest request, List<VariantsErrorListResponse> failedRequests, String code, String message);

  /**
   * @param savedProductData
   * @return
   */
  ApiErrorCode checkArchivedSuspendedRejectedFaultyImageForUpdate(ProductAndItemsResponse savedProductData);

  /**
   * Validate L5 update request
   *
   * @param requests
   * @param itemPickupPointListingResponseMap
   * @param wholesalePriceSkuResponseMap
   * @param failedRequests
   * @param errorCode
   * @param multiPickupPointEnabledForSeller
   * @param wholesaleMappingResponse
   * @throws Exception
   */
  void validateListOfVariantsL5(List<ProductVariantPriceStockAndImagesRequest> requests,
      Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
      boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse) throws Exception;

  /**
   * Validate upc code new flow
   *
   * @param skuCodeUpcCodeMap
   * @param errorCode
   * @param productVariantUpdateRequest
   * @param successValidationVariantList
   */
  void validateUpcCodeL5LevelNew(Map<String, String> skuCodeUpcCodeMap, SimpleStringResponse errorCode,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList);

  /**
   * @param request
   * @param failedRequests
   * @return
   */
  void validatePriceLockCampaignRequest(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests) throws Exception;

  /**
   *Update Price Data in campaign
   * @param request
   * @param failedRequests
   * @throws Exception
   */
  void updatePriceLockCampaignRequestL5(ProductVariantPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, String categoryCode, ItemPickupPointRequest modifiedItemPickupPoint) throws Exception;

  /**
   *validate Price Data in campaign
   * @param request
   * @param failedRequests
   * @param categoryCode
   * @param modifiedItemPickupPoint
   * @throws Exception
   */
  void validatePriceLockCampaignRequestL5(ProductVariantPriceStockAndImagesRequest request,
    List<VariantsErrorListResponse> failedRequests, String categoryCode,
    ItemPickupPointRequest modifiedItemPickupPoint) throws Exception;

  /**
   *
   * @param productItems
   * @param failedRequests
   * @return
   */
  List<ProductPriceStockAndImagesRequest> validateListOfVariantsForNeedCorrection(
      List<ProductPriceStockAndImagesRequest> productItems, List<VariantsErrorListResponse> failedRequests)
      throws Exception;

  /**
   * @param productItems
   * @param failedRequests
   * @return
   * @throws Exception
   */
  List<ProductVariantPriceStockAndImagesRequest> validateListOfVariantsForNeedCorrectionL5(
      List<ProductVariantPriceStockAndImagesRequest> productItems, List<VariantsErrorListResponse> failedRequests)
      throws Exception;

  /**
   * Validate L5 update request
   *
   * @param requests
   * @param itemSummaryResponseMap
   * @param wholesalePriceSkuResponseMap
   * @param failedRequests
   * @param errorCode
   * @param multiPickupPointEnabledForSeller
   * @param wholesaleMappingResponse
   * @throws Exception
   */
  void validateListOfVariantsL5WithSuccess(List<ProductVariantPriceStockAndImagesRequest> requests,
    Map<String, ItemPickupPointListingResponse> itemSummaryResponseMap,
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
    List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
    boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse)
    throws Exception;

  /**
   * Validate L5 add request
   *
   * @param productVariantUpdateRequest
   * @param errorCode
   * @param profileResponse
   * @param newlyAddedProductItemRequests
   * @param savedItemPickupPointDataMap
   */
  void validateNewL5AdditionRequests(ProductVariantUpdateRequest productVariantUpdateRequest,
      SimpleStringResponse errorCode, ProfileResponse profileResponse,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) throws Exception;

  /**
   * @param skuCodeUpcCodeMap
   * @param errorCode
   * @param productPriceStockAndImagesRequest
   */
  void validateUpcCodeL5Level(Map<String, String> skuCodeUpcCodeMap,
      SimpleStringResponse errorCode, ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest);

  /**
   * Validate L5 request new
   *
   * @param productVariantUpdateRequest
   * @param errorCode
   * @param newlyAddedProductItemRequests
   * @param profileResponse
   * @throws Exception
   */
  void validateL5UpdateRequestNew(ProductVariantUpdateRequest productVariantUpdateRequest,
    SimpleStringResponse errorCode, List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests, ProfileResponse profileResponse) throws Exception;

  /**
   * @param productVariantUpdateRequest
   * @param businessPartnerCode
   * @param profileResponse
   */
  void validateAndSetFbbActiveFlagAtL5(ProductVariantUpdateRequest productVariantUpdateRequest, String businessPartnerCode,
      ProfileResponse profileResponse)
    throws Exception;

  /**
   * Validate if setting free sample to online or cnc
   * @param productVariantUpdateRequest
   * @param errorCode
   */
  void validateStatusAndCNCUpdateAtL5(ProductVariantUpdateRequest productVariantUpdateRequest,
    SimpleStringResponse errorCode);

  /**
   * validate pickup point code in request
   *
   * @return
   * @throws Exception
   */
  void pickupPointCodeValidation(ProductL3UpdateRequest l3UpdateRequest) throws Exception;

  /**
   * Validate bundle product request for edit
   *
   * @param l3UpdateRequest
   * @param productEditValidationDTO
   * @param profileResponse
   * @return
   */
  ProductEditValidationDTO validateBundleProduct(ProductL3UpdateRequest l3UpdateRequest,
      ProductL3Response savedProductData, ProductEditValidationDTO productEditValidationDTO,
      ProfileResponse profileResponse);
  /**
   * validate Eligibility For Variant And L5 Deletion
   *
   * @param profileResponse        businessPartner Response
   * @param productL3UpdateRequest edit Request
   * @param savedProductData ProductL3Response
   */
  void validateEligibilityForVariantAndL5Deletion(ProfileResponse profileResponse, ProductL3UpdateRequest productL3UpdateRequest,
    ProductLevel3 savedProductData) throws Exception;
}

