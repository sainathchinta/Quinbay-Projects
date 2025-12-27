package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.CogsDataResponse;
import com.gda.mta.product.dto.CogsUpdateRequests;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemL5ListingRequest;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.MasterDataUpdateRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ProductL3BasicResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductAndItemPickupPontL5Response;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;

import com.gda.mta.product.dto.ProductL3ListingRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.response.ProductL3ListingResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ItemSkuPpCodeRequest;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.NewlyAddedL5Response;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

public interface ProductLevel3V2Service {


  /**
   * Update L5 item from listing
   *
   * @param storeId
   * @param productSku
   * @param request
   * @param isExternalOnly
   * @return
   */
  ApiErrorCode productQuickEditV2(String storeId, String productSku,
    ProductLevel3QuickEditV2Request request, boolean isExternalOnly) throws Exception;

  /**
   * get item summary L4 response
   *
   * @param storeId
   * @param productSku
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ItemSummaryL4Response> getItemSummaryL4Response(String storeId, String productSku, int page, Integer size)
      throws Exception;

  /**
   * Get Product L3 List by request
   *
   * @param storeId
   * @param request
   * @param page
   * @param size
   * @return
   */
  Page<ProductL3ListingResponse> getProductL3List(String storeId, ProductL3ListingRequest request,
      Integer page, Integer size) throws Exception;

  /**
   * Validate product edit update request
   *
   * @param request
   * @param savedProductData
   */
  void validateProductEditInfo(ProductL3UpdateRequest request, ProductL3Response savedProductData) throws Exception;


  /**
   * Validate product description length
   *
   * @param description product description
   * @throws Exception if description length is more than threshold defined in properties file
   */

  void validateDescriptionLength(String description) throws Exception;

  /**
   * Validate youtube url is valid
   *
   * @param request request can be MasterDataUpdateRequest or ProductL3UpdateRequest
   * @throws Exception if url is not valid or youtube video is not active
   */

  <T extends MasterDataUpdateRequest> void validateYoutubeUrlIsValid(T request,
    ProductL3Response savedProductData) throws Exception;

  /**
   *
   * @param request
   * @param savedProductData
   * @return
   * @throws Exception
   */
  ProductL3UpdateRequest editRequestToBillOfMaterialRecipeRequest(ProductL3UpdateRequest request,
      ProductL3Response savedProductData);

  /**
   * set ProductLevel3 request
   *
   * @param request
   * @return
   */
  ProductLevel3 generateProductLevel3(ProductL3UpdateRequest request);

  /**
   * API to update the logistics information
   *
   * @param product
   * @param isOnlyExternal
   * @param isNeedCorrection
   * @param combineContentAndLogisticsPcbUpdate
   * @param combinePreOrderUpdate
   * @return
   * @throws Exception
   */
  ApiErrorCode updateLogistics(ProductLevel3UpdateRequest product, boolean isOnlyExternal, boolean isNeedCorrection,
      boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate) throws Exception;

  /**
   *
   * @param l3UpdateRequest
   * @param variantUpdateRequest
   * @return ItemsPriceStockImagesUpdateResponse
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse updateProductItemImages(
    ProductL3UpdateRequest l3UpdateRequest, ProductVariantUpdateRequest variantUpdateRequest)
    throws Exception;

  /**
   * Update product edit info
   *
   * @param request
   * @param isOnlyExternal
   * @param combineContentAndLogisticsPcbUpdate
   * @param combinePreOrderUpdate
   * @param newImagesAdded
   * @param productL3UpdateRequest
   * @return
   */
  EditProductResponse updateEditInfo(ProductLevel3 request, boolean isOnlyExternal,
      boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate, ProfileResponse profileResponse , ProductL3Response productL3Response,
    boolean newImagesAdded, ProductCollection productCollection, ProductL3UpdateRequest productL3UpdateRequest) throws Exception;

  void takeDownProduct(String storeId, String productSku, String productName) throws Exception;

  /**
   * API to fetch the L3 products details by product sku.
   *
   * @param storeId
   * @param productSku
   * @param concatValueAndValueTypes
   * @param needInventoryData
   * @return
   * @throws Exception
   */
  ProductLevel3DetailsV2Response fetchL3ProductDetailsByProductSku(String storeId,
    String productSku, boolean concatValueAndValueTypes, boolean needInventoryData) throws Exception;

  /**
   * API to perform partial updates for L5 listing.
   *
   * @param storeId
   * @param productSku
   * @param request
   * @return
   * @throws Exception
   */
  ItemPriceStockQuickUpdateResponse quickEditPatching(String storeId, String productSku,
    ProductLevel3QuickEditV2Request request) throws Exception;
  /**
   * Edit L5 data
   *
   * @param storeId
   * @param businessPartnerCode
   * @param productVariantUpdateRequest
   * @param editResponse
   * @return
   */
  ItemsPriceStockImagesUpdateResponse editPriceStockVariantsInfo(String storeId, ProductLevel3 businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse) throws Exception;

  /**
   * @param productL3UpdateRequest
   * @param editResponse
   * @return ProductVariantUpdateRequest
   */
  ProductVariantUpdateRequest toProductVariantUpdateRequest(ProductL3UpdateRequest productL3UpdateRequest,
      EditProductResponse editResponse);

  /**
   * get l5 listing by product sku
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param page
   * @param size
   * @param itemL5ListingRequest
   * @param needInventoryData
   * @return
   * @throws Exception
   */
  Page<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(String storeId,
    String username, String requestId, int page, int size,
    ItemL5ListingRequest itemL5ListingRequest, boolean needInventoryData) throws Exception;

  /**
   *
   * add new L5's added during need revision to inventory
   *
   * @param productBusinessPartner
   * @param newlyAddedL5Responses
   * @param profileResponse
   * @throws Exception
   */
  void insertInventoryForNewlyAddedL5DuringNeedRevision(ProductBusinessPartner productBusinessPartner,
      List<NewlyAddedL5Response> newlyAddedL5Responses, ProfileResponse profileResponse) throws Exception;

  /**
   * save need revision l3 history
   *
   * @param businessPartnerCode
   * @param request
   * @param productBusinessPartner
   * @throws Exception
   */
  void saveL3NeedRevisionHistory(String businessPartnerCode, ProductVariantUpdateRequest request,
      ProductBusinessPartner productBusinessPartner) throws Exception;

  FbbCreatePickupPointResponse createDefaultFbbPickupPoint(FbbCreatePickupPointRequest fbbCreatePickupPointRequest)
      throws Exception;

  /**
   * return product l5 details by
   *
   * @param storeId           non null store id
   * @param itemSkusRequest   itemSku and pickupPointCode request list
   * @param needInventoryData
   * @return ProductAndItemPickupPontL5Response
   * @throws Exception
   */
  List<ProductAndItemPickupPontL5Response> getProductDetailsByItemSkuAndPickupPointCode(String storeId, List<ItemSkuPpCodeRequest> itemSkusRequest,
      boolean needInventoryData) throws Exception;


  /**
   * validate Shipping And Dimension for PDP edit
   *
   * @param request : PDP edit request
   * @param relaxShippingWeightValidation
   * @return
   * @throws Exception Application Runtime on Validation Failure
   */

  <T extends MasterDataUpdateRequest> ApiErrorCode validateShippingAndDimensionForEdit(T request, boolean relaxShippingWeightValidation) throws Exception;

  /**
   * Validate L5 update request
   *
   * @param productVariantUpdateRequest
   * @param profileResponse
   * @return
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse validateL5UpdateRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
    ProfileResponse profileResponse) throws Exception;

  /**
   * validation for edit request
   *
   * @param requestId
   * @param request
   * @param productLevel3Edit
   * @param editResponse
   * @param profileResponse business partner profile
   * @return
   * @throws Exception
   */
  ProductEditValidationDTO  validationsForEdit(String requestId, ProductL3UpdateRequest request,
      ProductLevel3 productLevel3Edit, EditProductResponse editResponse, ProductL3Response savedProductData,
    ProfileResponse profileResponse) throws Exception;

  /**
   * validation for product l3 response
   * @param productLevel3Edit
   * @param editResponse
   * @param profileResponse
   * @param combineContentAndLogisticsPcbUpdate
   * @param savedProductData
   * @param pickupPointDeleteRequests
   * @param addingPickupPoints
   * @return
   * @throws Exception
   */
  ProductEditValidationDTO validationForProductL3ResponseAndNeedRevisionUpdate(ProductLevel3 productLevel3Edit,
      EditProductResponse editResponse, ProfileResponse profileResponse, boolean combineContentAndLogisticsPcbUpdate,
      ProductL3Response savedProductData, List<PickupPointDeleteRequest> pickupPointDeleteRequests, ProductL3UpdateRequest productL3UpdateRequest, boolean addingPickupPoints, boolean isPureExternalUser) throws Exception;
  /**
   * Validate existing order on product name edit
   *
   * @param productMasterDataEditRequest product master data edit request
   * @return ApiErrorCode
   */

  ApiErrorCode validateExistingOrderOnProductNameEdit(
    ProductMasterDataEditRequest productMasterDataEditRequest);

  /**
   * Get ProductL3Response from x-product
   *
   * @param productSku must not be empty
   * @return ProductL3Response
   * @throws Exception
   */
  ProductL3Response getProductL3ResponseFromXProduct(String productSku) throws Exception;

  /**
   * Get AttributeResponse list from pcb
   *
   * @param fetchOnlyBasicAttributeDetails boolean
   * @param attributeCodesRequest          must not be empty
   * @return AttributeResponse
   * @throws Exception
   */
  Map<String, String> getAttributeCodeAndIdMap(boolean fetchOnlyBasicAttributeDetails,
      AttributeCodesRequest attributeCodesRequest);

  /**
   * perform Product Detail Edit
   *
   * @param isOnlyExternal boolean
   * @param combinePreOrderUpdate boolean
   * @param editProductResponse edit response
   * @param combineContentAndLogisticsPcbUpdate boolean
   * @param profileResponse profileResponse
   * @return ProductEditValidationDTO
   */
  ProductEditValidationDTO performProductDetailEdit(boolean isOnlyExternal,
    boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate,
    EditProductResponse editProductResponse, ProductEditValidationDTO productEditValidationDTO,
    ProfileResponse profileResponse) throws Exception;

  /**
   * updating brand details of product
   *
   * @param productCode
   * @param brandUpdateRequest
   * @throws Exception
   */
  void updateBrandDataOfProduct(String productCode, BrandUpdateRequest brandUpdateRequest) throws Exception;
  /**
   * update AllCollections and Downstream And Process History For PDPEdit
   *
   * @param productDetailEditDTO
   * @return
   * @throws Exception
   */
  Pair<CombinedEditItemResponse, EditProductItemAndImageResponse> updateAllCollectionsDownstreamAndProcessHistoryForPDPEdit(
    ProductDetailEditDTO productDetailEditDTO, EditProductResponse editProductResponse) throws Exception;

  /**
   * Publish history to PCB
   *
   * @param productCode
   * @param businessPartnerCode
   * @param auditTrailDtoList
   */
  public void publishProductLevelHistoryToPcbForDistributionUpdate(String productCode, String businessPartnerCode, List<AuditTrailDto> auditTrailDtoList);

  /**
   * update PCB for content, Image and UPC PDPEdit
   *
   * @param productDetailEditDTO non null
   * @param productCollection non null
   * @return EditProductItemAndImageResponse PCB editResponse
   * @throws Exception
   */
  EditProductItemAndImageResponse performPCBUpdateForPDPEditRequest(
    ProductDetailEditDTO productDetailEditDTO, ProductCollection productCollection)
    throws Exception;

  /**
   * Get Product Count
   *
   * @param storeId
   * @param businessPartnerCode
   * @param incrementCounter
   * @return
   */
  Long getProductCount(String storeId, String businessPartnerCode, boolean incrementCounter) throws Exception;

  /**
   * Get count from cacheable
   *
   * @param storeId
   * @param businessPartnerCode
   * @return
   * @throws Exception
   */
  Long getProductCountCacheable(String storeId, String businessPartnerCode) throws Exception;

  /**
   * get eligibility for need revision products deletion
   *
   * @param storeId
   * @param needRevisionEligibilityRequestList
   * @param processBulkFlow
   * @return
   */
  List<NeedRevisionEligibilityResponse> eligibilityForNeedRevisionDeletion(String storeId,
      List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequestList, boolean processBulkFlow)
      throws ApplicationRuntimeException;

  /**
   * Get product l3 basic response
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductL3BasicResponse getProductL3BasicResponse(String storeId, String productCode);

  /**
   * Check if seller sku already  exists
   *
   * @param storeId
   * @param omniChannelSkuRequest
   * @return
   */
  ValidOmniChannelSkuResponse checkIfOmniChannelSkuExists(String storeId, OmniChannelSkuRequest omniChannelSkuRequest);

  /**
   * Update COGS value for product
   * @param productSku product SKU
   * @param request COGS update request
   * @throws Exception
   */
  void updateCogsValue(String productSku, CogsUpdateRequests request) throws Exception;

  /**
   * Get COGS data for product
   * @param productSku product SKU
   * @param page page number
   * @param size page size
   * @return list of COGS data responses
   * @throws Exception
   */
  List<CogsDataResponse> getCogsData(String productSku, int page, int size) throws Exception;
}
