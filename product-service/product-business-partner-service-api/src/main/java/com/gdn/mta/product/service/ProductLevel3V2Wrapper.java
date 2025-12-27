package com.gdn.mta.product.service;

import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.OrderCancellationDto;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

public interface ProductLevel3V2Wrapper {

  /**
   * Updating L4 And L5 To Offline For OrderCancellation
   *
   * @param orderCancellationDto
   * @throws Exception
   */
  void updateL4AndL5ToOfflineForOrderCancellation(OrderCancellationDto orderCancellationDto)
      throws Exception;

  /**
   * validation for edit request
   *
   * @param requestId
   * @param request
   * @param isOnlyExternal
   * @param combineContentAndLogisticsPcbUpdate
   * @param combinePreOrderUpdate
   * @return
   * @throws Exception
   */
  ProductEditValidationDTO editProductDetails(String requestId, ProductL3UpdateRequest request, boolean isOnlyExternal, boolean combineContentAndLogisticsPcbUpdate,
      boolean combinePreOrderUpdate) throws Exception;

  /**
   * Edit product master data by productSku
   *
   * @param storeId storeId
   * @param requestId requestId
   * @param username username
   * @param productMasterDataEditRequest edit request containing product master data
   * @return ApiErrorCode indicating the result of the operation
   */
  ApiErrorCode editProductMasterData(String storeId, String requestId, String username,
    ProductMasterDataEditRequest productMasterDataEditRequest) throws Exception;

  /**
   *
   * @param productMasterDataEditRequest product master data edit request
   * @param masterProductEditDTO master product edit DTO
   * @throws Exception
   */
  void updateCategoryForAutoCategoryChange(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) throws Exception;

  /**
   * Publish image QC for content change
   *
   * @param productMasterDataEditRequest edit request containing product master data
   * @param masterProductEditDTO master product edit DTO
   */
  void publishImageQCForContentChange(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO);

  /**
   * Check if omniChannel already exists
   *
   * @param storeId
   * @param omniChannelSkuRequest
   * @return
   */
  OmniChannelMapAndSkuResponse checkIfOmniChannelSkuExists(String storeId, OmniChannelSkuRequest omniChannelSkuRequest);
}
