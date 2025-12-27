package com.gdn.mta.product.service;

import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductCodeAndSkuRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.ProductSkuDetailResponse;
import org.springframework.data.domain.Page;

import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;

import java.util.List;

public interface ProductL3Service {

  /**
   * API to fetch the L3 products details by product sku.
   *
   * @param storeId
   * @param productSku
   * @param isNeedCorrection
   * @param fullFetch
   * @param concatenateValueWithValueType
   * @return
   * @throws Exception
   */
  ProductL3DetailsResponse getL3ProductDetailsByProductSku(String storeId, String productSku,
    boolean isNeedCorrection, boolean fullFetch, boolean concatenateValueWithValueType) throws Exception;

  /**
   * get l5 listing by product sku
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param page
   * @param size
   * @param itemPickupPointListingL3Request
   * @param onlyDefaultViewConfig
   * @param concatenateValueWithValueType
   * @param needInventoryData
   * @return
   * @throws Exception
   */
  Page<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(String storeId, String username, String requestId,
      int page, int size, ItemPickupPointListingL3Request itemPickupPointListingL3Request,
      boolean onlyDefaultViewConfig, boolean concatenateValueWithValueType, boolean needInventoryData) throws Exception;

  /**
   * Find itemPickupPoint summary by list of product skus
   *
   * @param page
   * @param size
   * @param productSkuList
   * @param businessPartnerCode
   * @param onlineOrCnc
   * @param accessiblePickupPoints
   * @return
   */
  Page<ProductLevel3SummaryResponse> getItemPickupPointByProductSkus(int page, int size,
    List<String> productSkuList, String businessPartnerCode, boolean onlineOrCnc,
      List<String> accessiblePickupPoints)
    throws Exception;

  /**
   *
   * @param itemSkuPickupPointRequest
   */
  List<DeleteInProgressL5Response> deleteInProgressL5ForDeletePickupPoint(String storeId,
    ItemSkuPickupPointRequest itemSkuPickupPointRequest) throws Exception;

  /**
   * get Product Sku detail response
   *
   * @param storeId                  String
   * @param productCodeAndSkuRequest ProductCodeAndSkuRequest
   * @return ProductSkuDetailResponse
   */
  ProductSkuDetailResponse getProductSkuDetailResponse(String storeId,
    ProductCodeAndSkuRequest productCodeAndSkuRequest);
}
