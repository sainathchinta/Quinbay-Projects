package com.gdn.mta.product.service.util;

import java.util.List;

import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

public interface WholesaleValidationUtil {

  /**
   * Validate Wholesale rules and config on flow 1 product creation
   *  @param categoryCode
   * @param productItemCreationRequests
   * @param validateAtL4Level
   */
  ApiErrorCode validateWholesaleConfigOnFlow1(String categoryCode,
      List<ProductItemCreationRequest> productItemCreationRequests, boolean validateAtL4Level);

  /**
   * Validate Wholesale rules and config on flow 2 product creation
   *
   * @param categoryCode
   * @param productItemCreationRequests
   */
  ApiErrorCode validateWholesaleConfigOnFlow2(String categoryCode, List<ProductItemBusinessPartnerRequest> productItemCreationRequests);

  /**
   * Validate wholesale request for update API
   *
   * @param storeId
   * @param productPriceAndWholesaleRequest
   * @param itemSku
   */
  ApiErrorCode validateWholesalePriceRequestForUpdate(String storeId,
      ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest, String itemSku,
      ProductAndItemsResponse savedProductData, int minimumPrice);

  /**
   * Validate wholesale request for update API
   *
   * @param categoryCode
   * @param productItemWholesalePriceRequests
   * @param itemRequest
   * @param minimumPrice
   * @param itemSku
   * @param wholesalePriceActivated
   * @param wholesaleMappingResponse
   * @return
   */
  ApiErrorCode validateWholesaleConfigOnUpdate(String categoryCode,
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests, ItemRequest itemRequest,
      Integer minimumPrice, String itemSku, Boolean wholesalePriceActivated,
      WholesaleMappingResponse wholesaleMappingResponse);
}
