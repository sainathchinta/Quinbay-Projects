package com.gdn.mta.product.repository;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;

import java.util.List;

public interface OfflineItemRepository {

  List<OfflineItemPriceResponse> findOfflineItemByBusinessPartnerCodeAndItemSku(
      String businessPartnerCode, String itemSku) throws Exception;

  OfflineItemResponse findOfflineItemByBusinessPartnerCodeAndMerchantSkus(
      String businessPartnerCode, List<String> merchantSkus) throws Exception;

  /**
   * Updates the offline item's price of a specific SKU for all stores / pickup points
   *
   * @param merchantCode must not be blank
   * @param request must not be null
   * @return GdnBaseRestResponse
   * @throws Exception
   */
  GdnBaseRestResponse updateOfflineItemPriceByItemSku(String merchantCode,
      UpdateOfflineItemPriceRequest request) throws Exception;
}
