package com.gdn.partners.pbp.outbound.sellerLogistics;

import java.util.List;

import com.gdn.seller.logistics.web.model.request.SaveSkuLogisticProductRequest;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.SaveSkuLogisticProductResponse;

public interface SellerLogisticsOutbound {

  List<GetSkuLogisticProductResponse> getSkuLogistics(String itemSku, String merchantCode,
      String merchantDeliveryType) throws Exception;

  SaveSkuLogisticProductResponse saveSkuLogistics(SaveSkuLogisticProductRequest requestBody,
      boolean isActive) throws Exception;

}
