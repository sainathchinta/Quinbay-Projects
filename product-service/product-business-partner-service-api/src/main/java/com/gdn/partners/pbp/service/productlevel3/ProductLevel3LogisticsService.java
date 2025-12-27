package com.gdn.partners.pbp.service.productlevel3;

import java.util.List;

import com.gdn.mta.product.entity.ProductLevel3Logistics;

public interface ProductLevel3LogisticsService {
  List<ProductLevel3Logistics> findLogisticsByItemSku(String itemSku, String merchantCode,
      String merchantDeliveryType);

  Boolean saveLogisticsByItemSku(List<String> itemSku, String merchantCode,
      List<ProductLevel3Logistics> logistics, boolean isActive);
}
