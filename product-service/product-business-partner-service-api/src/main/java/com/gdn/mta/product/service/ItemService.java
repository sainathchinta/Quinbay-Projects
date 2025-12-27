package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.enums.ProductStatus;

public interface ItemService {

  void publishItemStatusEvent(List<ProductBusinessPartner> productBusinessPartners,
      ProductStatus productStatus, String storeId) throws Exception;

  void publishItemStatusEvent(String productCode, ProductStatus productStatus);
}
