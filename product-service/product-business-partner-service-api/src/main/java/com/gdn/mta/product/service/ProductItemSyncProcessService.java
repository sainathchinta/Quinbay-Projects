package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.mta.product.entity.ProductItemSyncProcess;

public interface ProductItemSyncProcessService {

  ProductItemSyncProcess save(ProductItemSyncProcess syncProcess);

  List<ProductItemSyncProcess> findAllProcessEligibleForNotification(String storeId);

}
