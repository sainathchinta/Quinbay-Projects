package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.ProductArchive;

public interface ProductArchiveService {

  /**
   * Add Product To ProductArchive
   *
   * @param productArchiveList
   * @return
   */
  List<ProductArchive> addProductsToProductArchive(List<ProductArchive> productArchiveList);
}
