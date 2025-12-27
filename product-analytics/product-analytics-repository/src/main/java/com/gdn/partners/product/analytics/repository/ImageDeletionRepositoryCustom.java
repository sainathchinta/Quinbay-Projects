package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ImageDeletion;

import java.util.List;
import java.util.Set;

public interface ImageDeletionRepositoryCustom {

  /**
   * Fetch the terminated products in batch to delete images
   */
  List<ImageDeletion> fetchProductCodeForImageDeletion(String storeId, Set<String> status);
}
