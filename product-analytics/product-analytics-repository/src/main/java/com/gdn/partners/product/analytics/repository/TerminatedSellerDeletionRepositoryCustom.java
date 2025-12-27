package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.TerminatedSellerDeletion;

import java.util.List;
import java.util.Set;

public interface TerminatedSellerDeletionRepositoryCustom {

  /**
   * Fetch the terminated seller's products in batch to publish
   */
  List<TerminatedSellerDeletion> fetchTerminatedSellerProductsForDeletion(String storeId,
      Set<String> status);
}
