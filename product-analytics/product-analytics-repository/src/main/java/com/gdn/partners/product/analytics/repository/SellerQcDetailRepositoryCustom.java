package com.gdn.partners.product.analytics.repository;

import java.util.List;

import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.mongodb.bulk.BulkWriteResult;

public interface SellerQcDetailRepositoryCustom {

  /**
   * Upsert for given list of AutoQCDetail
   *
   * @param sellerQCDetailList
   * @return
   */
  BulkWriteResult bulkWriteSellerQcDetail(List<SellerQCDetail> sellerQCDetailList);

}