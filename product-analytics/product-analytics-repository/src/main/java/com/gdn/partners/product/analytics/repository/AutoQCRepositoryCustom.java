package com.gdn.partners.product.analytics.repository;

import java.util.List;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.mongodb.bulk.BulkWriteResult;

public interface AutoQCRepositoryCustom {

  /**
   * Upsert for given list of AutoQCDetail
   *
   * @param autoQCDetailList
   * @param sellerFieldsChangeResponseList
   * @param fieldNameList
   * @return
   */
  BulkWriteResult bulkWriteAutoQcDetail(List<AutoQCDetail> autoQCDetailList,
      List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList, List<String> fieldNameList);

}