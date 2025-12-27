package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import com.mongodb.bulk.BulkWriteResult;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface AutoApprovedRepositoryCustom {

  /**
   * fetch list of auto approved products
   *
   * @param request
   * @param pageable
   * @param isProductCode
   * @return
   */
  Page<AutoApprovedProducts> fetchListOfAutoApprovedProducts(AutoApprovedWebRequest request,
    Pageable pageable, boolean isProductCode);

  /**
   * Upsert for given list of Auto approved product list
   *
   * @param autoApprovedProducts List of auto approved auto products
   * @return Bulk write result
   */
  BulkWriteResult bulkWriteAutoApprovedProducts(List<AutoApprovedProducts> autoApprovedProducts);

  /**
   * Returns number of record in auto approved collection
   * @return
   */
  long countNumberOfRecords();
}