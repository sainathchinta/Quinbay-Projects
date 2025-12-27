package com.gdn.partners.pbp.service.productlevel3;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryCount;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;

public interface ProductLevel3AggregatorServiceOld {
  Page<ProductLevel3Summary> aggregateProductLevel3Summary(
      ProductLevel3SummaryFilter filterRequest, Pageable paging, SortOrder sort)
      throws Exception;
  Page<ProductLevel3SummaryMinified> aggregateProductLevel3SummaryMinified(
      ProductLevel3SummaryFilter filterRequest, Pageable paging, SortOrder sort) throws Exception;

  /**
   * Aggregate products details based on filter params
   *
   * @param filterRequest filter parameters
   * @param pageRequest   paging details
   *
   * @return product summary without inventory details
   *
   * @throws Exception
   */
  Page<ProductLevel3Summary> aggregateProductSummaryWithoutInventory(ProductLevel3SummaryFilter filterRequest,
    Pageable pageRequest) throws Exception;
}
