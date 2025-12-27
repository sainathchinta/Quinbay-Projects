package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.entity.Dimension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface DimensionRepositoryCustom {
  /**
   * Get list of attribute
   * @param storeId
   * @param keyword
   * @param sortByFieldName
   * @param sortOrder
   * @param pageable
   * @return
   */
  Page<Dimension> findByStoreIdAndKeywordAndMarkForDeleteFalseOrderByName(
      String storeId, String keyword, String sortByFieldName,
      String sortOrder, Pageable pageable);
}
