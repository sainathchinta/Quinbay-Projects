package com.gdn.mta.bulk.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.entity.ProductRecatStatus;

public interface ProductRecatStatusCustomRepository {

  /**
   * find product recate status summary
   * @param storeId
   * @param requestCode
   * @param status
   * @param keyword
   * @param pageable
   * @return
   */
  Page<ProductRecatStatus> findProductRecatStatusByStatusFilterAndKeyword(String storeId, String requestCode, String status, String keyword, Pageable pageable);

}
