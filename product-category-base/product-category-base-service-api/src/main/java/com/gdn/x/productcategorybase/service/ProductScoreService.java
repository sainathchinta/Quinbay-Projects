package com.gdn.x.productcategorybase.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface ProductScoreService {

  /**
   * API to find products which are to be updated
   *
   * @param pageable
   * @throws Exception
   */
  Page<String> findByMarkForDeleteFalseAndUpdatedFalse(Pageable pageable);

  /**
   * API to update products
   *
   * @param products
   * @throws Exception
   */
  void updateProducts(List<String> products);
}
