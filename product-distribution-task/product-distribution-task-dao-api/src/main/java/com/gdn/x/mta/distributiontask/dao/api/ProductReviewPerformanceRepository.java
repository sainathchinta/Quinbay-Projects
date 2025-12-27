package com.gdn.x.mta.distributiontask.dao.api;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.ProductReviewPerformance;

public interface ProductReviewPerformanceRepository extends JpaRepository<ProductReviewPerformance, String> {

  ProductReviewPerformance findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode)
      throws Exception;

}
