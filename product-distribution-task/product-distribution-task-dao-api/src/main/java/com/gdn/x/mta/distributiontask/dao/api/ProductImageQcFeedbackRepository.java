package com.gdn.x.mta.distributiontask.dao.api;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.ProductImageQcFeedback;

public interface ProductImageQcFeedbackRepository extends JpaRepository<ProductImageQcFeedback, String> {

  ProductImageQcFeedback findByStoreIdAndProductCode(String storeId, String productCode);

  void deleteByStoreIdAndProductCode(String storeId, String productCode);
}
