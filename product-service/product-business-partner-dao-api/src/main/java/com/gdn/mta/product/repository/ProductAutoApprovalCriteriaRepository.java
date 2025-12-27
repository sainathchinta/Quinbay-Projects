package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;

public interface ProductAutoApprovalCriteriaRepository
    extends JpaRepository<ProductAutoApprovalCriteria, String> {

  long deleteByStoreIdAndProductCode(String storeId, String productCode);
}
