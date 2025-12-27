package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.ProductDataAutoFixHistory;

public interface ProductDataAutoFixHistoryRepository extends JpaRepository<ProductDataAutoFixHistory,String> {
}
