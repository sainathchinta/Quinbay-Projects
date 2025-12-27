package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;

public interface ProductImageQcProcessingResponseRepository
    extends JpaRepository<ProductImageQcProcessingResponse, String> {

  ProductImageQcProcessingResponse findByStoreIdAndProductCode(String storeId, String productCode);

  List<ProductImageQcProcessingResponse> findByStoreIdAndProductCodeIn(String storeId, List<String> productCodes);

  long deleteByStoreIdAndProductCode(String storeId, String productCode);
}
