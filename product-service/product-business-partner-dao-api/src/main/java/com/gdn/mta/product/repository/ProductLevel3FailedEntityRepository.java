package com.gdn.mta.product.repository;

import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ProductLevel3FailedEntityRepository extends JpaRepository<ProductLevel3FailedEntity, String> {

  ProductLevel3FailedEntity findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  Page<ProductLevel3FailedEntity> findByStoreIdAndRetryCountLessThanEqualAndMarkForDeleteFalse(String storeId,
    int maxRetryCount, Pageable pageable);

  /**
   *
   * @param storeId
   * @param maxRetryCount
   * @return
   */
  List<ProductLevel3FailedEntity> findByStoreIdAndRetryCountGreaterThanAndMarkForDeleteFalse(String storeId, int maxRetryCount);
}
