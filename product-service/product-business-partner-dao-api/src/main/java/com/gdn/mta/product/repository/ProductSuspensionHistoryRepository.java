package com.gdn.mta.product.repository;

import java.util.List;

import jakarta.persistence.PersistenceException;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.mta.product.entity.ProductSuspensionHistory;


public interface ProductSuspensionHistoryRepository extends JpaRepository<ProductSuspensionHistory, String> {

  Page<ProductSuspensionHistory> findByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId,
      String productSku, Pageable pageable) throws PersistenceException;

  List<ProductSuspensionHistory> findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  ProductSuspensionHistory findTop1ByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId,
      String productSku);

  @Modifying
  @Query(value = "DELETE FROM PRD_PRODUCT_SUSPENSION_HISTORY WHERE store_id = ?1 AND product_sku IN (SELECT "
      + "gdn_product_sku FROM PRD_PRODUCT_BUSINESS_PARTNER WHERE store_id = ?1 AND product_id = ?2)", nativeQuery = true)
  void deleteByStoreIdAndProductId(String storeId, String productId);
}
