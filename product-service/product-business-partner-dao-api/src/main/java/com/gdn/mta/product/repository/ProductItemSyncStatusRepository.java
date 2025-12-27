package com.gdn.mta.product.repository;

import java.util.Date;
import java.util.List;

import com.gdn.mta.product.entity.ProductItemSyncProcessSummary;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.enums.ProductSyncStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author anand
 * @since Sep 2019
 */
public interface ProductItemSyncStatusRepository extends JpaRepository<ProductItemSyncStatus, String> {

  ProductItemSyncStatus findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(String storeId, String businessPartnerCode,
    String itemSku);

  List<ProductItemSyncStatus> findByStoreIdAndBusinessPartnerCodeAndLinkedBusinessPartnerCodeAndProductSyncStatus(
    String storeId, String businessPartnerCode, String linkedPartnerCode, ProductSyncStatus productSyncStatus);

  List<ProductItemSyncStatus> findByStoreIdAndBusinessPartnerCodeAndLinkedBusinessPartnerCodeAndGdnItemSkuIn(
    String storeId, String businessPartnerCode, String linkedPartnerCode, List<String> itemSKUs);

  @Query(value = "SELECT new com.gdn.mta.product.entity.ProductItemSyncProcessSummary("
    + "productItemSyncStatus.productSyncStatus as productSyncStatus, count(productItemSyncStatus) as count, "
    + "productItemSyncStatus.businessPartnerCode as businessPartnerCode, productItemSyncStatus.processId as processId) "
    + "from com.gdn.mta.product.entity.ProductItemSyncStatus productItemSyncStatus "
    + "where productItemSyncStatus.storeId=:storeId AND productItemSyncStatus.processId=:processId "
    + "group by productItemSyncStatus.productSyncStatus, productItemSyncStatus.businessPartnerCode, productItemSyncStatus.processId ")
  List<ProductItemSyncProcessSummary> getCountByProcessIdGroupByProductSyncStatus(@Param("storeId") String storeId,
    @Param("processId") String processId);

  @Modifying
  @Transactional
  @Query(value = "UPDATE com.gdn.mta.product.entity.ProductItemSyncStatus syncStatus SET syncStatus.productSyncStatus =:newStatus "
      + "WHERE syncStatus.storeId=:storeId AND syncStatus.productSyncStatus =:existingStatus AND syncStatus.updatedDate <:retryInterval")
  int updateSyncStatusForSyncRetry(@Param("storeId") String storeId, @Param("retryInterval") Date retryInterval,
    @Param("existingStatus") ProductSyncStatus existingStatus, @Param("newStatus") ProductSyncStatus newStatus);

}
