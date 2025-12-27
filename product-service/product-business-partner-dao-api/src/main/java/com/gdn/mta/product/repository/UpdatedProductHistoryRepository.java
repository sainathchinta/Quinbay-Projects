package com.gdn.mta.product.repository;


import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.product.entity.UpdatedProductHistory;

public interface UpdatedProductHistoryRepository extends JpaRepository<UpdatedProductHistory, String> {

  @Query("SELECT la FROM UpdatedProductHistory la where (la.gdnSku = ?1 OR (la.productSku = ?2 AND la.gdnSku = 'DEFAULT')) AND la.onlineStatus = true ORDER BY la.accessTime DESC, la.auditTrailId DESC")
  Page<UpdatedProductHistory> findByGdnSkuAndProductSkuAndOnlineStatusTrue(String gdnSku, String productSku, Pageable pageable);

  Long countByGdnSkuAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
      String gdnSku, Date fromDate, List<String> activityTypes);

  UpdatedProductHistory findFirstByGdnSkuAndPickupPointCodeAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(String gdnSku,
      String pickupPointCode, Date fromDate, List<String> activityTypes);

  Page<UpdatedProductHistory> findByProductSkuOrderByAccessTimeDesc(String productSku, Pageable pageable);

  @Query("SELECT la FROM UpdatedProductHistory la where "
      + "la.productSku = ?1 AND (la.gdnSku = ?2 OR la.gdnSku = 'DEFAULT' OR la.gdnName LIKE '%' || ?2 || '%' )"
      + "ORDER BY la.accessTime DESC")
  Page<UpdatedProductHistory> findByProductSkuAndKeyword(String productSku, String keyword, Pageable pageable);

  List<UpdatedProductHistory> findByAuditTrailIdInOrderByAccessTimeDesc(List<String> auditTrailId);

  @Query("SELECT auditTrailId FROM UpdatedProductHistory where accessTime <= :access_time")
  List<String> findAuditTrailIdByAccessTime(@Param("access_time") Date access_time, Pageable pageable);

  @Modifying
  @Query(value = "Delete from PRD_UPDATED_PRODUCT_HISTORY where access_time <= :access_time", nativeQuery = true)
  void deleteByAccessTime(@Param("access_time") Date access_time);

  @Modifying
  @Query(value = "Delete from PRD_UPDATED_PRODUCT_HISTORY where audit_trail_id in (:auditTrailIds)", nativeQuery = true)
  void deleteByAuditTrailIds(@Param("auditTrailIds") List<String> auditTrailIds);

  Page<UpdatedProductHistory> findByAccessTimeBetweenOrderByAccessTime(
      Date starteDate, Date endDate, Pageable pageable);

  Page<UpdatedProductHistory> findByProductSkuAndAccessTimeBetweenAndOnlineStatusTrueOrderByAccessTimeDesc(String productSku,
      Date startDate, Date endDate, Pageable pageable);

  @Query("SELECT la FROM UpdatedProductHistory la where "
      + "la.productSku = ?1 AND la.accessTime >= ?2 AND la.accessTime < ?3 AND lower(la.gdnName) LIKE '%' || lower(?4) || '%' "
      + "AND la.onlineStatus = true ORDER BY la.accessTime DESC")
  Page<UpdatedProductHistory> findByProductSkuAndAccessTimeBetweenAndVariantNameAndOnlineStatusTrue(String productSku,
      Date startDate, Date endDate, String keyword, Pageable pageable);

  @Query("SELECT la FROM UpdatedProductHistory la where "
      + "la.productSku = ?1 AND la.accessTime >= ?2 AND la.accessTime < ?3 AND lower(la.activity) LIKE '%' || lower(?4) || '%' "
      + "AND la.onlineStatus = true ORDER BY la.accessTime DESC")
  Page<UpdatedProductHistory> findByProductSkuAndAccessTimeBetweenAndActivityAndOnlineStatusTrue(String productSku, Date startDate,
      Date endDate, String keyword, Pageable pageable);

  Page<UpdatedProductHistory> findByGdnSkuAndPickupPointCodeAndOnlineStatusFalseOrderByAccessTimeDesc(String itemSku,
      String pickupPointCode, Pageable pageable);

  @Modifying
  @Query(value = "DELETE FROM PRD_UPDATED_PRODUCT_HISTORY WHERE product_sku IN (SELECT gdn_product_sku FROM "
      + "PRD_PRODUCT_BUSINESS_PARTNER WHERE store_id = ?1 AND product_id = ?2)", nativeQuery = true)
  void deleteByStoreIdAndProductId(String storeId, String productId);
}
