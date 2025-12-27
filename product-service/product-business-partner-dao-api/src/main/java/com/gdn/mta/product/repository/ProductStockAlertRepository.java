package com.gdn.mta.product.repository;

import com.gdn.mta.product.entity.PbpStockAlert;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Date;
import java.util.List;

import jakarta.persistence.PersistenceException;

public interface ProductStockAlertRepository extends
		JpaRepository<PbpStockAlert, String> {
  
    PbpStockAlert findByGdnSkuAndMarkForDeleteFalse(String gdnSku);
  
    List<PbpStockAlert> findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(String gdnSku);
    
    List<PbpStockAlert> findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(String gdnSku);

    List<PbpStockAlert> findByGdnSkuOrderByUpdatedDateDesc(String gdnSku);

    @Query("SELECT psa from PbpStockAlert psa WHERE psa.markForDelete = false and psa.isOos = true and psa.oosAlertAttempt = ?1")
    Page<PbpStockAlert> findByOosAlertAttempt(Integer oosAlertAttempt, Pageable pageable) throws PersistenceException;

    @Query("SELECT psa from PbpStockAlert psa WHERE psa.markForDelete = false and psa.businessPartnerCode = ?1 and psa.oosAlertAttempt < ?2 and (psa.isMinimumStock = true or psa.isOos = true)")
    List<PbpStockAlert> findPbpStockAlertByBusinessPartnerCode(String businesspartnerCode, Integer maxStockAlertAttempt)
        throws PersistenceException;

    @Query("SELECT psa.gdnSku from PbpStockAlert psa WHERE psa.markForDelete = false and psa.businessPartnerCode = ?1 and psa.oosAlertAttempt <= ?2 and (psa.isMinimumStock = true or psa.isOos = true)")
    List<String> findGdnSkuStockAlertByBusinessPartnerCode(String businesspartnerCode, Integer maxStockAlertAttempt)
        throws PersistenceException;

	  @Modifying(clearAutomatically = true)
    @Query("update PbpStockAlert psa set psa.oosAlertAttempt = ?2, psa.updatedDate = CURRENT_TIMESTAMP where psa.gdnSku = ?1 and psa.markForDelete = false")
    void updateOosAlertAttempt(String gdnSku, Integer oosAlertAttempt);

    @Modifying(clearAutomatically = true)
    @Query("update PbpStockAlert psa set psa.oosAlertAttempt = ?2, psa.updatedDate = CURRENT_TIMESTAMP where psa.id = ?1 and psa.markForDelete = false")
    void updateOosAlertAttemptById(String id, Integer oosAlertAttempt);


    @Query(value = "SELECT DISTINCT BUSINESS_PARTNER_CODE FROM PRD_PBP_STOCK_ALERT WHERE MARK_FOR_DELETE = false and OOS_ALERT_ATTEMPT < ?1 and (IS_MINIMUM_STOCK = true or IS_OOS = true)", nativeQuery = true)
    List<String> findListBusinessPartnerMinimumStock(Integer maxStockAlertAttempt) throws PersistenceException;

    @Query(value = "SELECT COUNT(GDN_SKU) FROM PRD_PBP_STOCK_ALERT WHERE MARK_FOR_DELETE = FALSE AND BUSINESS_PARTNER_CODE = ?1 AND IS_MINIMUM_STOCK = TRUE AND IS_OOS = FALSE", nativeQuery = true)
    Integer countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(String businesspartnerCode)
        throws PersistenceException;

    @Query(value = "SELECT COUNT(GDN_SKU) FROM PRD_PBP_STOCK_ALERT WHERE MARK_FOR_DELETE = FALSE AND BUSINESS_PARTNER_CODE = ?1 AND OOS_ALERT_ATTEMPT <= ?2 and IS_OOS = TRUE", nativeQuery = true)
    Integer countGdnSkuWithOosStockAlertByBusinessPartnerCode(String businesspartnerCode, Integer maxStockAlertAttempt)
        throws PersistenceException;

    @Query(value = "SELECT AVAILABLE_STOCK FROM PRD_PBP_STOCK_ALERT WHERE MARK_FOR_DELETE = FALSE AND BUSINESS_PARTNER_CODE = ?1 AND AVAILABLE_STOCK > 0 LIMIT 1", nativeQuery = true)
    Integer findAvailableStockByBusinessPartnerCode(String businessPartnerCode) throws PersistenceException;

    Page<PbpStockAlert> findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
        String storeId, Date date, Pageable pageable);

    Page<PbpStockAlert> findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(String storeId,
        Date date, Pageable pageable);

    @Modifying
    @Query(value = "DELETE FROM PRD_PBP_STOCK_ALERT WHERE gdn_sku IN (SELECT gdn_product_item_sku FROM "
        + "PRD_PRODUCT_ITEM_BUSINESS_PARTNER WHERE product_business_partner_id IN (SELECT id FROM "
        + "PRD_PRODUCT_BUSINESS_PARTNER WHERE store_id = ?1 AND product_id = ?2))", nativeQuery = true)
    void deleteByStoreIdAndProductId(String storeId, String productId);
}
