package com.gdn.mta.product.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;

public interface ProductCollectionRepository
    extends JpaRepository<ProductCollection, String>, JpaSpecificationExecutor<ProductCollection> {
  
  ProductCollection findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);
  
  ProductCollection findByStoreIdAndProductCode(String storeId, String productCode);
  
  @Query("SELECT pc FROM ProductCollection pc WHERE (pc.updatedStepDate BETWEEN ?1 AND ?2)"
      + " AND pc.activated = ?3 AND pc.viewable = ?4 AND pc.markForDelete IS FALSE")
  List<ProductCollection> findByStoreIdAndUpdatedDateBetweenAndActivatedAndViewableAndMarkForDeleteFalse(
      Date startUpdatedDate, Date endUpdatedDate, boolean activated, boolean viewable);

  @Query("SELECT pc FROM ProductCollection pc WHERE pc.storeId = ?1 "
             + "AND pc.markForDelete IS FALSE AND pc.state = 'NEED_CORRECTION' AND pc.updatedStepDate <= ?2")
  List<ProductCollection> findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(String storeId, Date maxLastUpdatedDate);

  ProductCollection findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId);

  @Modifying(clearAutomatically = true)
  @Query("update ProductCollection pc set pc.approvalStatus = ?3, pc.approvalStatusTimestamp = ?4 WHERE pc.storeId = ?1 AND pc.productCode = ?2 AND pc.approvalStatusTimestamp < ?4")
  void updateProductStateByProductCode(String storeId, String productCode, int status, Date timestamp);

  @Query("SELECT pc FROM ProductCollection pc where pc.storeId = :storeId AND pc.productId IN (:productIds)")
  List<ProductCollection> findByStoreIdAndProductIds(@Param("storeId") String storeId,
      @Param("productIds") List<String> productIds);

  @Query("SELECT pc FROM ProductCollection pc where pc.storeId = :storeId AND pc.productId IN (:productIds) AND pc.markForDelete = false AND pc.postLive = false AND (pc.viewable = false OR pc.edited = true)")
  List<ProductCollection> findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(
      @Param("storeId") String storeId, @Param("productIds") Set<String> productIds);

  List<ProductCollection> findByStoreIdAndProductIdInAndMarkForDeleteFalse(String storeId, Set<String> productIds);

  @Query("SELECT id FROM ProductCollection pc where pc.storeId = :storeId AND ((pc.state = 'IN_PROGRESS' AND pc.activated = true AND pc.markForDelete = false) OR (pc.state = 'ACTIVE' AND pc.markForDelete = true))")
  List<String> findInactiveProducts(@Param("storeId") String storeId);

  @Query("SELECT pc FROM ProductCollection pc WHERE pc.productCode IN (?1) AND pc.storeId = ?2"
             + " AND pc.activated IS TRUE AND pc.viewable IS TRUE AND pc.markForDelete IS FALSE "
             + "ORDER BY pc.updatedStepDate DESC")
  List<ProductCollection>
  findByProductCodeInAndStoreIdAndActivatedTrueAndViewableTrueAndMarkForDeleteFalse(
      List<String> productCodes, String storeId);

  @Modifying(clearAutomatically = true)
  @Query("UPDATE ProductCollection pc set pc.reviewPending = true, pc.stuckProductRetryCount = pc.stuckProductRetryCount + 1 "
      + "where pc.storeId = :storeId and pc.productCode in :productCodeList")
  void updateCronJobRetryCount(@Param("storeId") String storeId, @Param("productCodeList") List<String> productCodeList);

  Page<ProductCollection>
  findByStoreIdAndActivatedAndViewableAndMarkForDeleteFalseAndStateAndImageResizedTrueOrderByProductCodeAsc(
      String storeId, boolean activated, boolean viewable, String state, Pageable pageable);

  Page<ProductCollection>
  findByStoreIdAndActivatedAndReviewPendingAndMarkForDeleteFalseAndImageResizedTrueOrderByProductCodeAsc(
      String storeId, boolean activated, boolean reviewPending, Pageable pageable);

  Page<ProductCollection> findByStoreIdAndImageResizedTrueAndUpdatedDateBetweenOrderByProductCodeAsc(String storeId,
      Date startDate, Date endDate, Pageable pageable);

  @Modifying(clearAutomatically = true)
  @Query("UPDATE ProductCollection pc set pc.assignedBy = :assignedBy , pc.assignedTo = :assignedTo , "
      + "pc.updatedDate = CURRENT_TIMESTAMP where productCode in :productCodeList "
      + "and pc.markForDelete = false and pc.state = 'DRAFT' and storeId = :storeId")
  int updateAssignmentStatus(@Param("storeId") String storeId, @Param("productCodeList") List<String> productCodeList,
      @Param("assignedTo") String assignedTo, @Param("assignedBy") String assignedBy);

  @Query("select pc.id from ProductCollection pc where pc.storeId = :storeId and pc.productCode = :productCode")
  String findIdByStoreIdAndProductCode(@Param("storeId") String storeId, @Param("productCode") String productCode);

  Page<ProductCollection> findByBrandCodeAndBrandApprovalStatusAndMarkForDeleteFalse(String brandRequestCode,
      BrandApprovalStatus brandApprovalStatus, Pageable pageable);

  Page<ProductCollection> findByStoreIdAndStateInAndViewableAndImageResizedAndImageResizeRetryLessThanAndMarkForDeleteFalseAndUpdatedDateLessThanOrderByUpdatedDateDesc(
      String storeId, List<String> state, boolean viewable, boolean imageResized, int maxRetry, Date date,
      Pageable pageable);

  @Modifying(clearAutomatically = true)
  @Query("update ProductCollection pc set pc.brandCode = :brandCode, pc.brandApprovalStatus = :brandApprovalStatus, pc.brand = :brandName "
             + "WHERE pc.productCode = :productCode")
  int updateBrandCodeAndBrandApprovalStatus(@Param("brandCode") String brandCode,
      @Param("brandApprovalStatus") BrandApprovalStatus brandApprovalStatus, @Param("brandName") String brandName,
      @Param("productCode") String productCode);

  @Modifying(clearAutomatically = true)
  @Query("update ProductCollection pc set pc.brandApprovalStatus = :brandApprovalStatus "
             + "WHERE pc.productCode = :productCode")
  int updateBrandApprovalStatus(@Param("productCode") String productCode,
      @Param("brandApprovalStatus") BrandApprovalStatus brandApprovalStatus);

  @Query("SELECT pc FROM ProductCollection pc WHERE pc.productCode IN (?1) AND pc.storeId = ?2 AND pc.markForDelete IS "
             + "TRUE")
  List<ProductCollection> findByProductCodeInAndStoreIdAndMarkForDeleteTrue(List<String> productCodes, String storeId);

  @Query("Select reviewerNotes from ProductCollection where storeId = ?1 and productCode = ?2")
  String getReviewerNotesByStoreIdAndProductCode(String storeId, String productCode);

  @Query("SELECT pc FROM ProductCollection pc WHERE pc.productCode IN (?1) AND pc.storeId = ?2 AND pc.markForDelete IS "
      + "FALSE")
  List<ProductCollection> findByProductCodeInAndStoreIdAndMarkForDeleteFalse(List<String> productCodes, String storeId);

  Page<ProductCollection> findByStoreIdAndStateInAndMarkForDeleteFalseOrderByProductCode(
      String storeId, List<String> states, Pageable pageable);

  List<ProductCollection> findByStoreIdAndProductNameAndBusinessPartnerCodeAndMarkForDeleteFalse(String storeId,
      String productName, String businessPartnerCode);

  List<ProductCollection> findByBusinessPartnerCodeAndProductCodeIn(String merchantCode, List<String> productCode);

  List<ProductCollection> findByStoreIdAndProductCodeIn(String storeId, List<String> productCode);

  @Query(value = "SELECT product_code FROM PRD_PRODUCT_COLLECTION ppc "
      + "INNER JOIN PRD_PRODUCT_BUSINESS_PARTNER ppbp ON ppc.PRODUCT_ID=ppbp.PRODUCT_ID  "
      + "WHERE ppbp.GDN_PRODUCT_SKU = ?1", nativeQuery = true)
  String getProductCodeByGdnSku(String productSku);

  @Query(value = "SELECT ppc.* FROM PRD_PRODUCT_COLLECTION ppc INNER JOIN PRD_PRODUCT_BUSINESS_PARTNER ppbp ON "
      + "ppc.PRODUCT_ID=ppbp.PRODUCT_ID WHERE ppbp.GDN_PRODUCT_SKU = ?1", nativeQuery = true)
  ProductCollection findProductByGdnSku(String productSku);

  @Modifying(clearAutomatically = true)
  @Query("update ProductCollection pc set pc.autoApprovalType = :autoApprovalType "
             + "WHERE pc.productCode = :productCode")
  int updateAutoApprovalTypeByProductCode(@Param("autoApprovalType") AutoApprovalType autoApprovalType,
      @Param("productCode") String productCode);

  List<ProductCollection> findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(String storeId,
    Date startUpdatedDate, Date endUpdatedDate);

  Page<ProductCollection> findByStoreIdAndStateAndReviewPendingFalseAndUpdatedDateBetweenAndMarkForDeleteFalse(
      String storeId, String state, Date startUpdatedDate, Date endUpdatedDate, Pageable pageable);

  Page<ProductCollection> findByStoreIdAndStateAndPostLiveAndEditedAndResubmitCountAndImageResizedTrueAndUpdatedDateBetweenAndMarkForDeleteFalse(
      String storeId, String state, boolean postLive, boolean edited, int resubmitCount, Date startUpdatedDate,
      Date endUpdatedDate, Pageable pageable);

  List<ProductCollection> findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
      Date fromDate, Date tillDate);

  long deleteByStoreIdAndProductCode(String storeId, String productCode);

  Page<ProductCollection> findByAddDeleteVariantStatusAndMarkForDeleteFalseAndCreatedDateBefore(
      AddDeleteVariantStatus addDeleteVariantStatus, Date hoursBefore, Pageable pageable);

  @Modifying
  @Query("UPDATE ProductCollection pc SET pc.addDeleteVariantStatus = :status WHERE pc.productCode "
    + "= :productCode")
  void updateAddDeleteVariantStatus(@Param("productCode") String productCode, @Param("status") AddDeleteVariantStatus status);


  @Query("SELECT new com.gda.mta.product.dto.response.ProductCodeAndNameDetails(pc.productCode, pc.productName) "
      + "FROM ProductCollection pc "
      + "WHERE pc.businessPartnerCode = :businessPartnerCode AND pc.state = 'NEED_CORRECTION' AND pc.updatedDate > :updatedDate order by updatedDate")
  Page<ProductCodeAndNameDetails> findNeedRevisionProductsDetailsByBusinessPartnerCodeUpdatedDate(
      @Param("businessPartnerCode") String businessPartnerCode, @Param("updatedDate") Date updatedDate, Pageable pageable);
}
