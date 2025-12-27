package com.gdn.x.productcategorybase.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.Product;


public interface ProductRepository extends JpaRepository<Product, String> {

  String FIND_BY_CATEGORY_QUERY = "SELECT p FROM Product p " + "INNER JOIN p.productCategories pc "
      + "WHERE pc.category.id = :categoryId " + "AND p.storeId = :storeId";

  String FIND_BY_CATEGORY_QUERY_FILTERED = "SELECT p FROM Product p " + "INNER JOIN p.productCategories pc "
    + "WHERE pc.category.id = :categoryId AND p.storeId = :storeId AND p.markForDelete = false AND p.activated=true "
    + "AND p.viewable=true";
  String VIDEO_UPDATE_QUERY =
    "UPDATE Product p SET p.video = :video, p.updatedDate = CURRENT_TIMESTAMP WHERE p.storeId = :storeId AND p.productCode = :productCode";

  Long countByStoreIdAndProductCode(String storeId, String productCode);

  @Query(value = "SELECT p FROM Product p WHERE p.storeId = :storeId AND p.brand LIKE :brand% AND p.markForDelete = false")
  Page<Product> findByStoreIdAndBrandStartingWithAndMarkForDeleteFalse(@Param("storeId") String storeId, @Param("brand") String brand, Pageable pageable);

  @Query(ProductRepository.FIND_BY_CATEGORY_QUERY_FILTERED)
  Page<Product> findByStoreIdAndCategoryIdAndMarkForDeleteFalseAndActivatedTrue(@Param("storeId") String storeId,
    @Param("categoryId") String categoryId, Pageable pageable);

  @Query(ProductRepository.FIND_BY_CATEGORY_QUERY)
  Page<Product> findByStoreIdAndCategoryIdAndMarkForDeleteFalse(@Param("storeId") String storeId,
      @Param("categoryId") String categoryId, Pageable pageable);

  Product findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  /**
   * find Product by Store Id and Id
   *
   * @param storeId
   * @param id
   * @return
   */
  Product findByStoreIdAndId(String storeId, String id);

  List<Product> findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDesc(String storeId);

  Page<Product> findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDesc(String storeId, Pageable pageable);

  Page<Product> findByStoreIdAndBrandIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String brandName, Pageable pageable);

  List<Product> findByStoreIdAndMarkForDeleteOrderByUpdatedDateDesc(String storeId, boolean markForDelete);

  Page<Product> findByStoreIdAndMarkForDeleteOrderByUpdatedDateDesc(String storeId, boolean markForDelete,
      Pageable pageable);

  List<Product> findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
      String storeId, String name, String createdBy);

  Page<Product> findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
      String storeId, String name, String createdBy, Pageable pageable);

  List<Product> findByStoreIdAndNameContainingIgnoreCaseAndMarkForDeleteFalse(String storeId, String name);

  Page<Product> findByStoreIdAndNameContainingIgnoreCaseAndMarkForDeleteFalse(String storeId, String name,
      Pageable pageable);

  List<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
      String storeId, boolean viewable, boolean activated, String name);

  Page<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
      String storeId, boolean viewable, boolean activated, String name, Pageable pageable);

  List<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
      String storeId, boolean viewable, boolean activated, String name, String updatedBy);

  Page<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
      String storeId, boolean viewable, boolean activated, String name, String updatedBy, Pageable pageable);

  Product findByStoreIdAndProductCode(String storeId, String productCode);

  Product findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  List<Product> findByStoreIdAndProductCodeContainingIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String productCode);

  Page<Product> findByStoreIdAndProductCodeContainingIgnoreCaseAndMarkForDeleteFalse(String storeId, String productCode,
      Pageable pageable);

  Page<Product> findByStoreIdAndShippingWeightGreaterThanEqualAndMarkForDeleteFalse(String storeId,
      Double shippingWeight, Pageable pageable);

  Page<Product> findByStoreIdAndShippingWeightLessThanEqualAndMarkForDeleteFalse(String storeId, Double shippingWeight,
      Pageable pageable);

  @Query(value = "SELECT p FROM Product p WHERE p.storeId = :storeId AND p.uniqueSellingPoint LIKE :uniqueSellingPoint% AND p.markForDelete = false")
  Page<Product> findByStoreIdAndUniqueSellingPointStartingWithAndMarkForDeleteFalse(@Param("storeId") String storeId,
      @Param("uniqueSellingPoint") String uniqueSellingPoint, Pageable pageable);

  List<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(String storeId,
      boolean viewable, boolean activated);

  Page<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(String storeId,
      boolean viewable, boolean activated, Pageable pageable);

  Page<Product> findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndReviewPendingFalseOrderByUpdatedDateDesc(String storeId,
      boolean viewable, boolean activated, Pageable pageable);

  List<Product> findByStoreIdAndViewableAndMarkForDeleteFalse(String storeId, boolean viewable);
  @Deprecated
  Page<Product> findByStoreIdAndViewableAndMarkForDeleteFalse(String storeId, boolean viewable, Pageable pageable);

  List<Product> findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(String storeId, Double weight);

  Page<Product> findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(String storeId, Double weight,
      Pageable pageable);

  List<Product> findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(String storeId, Double weight);

  Page<Product> findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(String storeId, Double weight,
      Pageable pageable);

  Page<Product> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode,
      Pageable pageable);

  Long countByStoreIdAndViewableAndMarkForDeleteFalse(String storeId, boolean viewable);
  
  @Query(value = "SELECT pimg.location_path, pimg.active, pimg.original_image FROM pcc_product p"
      + " JOIN pcc_product_images pimg ON pimg.product_id=p.id"
      + " WHERE pimg.mark_for_delete=false AND p.store_id=?1 AND p.product_code=?2",
      nativeQuery = true)
  List<Object[]> getAllProductImagesByProductCode(String storeId, String productCode);
  
  Long countByStoreIdAndBrandIgnoreCase(String storeId, String brandName);

  @Query("select p.productCode from Product p " +
      "where p.storeId = :storeId " +
      "order by p.productCode")
  Page<String> getAllProductCodes(@Param("storeId") String storeId, Pageable pageable);

  Page<Product> findByStoreIdAndReviewPendingFalseAndUpdatedDateBetween(String storeId, Date startDate, Date endDate, Pageable pageable);

  Slice<Product> findByStoreIdAndUpdatedByAndMarkForDeleteFalse(String storeId, String updatedBy,
      Pageable pageable);

  @Query("select p FROM Product p where p.storeId = :storeId and p.markForDelete = true and "
      + "p.updatedDate > :startDate and p.updatedDate < :endDate")
  Page<Product> findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateBetween(@Param("storeId") String storeId,
      @Param("startDate") Date startDate, @Param("endDate") Date endDate, Pageable pageable);

  @Query("select p FROM Product p where p.storeId = :storeId and p.markForDelete = true and "
      + "p.updatedDate < :updatedDate")
  Page<Product> findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateLessThan(@Param("storeId") String storeId,
      @Param("updatedDate") Date updatedDate, Pageable pageable);

  List<Product> findByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId, List<String> productCodes);

  @Query(value = "SELECT p.product_code FROM pcc_product p WHERE p.store_id = ?1 AND p.updated_date < ?2 AND p.mark_for_delete = TRUE AND p.picked_for_deletion = FALSE ORDER BY updated_date LIMIT ?3", nativeQuery = true)
  List<String> findByStoreIdAndUpdatedDateLessThanAndMarkForDeleteTrueAndPickedForDeletionFalse(String storeId,
      Date date, int limit);

  @Modifying
  @Query(value = "UPDATE pcc_product SET picked_for_deletion = ?3, updated_date = CURRENT_TIMESTAMP WHERE store_id = ?1 AND product_code IN ?2", nativeQuery = true)
  void updatePickedForDeletionByProductCode(String storeId, List<String> productCodes, boolean pickedForDeletion);

  @Modifying
  @Query(value = "DELETE FROM pcc_product WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;

  @Modifying
  @Query(value = VIDEO_UPDATE_QUERY)
  void updateVideoByStoreIDAndProductCode(@Param("storeId") String storeId,
    @Param("productCode") String productCode, @Param("video") String video);
}
