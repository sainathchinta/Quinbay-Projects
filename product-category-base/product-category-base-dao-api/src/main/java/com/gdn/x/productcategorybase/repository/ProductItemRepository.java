package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;

public interface ProductItemRepository extends JpaRepository<ProductItem, String>, ProductItemRepositoryCustom {

  Long countByStoreIdAndSkuCode(String storeId, String skuCode);

  Page<ProductItem> findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String generatedItemName, Pageable pageable);

  List<ProductItem> findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(String storeId, String sellerCode,
      List<String> omniChannelSkus);

  ProductItem findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  List<ProductItem> findByStoreIdAndMarkForDeleteFalse(String storeId);

  Page<ProductItem> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  ProductItem findByStoreIdAndSkuCodeAndMarkForDeleteFalse(String storeId, String skuCode);

  List<ProductItem> findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(String storeId, List<String> skuCodeList);

  List<ProductItem> findByStoreIdAndSkuCodeIn(String storeId, List<String> skuCodeList);

  List<ProductItem> findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(String
      storeId, String upcCode, boolean activated);

  Page<ProductItem> findByStoreIdAndIdIn(String storeId, List<String> productItemIds, Pageable pageable);

  Page<ProductItem> findByStoreIdAndUpcCodeIgnoreCaseAndMarkForDeleteFalse(String storeId, String
      upcCode, Pageable pageable);

  @Query(value = "SELECT p FROM ProductItem p WHERE p.storeId = :storeId AND p.upcCode LIKE :upcCode% AND p.markForDelete = false")
  Page<ProductItem> findByStoreIdAndUpcCodeStartingWithIgnoreCaseAndMarkForDeleteFalse(@Param("storeId") String
      storeId, @Param("upcCode") String upcCode, Pageable pageable);

  @Query("SELECT t FROM ProductItem t  where t.product.promoSKU = false and t.storeId = ?1 "
      + "AND t.markForDelete = false AND t.viewable = ?2 AND (( upper(t.generatedItemName) like '%'"
      + " || ?3 || '%') OR  (upper(t.upcCode) like '%' || ?4  || '%' )) ")
  Page<ProductItem> findProductItemList(String storeId, boolean viewable, String generatedItemName,
      String upcCode, Pageable pageable);

  @Query("SELECT t FROM ProductItem t  where t.storeId = ?1 "
      + "AND t.markForDelete = false AND t.viewable = ?2 AND (( upper(t.generatedItemName) like '%'"
      + " || ?3 || '%') OR  (upper(t.upcCode) like '%' || ?4  || '%' )) ")
  Page<ProductItem> findAllProductItemList(String storeId, boolean viewable, String generatedItemName,
      String upcCode, Pageable pageable);

  @Query("SELECT DISTINCT p.productCode,p.name FROM ProductItem t INNER JOIN t.product p where t.storeId = ?1"
      + " AND t.markForDelete = false  AND t.upcCode is not null "
      + " AND t.activated = true AND t.viewable = true AND p.activated = true AND p.viewable = true"
      + " AND (( upper(t.generatedItemName) like '%' || ?2 || '%') OR  (upper(t.upcCode) like ?3  || '%' ))")
  Page<Object[]> findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndUpcCodeIsNotNullOrUpcCodeAndMarkForDeleteFalse(
      String storeId, String generatedItemName, String upcCode, Pageable pageable) throws Exception;

  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByProductCode(String productCode);

  @Query(
      value = "SELECT piimg.location_path, piimg.active, piimg.original_image FROM pcc_product p"
          + " JOIN pcc_product_item pi ON pi.product_id=p.id"
          + " JOIN pcc_product_item_images piimg ON piimg.product_item_id=pi.id"
          + " WHERE piimg.mark_for_delete=false AND pi.mark_for_delete=false AND p.store_id=?1 AND p.product_code=?2",
      nativeQuery = true)
  List<Object[]> getAllProductItemImagesByProductCode(String storeId, String productCode);

  @Query(value = "SELECT p FROM ProductItem p WHERE p.product IN (SELECT pc FROM Product pc WHERE "
      + "pc.productCode IN (?1)) AND p.activated = true AND p.viewable = true AND p.product.promoSKU = false AND p.markForDelete = false")
  List<ProductItem> findByListProductCodeWithActivatedAndViewableTrueAndPromoSkuFalse(List<String> productCodes);

  @Query(value = "SELECT p FROM ProductItem p WHERE p.product IN (SELECT pc FROM Product pc WHERE "
      + "pc.productCode IN (?1)) AND p.product.promoSKU = false AND p.markForDelete = false")
  List<ProductItem> findByListProductCodeWithPromoSkuFalseAndMarkForDeleteFalse(List<String> productCodes);

  @Query(value = "SELECT p FROM ProductItem p WHERE p.product IN (SELECT pc FROM Product pc WHERE "
      + "pc.productCode IN (?1)) AND p.activated = true AND p.viewable = true AND p.markForDelete = false")
  List<ProductItem> findByListProductCodeWithActivatedTrueAndViewableTrue(List<String> productCodes);

  @Query(value = "SELECT p FROM ProductItem p WHERE p.product IN (SELECT pc FROM Product pc WHERE "
      + "pc.productCode IN (?1)) AND p.markForDelete = false")
  List<ProductItem> findByListProductCodeWithMarkForDeleteFalse(List<String> productCodes);

  @Query(value = "select pi.id from pcc_product_item pi join pcc_product p on pi.product_id = p.id "
      + "join pcc_product_category pc on p.id = pc.product_id "
      + "where pi.upc_code = ?1 and pi.is_activated = true and pi.is_viewable = true and pi.mark_for_delete = FALSE and p.review_pending = false and p.is_promo_sku = FALSE "
      + "and p.mark_for_delete = FALSE and  pc.mark_for_delete = FALSE and pc.category_id in (?2)", nativeQuery = true)
  List<String> findByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(String upcCode, List<String> categoryIds);

  @Query(value = "select pi.id from pcc_product_item pi join pcc_product p on pi.product_id = p.id "
      + "join pcc_product_category pc on p.id = pc.product_id "
      + "where pi.upc_code = ?1 and pi.is_activated = true and pi.is_viewable = true and pi.mark_for_delete = FALSE and p.review_pending = false  "
      + "and p.mark_for_delete = FALSE and pc.mark_for_delete = FALSE and pc.category_id in (?2)", nativeQuery = true)
  List<String> findByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(String upcCode, List<String> categoryIds);

  @Query(value =
      "SELECT p.product.id FROM ProductItem p WHERE p.upcCode = ?1 AND p.activated = true AND p.viewable = true "
          + "AND p.product.reviewPending = false AND p.product.promoSKU = false AND p.markForDelete = false")
  List<String> findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(String upcCode);

  @Query(value =
      "SELECT p.product.id FROM ProductItem p WHERE p.upcCode = ?1 AND p.activated = true AND p.viewable = true "
          + "AND p.product.reviewPending = false AND p.markForDelete = false")
  List<String> findProductIdByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(String upcCode);

  Page<ProductItem> findByStoreIdAndProductAndActivatedTrueAndViewableTrueAndMarkForDeleteFalseOrderBySkuCode(
      String storeId, Product product, Pageable pageable);

  List<ProductItem> findByStoreIdAndProductId(String storeId, String productId);

  @Query(value = "SELECT pi.generated_item_name FROM pcc_product_item pi JOIN pcc_product p ON pi.product_id = p.id "
      + "WHERE pi.mark_for_delete = FALSE AND p.mark_for_delete = FALSE AND pi.upc_code = ?1 AND p.product_code= ?2 "
      + "AND pi.sku_code != ?3", nativeQuery = true)
  List<String> findItemNameByWithUPCCodeAndProductCodeMarkForDeleteFalse(String upcCode, String productCode,
      String skuCode);

  ProductItem findByStoreIdAndSkuCode(String storeId, String skuCode);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_item WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;

  @Query(value = "SELECT pi.sku_code FROM pcc_product_item pi JOIN pcc_product p ON pi.product_id = p.id "
      + "WHERE pi.mark_for_delete = FALSE AND p.mark_for_delete = FALSE AND pi.upc_code IN ?1 AND p.product_code= ?2 "
      + "AND pi.sku_code NOT IN ?3", nativeQuery = true)
  List<String> findItemCodeByUPCCodeAndProductCodeAndSkuCodesInMarkForDeleteFalse(List<String> upcCodes,
      String productCode, List<String> skuCode);

  @Query(value = "SELECT pi.sku_code FROM pcc_product_item pi JOIN pcc_product p ON pi.product_id = p.id "
      + "WHERE pi.mark_for_delete = FALSE AND p.mark_for_delete = FALSE AND pi.upc_code IN ?1 "
      + "AND p.product_code= ?2", nativeQuery = true)
  List<String> findItemCodeByUPCCodeAndProductCodeAndMarkForDeleteFalse(List<String> upcCodes, String productCode);

  ProductItem findFirstBySourceItemCodeIn(List<String> sourceItemCode);
}