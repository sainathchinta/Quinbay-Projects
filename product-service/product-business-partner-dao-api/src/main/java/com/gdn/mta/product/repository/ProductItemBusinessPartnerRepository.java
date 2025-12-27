package com.gdn.mta.product.repository;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;

public interface ProductItemBusinessPartnerRepository extends JpaRepository<ProductItemBusinessPartner, String> {

  ProductItemBusinessPartner findByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(String storeId, String productItemSku);

  ProductItemBusinessPartner findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(String storeId, String productItemSku,
      String pickupPointId);

  @Query(
      "select productCode from ProductCollection where productId = (select productId from ProductBusinessPartner where "
          + "id = (select productBusinessPartner.id from ProductItemBusinessPartner where gdnProductItemSku = :itemSku "
          + "and markForDelete = false)) and reviewPending = true")
  String findProductCodeByItemSku(@Param("itemSku") String itemSku);

  ProductItemBusinessPartner findFirstByStoreIdAndProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
      String storeId, String businessPartnerId, boolean fbbActive);

  @Modifying
  @Query("update ProductItemBusinessPartner set merchantSku = ?3 where storeId = ?1 and gdnProductItemSku = ?2 and markForDelete = false")
  void updateMerchantSkuByStoreIdAndItemSku(String storeId, String itemSku, String merchantSku);

  List<ProductItemBusinessPartner> findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(String storeId,
    List<String> itemSkus);

  List<ProductItemBusinessPartner> findByStoreIdAndGdnProductItemSku(String storeId, String itemSku);

  List<ProductItemBusinessPartner> findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuAndPickupPointIdIn(
      String storeId, String itemSku, List<String> pickupPoints);

  List<ProductItemBusinessPartner> findByStoreIdAndMarkForDeleteFalseAndProductItemIdIn(String storeId,
      List<String> productItemId);

  List<ProductItemBusinessPartner> findByStoreIdAndProductItemIdIn(String storeId, Collection<String> productItemId);

  List<ProductItemBusinessPartner> findByStoreIdAndProductItemIdAndMarkForDeleteFalse(String storeId, String productItemId);

  Page<ProductItemBusinessPartner> findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalseOrderByGdnProductItemSku(String storeId,
      String productBusinessPartnerId, Pageable pageable);

  List<ProductItemBusinessPartner> findByStoreIdAndProductBusinessPartnerId(String storeId, String productBusinessPartnerId);

  List<ProductItemBusinessPartner> findByStoreIdAndProductBusinessPartnerIdAndMarkForDeleteFalse(String storeId, String productBusinessPartnerId);

  List<ProductItemBusinessPartner> findFirst2ByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(String storeId,
    String itemSku);

  @Modifying
  @Query("update ProductItemBusinessPartner set markForDelete = true, fbbActive = false, updatedDate = CURRENT_TIMESTAMP, "
      + "updatedBy = ?1 where storeId = " + "?2 and gdnProductItemSku IN ?3 and pickupPointId = ?4 ")
  void updateMarkForDeleteTrueAndFbbFalseByStoreIdAndItemSkuInAndPickupPointId(String username, String storeId,
      List<String> gdnProductItemSku, String pickupPointId);

  @Modifying
  @Query("update ProductItemBusinessPartner set markForDelete = true, updatedDate = CURRENT_TIMESTAMP, "
    + "updatedBy = ?1 where storeId = " + "?2 and gdnProductItemSku IN ?3 and pickupPointId = ?4 ")
  void updateMarkForDeleteByStoreIdAndItemSkuInAndPickupPointId(String username, String storeId,
    List<String> gdnProductItemSku, String pickupPointId);

  Page<ProductItemBusinessPartner> findByStoreIdAndGdnProductItemSkuInAndFbbActiveTrueAndMarkForDeleteFalse(
      String storeId, List<String> itemSKuList, Pageable pageable);

  @Modifying
  @Query("update ProductItemBusinessPartner set fbbActive = ?4, updatedDate = CURRENT_TIMESTAMP "
    + "where storeId = ?1 and productBusinessPartner.id IN ?2 and pickupPointId = ?3 ")
  void updateFbbFlagByStoreIdAndProductBusinessPartnerIdInAndPickupPointId(String storeId,
    String productBusinessPartnerId, String pickupPointId, boolean fbbActive);

  ProductItemBusinessPartner findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(String productBusinessPartnerId,
      boolean fbbActive);

  ProductItemBusinessPartner findFirstByProductBusinessPartnerIdAndCncActiveAndMarkForDeleteFalse(String productBusinessPartnerId,
      boolean cncActive);

  @Modifying
  @Query(value = "DELETE FROM PRD_PRODUCT_ITEM_BUSINESS_PARTNER WHERE product_business_partner_id IN (SELECT id "
      + "FROM PRD_PRODUCT_BUSINESS_PARTNER WHERE store_id = ?1 AND product_id = ?2)", nativeQuery = true)
  void deleteByStoreIdAndProductId(String storeId, String productId);

  ProductItemBusinessPartner findByStoreIdAndGdnProductItemSkuAndPickupPointId(String storeId, String productItemSku,
    String pickupPointId);
}
