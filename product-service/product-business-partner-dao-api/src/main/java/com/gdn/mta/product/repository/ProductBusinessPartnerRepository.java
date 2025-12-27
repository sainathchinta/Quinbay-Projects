package com.gdn.mta.product.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import jakarta.persistence.PersistenceException;

import com.gdn.mta.product.entity.ProductSkuBusinessPartnerDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.InProgressProductsBySizeChartCodeResponse;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ItemSkuToItemIdMapping;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;

public interface ProductBusinessPartnerRepository extends JpaRepository<ProductBusinessPartner, String> {

  String QUERY_COUNT_REJECTED_PRODUCTS = "select count(1) FROM prd_product_business_partner pbp, prd_product_workflow pw, prd_product_collection pc "
      + "WHERE pbp.product_id = pw.product_id AND pw.product_id = pc.product_id AND pw.state = 4 "
      + "AND pbp.store_id = ?1 AND pw.store_id = ?1 AND pbp.business_partner_id = ?2 AND pbp.mark_for_delete = TRUE";

  String QUERY_COUNT_IN_PROGRESS_L5 =
    "select count(*) FROM prd_product_item_business_partner prdItemBusinessPartner JOIN prd_product_business_partner prdBusinessPartner ON prdItemBusinessPartner.product_business_partner_id = prdBusinessPartner.id where prdBusinessPartner.store_id = ?1 AND prdBusinessPartner.gdn_product_sku = ?2 AND prdItemBusinessPartner.mark_for_delete = FALSE";

  String FIND_PRODUCT_STATE_BY_ITEM_SKU_QUERY = "SELECT state FROM prd_product_business_partner WHERE "
      + "store_id = :storeId AND id IN (SELECT product_business_partner_id FROM prd_product_item_business_partner "
      + "WHERE gdn_product_item_sku = :itemSku)";
  String UPDATE_PRODUCT_MASTER_DATA_AND_SIZE_CHART_CODE =
      "UPDATE ProductBusinessPartner p " + "SET p.productName = :productName, "
          + "p.categoryCode = :categoryCode, " + "p.categoryName = :categoryName, "
          + "p.sizeChartCode = :sizeChartCode, " + "p.updatedDate = CURRENT_TIMESTAMP, "
          + "p.updatedBy = :updatedBy, " + "p.brand = :brand "
          + "WHERE p.gdnProductSku = :productSku";
  String UPDATE_SIZE_CHART_CODE =
    "UPDATE ProductBusinessPartner p " + "SET p.sizeChartCode = :sizeChartCode, "
      + "p.updatedDate = CURRENT_TIMESTAMP, " + "p.updatedBy = :updatedBy "
      + "WHERE p.gdnProductSku = :productSku";

  String UPDATE_SIZE_CHART_CODE_AND_BRAND =
      "UPDATE ProductBusinessPartner p " + "SET p.sizeChartCode = :sizeChartCode, "
          + "p.brand = :brand, " + "p.updatedDate = CURRENT_TIMESTAMP, "
          + "p.updatedBy = :updatedBy " + "WHERE p.gdnProductSku = :productSku";

  String UPDATE_BRAND = "UPDATE ProductBusinessPartner p " + "SET p.brand = :brand, "
      + "p.updatedDate = CURRENT_TIMESTAMP, " + "p.updatedBy = :updatedBy "
      + "WHERE p.gdnProductSku = :productSku";


  String UPDATE_ITEM_VIEW_CONFIG_AND_PRODUCT_TYPE =
    "update ProductItemBusinessPartner pibp set pibp.buyable = ?2 , pibp.display = ?3, "
      + "pibp.productType = case when ?4 is not null then ?4 else pibp.productType end "
      + "where pibp.productBusinessPartner.id in (select pbp.id from "
      + "ProductBusinessPartner pbp where pbp.productId = ?1)";


  Page<ProductBusinessPartner> findByStoreIdAndActivatedTrueAndMarkForDeleteFalse(String storedId, Pageable pageable);

  ProductBusinessPartner findByStoreIdAndBusinessPartnerIdAndGdnProductSkuAndMarkForDeleteFalse(String storeId,
      String businessPartnerId, String gdnProductSku) throws PersistenceException;

  ProductBusinessPartner findFirstByGdnProductSku(String gdnProductSku);

  Page<ProductBusinessPartner> findByStoreIdAndBusinessPartnerIdAndMarkForDeleteFalseOrderByCreatedDateDesc(
      String storeId, String businessPartnerId, Pageable pageable);

  Page<ProductBusinessPartner> findByStoreIdAndBusinessPartnerIdAndMarkForDeleteTrueOrderByCreatedDateDesc(
    String storeId, String businessPartnerId, Pageable pageable);

  Page<ProductBusinessPartner> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  @Query("SELECT pbp FROM ProductBusinessPartner pbp WHERE pbp.storeId = ?1 AND pbp.id"
      + " IN (SELECT pibp.productBusinessPartner.id FROM"
      + " ProductItemBusinessPartner pibp WHERE pibp.pickupPointId = ?2)" + " AND pbp.markForDelete = FALSE")
  Page<ProductBusinessPartner> findByStoreIdAndPickupPointIdAndMarkForDeleteFalse(String storeId, String pickupPointId,
      Pageable pageable) throws PersistenceException;

  @Query(value = "SELECT gdn_product_item_sku FROM PRD_PRODUCT_ITEM_BUSINESS_PARTNER pibp WHERE pibp.product_business_partner_id in" +
          "(SELECT id FROM PRD_PRODUCT_BUSINESS_PARTNER pbp WHERE pbp.gdn_product_sku = ?1) AND pibp.mark_for_delete = FALSE", nativeQuery = true)
  List<String> findListOfItemSkusbyProductSku(String productSku) throws PersistenceException;

  @Query(value = ProductBusinessPartnerRepository.FIND_PRODUCT_STATE_BY_ITEM_SKU_QUERY, nativeQuery = true)
  String findProductStateByStoreIdAndItemSku(@Param("storeId") String storeId, @Param("itemSku") String itemSku);

  @Query(
      "SELECT NEW com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse(pbp.gdnProductSku as productSku, pibp.pickupPointId as pickupPointCode, pibp.gdnProductItemSku as itemSku) "
          + "FROM ProductItemBusinessPartner pibp "
          + "join pibp.productBusinessPartner pbp " + "WHERE pbp.businessPartnerId = ?1 AND pbp.state IN ?3 "
          + "AND pibp.pickupPointId = ?2 AND pibp.markForDelete = FALSE order by pibp.gdnProductItemSku")
  Page<InProgressProductsByPickupPointCodeResponse> findProductDataInProgress(String businessPartnerId,
      String pickupPointId, List<String> stateList, Pageable pageable);

  @Query("SELECT NEW com.gda.mta.product.dto.response.InProgressProductsBySizeChartCodeResponse("
      + "pbp.gdnProductSku, pbp.sizeChartCode)" + " FROM ProductBusinessPartner pbp"
      + " WHERE pbp.storeId = ?1" + "  AND pbp.markForDelete = FALSE" + " AND pbp.state IN ?2"
      + "  AND pbp.sizeChartCode = ?3")
  Page<InProgressProductsBySizeChartCodeResponse> findInProgressProductsWithSizeChartCode(
      String storeId, List<String> stateList, String sizeChartCode, Pageable pageable);

  Page<ProductBusinessPartner> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId,
      Pageable pageable);

  List<ProductBusinessPartner> findByStoreIdAndProductId(String storeId, String productId);

  List<ProductBusinessPartner> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId,
      String productId) throws PersistenceException;

  @Query("select pbpa  FROM ProductBusinessPartnerAttribute pbpa where pbpa.attributeId in ?1 and "
      + "pbpa.productBusinessPartner = (from ProductBusinessPartner where productId = ?2)")
  List<ProductBusinessPartnerAttribute> findByProductId(List<String> attributeIdList,
      String productId);

  @Query("select DISTINCT piba.productType  FROM ProductItemBusinessPartner piba where "
      + "piba.productBusinessPartner.id in (select pbp.id from ProductBusinessPartner pbp where productId = ?1)")
  public Integer getProductTypeBasedOnProductId(String productId);


  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 AND pw.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "ORDER BY pw.updatedDate desc")
  Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerId(
      String storeId, String businessPartnerId, Pageable pageable);

  @Query(value = "SELECT product_business_partner_id FROM prd_product_item_business_partner WHERE store_id = ?1 "
      + "and merchant_sku = ?2 and product_business_partner_id in (SELECT id FROM prd_product_business_partner "
      + "WHERE store_id = ?1 and business_partner_id = ?3 and mark_for_delete = FALSE) and "
      + "mark_for_delete = FALSE LIMIT 1", nativeQuery = true)
  List<String> findByStoreIdAndMerchantSkuAndBusinessPartnerId(String storeId, String merchantSku,
      String businessPartnerId);

  List<ProductBusinessPartner> findByStoreIdAndId(String storeId, String id);

  @Query(value = ProductBusinessPartnerRepository.QUERY_COUNT_REJECTED_PRODUCTS, nativeQuery = true)
  Object countRejectedProductsByBusinessPartnerId(String storeId, String businessPartnerId);

  @Query(value = "select count(1) FROM prd_product_business_partner pbp WHERE pbp.state = 'DELETED' AND "
      + "pbp.store_id = ?1 AND pbp.business_partner_id = ?2 AND pbp.mark_for_delete = TRUE", nativeQuery = true)
  Object countRejectedProductsByBusinessPartnerIdFromPbp(String storeId, String businessPartnerId);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "AND lower(pbp.productName) LIKE '%' || lower(?3) || '%' "
      + "ORDER BY pw.updatedDate desc")
  Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerIdAndProductName(
      String storeId, String businessPartnerId, String productName, Pageable pageable);

  @Query(
      value = "select DISTINCT NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitted_date, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductBusinessPartner pbp, ProductWorkflow pw, ProductCollection pc, ProductItemBusinessPartner pibp "
      + "WHERE pbp.productId = pw.productId AND pw.productId=pc.productId AND pw.state = 4 "
      + "AND  pibp.productBusinessPartner.id = pbp.id "
      + "AND pbp.storeId = ?1 AND pw.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "AND pibp.merchantSku = ?3 "
      + "ORDER BY pw.updatedDate desc",
      countQuery = "SELECT COUNT(DISTINCT pbp.productName, pbp.categoryName, pbp.brand, pbp.createdDate, pbp.createdBy, pw.notes, pw.updatedDate, pc.productCode) "
      + "FROM ProductBusinessPartner pbp, ProductWorkflow pw, ProductCollection pc, ProductItemBusinessPartner pibp "
      + "WHERE pbp.productId = pw.productId AND pw.productId=pc.productId AND pw.state = 4 "
      + "AND  pibp.productBusinessPartner.id = pbp.id "
      + "AND pbp.storeId = ?1 AND pw.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "AND pibp.merchantSku = ?3")
  Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerIdAndMerchantSku(
      String storeId, String businessPartnerId, String merchantSku, Pageable pageable);

  @Query(
      value = "SELECT DISTINCT NEW com.gdn.mta.product.entity.ItemSkuToItemIdMapping (pibp.productItemId, pibp.gdnProductItemSku, pibp.merchantSku)"
          + " FROM ProductItemBusinessPartner pibp WHERE pibp.storeId = :storeId AND pibp.productBusinessPartnerId = :productBusinessPartnerId "
          + "ORDER BY pibp.gdnProductItemSku",
      countQuery = "SELECT COUNT(DISTINCT pibp.productItemId, pibp.gdnProductItemSku, pibp.merchantSku) FROM ProductItemBusinessPartner pibp"
          + " WHERE pibp.storeId = :storeId AND pibp.productBusinessPartnerId = :productBusinessPartnerId")
  Page<ItemSkuToItemIdMapping> findItemSkuToItemIdMappingByProductBusinessPartnerId(@Param("storeId") String storeId,
      @Param("productBusinessPartnerId") String productBusinessPartnerId, Pageable pageable);

  @Query(
      value = "SELECT DISTINCT NEW com.gdn.mta.product.entity.ItemSkuToItemIdMapping (pibp.productItemId, pibp.gdnProductItemSku, pibp.merchantSku)"
          + " FROM ProductItemBusinessPartner pibp WHERE pibp.storeId = :storeId AND pibp.productBusinessPartnerId = :productBusinessPartnerId AND pibp.markForDelete = false "
          + "ORDER BY pibp.gdnProductItemSku",
      countQuery = "SELECT COUNT(DISTINCT pibp.productItemId, pibp.gdnProductItemSku, pibp.merchantSku) FROM ProductItemBusinessPartner pibp"
          + " WHERE pibp.storeId = :storeId AND pibp.productBusinessPartnerId = :productBusinessPartnerId AND pibp.markForDelete = false")
  Page<ItemSkuToItemIdMapping> findItemSkuToItemIdMappingByProductBusinessPartnerIdAndMarkForDeleteFalse(
      @Param("storeId") String storeId, @Param("productBusinessPartnerId") String productBusinessPartnerId,
      Pageable pageable);

  @Query(
      value = "SELECT DISTINCT NEW com.gdn.mta.product.entity.ItemSkuToItemIdMapping (pibp.productItemId, pibp.gdnProductItemSku, pibp.merchantSku)"
          + " FROM ProductItemBusinessPartner pibp WHERE pibp.storeId = :storeId AND pibp.productBusinessPartnerId = :productBusinessPartnerId AND pibp.markForDelete = false "
          + "ORDER BY pibp.gdnProductItemSku",
      countQuery = "SELECT COUNT(DISTINCT pibp.productItemId, pibp.gdnProductItemSku, pibp.merchantSku) FROM ProductItemBusinessPartner pibp"
          + " WHERE pibp.storeId = :storeId AND pibp.productBusinessPartnerId = :productBusinessPartnerId AND pibp.markForDelete = false")
  List<ItemSkuToItemIdMapping> findAllItemSkuToItemIdMappingByProductBusinessPartnerId(@Param("storeId") String storeId,
      @Param("productBusinessPartnerId") String productBusinessPartnerId);

  @Query("select piba.minimumStock FROM ProductItemBusinessPartner piba where "
      + "piba.gdnProductItemSku = ?1 AND piba.markForDelete = FALSE")
  Integer getMinimumStockByGdnProductItemSku(String itemSku);

  @Modifying @Query("update ProductItemBusinessPartner piba set piba.minimumStock = ?2 where "
	      + "piba.gdnProductItemSku = ?1") void updateMinimumStockByGdnProductItemSku(String itemSku,
      Integer minimumStock);

  @Query(value = "SELECT created_by FROM prd_product_item_business_partner WHERE gdn_product_item_sku = ?1 AND mark_for_delete = FALSE LIMIT 1", nativeQuery = true)
  String getCreatedByItemBusinessPartnerByGdnProductItemSku(String itemSku);

  @Modifying(clearAutomatically = true)
  @Query("update ProductItemBusinessPartner pibp set pibp.buyable = false , pibp.display = false , pibp.cncBuyable = false , pibp.cncDiscoverable = false"
      + " where pibp.productBusinessPartner.id in (select pbp.id from ProductBusinessPartner pbp where pbp.productId = ?1)")
  int markItemsAsUnBuyableAndUnViewable(String productId);

  @Modifying(clearAutomatically = true)
  @Query("update ProductBusinessPartnerAttribute pbpa set value = ?1 "
      + "where attributeId = ?2 and productBusinessPartner.id = "
      + "(select pbp.id from ProductBusinessPartner pbp where pbp.productId = ?3)")
  void updateSkuTrueAttributeInProductBusinessPartnerAttribute(String value, String attributeId, String productId);

  @Modifying(clearAutomatically = true)
  @Query(value = "DELETE FROM PRD_PRODUCT_ITEM_BUSINESS_PARTNER "
      + "USING PRD_PRODUCT_BUSINESS_PARTNER "
      + "where PRD_PRODUCT_ITEM_BUSINESS_PARTNER.PRODUCT_BUSINESS_PARTNER_ID = PRD_PRODUCT_BUSINESS_PARTNER.ID AND "
      + "PRD_PRODUCT_BUSINESS_PARTNER.BUSINESS_PARTNER_ID = ?1 AND PRD_PRODUCT_BUSINESS_PARTNER.PRODUCT_ID = ?2", nativeQuery = true)
  void markItemsAsDeletedOnProductResubmission(String businessPartnerId, String productId);


  @Modifying(clearAutomatically = true)
  @Query("DELETE FROM ProductBusinessPartner "
             + "where businessPartnerId = ?1 and productId = ?2")
  void markProductAsDeletedOnProductResubmission(String businessPartnerId, String productId);

  @Modifying(clearAutomatically = true)
  @Query(value = "DELETE FROM PRD_PRODUCT_BUSINESS_PARTNER_ATTRIBUTE "
      + "USING PRD_PRODUCT_BUSINESS_PARTNER "
      + "where PRD_PRODUCT_BUSINESS_PARTNER_ATTRIBUTE.PRODUCT_BUSINESS_PARTNER_ID = PRD_PRODUCT_BUSINESS_PARTNER.ID AND "
      + "PRD_PRODUCT_BUSINESS_PARTNER.BUSINESS_PARTNER_ID = ?1 AND PRD_PRODUCT_BUSINESS_PARTNER.PRODUCT_ID = ?2", nativeQuery = true)
  void markBusinessPartnerAttributesAsDeletedOnProductResubmission(String businessPartnerId, String productId);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 AND pw.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "ORDER BY pw.updatedDate asc")
  Page<RejectedSkuProductCollection> findRejectedProductsByStoreIdAndBusinessPartnerIdAndUpdatedDateAsc(
      String storeId, String businessPartnerId, Pageable pageable);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 AND pw.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "ORDER BY pbp.createdDate asc")
  Page<RejectedSkuProductCollection> findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateAsc(
      String storeId, String businessPartnerId, Pageable pageable);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 AND pw.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "ORDER BY pbp.createdDate desc")
  Page<RejectedSkuProductCollection> findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateDesc(
      String storeId, String businessPartnerId, Pageable pageable);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "AND lower(pbp.productName) LIKE '%' || lower(?3) || '%' "
      + "ORDER BY pw.updatedDate asc")
  Page<RejectedSkuProductCollection> findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndUpdatedDateAsc(
      String storeId, String businessPartnerId, String productName, Pageable pageable);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "AND lower(pbp.productName) LIKE '%' || lower(?3) || '%' "
      + "ORDER BY pbp.createdDate asc")
  Page<RejectedSkuProductCollection> findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateAsc(
      String storeId, String businessPartnerId, String productName, Pageable pageable);

  @Query("select NEW com.gdn.mta.product.entity.RejectedSkuProductCollection (pbp.productName, "
      + "pbp.categoryName, pbp.brand, pbp.createdDate as submitDate, "
      + "pbp.createdBy as initiator, pw.notes as rejectedReason, "
      + "pw.updatedDate as rejectedDate, pc.productCode as productCode) "
      + "FROM ProductCollection pc, ProductBusinessPartner pbp,ProductWorkflow pw "
      + "WHERE pbp.productId = pw.productId AND pw.productId = pc.productId AND pw.state = 4"
      + " AND pbp.storeId = ?1 "
      + "AND pbp.businessPartnerId = ?2 AND pbp.markForDelete = TRUE "
      + "AND lower(pbp.productName) LIKE '%' || lower(?3) || '%' "
      + "ORDER BY pbp.createdDate desc")
  Page<RejectedSkuProductCollection> findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateDesc(
      String storeId, String businessPartnerId, String productName, Pageable pageable);

  @Query(value =
             "SELECT gdn_product_item_sku from PRD_PRODUCT_ITEM_BUSINESS_PARTNER where PRODUCT_BUSINESS_PARTNER_ID in ("
                 + "SELECT id from PRD_PRODUCT_BUSINESS_PARTNER where PRODUCT_ID = ?1)", nativeQuery = true)
  List<String> getAllItemSkusByProductId(String productId);

  @Query(value = "SELECT gdn_product_item_sku from PRD_PRODUCT_ITEM_BUSINESS_PARTNER where STORE_ID = :storeId and PRODUCT_BUSINESS_PARTNER_ID = :productBusinessPartnerId", nativeQuery = true)
  List<String> getAllItemSkusByProductBusinessPartnerId(@Param("storeId") String storeId,
      @Param("productBusinessPartnerId") String productBusinessPartnerId);

  List<ProductBusinessPartner> findByStoreIdAndIdIn(String storeId, Set<String> productBusinessPartnerIds);

  @Query(value = "SELECT gdn_product_sku from PRD_PRODUCT_BUSINESS_PARTNER where STORE_ID = :storeId and ID = :id", nativeQuery = true)
  String getAllProductSkusByProductBusinessPartnerId(@Param("storeId") String storeId, @Param("id") String id);

  @Query(value =
      "SELECT NEW com.gdn.mta.product.entity.ItemFlagDetails (pibp.gdnProductItemSku, pibp.pickupPointId, pibp.buyable, pibp.display, pibp.cncBuyable, pibp.cncDiscoverable, pibp.productItemId) "
          + "from ProductItemBusinessPartner pibp join pibp.productBusinessPartner pbp where " + "pbp.productId = ?1")
  List<ItemFlagDetails> getAllItemSkusViewConfigByProductId(String productId);

  ProductBusinessPartner findFirstByStoreIdAndBusinessPartnerId(String storeId, String businessPartnerId);

  ProductBusinessPartner findByStoreIdAndBusinessPartnerIdAndProductIdAndMarkForDeleteTrue(String storeId, String businessPartnerId, String productId);

  @Modifying
  @Query(value = "update prd_product_business_partner set product_id = ?1 , updated_date = CURRENT_TIMESTAMP, updated_by = 'SYSTEM' where product_id = ?2 and gdn_product_sku = ?3", nativeQuery = true)
  void updateProductIdForMigration(String newProductId, String oldProductId, String gdnProductSku) throws PersistenceException;

  @Query(value = "SELECT gdn_product_sku from PRD_PRODUCT_BUSINESS_PARTNER where PRODUCT_ID in ("
      + "SELECT product_id from PRD_PRODUCT_COLLECTION where PRODUCT_CODE = ?1)", nativeQuery = true)
  List<String> getGdnSkuByProductCode(String productCode);

  @Query(value = "select count(1) from ProductWorkflow pw where pw.productId in (select pbp.productId from ProductBusinessPartner pbp where pbp.gdnProductSku = ?1) and pw.state = 4")
  int countRejectedProductByGdnProductSku(String gdnProductSku);

  List<ProductBusinessPartner> findByStoreIdAndProductIdAndStateAndMarkForDeleteFalse(String storeId,
      String productId, String state) throws PersistenceException;

  @Query(value = "SELECT gdn_product_sku FROM prd_product_business_partner WHERE id IN (SELECT product_business_partner_id FROM prd_product_item_business_partner WHERE gdn_product_item_sku = ?1)", nativeQuery = true)
  String findProductSkuByItemSku(String itemSku);

  @Modifying
  @Query(value = "update prd_product_business_partner set product_name = ?2 , category_code = ?3 , category_name = ?4 , brand = ?5 ,updated_date = CURRENT_TIMESTAMP, updated_by = 'SYSTEM' where gdn_product_sku = ?1", nativeQuery = true)
  void updateProductMasterData(String productSku, String productName, String categoryCode, String categoryName, String brand);

  @Query(value = "SELECT * FROM prd_product_business_partner WHERE product_id IN (SELECT product_id FROM prd_product_collection WHERE product_code = ?1)", nativeQuery = true)
  List<ProductBusinessPartner> findProductBusinessPartnerByProductCode(String productCode);

  List<ProductBusinessPartner> findByStoreIdAndGdnProductSkuIn(String storeId, List<String> productSkus);

  @Query(value = "SELECT gdn_product_sku, state FROM prd_product_business_partner  "
    + "WHERE store_id = ?1 AND business_partner_id = ?2 AND state in (?3) AND "
    + "mark_for_delete = false order by updated_date", nativeQuery = true)
  List<Object[]> findByStoreIdAndAndBusinessPartnerIdAndStateInAndMarkForDeleteFalseOrderByUpdatedDate(
    String storeId, String businessPartnerId, List<String> state);

  @Query(value = ProductBusinessPartnerRepository.QUERY_COUNT_IN_PROGRESS_L5, nativeQuery = true)
  int countInProgressL5ByProductSku(String storeId, String productSku);


  @Modifying
  @Query(value = "update prd_product_business_partner set fbb_activated = ?3 , updated_date = now"
    + "() where id = ?2 and store_id = ?1", nativeQuery = true)
  void updateFbbFlagByStoreIdAndId(String storeId, String id, boolean fbbActivated);

  ProductBusinessPartner findFirstByStoreIdAndProductId(String storeId, String productId);

  long deleteByStoreIdAndProductId(String storeId, String productId);

  @Modifying
  @Query(value = UPDATE_PRODUCT_MASTER_DATA_AND_SIZE_CHART_CODE)
  void updateProductMasterDataAndSizeChart(@Param("productSku") String productSku,
    @Param("productName") String productName, @Param("categoryCode") String categoryCode,
    @Param("categoryName") String categoryName, @Param("sizeChartCode") String sizeChartCode,
    @Param("updatedBy") String updatedBy, @Param("brand") String brand);

  @Modifying(clearAutomatically = true)
  @Query(value = UPDATE_ITEM_VIEW_CONFIG_AND_PRODUCT_TYPE)
  void updateItemsViewConfigAndProductType(String productId, boolean buyable, boolean display,
    Integer productType);

  @Modifying
  @Query(value = UPDATE_SIZE_CHART_CODE)
  void updateSizeChartDetails(@Param("productSku") String productSku,
    @Param("sizeChartCode") String sizeChartCode, @Param("updatedBy") String updatedBy);

  @Modifying
  @Query(value = UPDATE_SIZE_CHART_CODE_AND_BRAND)
  void updateSizeChartDetailsAndBrand(@Param("productSku") String productSku,
      @Param("sizeChartCode") String sizeChartCode, @Param("brand") String brand,
      @Param("updatedBy") String updatedBy);

  @Modifying
  @Query(value = UPDATE_BRAND)
  void updateBrand(@Param("productSku") String productSku,
      @Param("brand") String brand, @Param("updatedBy") String updatedBy);

  @Query("SELECT NEW com.gdn.mta.product.entity.ProductSkuBusinessPartnerDTO (pc.productCode, pbp.gdnProductSku, pc.businessPartnerCode, pbp.submittedDate,pbp.productName) "
      + "FROM ProductBusinessPartner pbp "
      + "JOIN ProductCollection pc ON pbp.productId = pc.productId "
      + "WHERE pc.productCode IN :productCodes")
  List<ProductSkuBusinessPartnerDTO> getGdnSkuListByProductCodes(@Param("productCodes") List<String> productCodesList);

  @Query("SELECT DISTINCT pbp.businessPartnerId FROM ProductBusinessPartner pbp where pbp.storeId = :storeId"
      + " AND pbp.markForDelete = false AND pbp.state = :state AND pbp.submittedDate BETWEEN :startUpdatedDate AND :endUpdatedDate order by businessPartnerId")
  Page<String> findDistinctBusinessPartnerCodesByStateAndUpdatedStepDate(@Param("storeId") String storeId,
      @Param("state") String state, @Param("startUpdatedDate") Date startUpdatedDate,
      @Param("endUpdatedDate") Date endUpdatedDate, Pageable pageable);

}
