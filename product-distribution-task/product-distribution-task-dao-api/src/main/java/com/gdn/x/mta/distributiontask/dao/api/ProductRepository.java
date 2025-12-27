package com.gdn.x.mta.distributiontask.dao.api;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.StuckProductsDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

public interface ProductRepository
    extends JpaRepository<Product, String>, JpaSpecificationExecutor<Product>, ProductCustomRepository {

  Product findByIdAndMarkForDeleteFalse(String id);

  Product findByProductCodeAndMarkForDeleteFalse(String code);

  List<Product> findByProductCodeInAndMarkForDeleteFalse(List<String> code);

  @Query("select productCode from Product where brandCode =:brandCode and markForDelete = false ")
  List<String> findProductCodeByBrandCodeAndMarkForDeleteFalse(@Param("brandCode") String brandCode);

  @Query(value = "SELECT pp.id as product_id, ppi.id as product_item_id, pp.product_code as "
    + "product_code FROM pdt_product pp JOIN pdt_product_item ppi ON pp.id = ppi.product"
      + " WHERE pp.brand_code =:brandCode AND pp.mark_for_delete = false", nativeQuery = true)
  List<Object[]> findIdByBrandCodeAndMarkForDeleteFalse(@Param("brandCode") String brandCode);

  Product findByProductCodeAndVersionAndMarkForDeleteFalse(String productCode, long version);

  @Modifying
  @Query("UPDATE Product set brandCode =:brandCode, brand =:brand, brandApprovalStatus =:brandApprovalStatus "
      + "WHERE id IN :productIds")
  void updateBrandDetail(@Param("brandCode") String brandCode, @Param("brand") String brand,
      @Param("brandApprovalStatus") String brandApprovalStatus, @Param("productIds") List<String> productIds);

  Product findByProductCode(String code);

  @Query(value = "SELECT id FROM pdt_product WHERE store_id = :storeId and product_code = :productCode and mark_for_delete = FALSE", nativeQuery = true)
  String getIdByStoreIdAndProductCodeAndMarkForDelete(@Param("storeId") String storeId,
      @Param("productCode") String productCode);

  @Modifying
  @Query("update Product u set u.state = :state where u.id=:id")
  void updateWorkflowState(@Param("id") String id, @Param("state") WorkflowState state);

  @Query("select product from Product product where product.productCode in :productCodes and product.state in :states and product.markForDelete = false")
  List<Product> findByProductCode(@Param("productCodes") List<String> productCodes,
      @Param("states") List<WorkflowState> state);

  @Modifying
  @Query("update Product product set product.currentVendor = :vendor, product.productCreatedDate = :dateTime, product.state = com.gdn.x.mta.distributiontask.model.type.WorkflowState.IN_REVIEW where product.productCode in :productCodeList")
  void updateProductVendor(@Param("productCodeList") List<String> productCodeList,
      @Param("vendor") Vendor vendor, @Param("dateTime") Date date);


  @Query(value = "select new com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO(p"
      + ".state, count(p)) from Product p where p.currentVendor = :vendor AND ((p.state = "
      + "'PASSED' AND p.markForDelete = true AND productCreatedDate >= :createdDate) OR"
      + "(p.state <> 'PASSED' AND p.markForDelete = false)) AND p.storeId = :storeId group by state")
  List<VendorProductStatusDTO> findAllProductStatusForVendor(@Param("vendor") Vendor vendor, @Param("storeId") String
      storeId, @Param("createdDate") Date createdDate);

  @Query(value = "select p from Product p where p.currentVendor = :vendor "
      + "AND p.state = 'PASSED' AND p.markForDelete = true AND productCreatedDate >= :startDate "
      + "AND p.storeId = :storeId")
  Page<Product> findAllPassedStatusProductsForVendor(@Param("vendor") Vendor vendor, @Param("storeId") String
      storeId, @Param("startDate") Date startDate, Pageable pageable);

  @Query(
      "SELECT new com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper(product"
          + ".businessPartnerCode,product.businessPartnerName) from Product product"
          + " WHERE product.markForDelete = false AND product.vendorId = :vendor_id"
          + " GROUP BY product.businessPartnerCode,product.businessPartnerName"
          + " ORDER BY product.businessPartnerName asc")
  Page<ProductBusinessPartnerMapper> findProductBusinessPartnerForVendor(
      @Param("vendor_id") String vendorId, Pageable pageable);


  @Modifying
  @Query("update Product product set product.currentVendor.id = NULL where product.currentVendor.id = :vendorId")
  void clearVendorForProduct(@Param("vendorId") String vendorId);

  @Query(
      "select DISTINCT product.productCode from Product product where product.productCode in "
          + ":productCodes and product.markForDelete = false")
  List<String> findProductCodesProductCodesIn(@Param("productCodes") List<String> productCodes);

  @Query(value = "select p.id, p.productCode from Product p where "
      + "p.markForDelete = true AND p.state not in ('NEED_CORRECTION') "
      + "AND p.updatedDate <= :updateDate order by p.id asc")
  Slice<Object[]> findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(@Param("updateDate") Date updateDate,
      Pageable pageable);

  @Query(value = "select p from Product p where "
      + "p.markForDelete = true AND p.state not in ('NEED_CORRECTION') "
      + "AND p.updatedDate <= :updateDate and pickedForDeletion = false order by p.id asc")
  Slice<Product> findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(
      @Param("updateDate") Date updateDate, Pageable pageable);

  @Modifying
  @Query(value = "Delete from pdt_product p where p.id in (:productIds)", nativeQuery = true)
  void deleteById(@Param("productIds") List<String> productIds);

  @Query(value = "select product from Product product "
      + "where product.markForDelete = false and product.state = 'PASSED' and product.qcRetryCount <= :qcRetryCount "
      + "and product.updatedDate < :deltaTime order by product.updatedDate")
  List<Product> findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(
      @Param("qcRetryCount") int qcRetryCount, @Param("deltaTime") Date deltaTime, Pageable pageable);

  @Modifying
  @Query(value = "update Product p set p.qcRetryCount = p.qcRetryCount + 1 where p.productCode = :productCode")
  void updateQcRetryCount(@Param("productCode") String productCode);

  @Query(value = "select new com.gdn.x.mta.distributiontask.model.dto.StuckProductsDTO(p.productCode, p.state, p.createdDate, p.updatedDate)"
      + " from Product p where p.markForDelete = false and p.state = 'PASSED' and p.qcRetryCount >= :qcRetryCount"
      + " order by p.createdDate DESC")
  List<StuckProductsDTO> getProductsAboveQcRetryCount(@Param("qcRetryCount") int qcRetryCount);

  @Query(value = "select p.* from pdt_product p JOIN pdt_product_reviewer pr ON p.product_code = pr.product_code"
      + " where p.store_id = :storeId AND p.mark_for_delete = :markForDelete AND p.state IN (:states) AND "
      + "p.is_post_live = :postLive AND p.updated_date > :updatedDate AND pr.approver_assignee IS NULL "
      + "ORDER BY p.updated_date LIMIT :limit", nativeQuery = true)
  List<Product> findUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(@Param("storeId") String storeId,
      @Param("states") List<String> states, @Param("markForDelete") boolean markForDelete,
      @Param("postLive") boolean postLive, @Param("updatedDate") Date date, @Param("limit") int pageSize);

  @Modifying
  @Query(value = "update Product p set p.postLive = :postLive where p.productCode = :productCode")
  void updateProductPostLiveFlag(@Param("productCode") String productCode, @Param("postLive") boolean postLive);

  Page<Product> findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId,
      Pageable pageable);

  Page<Product> findByStoreIdAndUpdatedDateBetween(String storeId, Date startDate, Date endDate, Pageable pageable);

  Page<Product> findByStoreIdAndBusinessPartnerCodeAndStateAndCategoryCodeInAndMarkForDeleteFalse(String storeId,
      String sellerCode, WorkflowState state, Set<String> categoryCodes, Pageable pageable);

  Page<Product> findByStoreIdAndStateAndUpdatedDateBetweenAndMarkForDeleteTrue(String storeId, WorkflowState state,
      Date startUpdatedDate, Date endUpdatedDate, Pageable pageable);
}
