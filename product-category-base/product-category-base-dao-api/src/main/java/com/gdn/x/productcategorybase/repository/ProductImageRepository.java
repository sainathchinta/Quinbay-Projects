package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.dto.ProductImageDTO;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageSingle;

public interface ProductImageRepository extends JpaRepository<ProductImage, String> {
  String PRODUCT_IMAGE_DTO_WITH_PRODUCT_CODE =
      "com.gdn.x.productcategorybase.dto.ProductImageDTO";
  
  @Modifying(clearAutomatically = true)
  @Query("update ProductImage x set x.locationPath = ?1, x.active = true where x.hashCode = ?2")
  void updateProductImageName(String locationPath, String hashCode);
  
  @Query("select pi from ProductImageSingle pi " +
      "where pi.markForDelete = false and pi.productId IN(:productIds)")
  List<ProductImageSingle> findProductImagesByProductIds(@Param("productIds") List<String> productIds);  
  
  @Query("select new "
      + PRODUCT_IMAGE_DTO_WITH_PRODUCT_CODE
      + "(p.id, p.productCode, pi.locationPath, pi.isMainImages, pi.sequence, pi.active) "
      + "from ProductImage pi JOIN pi.product p "
      + "where p.storeId = :storeId and p.productCode IN(:productCodes) and pi.markForDelete = false")
  Page<ProductImageDTO> findProductImagesByProductCodes(@Param("storeId") String storeId,
      @Param("productCodes") List<String> productCodes, Pageable pageable);

  List<ProductImage> findByStoreIdAndProductId(String storeId, String productId);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_images WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;

  String FIND_PRODUCT_LOCATION_PATH_BY_PRODUCT_CODE_AND_STOREID_QUERY =
      "SELECT location_path FROM pcc_product_images ppi INNER JOIN pcc_product pp ON ppi.product_id = pp.id WHERE ppi.mark_for_delete = FALSE AND ((ppi.edited = true AND ppi.active = TRUE) OR ppi.original_image IS NULL OR ppi.original_image = FALSE) AND ppi.location_path NOT LIKE '%/resize/%' AND ppi.location_path NOT LIKE '%catalog-image/%' AND pp.product_code IN (:productCodeList) AND pp.store_id = :storeId AND pp.mark_for_delete = FALSE";

  @Query(value = FIND_PRODUCT_LOCATION_PATH_BY_PRODUCT_CODE_AND_STOREID_QUERY, nativeQuery = true)
  List<String> findProductLocationPathByProductCodeAndStoreId(@Param("productCodeList") List<String> productCodeList,
      @Param("storeId") String storeId);
}
