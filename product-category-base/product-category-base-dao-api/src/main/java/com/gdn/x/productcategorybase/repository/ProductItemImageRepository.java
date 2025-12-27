package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.ProductItemImage;

public interface ProductItemImageRepository extends JpaRepository<ProductItemImage, String> {
  
  @Modifying(clearAutomatically = true)
  @Query("update ProductItemImage x set x.locationPath = ?1, x.active = true where x.hashCode = ?2")
  void updateProductItemImageName(String locationPath, String hashCode);

  List<ProductItemImage> findByStoreIdAndProductItemIdIn(String storeId, List<String> productItemIds);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_item_images WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;

  String FIND_PRODUCT_ITEM_IMAGE_LOCATION_PATHS_BY_PRODUCT_CODE_LIST_AND_STORE_ID_QUERY =
      "SELECT location_path FROM pcc_product_item_images ppii INNER JOIN pcc_product_item ppi ON ppii.product_item_id = ppi.id INNER JOIN pcc_product pp ON ppi.product_id = pp.id WHERE ppii.mark_for_delete = FALSE AND ((ppii.edited = true AND ppii.active = TRUE) OR ppii.original_image IS NULL OR ppii.original_image = FALSE) AND ppii.location_path NOT LIKE '%/resize/%' AND ppii.location_path NOT LIKE '%catalog-image/%' AND ppi.mark_for_delete = FALSE AND pp.product_code IN (:productCodeList) and pp.store_id = :storeId AND pp.mark_for_delete = FALSE";

  @Query(value = FIND_PRODUCT_ITEM_IMAGE_LOCATION_PATHS_BY_PRODUCT_CODE_LIST_AND_STORE_ID_QUERY, nativeQuery = true)
  List<String> findProductItemImageLocationPathsByProductCodeListAndStoreId(
      @Param("productCodeList") List<String> productCodeList, @Param("storeId") String storeId);
}
