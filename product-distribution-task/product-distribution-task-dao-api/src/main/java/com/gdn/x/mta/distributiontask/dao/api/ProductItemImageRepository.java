package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * Created by virajjasani on 23/09/16.
 */
public interface ProductItemImageRepository extends JpaRepository<ProductItemImage, String> {

  @Modifying
  @Query(
      value = "update pdt_product_item_image piimg set mark_for_delete = true, updated_by = :updatedBy, updated_date = current_timestamp "
          + "from pdt_product_item pi where piimg.product_item = pi.id and pi.product = :productId",
      nativeQuery = true)
  void deleteByProductId(@Param("productId") String productId,
      @Param("updatedBy") String updatedBy);

  @Modifying
  @Query(value = "delete from pdt_product_item_image pii where pii.product_item IN (:productItemIdList)", nativeQuery =  true)
  void deleteByProductItemIds(@Param("productItemIdList") List<String> productItemIdList);

  @Query(value = "select b.* from pdt_product_item a join pdt_product_item_image b on a.id = b.product_item where a.product = ?1", nativeQuery = true)
  List<ProductItemImage> findByProductId(String productId);

  @Modifying
  @Query(value = "UPDATE pdt_product_item_image set location_path = :newLocationPath WHERE product_item IN :productItemIds and location_path = :oldLocationPath", nativeQuery = true)
  void updateLocationPathByProductItem(@Param("newLocationPath") String newLocationPath,
      @Param("productItemIds") List<String> productItemIds, @Param("oldLocationPath") String oldLocationPath);

}
