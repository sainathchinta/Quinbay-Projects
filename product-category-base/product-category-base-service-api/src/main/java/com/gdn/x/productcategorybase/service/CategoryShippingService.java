package com.gdn.x.productcategorybase.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.entity.CategoryShipping;

public interface CategoryShippingService extends GdnBaseService<CategoryShipping> {

  List<CategoryShipping> findByCategoryCode(String storeId, String categoryName);

  Page<CategoryShipping> findByCategoryCode(String storeId, String categoryName, Pageable pageable);

  List<CategoryShipping> findByShippingCode(String storeId, String shippingCode);

  Page<CategoryShipping> findByShippingCode(String storeId, String shippingCode, Pageable pageable);

  List<CategoryShipping> findByStoreId(String storeId);

  CategoryShipping findByStoreIdAndId(String storeId, String id);

  void markForDeleteCategoryShipping(String storeId, List<CategoryShipping> categoryShippings) throws Exception;

  void markForDeleteCategoryShipping(String storeId, String id) throws Exception;

  /**
   * To generate shipping weight
   *
   * @param storeId
   * @param categoryCode
   * @param length
   * @param height
   * @param weight
   * @param width
   * @return Shipping weight
   */
  double generateShippingWeight(String storeId, String categoryCode, double length, double height, double weight,
      double width) throws Exception;

}
