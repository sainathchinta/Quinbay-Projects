package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.dto.AddProductAttributesDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryDTO;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;

public interface ProductCategoryService extends GdnBaseService<ProductCategory> {

  public ProductCategory findByStoreIdAndId(String storeId, String id);

  public CategorySummaryDTO movePrdCategoryByProductCode(String storeId, String productCode, 
      String categoryCode) throws Exception;
  
  public List<ProductAttribute> addProductAttributes(String storeId, AddProductAttributesDTO request) throws Exception;

  /**
   * get list of master parent categories by productCode
   * @param storeId
   * @param productCode
   * @return
   * @throws Exception
   */
  List<Category> getMasterParentCategoryByProductCode(String storeId, String productCode)
      throws Exception;

  /**
   * Get Parent Category by Category Code
   * @param storeId
   * @param categoryCode
   * @return
   */
  Category getParentCategoryByCategoryCode(String storeId, String categoryCode);

  /**
   * To find the count of items present in category based on product IDs
   * @param productIds
   * @return
   */
  List<Object[]> findCategoryCountByProductIdInAndMarkForDeleteFalse(List<String> productIds);

  /**
   * To find productIDs based on the category Ids and product Ids
   * @param productIds
   * @param categoryIds
   * @return
   */
  List<String> findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(List<String> productIds, List<String> categoryIds);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductCategory> getProductCategoriesByStoreIdAndProductIdCached(String storeId, String productId);

  /**
   * delete product category using id's
   * @param productCategoryIds
   */
  void deleteByProductCategoryIds(List<String> productCategoryIds);
}
