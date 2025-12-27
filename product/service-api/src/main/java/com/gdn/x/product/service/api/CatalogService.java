package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;

import java.util.List;
import java.util.Map;

public interface CatalogService {

  /**
   * @param username
   * @param requestId
   * @param categoryCodes
   * @return
   */
  List<ItemCatalogVO> getItemCatalogsWithCategoryHierarchy(String username, String requestId,
      List<String> categoryCodes) throws Exception;

  /**
   * Get item catalog with category hierarchy. ItemCatalogVOV2 supports addition of new instance variables with
   * downstream compatibility.
   *
   * @param username
   * @param requestId
   * @param categoryCodes
   * @return
   * @throws Exception
   */
  List<ItemCatalogVOV2> getItemCatalogsWithCategoryHierarchyV2(String username, String requestId,
      List<String> categoryCodes) throws Exception;

  /**
   * @param username
   * @param requestId
   * @param product
   * @return
   */
  List<ItemCatalogVO> getItemCatalogsWithCategoryHierarchy(String username, String requestId,
      Product product) throws Exception;

  /**
   * get itemCatalog with category hierarchy , for category codes exists in PCB
   *
   * @param username
   * @param requestId
   * @param categoryCodes
   * @return
   * @throws Exception
   */
  List<ItemCatalogVO> getItemCatalogsWithCategoryHierarchyExistsInPCB(String username,
      String requestId, List<String> categoryCodes) throws Exception;

  /**
   * Fetch map of category code to itemCatalogs
   *
   * @param username
   * @param requestId
   * @param categoryCodes
   * @return
   */
  Map<String, List<ItemCatalogVO>> getCategoryCodeToItemCatalogsMap(String username, String requestId,
      List<String> categoryCodes);

  /**
   * get Item Catalog by product
   * @param username
   * @param requestId
   * @param product
   * @return
   */
  List<ItemCatalogVO> getItemCatalogsByProduct(String username, String requestId, Product product) throws Exception;
}
