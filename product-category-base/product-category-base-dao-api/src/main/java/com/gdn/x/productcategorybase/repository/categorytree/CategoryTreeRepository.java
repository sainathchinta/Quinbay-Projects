package com.gdn.x.productcategorybase.repository.categorytree;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.Category;

public interface CategoryTreeRepository extends JpaRepository<Category, String> {

  String QUERY_FIND_CATEGORY_NODE_BY_CATEGORY_CODES_AND_ACTIVE =
      "SELECT c.category_code, c.name, (SELECT category_code FROM pcc_category WHERE id = c.parent_category_id) AS parent_category_code, c.is_activated, "
          + "(SELECT COUNT(1) FROM pcc_category WHERE parent_category_id = c.id AND is_activated = ?4) FROM pcc_category AS c WHERE "
          + "c.store_id = ?1 AND c.catalog_id = (SELECT id FROM pcc_catalog WHERE catalog_code = ?2) AND c.category_code IN (?3) AND "
          + "c.is_activated = ?4 AND c.mark_for_delete = FALSE";

  String QUERY_FIND_CATEGORY_NODE_BY_PARENT_CATEGORY_CODE_AND_ACTIVE =
      "SELECT c.category_code, c.name, (SELECT category_code FROM pcc_category WHERE id = c.parent_category_id) AS parent_category_code, c.is_activated, "
          + "(SELECT COUNT(1) FROM pcc_category WHERE parent_category_id = c.id AND is_activated = ?4) FROM pcc_category AS c WHERE "
          + "c.store_id = ?1 AND c.catalog_id = (SELECT id FROM pcc_catalog WHERE catalog_code = ?2) AND "
          + "c.parent_category_id = (SELECT id FROM pcc_category WHERE category_code = ?3) AND c.is_activated = ?4 AND c.mark_for_delete = FALSE";

  String QUERY_FIND_CATEGORY_ROOT_NODE_BY_ACTIVE =
      "SELECT c.category_code, c.name, c.parent_category_id, c.is_activated, (SELECT COUNT(1) FROM pcc_category WHERE parent_category_id = c.id AND is_activated = ?3) "
          + "FROM pcc_category AS c WHERE c.store_id = ?1 AND c.catalog_id = (SELECT id FROM pcc_catalog WHERE catalog_code = ?2) AND "
          + "c.parent_category_id IS NULL AND c.is_activated = ?3 AND c.mark_for_delete = FALSE";

  String QUERY_FIND_BY_CATEGORY_CODES_AND_ACTIVE =
      "WITH RECURSIVE category(category_code, name, parent_category_code, is_activated, child_count) AS (SELECT c.category_code, c.name, "
          + "(SELECT category_code FROM pcc_category WHERE id = c.parent_category_id) AS parent_category_code, c.is_activated, "
          + "(SELECT COUNT(1) FROM pcc_category WHERE parent_category_id = c.id AND is_activated = ?4) AS child_count FROM pcc_category AS c WHERE c.store_id = ?1 AND "
          + "c.catalog_id = (SELECT id FROM pcc_catalog WHERE catalog_code = ?2) AND c.category_code IN (?3) AND c.is_activated = ?4 AND c.mark_for_delete = FALSE "
          + "UNION ALL SELECT c2.category_code, c2.name, ct.category_code, c2.is_activated, (SELECT COUNT(1) FROM pcc_category WHERE parent_category_id = c2.id AND is_activated = ?4) AS child_count "
          + "FROM pcc_category AS c2, category AS ct WHERE c2.parent_category_id = (SELECT id FROM pcc_category WHERE category_code = ct.category_code) AND c2.store_id = ?1 "
          + "AND c2.catalog_id = (SELECT id FROM pcc_catalog WHERE catalog_code = ?2) AND c2.is_activated = ?4 AND c2.mark_for_delete = FALSE) "
          + "SELECT * FROM category";

  @Query(value = CategoryTreeRepository.QUERY_FIND_CATEGORY_NODE_BY_CATEGORY_CODES_AND_ACTIVE, nativeQuery = true)
  List<Object[]> findCategoryNodeByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(String storeId,
      String catalogCode, List<String> categoryCodes, boolean active) throws Exception;

  @Query(value = CategoryTreeRepository.QUERY_FIND_CATEGORY_NODE_BY_PARENT_CATEGORY_CODE_AND_ACTIVE, nativeQuery = true)
  List<Object[]> findCategoryNodeByStoreIdAndCatalogCodeAndParentCategoryCodeAndActiveAndMarkForDeleteFalse(
      String storeId, String catalogCode, String parentCategoryCode, boolean active) throws Exception;

  @Query(value = CategoryTreeRepository.QUERY_FIND_CATEGORY_ROOT_NODE_BY_ACTIVE, nativeQuery = true)
  List<Object[]> findCategoryRootNodeByStoreIdAndCatalogCodeAndActiveAndMarkForDeleteFalse(String storeId,
      String catalogCode, boolean active) throws Exception;

  @Query(value = CategoryTreeRepository.QUERY_FIND_BY_CATEGORY_CODES_AND_ACTIVE, nativeQuery = true)
  List<Object[]> findByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(String storeId,
      String catalogCode, List<String> categoryCodes, boolean active) throws Exception;

}
