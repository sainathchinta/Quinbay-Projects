package com.gdn.x.productcategorybase.repository;

import java.util.List;
import java.util.stream.Stream;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndNameDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeNodeDTO;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;

public interface CategoryRepository extends JpaRepository<Category, String>,
    CategoryRepositoryCustom {

  Category findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(String storeId, String categoryCode);

  Category findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  Category findByStoreIdAndId(String storeId, String id);

  List<Category> findByStoreIdAndMarkForDeleteFalse(String storeId);

  Page<Category> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<Category> findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(String storeId,
      Category parentCategory, CatalogType catalogType);

  List<Category> findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(String storeId,
      Category parentCategory, CatalogType catalogType, boolean activated);

  Page<Category> findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(String storeId,
      Category parentCategory, CatalogType catalogType, Pageable pageable);

  Page<Category> findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(String storeId,
      Category parentCategory, CatalogType catalogType, boolean activated, Pageable pageable);

  List<Category> findByStoreIdAndParentCategoryAndMarkForDeleteFalse(String storeId, Category parentCategory);

  Page<Category> findByStoreIdAndParentCategoryAndMarkForDeleteFalse(String storeId, Category parentCategory,
      Pageable pageable);

  long countByStoreIdAndParentCategoryAndActivatedTrueAndMarkForDeleteFalse(String storeId, Category parentCategory);

  long countByStoreIdAndParentCategoryAndActivatedFalseAndMarkForDeleteFalse(String storeId, Category parentCategory);

  long countByStoreIdAndParentCategoryAndMarkForDeleteFalse(String storeId, Category parentCategory);

  long countByStoreIdAndParentCategoryAndHalalCategoryAndMarkForDeleteFalse(String storeId, Category parentCategory,
      boolean halalCategory);

  List<Category> findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(String storeId,
      String parentCategoryId);

  @Query(
      value = "select c from Category c where c.storeId = :storeId and c.markForDelete = false and c.categoryCode in (:categoryCodes)")
  Page<Category> findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(@Param("storeId") String storeId,
      @Param("categoryCodes") List<String> categoryCodes, Pageable pageable);

 List<Category> findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(String storeId, List<String> categoryCodes);

  @Query(
      value = "select c from Category c where c.storeId = :storeId and c.activated = :activated and c.markForDelete = false and c.categoryCode in (:categoryCodes)")
  Page<Category> findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(@Param("storeId") String storeId,
      @Param("categoryCodes") List<String> categoryCodes, @Param("activated") boolean activated, Pageable pageable);

  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByCategoryCode(String categoryCode);

  @Modifying(clearAutomatically = true)
  @Query("update Category c set parentCategory = (select b from Category b where b.id=:parentCategoryId) where id=:id ")
  void setParentCategoryId(@Param("parentCategoryId") String parentCategoryId, @Param("id") String id);

  @Query("select c.id, pc.id from Category c left outer join c.parentCategory pc where c" +
      ".markForDelete = FALSE and c.activated = TRUE")
  List<Object[]> getCategoryAndParentCategories();

  @Query("SELECT c FROM Category c LEFT JOIN c.catalog c2"
      + " WHERE c.storeId = ?1 AND lower(c.name) LIKE '%' || lower(?2) || '%'"
      + " AND c2.catalogType = ?3 AND c.markForDelete = FALSE")
  Page<Category> findByStoreIdAndCategoryNameAndCatalogTypeAndMarkForDeleteFalse(String storeId, String categoryName,
      CatalogType catalogType, Pageable pageable) throws Exception;

  @Query("select c.id from Category c where c.markForDelete = false and c.activated = true and "
      + "c.parentCategory is null")
  List<String> getParentCategories();

  @Query("select new com.gdn.x.productcategorybase.dto.CategoryTreeDTO(c.id, c.categoryCode, c"
    + ".name, c.nameEnglish, c.parentCategory.id, c.genericTemplateEligible, c.documentType, c"
    + ".b2bExclusive) from Category c where c.markForDelete = false and c.activated = true and "
    + "c.catalog.id = :catalogId and c.storeId = :storeId order by c.sequence")
  List<CategoryTreeDTO> getAllCategoryByCatalogName(@Param("storeId") String storeId,
    @Param("catalogId") String catalogId);

  @Query("select new com.gdn.x.productcategorybase.dto.CategoryTreeDTO(c.id, c.categoryCode, c.name, c.parentCategory.id) from Category c where c.markForDelete = false and c.activated = true and "
      + "c.catalog.id = :catalogId and c.storeId = :storeId order by c.sequence")
  Stream<CategoryTreeDTO> getStreamedAllCategoryByCatalogId(@Param("storeId") String storeId, @Param("catalogId") String catalogId);

  @Query("select new com.gdn.x.productcategorybase.dto.CategoryTreeDTO(c.id, c.categoryCode, c.name, c.parentCategory.id) from Category c where c.markForDelete = false and c.activated = true and "
      + "c.catalog.id = :catalogId and c.storeId = :storeId order by c.name")
  List<CategoryTreeDTO> getAllCategoryByCatalogId(@Param("storeId") String storeId, @Param("catalogId") String catalogId);

  @Query(value = "with recursive category_data(id, category_code, name, parent_category_id) AS(" +
    "select id, category_code, name, parent_category_id, sequence from pcc_category where category_code IN (:filterCategory) and " +
    "mark_for_delete = false and is_activated = true and catalog_id = :catalogId and store_id = :storeId " +
    "UNION ALL " +
    "select i.id, i.category_code, i.name, i.parent_category_id, i.sequence from category_data o, pcc_category i " +
    "where o.id = i.parent_category_id and " +
    "i.mark_for_delete = false and i.is_activated = true and i.catalog_id = :catalogId and i.store_id = :storeId " +
      ") "+
    "select * from category_data order by sequence ASC", nativeQuery = true)
  List<Object[]> getCategoryByParentIdNative(@Param("storeId") String storeId, @Param("catalogId") String catalogId,
      @Param("filterCategory") List<String> filterCategoryCodes);

  @Query(value = "SELECT c FROM Category c WHERE c.markForDelete = false AND c.storeId = :storeId AND c.id in (:ids)")
  List<Category> findByStoreIdAndIdAndMarkForDeleteFalse(@Param("storeId") String storeId,@Param("ids") List<String> ids);
  
  @Query(value = "SELECT c FROM Category c WHERE c.markForDelete = false AND c.storeId = :storeId AND c.parentCategory.categoryCode = :parentCode")
  List<Category> findByStoreIdAndParentIdAndMarkForDeleteFalse(@Param("storeId") String storeId, @Param("parentCode") String parentCode);

  List<Category> findByStoreIdAndIdInAndMarkForDeleteFalse(String storeId, List<String> ids);

  @Query("select new com.gdn.x.productcategorybase.dto.CategoryTreeNodeDTO(c.id, c.name, c.categoryCode, c.parentCategoryId) from Category c where c.activated = true and c.markForDelete = false and c.catalog.id = ?1")
  Page<CategoryTreeNodeDTO> findByActivatedTrueAndMarkForDeleteFalseAndCatalogIdOrderByCategoryCode(String catalogId,
      Pageable pageable);

  @Query(value = "SELECT c.id FROM Category c WHERE c.markForDelete = false AND c.storeId = :storeId AND c.categoryCode in (:categoryCodes)")
  List<String> findIdsByStoreIdAndCategoryCodes(@Param("storeId") String storeId,
      @Param("categoryCodes") List<String> categoryCodes);

  @Query(value = "SELECT c.id, c.categoryCode FROM Category c WHERE c.markForDelete = false AND c.storeId = :storeId AND c.categoryCode in (:categoryCodes) "
      + "order by c.categoryCode")
  Page<Object[]> findByStoreIdAndCategoryCodes(@Param("storeId") String storeId,
      @Param("categoryCodes") List<String> categoryCodes, Pageable pageable);

  @Query(value = "SELECT c.id, c.categoryCode FROM Category c WHERE c.markForDelete = false AND c.storeId = :storeId AND c.parentCategoryId = :parentCategoryId")
  List<Object[]> findByStoreIdAndParentCategoryId(@Param("storeId") String storeId,
      @Param("parentCategoryId") String parentCategoryId);

  @Query(value = "SELECT c.id, c.categoryCode FROM Category c WHERE c.markForDelete = false AND c"
    + ".storeId = :storeId AND c.parentCategoryId = :parentCategoryId AND c.activated = true order by c.categoryCode")
  List<Object[]> findByStoreIdAndParentCategoryIdAndIsActivatedTrue(
    @Param("storeId") String storeId, @Param("parentCategoryId") String parentCategoryId);

  @Query(value =
      "SELECT new com.gdn.x.productcategorybase.dto.CategoryCodeAndNameDTO(c.categoryCode, c.name, c.nameEnglish) FROM Category c WHERE c.markForDelete = false AND "
          + "c.storeId = :storeId AND c.categoryCode in (:categoryCodes)")
  List<CategoryCodeAndNameDTO> findNameByStoreIdAndCategoryCodes(@Param("storeId") String storeId,
      @Param("categoryCodes") List<String> categoryCodes);

  @Query(value = "SELECT c.categoryCode FROM Attribute a JOIN CategoryAttribute ca ON a.id = ca.attribute.id JOIN Category c ON ca.category.id = c.id WHERE a.attributeCode = :attributeCode AND a.markForDelete= false AND a.storeId= :storeId AND ca.markForDelete = false AND ca.storeId = :storeId AND c.markForDelete = false AND c.storeId = :storeId")
  List<String> getCategoryCodesByAttributeCode(String storeId, String attributeCode);
}
