package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.dto.CategoryCodeAndUmkmFlagDTO;
import com.gdn.x.productcategorybase.entity.CategoryReference;

public interface CategoryReferenceRepository extends JpaRepository<CategoryReference, String> {

  String CATEGORY_CODE_AND_UMKM_FLAG_DTO = "com.gdn.x.productcategorybase.dto.CategoryCodeAndUmkmFlagDTO";

  CategoryReference findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  Page<CategoryReference> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  @Query(value = "select new " + CATEGORY_CODE_AND_UMKM_FLAG_DTO
      + "(cat.categoryCode, cat.umkm, cat.catalog) from Category cat where cat.id in (select cref.salesCategory.id from CategoryReference cref"
      + " where cref.masterCategory.id = :masterCategoryReferenceId and cref.markForDelete = false)")
  List<CategoryCodeAndUmkmFlagDTO> findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryId(
      @Param("masterCategoryReferenceId") String masterCategoryId);

  @Query(value = "select new " + CATEGORY_CODE_AND_UMKM_FLAG_DTO
      + "(cat.categoryCode, cat.umkm, cat.catalog) from Category cat where cat.id in (select cref.salesCategory.id from CategoryReference cref"
      + " where cref.masterCategory.id = :masterCategoryReferenceId and cref.markForDelete = false) and cat.halalCategory = :halalCategory")
  List<CategoryCodeAndUmkmFlagDTO> findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryIdAndHalalCategory(
      @Param("masterCategoryReferenceId") String masterCategoryId, @Param("halalCategory") boolean halalCategory);

  @Query(value = "select new " + CATEGORY_CODE_AND_UMKM_FLAG_DTO
      + "(cat.categoryCode, cat.umkm, cat.catalog) from Category cat where cat.id in (select cref.salesCategory.id from CategoryReference cref"
      + " where cref.masterCategory.id = :masterCategoryReferenceId)")
  List<CategoryCodeAndUmkmFlagDTO> findAllSalesCategoryCodesByMasterCategoryId(
      @Param("masterCategoryReferenceId") String masterCategoryId);

  @Query(value = "select new " + CATEGORY_CODE_AND_UMKM_FLAG_DTO
      + "(cat.categoryCode, cat.umkm, cat.catalog) from Category cat where cat.id in (select cref.salesCategory.id from CategoryReference cref"
      + " where cref.masterCategory.id = :masterCategoryReferenceId) and cat.halalCategory = :halalCategory")
  List<CategoryCodeAndUmkmFlagDTO> findAllSalesCategoryCodesByMasterCategoryIdAndHalalCategory(
      @Param("masterCategoryReferenceId") String masterCategoryId, @Param("halalCategory") boolean halalCategory);

  @Query(value = "SELECT c.salesCategoryReferenceId FROM CategoryReference c WHERE c.markForDelete = false AND c.storeId = :storeId AND "
      + "c.masterCategoryReferenceId = :categoryId")
  List<String> findIdsByStoreIdAndMasterCategoryReferenceId(@Param("storeId") String storeId,
      @Param("categoryId") String categoryId);

  @Query(value = "SELECT c.masterCategoryReferenceId FROM CategoryReference c WHERE c.markForDelete = false AND c.storeId = :storeId AND "
      + "c.salesCategoryReferenceId = :categoryId")
  List<String> findIdsByStoreIdAndSalesCategoryReferenceId(@Param("storeId") String storeId,
      @Param("categoryId") String categoryId);
}
