package com.gdn.x.productcategorybase.repository;

import java.util.Date;
import java.util.List;

import com.gdn.x.productcategorybase.dto.CategoryConfigurationDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import org.springframework.data.jpa.repository.Query;

public interface CategoryConfigurationRepository
    extends JpaRepository<CategoryConfiguration, String>, JpaSpecificationExecutor<CategoryConfiguration>,
    CategoryConfigurationCustomRepository {

  List<CategoryConfiguration> findByStoreIdAndCategoryIn(String storeId, List<Category> categoryLists);

  CategoryConfiguration findByStoreIdAndCategory(String storeId, Category category);

  CategoryConfiguration findByStoreIdAndCategoryAndMarkForDeleteFalse(String storeId, Category category);

  Page<CategoryConfiguration> findByStoreIdAndUpdatedDateGreaterThan(String storeId, Date date, Pageable pageable);

  Long countByStoreIdAndMarkForDeleteFalse(String storeId);

  @Query("select new com.gdn.x.productcategorybase.dto.CategoryConfigurationDTO(cc.id, cc.markForDelete, cc.categoryId, cc.reviewConfig) from CategoryConfiguration cc where cc.storeId = ?1 AND cc.markForDelete = false")
  List<CategoryConfigurationDTO> findAllCategoryConfigurationDTO(String storeId);
}
