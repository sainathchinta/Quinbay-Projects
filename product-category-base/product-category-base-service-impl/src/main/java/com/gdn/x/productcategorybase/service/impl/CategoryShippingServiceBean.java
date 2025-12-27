package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.repository.CategoryShippingRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

@Service
@Transactional(readOnly = true)
public class CategoryShippingServiceBean implements CategoryShippingService {
  private static final String MASTER_CATALOG = "MASTER_CATALOG";
  private static final String CATEGORY_INFO_BY_CATEGORY_CODE = "Category with category code ";
  private static final String CATEGORY_INFO_NOT_FOUND = " is not found";
  private static final String CATEGORY_IS_NOT_MASTER_CATALOG = " is not master catalog";
  private static final String CATEGORY_CODE_EMPTY = " categoryCode should not be empty";
  private static final String STORE_ID_EMPTY = " storeId should not be empty";

  @Autowired
  private CategoryShippingRepository repository;

  @Autowired
  private CategoryService categoryService;

  @Override
  @Transactional(readOnly = false)
  public void delete(String id) throws Exception {
    // do nothing
  }

  @Override
  public List<CategoryShipping> findByCategoryCode(String storeId, String categoryCode) {
    return this.repository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
  }

  @Override
  public Page<CategoryShipping> findByCategoryCode(String storeId, String categoryCode, Pageable pageable) {
    return this.repository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode, pageable);
  }

  @Override
  public CategoryShipping findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public List<CategoryShipping> findByShippingCode(String storeId, String shippingCode) {
    return this.repository.findByStoreIdAndShippingCodeAndMarkForDeleteFalse(storeId, shippingCode);
  }

  @Override
  public Page<CategoryShipping> findByShippingCode(String storeId, String shippingCode, Pageable pageable) {
    return this.repository.findByStoreIdAndShippingCodeAndMarkForDeleteFalse(storeId, shippingCode, pageable);
  }

  @Override
  public List<CategoryShipping> findByStoreId(String storeId) {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId);
  }

  @Override
  public Page<CategoryShipping> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public CategoryShipping findByStoreIdAndId(String storeId, String id) {
    return this.repository.findByStoreIdAndId(storeId, id);
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategoryShipping(String storeId, List<CategoryShipping> categoryShippings) throws Exception {
    for (CategoryShipping categoryShipping : categoryShippings) {
      this.markForDeleteCategoryShipping(storeId, categoryShipping.getId());
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategoryShipping(String storeId, String id) throws Exception {
    CategoryShipping savedCatShipCode = this.repository.findByStoreIdAndId(storeId, id);
    if (savedCatShipCode == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform delete on non-existing data : " + id);
    }
    savedCatShipCode.setMarkForDelete(true);
    this.update(savedCatShipCode);
  }

  @Override
  @Transactional(readOnly = false)
  public String save(CategoryShipping entity) throws Exception {
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = false)
  public void update(CategoryShipping entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

  @Override
  public double generateShippingWeight(String storeId, String categoryCode, double length, double height, double weight,
      double width) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), STORE_ID_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(categoryCode), CATEGORY_CODE_EMPTY);
    Category savedCategory = getCategory(storeId, categoryCode);
    double shippingWeight =
        ConverterUtil.generateShippingWeight(length, width, height, weight, savedCategory.getLogisticAdjustment());
    return Math.round(shippingWeight * 1000D) / 1000D;
  }

  /**
   * Get category details
   *
   * @param storeId
   * @param categoryCode
   */
  private Category getCategory(String storeId, String categoryCode) throws Exception {
    List<Category> savedCategories =
        categoryService.findCategoryHierarchyByCategoryCode(storeId, categoryCode);
    if (CollectionUtils.isEmpty(savedCategories)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, CATEGORY_INFO_BY_CATEGORY_CODE + categoryCode
          + CATEGORY_INFO_NOT_FOUND);
    }
    if (!MASTER_CATALOG.equals(savedCategories.get(0).getCatalog().getCatalogType().name())) {
      throw new ApplicationException(ErrorCategory.VALIDATION, CATEGORY_INFO_BY_CATEGORY_CODE + categoryCode
          + CATEGORY_IS_NOT_MASTER_CATALOG);
    }
    return savedCategories.get(0);
  }
}
