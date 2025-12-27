package com.gdn.mta.product.service.generator;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.service.generator.GeneratorService;
import com.gdn.mta.product.util.BaseGenerator;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

@Service
@Transactional(readOnly = true)
public class GeneratorServiceBean implements GeneratorService {

  @Autowired
  private CategoryRepository categoryRepository;

  @Override
  public Double generateShippingWeight(Double length, Double width, Double height, Double weight, String categoryCode)
      throws Exception {
    List<CategoryResponse> savedCategories = this.categoryRepository.findHierarchyByCategoryCode(categoryCode);
    if (savedCategories == null || savedCategories.isEmpty()) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Category with category code " + categoryCode
          + " is not found");
    }
    CategoryResponse savedCategory = savedCategories.get(0);
    if (!CatalogType.MASTER_CATALOG.name().equals(savedCategory.getCatalog().getCatalogType())) {
      throw new ApplicationException(ErrorCategory.VALIDATION, "Category with category code " + categoryCode
          + " is not master catalog");
    }
    Double shippingWeight =
        BaseGenerator.generateShippingWeight(length, width, height, weight, savedCategory.getLogisticAdjustment());
    return Math.round(shippingWeight * 1000D) / 1000D;
  }

}
