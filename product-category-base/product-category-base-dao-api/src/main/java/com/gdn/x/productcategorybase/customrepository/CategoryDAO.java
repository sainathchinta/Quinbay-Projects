package com.gdn.x.productcategorybase.customrepository;

import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;

import java.util.List;

/**
 * Created by virajjasani on 25/07/16.
 */
public interface CategoryDAO {

  List<CustomCategoryDto> getCategoriesFromCatalogType(String storeId, CatalogType catalogType)
      throws Exception;
}
