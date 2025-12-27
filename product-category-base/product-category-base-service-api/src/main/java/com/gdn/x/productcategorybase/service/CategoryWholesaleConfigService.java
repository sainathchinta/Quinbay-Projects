package com.gdn.x.productcategorybase.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;

public interface CategoryWholesaleConfigService {

  WholesaleMappingResponse findByStoreIdAndCategoryId(String storeId, String categoryId, String categoryCode)
      throws Exception;

  CategoryUpdateHistoryDTO updateCategoryWholesaleMappings(String storeId,
      WholesalePriceConfiguration wholesalePriceConfiguration, Category category,
      boolean isParentCategory) throws JsonProcessingException;

  void updateCategoryWholesaleConfiguration(String storeId, WholesalePriceConfiguration wholesalePriceConfiguration);

  void updateWholesaleConfigForChildCategories(String storeId, WholesalePriceConfiguration wholesalePriceConfiguration,
      Category category) throws Exception;

}
