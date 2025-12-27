package com.gdn.mta.product.repository;

import com.gdn.mta.margin.webmodel.MarginCategoryResponse;

public interface MarginRepository {
  MarginCategoryResponse getMarginForStoreIdAndCategoryCode(String storeId , String categoryCode)
      throws Exception;

}
