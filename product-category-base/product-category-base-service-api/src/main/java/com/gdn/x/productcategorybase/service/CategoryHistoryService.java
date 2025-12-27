package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import com.gdn.x.productcategorybase.entity.CategoryHistory;
import org.springframework.data.domain.Page;

import java.util.List;

public interface CategoryHistoryService {

  /**
   * Save category history
   *
   * @param categoryHistoryEventModel Event Model
   */
   CategoryHistory saveCategoryHistory(CategoryHistoryEventModel categoryHistoryEventModel);

  /**
   *
   * @param storeId
   * @param categoryCode
   * @param page
   * @param size
   * @return
   */
  Page<CategoryHistoryResponse> fetchCategoryHistory(String storeId, String categoryCode, int page,
      int size);
}
