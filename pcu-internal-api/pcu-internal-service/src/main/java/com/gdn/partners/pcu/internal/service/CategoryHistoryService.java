package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.web.model.response.CategoryHistoryWebResponse;
import org.springframework.data.domain.Page;

public interface CategoryHistoryService {

  /**
   *
   * @param storeId
   * @param categoryCode
   * @param page
   * @param size
   * @return
   */
  Page<CategoryHistoryWebResponse> categoryHistory(String storeId, String categoryCode, int page,
      int size);
}
