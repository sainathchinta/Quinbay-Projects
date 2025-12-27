package com.gdn.mta.bulk.repository.pcb;

import java.util.List;

public interface ProductCategoryBaseRepository {

  /**
   * Get all child categories for bulk download by list of categoryCode
   *
   * @param requestId
   * @param username
   * @param categoryCodes
   * @return
   * @throws Exception
   */
  List<String> getAllChildCategoriesFromC1CategoryCode(String requestId, String username,
      List<String> categoryCodes) throws Exception;

}
