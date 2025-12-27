package com.gdn.x.productcategorybase.repository;

import java.util.List;

import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;

public interface OriginalSalesCategoryRepositoryCustom {

  /**
   * filter osc list on name /code/activated flag
   *
   * @param oscCode
   * @param activated
   * @param keyword
   * @return
   */
  List<OriginalSalesCategory> findByOscCodeAndNameAndActivated(String oscCode, String keyword, Boolean activated);

}
