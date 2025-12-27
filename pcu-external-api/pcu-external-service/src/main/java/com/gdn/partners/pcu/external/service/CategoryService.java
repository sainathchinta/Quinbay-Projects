package com.gdn.partners.pcu.external.service;

import java.util.List;

import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

/**
 * Created by govind on 14/12/2018 AD.
 */
public interface CategoryService {

  /**
   * find internal activation interval
   *
   * @param categoryCode must not blank
   * @return
   */
  String findInternalActivationIntervalInDaysByCategoryCode(String categoryCode);


  /**
   * find category detail for category ID
   *
   * @param categoryId must not blank
   * @return
   */
  CategoryDetailResponse getCategoryDetail(String categoryId);

  /**
   * find category detail for category ID
   *
   * @param attributeId must not blank
   * @return
   */
  AttributeResponse getAttributeDetail(String attributeId);

  /**
   * find category detail for category codes
   *
   * @param categoryCodes
   * @return
   */
  List<CategoryWebResponse> getCategoriesByCategoryCodes(List<String> categoryCodes) throws Exception;
}
