package com.gdn.x.product.dao.api;

import com.gdn.x.product.model.entity.SystemParameter;


public interface SystemParameterRepositoryCustom {

  /**
   *
   * @param systemParameter
   * @return null if not found
   */
  SystemParameter findAndUpdate(SystemParameter systemParameter);


}
