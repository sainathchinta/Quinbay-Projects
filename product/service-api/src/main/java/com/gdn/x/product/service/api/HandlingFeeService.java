package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;

public interface HandlingFeeService {

  /**
   * @param storeId must not be blank
   * @param handlingFeeList must not be null or empty list
   * @return never null
   */
  HandlingFeeResponse calculateHandlingFee(String storeId, List<HandlingFeeRequest> handlingFeeList);

  /**
   * @param storeId must not be blank
   * @return systemParameter
   */
  SystemParameter getAllSettingOfHandlingFee(String storeId);

  /**
   * @param systemParameter must not be null
   */
  void updateAllSettingOfHandlingFee(SystemParameter systemParameter);
}
