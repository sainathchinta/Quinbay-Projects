package com.gdn.mta.product.service;

import java.util.List;

import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gdn.common.exception.ApplicationException;

public interface ProductDataAutoFixHistoryService {
  /**
   * Insert new productDataAutoFixHistory
   *
   * @param productDataAutoFixHistoryDtoList
   */
  void saveHistory(List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList) throws ApplicationException;
}
