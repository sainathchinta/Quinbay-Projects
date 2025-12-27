package com.gdn.partners.pbp.outbound.warehouse;

import com.gda.mta.product.dto.UomStockValidationRequest;
import com.gda.mta.product.dto.response.UomStockValidationResponse;

import java.util.List;

public interface OMSOutBound {

  /**
   * validate if uom is editable from OMS side
   *
   * @param request
   * @return
   */
  List<UomStockValidationResponse> validateUomEditable(UomStockValidationRequest request);
}
