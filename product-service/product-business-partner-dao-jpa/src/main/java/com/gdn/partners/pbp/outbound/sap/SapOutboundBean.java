package com.gdn.partners.pbp.outbound.sap;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.outbound.sap.feign.SapFeign;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class SapOutboundBean implements SapOutbound {

  private static final String ERROR_MESSAGE = "Error when getting data from SAP for request: {} error : {}";

  @Autowired
  private SapFeign sapFeign;

  @Override
  public CogsValueResponse getCogsValueResponse(String materialCode) {
    GdnRestSimpleResponse<CogsValueResponse> response = sapFeign.getCogsValue(materialCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(ERROR_MESSAGE, materialCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }
}
