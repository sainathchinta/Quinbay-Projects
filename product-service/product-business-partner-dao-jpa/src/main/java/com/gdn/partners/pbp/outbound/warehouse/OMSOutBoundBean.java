package com.gdn.partners.pbp.outbound.warehouse;

import com.blibli.oss.backend.common.model.response.Response;
import com.gda.mta.product.dto.UomStockValidationRequest;
import com.gda.mta.product.dto.response.UomStockValidationResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.enums.ErrorMessage;
import com.gdn.partners.pbp.outbound.warehouse.feign.OMSFeign;
import com.gdn.x.product.constants.ErrorMessages;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Slf4j
@Service
@RequiredArgsConstructor
public class OMSOutBoundBean implements OMSOutBound {

  private final OMSFeign omsFeign;

  @Override
  public List<UomStockValidationResponse> validateUomEditable(UomStockValidationRequest request) {
    Response<List<UomStockValidationResponse>> response =
        omsFeign.validateUomEditable(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (Objects.isNull(response) || CollectionUtils.isEmpty(response.getData())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
    }
    return response.getData();
  }
}
