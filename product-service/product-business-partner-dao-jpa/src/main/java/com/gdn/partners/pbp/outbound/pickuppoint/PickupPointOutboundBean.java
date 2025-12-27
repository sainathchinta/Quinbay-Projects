package com.gdn.partners.pbp.outbound.pickuppoint;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.dto.BulkRequest;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodesRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;

@Component
public class PickupPointOutboundBean implements PickupPointOutbound {

  @Autowired
  private XbpFeign xbpFeign;

  @Override
  public List<ExternalPickupPointCodeResponseDetail> checkExternalPickupPointCodeAvailability(String requestId,
      String username, String merchantCode, List<String> externalPickupPointCodes) throws Exception {
    if (CollectionUtils.isEmpty(externalPickupPointCodes)) {
      return Collections.emptyList();
    }

    GdnRestSingleResponse<ExternalPickupPointCodeResponse> response =
        xbpFeign.filterPickupPointByExternalPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId, merchantCode,
            new ExternalPickupPointCodesRequest(externalPickupPointCodes));

    if (!response.isSuccess() || response.getValue() == null) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }

    return response.getValue().getExternalPickupPointCodeResponseDetails();
  }

  @Override
  public List<PickupPointResponse> getByPickupPointCodes(String requestId, List<String> pickupPointCodes)
      throws Exception {

    final GdnRestListResponse<PickupPointResponse> response =
        xbpFeign.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId, new BulkRequest<>(pickupPointCodes));

    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }

    return response.getContent();
  }
}
