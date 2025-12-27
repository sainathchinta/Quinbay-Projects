package com.gdn.mta.product.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.dto.PickupPointResponse;

@Repository
public class PickupPointRepositoryBean implements PickupPointRepository {

  @Autowired
  private XbpFeign xbpFeign;

  @Override
  public PickupPointResponse findByPickupPointCode(String pickupPointCode) throws Exception {
    GdnRestSingleResponse<PickupPointResponse> response =
        xbpFeign.getByPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), pickupPointCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    PickupPointResponse pickupPoint = response.getValue();
    return pickupPoint;
  }
}
