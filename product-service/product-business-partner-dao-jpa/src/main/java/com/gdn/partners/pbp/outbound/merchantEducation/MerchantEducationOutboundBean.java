package com.gdn.partners.pbp.outbound.merchantEducation;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.merchantEducation.feign.MerchantEducationFeign;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class MerchantEducationOutboundBean implements MerchantEducationOutbound {

  @Autowired
  private MerchantEducationFeign merchantEducationFeign;


  @Override
  public NotificationSettings findByUsernameAndStoreCode(String storeId, String userName, String storeCode) {
    SingleBaseResponse<NotificationSettings> response =
        merchantEducationFeign.findByUsernameAndStoreCode(storeId, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.REQUIRED_ID,
            Constants.DEFAULT_USERNAME, userName, storeCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("Failed to get response from merchantEducation  : {}", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }
}
