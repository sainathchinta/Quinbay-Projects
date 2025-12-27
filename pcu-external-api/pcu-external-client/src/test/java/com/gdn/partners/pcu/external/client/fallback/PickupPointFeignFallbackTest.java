package com.gdn.partners.pcu.external.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;

/**
 * Created by govind on 19/12/2018 AD.
 */
public class PickupPointFeignFallbackTest {

  private static final String BUSINESSPARTNER_CODE = "businesspartner-code";
  private static final String PICKUPPOINT_CODE= "pickuppoint-code";

  private PickupPointFeignFallback pickupPointFeignFallback = new PickupPointFeignFallback();

  @Test
  public void markDefaultAddressTest(){
    GdnBaseRestResponse response = pickupPointFeignFallback.markDefaultAddress(BUSINESSPARTNER_CODE, PICKUPPOINT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void fetchAccessiblePickupPointsTest(){
    GdnBaseRestResponse response = pickupPointFeignFallback.fetchAccessiblePickupPoints(0,10, new PickupPointFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
}
