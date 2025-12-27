package com.gdn.mta.product.repository;

import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.dto.PickupPointResponse;

public class PickupPointRepositoryBeanTest {

  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-0000001";
  private static final String DEFAULT_PICKUP_POINT_CODE_2 = "PP-0000002";

  @Mock
  private XbpFeign xbpFeign;

  @InjectMocks
  private PickupPointRepositoryBean pickupPointRepositoryBean;


  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    PickupPointResponse pickupPointData = new PickupPointResponse();

    GdnRestSingleResponse<PickupPointResponse> responseDetail =
        new GdnRestSingleResponse<PickupPointResponse>(pickupPointData, DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<PickupPointResponse> responseDetailError =
        new GdnRestSingleResponse<PickupPointResponse>("Read Timeout",
            ErrorCategory.UNSPECIFIED.getCode(), false, null, DEFAULT_REQUEST_ID);

    Mockito.when(
        xbpFeign.getByPickupPointCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
            Mockito.eq(DEFAULT_PICKUP_POINT_CODE))).thenReturn(responseDetail);
    Mockito.when(
        xbpFeign.getByPickupPointCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
            Mockito.eq(DEFAULT_PICKUP_POINT_CODE_2))).thenReturn(responseDetailError);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.xbpFeign);
  }

  @Test
  public void testFindByPickupPointCode() throws Exception {
    pickupPointRepositoryBean.findByPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    Mockito.verify(xbpFeign).getByPickupPointCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
        Mockito.eq(DEFAULT_PICKUP_POINT_CODE));
  }

  @Test
  public void testFindByPickupPointCodeWithMDCValue() throws Exception {
     MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    pickupPointRepositoryBean.findByPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    Mockito.verify(xbpFeign).getByPickupPointCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
        Mockito.eq(DEFAULT_PICKUP_POINT_CODE));
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
  }

  @Test
  public void testFindByPickupPointCodeWithError() throws Exception {
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        pickupPointRepositoryBean.findByPickupPointCode(DEFAULT_PICKUP_POINT_CODE_2);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(xbpFeign).getByPickupPointCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
        Mockito.eq(DEFAULT_PICKUP_POINT_CODE_2));
  }

}
