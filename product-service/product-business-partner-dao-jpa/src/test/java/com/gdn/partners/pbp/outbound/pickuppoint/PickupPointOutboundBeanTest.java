package com.gdn.partners.pbp.outbound.pickuppoint;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.dto.BulkRequest;
import com.gdn.x.businesspartner.dto.CncActivatedPickupPointResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodesRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;

public class PickupPointOutboundBeanTest {

  @InjectMocks
  private PickupPointOutboundBean pickupPointOutboundBean;

  @Mock
  private XbpFeign xbpFeign;

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String EXTERNAL_PP_CODE = "ext-pp-code-1";
  private static final String PP_CODE = "pp-code-1";

  private static final String ERROR_MSG = "error_msg";
  private static final String ERROR_CODE = "error_code";

  private List<String> externalPickupPointCodes = new ArrayList();
  private GdnRestSingleResponse<ExternalPickupPointCodeResponse> externalPickupPointResponse;
  private GdnRestSingleResponse<ExternalPickupPointCodeResponse> failedPickupPointResponse;
  private GdnRestSingleResponse<CncActivatedPickupPointResponse> cncActivatedPickupPointSingleResponse;
  private CncActivatedPickupPointResponse cncActivatedPickupPointResponse;
  private List<String> pickupPointCodes;

  private PickupPointResponse pickupPointResponse;
  private List<PickupPointResponse> pickupPointResponses;
  private GdnRestListResponse<PickupPointResponse> pickupPointResponseGdnRestListResponse;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    externalPickupPointCodes.add(EXTERNAL_PP_CODE);

    List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetails =
        new ArrayList<>();
    ExternalPickupPointCodeResponseDetail responseDetail = new ExternalPickupPointCodeResponseDetail();
    responseDetail.setCode(PP_CODE);
    responseDetail.setExternalPickupPointCode(EXTERNAL_PP_CODE);
    externalPickupPointCodeResponseDetails.add(responseDetail);


    ExternalPickupPointCodeResponse response = new ExternalPickupPointCodeResponse();
    response.setExternalPickupPointCodeResponseDetails(externalPickupPointCodeResponseDetails);

    externalPickupPointResponse = new GdnRestSingleResponse(response, REQUEST_ID);
    failedPickupPointResponse = new GdnRestSingleResponse(ERROR_MSG, ERROR_CODE, false, null, REQUEST_ID);

    pickupPointCodes = Arrays.asList(PP_CODE);

    cncActivatedPickupPointResponse = new CncActivatedPickupPointResponse();
    cncActivatedPickupPointResponse.setCncActivatedPickupPointCodes(pickupPointCodes);

    cncActivatedPickupPointSingleResponse =
        new GdnRestSingleResponse<>(cncActivatedPickupPointResponse, REQUEST_ID);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponses = new ArrayList<>();
    pickupPointResponses.add(pickupPointResponse);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.xbpFeign);
  }

  @Test
  public void checkExternalPickupPointCodeAvailability() throws Exception {
    Mockito.when(this.xbpFeign.filterPickupPointByExternalPickupPointCodes(
            Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(REQUEST_ID), Mockito.eq(MERCHANT_CODE),
            Mockito.eq(new ExternalPickupPointCodesRequest(externalPickupPointCodes))))
        .thenReturn(externalPickupPointResponse);
    this.pickupPointOutboundBean.checkExternalPickupPointCodeAvailability(REQUEST_ID, USERNAME, MERCHANT_CODE,
        externalPickupPointCodes);

    Mockito.verify(this.xbpFeign)
        .filterPickupPointByExternalPickupPointCodes(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(REQUEST_ID),
            Mockito.eq(MERCHANT_CODE), Mockito.eq(new ExternalPickupPointCodesRequest(externalPickupPointCodes)));
  }

  @Test
  public void checkExternalPickupPointCodeAvailability_emptyExternalPickupPointCodes() throws Exception {
    List<ExternalPickupPointCodeResponseDetail> result = this.pickupPointOutboundBean
        .checkExternalPickupPointCodeAvailability(REQUEST_ID, USERNAME, MERCHANT_CODE, null);

    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  public void checkExternalPickupPointCodeAvailability_failed() throws Exception {

    Mockito.when(this.xbpFeign.filterPickupPointByExternalPickupPointCodes(
            Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(REQUEST_ID), Mockito.eq(MERCHANT_CODE),
            Mockito.eq(new ExternalPickupPointCodesRequest(externalPickupPointCodes))))
        .thenReturn(failedPickupPointResponse);
    try {
      this.pickupPointOutboundBean.checkExternalPickupPointCodeAvailability(REQUEST_ID, USERNAME, MERCHANT_CODE,
          externalPickupPointCodes);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(this.xbpFeign)
          .filterPickupPointByExternalPickupPointCodes(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(REQUEST_ID),
              Mockito.eq(MERCHANT_CODE), Mockito.eq(new ExternalPickupPointCodesRequest(externalPickupPointCodes)));
    }
  }

  @Test
  public void checkExternalPickupPointCodeAvailability_Valid_SuccessWithNullValue() throws Exception {

    failedPickupPointResponse.setSuccess(true);

    Mockito.when(this.xbpFeign.filterPickupPointByExternalPickupPointCodes(
            Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(REQUEST_ID), Mockito.eq(MERCHANT_CODE),
            Mockito.eq(new ExternalPickupPointCodesRequest(externalPickupPointCodes))))
        .thenReturn(failedPickupPointResponse);
    try {
      this.pickupPointOutboundBean.checkExternalPickupPointCodeAvailability(REQUEST_ID, USERNAME, MERCHANT_CODE,
          externalPickupPointCodes);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(this.xbpFeign)
          .filterPickupPointByExternalPickupPointCodes(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(REQUEST_ID),
              Mockito.eq(MERCHANT_CODE), Mockito.eq(new ExternalPickupPointCodesRequest(externalPickupPointCodes)));
    }
  }

  @Test
  public void testGetByPickupPointCodes_success() throws Exception {
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponses = new ArrayList<>();
    pickupPointResponses.add(pickupPointResponse);

    pickupPointResponseGdnRestListResponse =
        new GdnRestListResponse<>(pickupPointResponses, new PageMetaData(0, 0, 10), REQUEST_ID);

    Mockito.when(
            this.xbpFeign.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, REQUEST_ID, new BulkRequest<>(pickupPointCodes)))
        .thenReturn(pickupPointResponseGdnRestListResponse);

    this.pickupPointOutboundBean.getByPickupPointCodes(REQUEST_ID, pickupPointCodes);

    Mockito.verify(this.xbpFeign)
        .getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, new BulkRequest<>(pickupPointCodes));
  }

  @Test
  public void testGetByPickupPointCodes_failedNullResponse() throws Exception {
    pickupPointResponseGdnRestListResponse = new GdnRestListResponse<>(null, new PageMetaData(0, 0, 0), REQUEST_ID);

    Mockito.when(
            this.xbpFeign.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, REQUEST_ID, new BulkRequest<>(pickupPointCodes)))
        .thenReturn(pickupPointResponseGdnRestListResponse);

    try {
      this.pickupPointOutboundBean.getByPickupPointCodes(REQUEST_ID, pickupPointCodes);
    } catch (Exception e) {
      Mockito.verify(this.xbpFeign)
          .getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, REQUEST_ID, new BulkRequest<>(pickupPointCodes));
    }
  }

  @Test
  public void testGetByPickupPointCodes_failedSuccessFalse() throws Exception {
    pickupPointResponseGdnRestListResponse = new GdnRestListResponse<>("", "", false, REQUEST_ID);

    Mockito.when(
            this.xbpFeign.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, REQUEST_ID, new BulkRequest<>(pickupPointCodes)))
        .thenReturn(pickupPointResponseGdnRestListResponse);

    try {
      this.pickupPointOutboundBean.getByPickupPointCodes(REQUEST_ID, pickupPointCodes);
    } catch (Exception e) {
      Mockito.verify(this.xbpFeign)
          .getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, REQUEST_ID, new BulkRequest<>(pickupPointCodes));
    }
  }
}
