package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;

public class PickupPointServiceTest {

  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "bp-code";
  private static final int page = 0;
  private static final int size = 1;
  private static final String PP_CODE = "pp-code";
  private static final String PP_CODE_1 = "pp-code-1";
  private static final String PP_NAME = "pp-name";
  private static final String PP_NAME_1 = "pp-name-1";

  private PickupPointFilterRequest pickupPointFilterRequest;
  private PickupPointResponse pickupPointResponse;
  private PickupPointResponse pickupPointResponse1;

  @InjectMocks
  private PickupPointServiceBean pickupPointServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @BeforeEach
  public void setUp(){
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(pickupPointServiceBean, "fetchPickupPointSize", size);

    pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setWaitingDeletion(false);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE);
    pickupPointResponse.setName(PP_NAME);

    pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE);
    pickupPointResponse1.setName(PP_NAME);
  }

  @AfterEach
  public void postProcess(){
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
  }

  @Test
  public void getPickupPointSummaryFilterTest() throws Exception {
    Mockito.when(businessPartnerRepository
      .filterBusinessPartnerPickupPointV2(page, size, pickupPointFilterRequest)).thenReturn(
      new PageImpl<>(Collections.singletonList(pickupPointResponse), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository
      .filterBusinessPartnerPickupPointV2(1, size, pickupPointFilterRequest)).thenReturn(
      new PageImpl<>(Collections.singletonList(pickupPointResponse),  PageRequest.of(0, 1), 1));
    pickupPointServiceBean.getPickupPointSummaryFilter(page, pickupPointFilterRequest);
    Mockito.verify(businessPartnerRepository)
      .filterBusinessPartnerPickupPointV2(page, size, pickupPointFilterRequest);
    Mockito.verify(businessPartnerRepository)
      .filterBusinessPartnerPickupPointV2(1, size, pickupPointFilterRequest);
  }

  @Test
  public void getSinglePickupPointSummaryFilterTest() throws Exception {
    Mockito.when(businessPartnerRepository
      .filterBusinessPartnerPickupPointV2(page, size, pickupPointFilterRequest)).thenReturn(
      new PageImpl<>(Collections.singletonList(pickupPointResponse), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository
      .filterBusinessPartnerPickupPointV2(1, size, pickupPointFilterRequest)).thenReturn(
      new PageImpl<>(Collections.singletonList(pickupPointResponse),  PageRequest.of(0, 1), 1));
    pickupPointServiceBean.getSinglePickupPointSummaryFilter(page, size, pickupPointFilterRequest);
    Mockito.verify(businessPartnerRepository)
      .filterBusinessPartnerPickupPointV2(page, size, pickupPointFilterRequest);
  }
}
