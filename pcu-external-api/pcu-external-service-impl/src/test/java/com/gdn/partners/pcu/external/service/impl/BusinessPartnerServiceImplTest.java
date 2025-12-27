package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.feign.XBPFeign;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

public class BusinessPartnerServiceImplTest {
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICK_UP_POINT_NAME = "pickupPoint1";
  private ProfileResponse profileResponse;
  private PickupPointDTO pickupPointDTO;
  private static final String BUSINESSPARTNER_CODE = "businessPartnerCode";
  private static final String PROFILE_ID = "profile_id";

  @Mock
  private XBPFeign xbpFeign;

  @InjectMocks
  private BusinessPartnerServiceImpl businessPartnerService;


  private GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setActivated(true);
    pickupPointDTO.setName(PICK_UP_POINT_NAME);
    profileResponse = new ProfileResponse();
    profileResponse.setActivated(true);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setPickupPoints(Arrays.asList(pickupPointDTO));
    gdnRestSingleResponse = new GdnRestSingleResponse<ProfileResponse>();
    gdnRestSingleResponse.setSuccess(Boolean.TRUE);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setId(PROFILE_ID);
    gdnRestSingleResponse.setValue(profileResponse);
  }

  @Test
  public void getProfileDetailByIdTest(){
    Mockito.when(xbpFeign.getProfileDetailById(PROFILE_ID)).thenReturn(gdnRestSingleResponse);
    ProfileResponse response = businessPartnerService.getProfileDetailById(PROFILE_ID);
    Mockito.verify(xbpFeign).getProfileDetailById(PROFILE_ID);
    Assertions.assertEquals(PROFILE_ID, response.getId());
  }

  @Test
  public void getProfileDetailById_whenClientExceptionTest() {
    ProfileResponse response = null;
    Mockito.when(xbpFeign.getProfileDetailById(PROFILE_ID)).thenReturn(null);
    try {
      response = businessPartnerService.getProfileDetailById(PROFILE_ID);
    } catch (ClientException ex) {
      Mockito.verify(xbpFeign).getProfileDetailById(PROFILE_ID);
      Assertions.assertNull(response);
    }
  }

  @Test
  public void filterBusinessPartnerTest() {
    Mockito.when(xbpFeign.filter(0, 1, new PickupPointFilterRequest()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    businessPartnerService.filterBusinessPartner(0, 1, new PickupPointFilterRequest());
    Mockito.verify(xbpFeign).filter(0, 1, new PickupPointFilterRequest());
  }

  @Test
  public void updateDefaultPickupPointCodeTest() {
    Mockito.when(xbpFeign.updateDefaultPickupPointCode(new MarkPickupPointAsDefaultRequest()))
        .thenReturn(new BaseResponse(null, null, true, null));
    businessPartnerService.updateDefaultPickupPointCode(new MarkPickupPointAsDefaultRequest());
    Mockito.verify(xbpFeign).updateDefaultPickupPointCode(new MarkPickupPointAsDefaultRequest());
  }

  @Test
  public void updateDefaultConfigurationTest() {
    Mockito.when(xbpFeign.updateDefaultConfiguration(Constants.PRODUCT_SETTING, new ProfileRequest()))
        .thenReturn(gdnRestSingleResponse);
    businessPartnerService.updateDefaultConfiguration(new ProfileRequest());
    Mockito.verify(xbpFeign).updateDefaultConfiguration(Constants.PRODUCT_SETTING, new ProfileRequest());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(xbpFeign);
  }

  @Test
  public void filterByBusinessPartnerCodeTest(){
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE))
        .thenReturn(gdnRestSingleResponse);
    ProfileResponse response =
        businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    Assertions.assertEquals(BUSINESSPARTNER_CODE, response.getBusinessPartnerCode());
  }

  @Test
  public void filterByBusinessPartnerCode_whenClientExceptionTest() {

    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE));
    } finally {
      Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    }
  }

  @Test
  public void getAllPickupPointsForBusinessPartnerTest() {
    PickupPointFilterRequest pickupPointFilterRequest =
        PickupPointFilterRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    ReflectionTestUtils.setField(businessPartnerService, "pickupPointFilterBatchSize", 1);
    Mockito.when(xbpFeign.filter(0, 1, pickupPointFilterRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 2), null));
    Mockito.when(xbpFeign.filter(1, 1, pickupPointFilterRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 2), null));
    businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign).filter(0, 1, pickupPointFilterRequest);
    Mockito.verify(xbpFeign).filter(1, 1, pickupPointFilterRequest);
  }

  @Test
  public void getPickupPointsForBusinessPartnerTest() {
    Set<String> codes = Collections.singleton("ppcode");
    PickupPointFilterRequest pickupPointFilterRequest =
      PickupPointFilterRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
        .codes(codes)
        .build();
    ReflectionTestUtils.setField(businessPartnerService, "pickupPointFilterBatchSize", 1);
    Mockito.when(xbpFeign.filter(0, 1, pickupPointFilterRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 2), null));
    Mockito.when(xbpFeign.filter(1, 1, pickupPointFilterRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 2), null));
    businessPartnerService.getPickupPointsForBusinessPartner(BUSINESS_PARTNER_CODE, codes);
    Mockito.verify(xbpFeign).filter(0, 1, pickupPointFilterRequest);
    Mockito.verify(xbpFeign).filter(1, 1, pickupPointFilterRequest);
  }

  @Test
  public void getAllPickupPointsForBusinessPartnerErrorTest() {
    PickupPointFilterRequest pickupPointFilterRequest =
        PickupPointFilterRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    ReflectionTestUtils.setField(businessPartnerService, "pickupPointFilterBatchSize", 1);
    Mockito.when(xbpFeign.filter(0, 1, pickupPointFilterRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(0, 1, 2), null));
    try {
      Assertions.assertThrows(ClientException.class,
          () -> businessPartnerService.getAllPickupPointsForBusinessPartner(BUSINESS_PARTNER_CODE));
    }
    finally {
      Mockito.verify(xbpFeign).filter(0, 1, pickupPointFilterRequest);
    }
  }

  @Test
  public void getPickupPointByCodeTest() {
    Mockito.when(xbpFeign.filterByCode(PICK_UP_POINT_NAME))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new PickupPointOutboundResponse(), null));
    businessPartnerService.getPickupPointByCode(PICK_UP_POINT_NAME);
    Mockito.verify(xbpFeign).filterByCode(PICK_UP_POINT_NAME);
  }

  @Test
  public void getPickupPointByCodeErrorTest() {
    Mockito.when(xbpFeign.filterByCode(PICK_UP_POINT_NAME))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new PickupPointOutboundResponse(), null));
    try {
      Assertions.assertThrows(ClientException.class,
          () -> businessPartnerService.getPickupPointByCode(PICK_UP_POINT_NAME));
    } finally {
      Mockito.verify(xbpFeign).filterByCode(PICK_UP_POINT_NAME);
    }
  }

  @Test
  public void getBusinessPartnerDetailsByListTest() {
    Mockito.when(xbpFeign.getBusinessPartnerDetailsByList(0, 1,
        BusinessPartnerFilterRequest.builder().businessPartnerCodes(Set.of(BUSINESS_PARTNER_CODE))
            .build())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.singletonList(profileResponse),
            new PageMetaData(1, 0, 1), null));
    SimpleMapStringResponse response = businessPartnerService.getBusinessPartnerDetailsByList(0, 1,
        BusinessPartnerFilterRequest.builder().businessPartnerCodes(Set.of(BUSINESS_PARTNER_CODE))
            .build());
    Assertions.assertTrue(response.getValue().containsKey(BUSINESS_PARTNER_CODE));
    Mockito.verify(xbpFeign).getBusinessPartnerDetailsByList(0, 1,
        BusinessPartnerFilterRequest.builder().businessPartnerCodes(Set.of(BUSINESS_PARTNER_CODE))
            .build());
  }
}