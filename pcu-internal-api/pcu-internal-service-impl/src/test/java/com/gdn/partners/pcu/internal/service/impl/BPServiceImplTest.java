package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.XBPFeign;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BPServiceImplTest {

  private static final String BUSINESS_PARTNER_CODE_1 = "BUSINESS_PARTNER_CODE_1";
  private static final String BUSINESS_PARTNER_NAME_1 = "BUSINESS_PARTNER_NAME_1";
  private static final String BUSINESS_PARTNER_CODE_2 = "BUSINESS_PARTNER_CODE_2";
  public static final String CM = "CM";
  private static final String REQUEST_ID = "requestId";
  public static final String CC = "CC";
  private static final String INTERNAL = "INTERNAL";

  private GdnRestListResponse<ProfileResponse> profileResponseGdnRestListResponse;
  private Map<String, ProfileResponse> profileResponseMap;
  private ProfileResponse profileResponse;

  @Mock
  private XBPFeign xbpFeign;

  @InjectMocks
  private BPServiceImpl bpService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setMerchantType(CC);
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME_1);
    profileResponse.setCompany(companyDTO);
    List<ProfileResponse> profileResponseList = new ArrayList<>();
    profileResponseList.add(profileResponse);
    ProfileResponse profileResponse2 = new ProfileResponse();
    profileResponse2.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_2);
    CompanyDTO companyDTO1 = new CompanyDTO();
    companyDTO1.setInternationalFlag(true);
    companyDTO1.setMerchantType(CM);
    profileResponse2.setCompany(companyDTO1);
    profileResponseList.add(profileResponse2);
    profileResponseGdnRestListResponse = new GdnRestListResponse<>(profileResponseList, new PageMetaData(), REQUEST_ID);
    profileResponseMap = profileResponseGdnRestListResponse.getContent().stream()
        .collect(Collectors.toMap(profileResponse1 -> profileResponse1.getBusinessPartnerCode(), Function.identity()));
    ReflectionTestUtils.setField(bpService, "internalBusinessPartnerSwitch", true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(xbpFeign);
  }

  @Test
  public void getProfileResponseMap() {
    ReflectionTestUtils.setField(bpService, "internalBusinessPartnerSwitch", false);
    when(xbpFeign.getAllActiveMerchantList(Mockito.any(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(profileResponseGdnRestListResponse);
    Map<String, ProfileResponse> profileResponseMap =
        this.bpService.getProfileResponseMap(Arrays.asList(BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_CODE_2));
    verify(xbpFeign).getAllActiveMerchantList(Mockito.any(), Mockito.eq(0), Mockito.anyInt());
    Assertions.assertNotNull(profileResponseMap);
    Assertions.assertEquals(2, profileResponseMap.size());
    Assertions.assertEquals(CC, profileResponseMap.get(BUSINESS_PARTNER_CODE_1).getCompany().getMerchantType());
  }

  @Test
  public void getProfileResponseMapInternalTest() {
    when(xbpFeign.getAllActiveMerchantList(Mockito.any(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(profileResponseGdnRestListResponse);
    Map<String, ProfileResponse> profileResponseMap =
        this.bpService.getProfileResponseMap(new ArrayList<>(List.of(INTERNAL, BUSINESS_PARTNER_CODE_2)));
    verify(xbpFeign).getAllActiveMerchantList(Mockito.any(), Mockito.eq(0), Mockito.anyInt());
    Assertions.assertNotNull(profileResponseMap);
    Assertions.assertEquals(2, profileResponseMap.size());
    Assertions.assertEquals(CC, profileResponseMap.get(BUSINESS_PARTNER_CODE_1).getCompany().getMerchantType());
  }

  @Test
  public void getProfileResponseMap_nullTest() {
    when(xbpFeign.getAllActiveMerchantList(Mockito.any(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(new GdnRestListResponse<>(null, new PageMetaData(), REQUEST_ID));
    Map<String, ProfileResponse> profileResponseMap =
        this.bpService.getProfileResponseMap(Arrays.asList(BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_CODE_2));
    verify(xbpFeign).getAllActiveMerchantList(Mockito.any(), Mockito.eq(0), Mockito.anyInt());
    Assertions.assertNotNull(profileResponseMap);
    Assertions.assertEquals(0, profileResponseMap.size());
    Assertions.assertNull(profileResponseMap.get(BUSINESS_PARTNER_CODE_1));
  }

  @Test
  public void getProfileResponseMapSuccessFalseTest() {
    when(xbpFeign.getAllActiveMerchantList(Mockito.any(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    Map<String, ProfileResponse> profileResponseMap =
        this.bpService.getProfileResponseMap(Arrays.asList(BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_CODE_2));
    verify(xbpFeign).getAllActiveMerchantList(Mockito.any(), Mockito.eq(0), Mockito.anyInt());
    Assertions.assertNotNull(profileResponseMap);
    Assertions.assertEquals(0, profileResponseMap.size());
    Assertions.assertNull(profileResponseMap.get(BUSINESS_PARTNER_CODE_1));
  }

  @Test
  public void getProfileResponseXBPExceptionMap() {
    when(xbpFeign.getAllActiveMerchantList(Mockito.any(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(null);
    Map<String, ProfileResponse> profileResponseMap =
        this.bpService.getProfileResponseMap(Arrays.asList(BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_CODE_2));
    verify(xbpFeign).getAllActiveMerchantList(Mockito.any(), Mockito.eq(0), Mockito.anyInt());
    Assertions.assertNotNull(profileResponseMap);
    Assertions.assertEquals(0, profileResponseMap.size());
  }

  @Test
  public void getProfileResponseEmptyRequestMap() {
    when(xbpFeign.getAllActiveMerchantList(Mockito.any(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(null);
    Map<String, ProfileResponse> profileResponseMap =
        this.bpService.getProfileResponseMap(new ArrayList<>());
    verify(xbpFeign, times(0)).getAllActiveMerchantList(Mockito.any(), Mockito.eq(0), Mockito.anyInt());
    Assertions.assertNotNull(profileResponseMap);
    Assertions.assertEquals(0, profileResponseMap.size());
  }

  @Test
  public void getProfileResponseByBusinessPartnerCode() {
    when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, REQUEST_ID));
    ProfileResponse profileResponseByBusinessPartnerCode =
        bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Assertions.assertNotNull(profileResponseByBusinessPartnerCode);
    Assertions.assertEquals(CC, profileResponseByBusinessPartnerCode.getCompany().getMerchantType());
  }

  @Test
  public void getProfileResponseByBusinessPartnerCodeXBPException() {
    when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    ProfileResponse profileResponseByBusinessPartnerCode =
        bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Assertions.assertNull(profileResponseByBusinessPartnerCode);
  }

  @Test
  public void getProfileResponseByNullBusinessPartnerCode() {
    ProfileResponse profileResponseByBusinessPartnerCode =
        bpService.getProfileResponseByBusinessPartnerCode(null);
    Assertions.assertNull(profileResponseByBusinessPartnerCode);
  }
}