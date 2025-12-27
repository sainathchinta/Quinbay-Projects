package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class UserPicServiceImplTest {

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @InjectMocks
  private UserPicServiceImpl userPicService;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  private static final String mppAllowedSellers = "CM,CC";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT_CODE = "PP-3003332";


  @Test
  public void validateUserPicPickupPoints_ExcludedChannelId() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("Host-To-Host");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn( Set.of("PP-3003332","PP-3003331"));
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        getMppEnabledBusinessPartnerProfile());
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003331"));
  }

  @Test
  public void validateUserPicPickupPoints_NotExcludedChannelId() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332","PP-3003331"));
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        getMppEnabledBusinessPartnerProfile());
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003331"));
  }

  @Test
  public void validateUserPicPickupPoints_UserPicSwitchDisabled() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332","PP-3003331"));
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        getMppEnabledBusinessPartnerProfile());
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",false);
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003331"));
  }

  @Test
  public void validateUserPicPickupPoints_UserHasAcessToAllPickUpPoint() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332","PP-3003331"));
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);

    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        getMppEnabledBusinessPartnerProfile());
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003331"));
  }

  @Test
  public void validateUserPicPickupPoints_UserIsNotMppEnabled() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332","PP-3003331"));
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);
    ProfileResponse businessPartnerProfile = getMppNotEnabledBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        businessPartnerProfile);
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003331"));
  }

  @Test()
  public void validateUserPicPickupPoints_UserAccessInvalidPickUpPoint() throws Exception {
    Exception thrownException = new Exception();
    try{
      ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
      ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
      Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
      Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332","PP-3003331"));
      ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);
      ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
          businessPartnerProfile.getBusinessPartnerCode());
      when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
          businessPartnerProfile);
      userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003335"));
    }
    catch (Exception e){
      thrownException = e;
    }
    finally {
      assertEquals(ValidationException.class,thrownException.getClass());
      assertEquals(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT,thrownException.getMessage());
    }
  }

  @Test()
  public void validateUserPicPickupPoints_InvalidBusinessPartnerCode() throws Exception {
    Exception thrownException = new Exception();
    try{
      ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
      ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
      Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
      Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332","PP-3003331"));
      ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);

      ProfileResponse businessPartnerProfile = null;
      when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
          null);
      when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
          null);
      userPicService.validateUserPicPickupPoints(Set.of("PP-3003332", "PP-3003335"));
    }
    catch (Exception e){
      thrownException = e;
    }
    finally {
      assertEquals(ApplicationRuntimeException.class,thrownException.getClass());
    }
  }

  @Test
  public void validateUserPicPickupPoints_success() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332"," PP-3003331","PP-3003333","PP-3003334"));
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        businessPartnerProfile);
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332"," PP-3003331","PP-3003333","PP-3003334"));
  }

  @Test
  public void validateUserPicPickupPoints_emptyRequest() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332"," PP-3003331","PP-3003333","PP-3003334"));
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        businessPartnerProfile);
    userPicService.validateUserPicPickupPoints(Set.of());
  }

  @Test
  public void validateUserPicPickupPoints_allAccess() throws Exception {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of());
    ReflectionTestUtils.setField(userPicService,"userPicFeatureSwitch",true);
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        businessPartnerProfile.getBusinessPartnerCode());
    when(businessPartnerService.filterByBusinessPartnerCode(anyString())).thenReturn(
        businessPartnerProfile);
    userPicService.validateUserPicPickupPoints(Set.of("PP-3003332"," PP-3003331","PP-3003333","PP-3003334"));
  }
  @Test
  public void isMppEnabled_NullProfile()throws Exception{
    Boolean response = userPicService.isMppEnabled(null);
    assertEquals(response,false);
  }
  @Test
  public void isMppEnabled_NullCompany()throws Exception{
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO nullCompany = null;
    businessPartnerProfile.setCompany(nullCompany);
    Boolean response = userPicService.isMppEnabled(businessPartnerProfile);
    assertEquals(response,false);
  }

  @Test
  public void isMppEnabled_CNCNotActivated() throws Exception{
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(false);
    businessPartnerProfile.setCompany(company);
    Boolean response = userPicService.isMppEnabled(businessPartnerProfile);
    assertEquals(response,false);
  }

  @Test
  public void isMppEnabled_CNCActivated() throws Exception{
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    businessPartnerProfile.setCompany(company);
    Boolean response = userPicService.isMppEnabled(businessPartnerProfile);
    assertEquals(response,true);
  }

  @Test
  public void isMppEnabled_MultiDefaultAddressFalse() throws Exception{
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(false);
    businessPartnerProfile.setCompany(company);
    businessPartnerProfile.setMultiDefaultAddressFlag(false);
    Boolean response = userPicService.isMppEnabled(businessPartnerProfile);
    assertEquals(response,false);
  }

  @Test
  public void isMppEnabled_InvalidMerchantType() throws Exception{
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM");
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(false);
    company.setMerchantType("CC");
    businessPartnerProfile.setCompany(company);
    businessPartnerProfile.setMultiDefaultAddressFlag(true);
    Boolean response = userPicService.isMppEnabled(businessPartnerProfile);
    assertEquals(response,false);
  }

  @Test
  public void isMppEnabled_ValidMerchantType() throws Exception{
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(false);
    company.setMerchantType("CC");
    businessPartnerProfile.setCompany(company);
    businessPartnerProfile.setMultiDefaultAddressFlag(true);
    Boolean response = userPicService.isMppEnabled(businessPartnerProfile);
    assertEquals(response,true);
  }

  @Test
  public void fetchAccessiblePickupPointCode_success() {
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", mppAllowedSellers);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    businessPartnerProfile.setCompany(company);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getPickupPoints()).thenReturn(
      Collections.singleton(PICKUP_POINT_CODE));
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
      businessPartnerProfile);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> ppCodes = userPicService.fetchAccessiblePickupPointCodes(null);
    Assertions.assertEquals(1, ppCodes.size());
    Assertions.assertTrue(ppCodes.contains(PICKUP_POINT_CODE));
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(mandatoryParameterHelper).getPickupPoints();
  }

  @Test
  public void fetchAccessiblePickupPointCode_success_emptySet() {
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", mppAllowedSellers);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host-To-Host"));
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    businessPartnerProfile.setCompany(company);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getPickupPoints()).thenReturn(
      Collections.singleton(PICKUP_POINT_CODE));
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
      businessPartnerProfile);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("Host-To-Host");
    Set<String> ppCodes = userPicService.fetchAccessiblePickupPointCodes(null);
    Assertions.assertEquals(0, ppCodes.size());
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void fetchAccessiblePickupPointCode_userPicSwitchOff() {
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", mppAllowedSellers);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", false);
    ProfileResponse businessPartnerProfile = getBusinessPartnerProfile();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    businessPartnerProfile.setCompany(company);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
      businessPartnerProfile);
    Set<String> ppCodes = userPicService.fetchAccessiblePickupPointCodes(null);
    Assertions.assertEquals(0, ppCodes.size());
    Assertions.assertFalse(ppCodes.contains(PICKUP_POINT_CODE));
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void fetchAccessiblePickupPointCode_mppEnableOff() {
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", mppAllowedSellers);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host"));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
      getBusinessPartnerProfile());
    when(mandatoryParameterHelper.getChannelId()).thenReturn("channel");
    Set<String> ppCodes = userPicService.fetchAccessiblePickupPointCodes(null);
    Assertions.assertEquals(0, ppCodes.size());
    Assertions.assertFalse(ppCodes.contains(PICKUP_POINT_CODE));
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void fetchAccessiblePickupPointCode_mppEnableOff_userPicOff() {
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", mppAllowedSellers);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", false);
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds", Set.of("Host"));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(
      getBusinessPartnerProfile());
    when(mandatoryParameterHelper.getChannelId()).thenReturn("channel");
    Set<String> ppCodes = userPicService.fetchAccessiblePickupPointCodes(null);
    Assertions.assertEquals(0, ppCodes.size());
    Assertions.assertFalse(ppCodes.contains(PICKUP_POINT_CODE));
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  private ProfileResponse getBusinessPartnerProfile() {
    String businessPartnerCode = "TEB-24219";
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    return ProfileResponse.builder().businessPartnerCode(businessPartnerCode).company(company).merchantStatus("ACTIVE")
        .build();
  }

  private ProfileResponse getMppEnabledBusinessPartnerProfile(){
    String businessPartnerCode = "TEB-24219";
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(false);
    company.setMerchantType("CC");
    return ProfileResponse.builder().businessPartnerCode(businessPartnerCode).company(company).merchantStatus("ACTIVE").multiDefaultAddressFlag(true)
        .build();
  }

  private ProfileResponse getMppNotEnabledBusinessPartnerProfile() {
    String businessPartnerCode = "TEB-24219";
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(false);
    return ProfileResponse.builder().businessPartnerCode(businessPartnerCode).company(company).merchantStatus("ACTIVE")
        .build();
  }

  @Test
  public void filterInaccessiblePickupPoints_flagOff() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 100);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", false);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getPickupPoints())
      .thenReturn(Set.of("PP-3003332", "PP-3003333"));
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(profileResponse, Set.of("PP-3003342"));
    assertEquals(Set.of("PP-3003342"), filteredPickupPoints);
  }

  @Test
  public void filterInaccessiblePickupPoints_allAccess() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 100);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(profileResponse, Collections.emptySet());
    assertEquals(Collections.emptySet(), filteredPickupPoints);
  }

  @Test
  public void filterInaccessiblePickupPoints_accessible() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 100);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003342"));
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(profileResponse, Set.of("PP-3003342"));
    assertEquals(Set.of("PP-3003342"), filteredPickupPoints);
  }

  @Test
  public void filterInaccessiblePickupPoints_nullRequest() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 100);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003342"));
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(profileResponse, null);
    assertNull(filteredPickupPoints);
  }

  @Test
  public void filterInaccessiblePickupPoints_crossThreshold() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 0);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003342"));
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(profileResponse, Set.of("PP-3003342"));
    assertEquals(Collections.emptySet(), filteredPickupPoints);
  }

  @Test
  public void filterInaccessiblePickupPoints_noAccess() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 100);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Set.of("PP-3003332"));
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(profileResponse, Set.of("PP-3003342"));
    assertEquals(Collections.emptySet(), filteredPickupPoints);
  }

  @Test
  public void filterInaccessiblePickupPoints_nullBusinessPartnerProfile() {
    ReflectionTestUtils.setField(userPicService, "userPicExcludedChannelIds",
      Set.of("Host-To-Host"));
    ReflectionTestUtils.setField(userPicService, "mppAllowedMerchantTypes", "CM,CC");
    ReflectionTestUtils.setField(userPicService, "filterThreshold", 100);
    ReflectionTestUtils.setField(userPicService, "userPicFeatureSwitch", true);
    ProfileResponse profileResponse = getMppEnabledBusinessPartnerProfile();
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn("TEB-24219");
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode("TEB-24219"))
      .thenReturn(profileResponse);
    Mockito.when(mandatoryParameterHelper.getPickupPoints()).thenReturn(Collections.emptySet());
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn("web");
    Set<String> filteredPickupPoints =
      userPicService.filterInaccessiblePickupPoints(null, Set.of("PP-3003342"));
    assertEquals(Set.of("PP-3003342"), filteredPickupPoints);
  }
}