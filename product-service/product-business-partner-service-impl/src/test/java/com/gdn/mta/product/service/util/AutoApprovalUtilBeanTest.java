package com.gdn.mta.product.service.util;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.ChangedFieldDto;
import com.gda.mta.product.dto.ImageQcProcessingDto;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gda.mta.product.dto.response.ImageQcConfidenceDto;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.partners.pbp.commons.constants.AutoQCConstants;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.google.common.collect.ImmutableMap;


public class AutoApprovalUtilBeanTest {

  @InjectMocks
  private AutoApprovalUtilBean autoApprovalUtilBean;

  @Mock
  private ObjectMapper objectMapper;

  private static final String NOTES = "notes";
  private static final String EDITED_BY_AMPHI_USERS = "EDITED_BY_AMPHI_USERS";
  private static final String OFFICIAL_SELLERS = "OFFICIAL_SELLERS";
  private static final String IS_EDITED_BY_INTERNAL_USER = "is_edited_by_internal_user";
  private static final String IS_OFFICIAL_STORE = "is_official_store";
  private static final String PREDICTION_BLUR_VALUES = "58";
  private static final String PREDICTION_BLUR_VALUES_1 = "59";
  private static final String PREDICTION_BLUR_VALUES_2 = "100";
  private static final String PREDICTION_WATERMARK_VALUES = "70";
  private static final String PREDICTION_WATERMARK_VALUES_1 = "71";
  private static final String PREDICTION_WATERMARK_VALUES_2 = "100";
  private static final String PREDICTION_TEST_VALUES = "0";
  private static final String PREDICTION_TEST_VALUES_1 = "1";
  private static final String PREDICTION_ADULT_VALUES = "50";
  private static final String PREDICTION_ADULT_VALUES_1 = "48";
  private static final String PREDICTION_ADULT_VALUES_2 = "49";
  private static final String LOCATION_PATH = "location_path";
  private static final String UPDATED_BY = "updated_by";
  private static final boolean OFFICIAL_STORE = true;
  private static final boolean WHITELIST_SELLER = true;
  private static final boolean WHITELIST_COMPANY_OFFICER = true;
  private static final Float C1_REJECTION_PERCENT = 10.0f;
  private static final String SELLER_BADGE = "GOLD";
  private static final String SELLER_NO_BADGE = "NO_BADGE";
  private static final Integer SELLER_CREATED_PRODUCTS_COUNT_180 = 6;
  private static final String APPLIED_RULE_CONFIG = "ruleConfig";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String AMPHI_USERS_CONTAINS = "@gdn-commerce.com";
  private static final String TRUE = "true";
  private static final String FALSE = "false";
  private static final String NULL = "null";
  private static final String BADGE_VALUE = "[\"GOLD\",\"SILVER\"]";
  private static final String C1_REJECTION_PERCENT_VALUE = "30";
  private static final String SELLER_CREATED_PRODUCTS_COUNT_180_VALUE = "2";
  private static final String KEY_VALUE = "keyValue";
  String BLUR = "Blur";
  String WATERMARK = "Watermark";
  String TEXT = "Text";
  String ADULT = "Adult";


  private List<AutoApprovalRules> autoApprovalRuleList;
  private AutoApprovalRules autoApprovalRuleAmphiUsers;
  private AutoApprovalRules autoApprovalRuleOfficialSellers;
  private AutoApprovalRuleDetailsDto[] autoApprovalRuleDetailsDtos;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDtoAmphiUsers;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDtoOfficialSellers;
  private List<AutoApprovalRuleDetailsDto> imageQcConfig;
  private List<ImageQcProcessingDto> imageQcData;
  private ImageQcProcessingDto imageQcProcessingDto;
  AutoQCDetailResponse autoQCDetailResponse;
  Map<String, AutoApprovalRulesDto> autoApprovalRulesMap;
  AutoApprovalsDetailDto autoApprovalsDetail;
  private AutoApprovalRulesDto autoApprovalRulesDto;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    autoApprovalRuleAmphiUsers = new AutoApprovalRules();
    autoApprovalRuleAmphiUsers.setRuleName(EDITED_BY_AMPHI_USERS);
    autoApprovalRuleOfficialSellers = new AutoApprovalRules();
    autoApprovalRuleOfficialSellers.setRuleName(OFFICIAL_SELLERS);
    autoApprovalRuleList = new ArrayList<>();
    autoApprovalRuleList.add(autoApprovalRuleAmphiUsers);
    autoApprovalRuleList.add(autoApprovalRuleOfficialSellers);
    autoApprovalRuleDetailsDtoAmphiUsers = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDtoAmphiUsers.setKeyName(IS_EDITED_BY_INTERNAL_USER);
    autoApprovalRuleDetailsDtoOfficialSellers = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDtoOfficialSellers.setKeyName(IS_OFFICIAL_STORE);
    autoApprovalRuleDetailsDtos = new AutoApprovalRuleDetailsDto[2];
    autoApprovalRuleDetailsDtos[0] = autoApprovalRuleDetailsDtoAmphiUsers;
    autoApprovalRuleDetailsDtos[1] = autoApprovalRuleDetailsDtoOfficialSellers;
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsBulr = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsBulr.setKeyName(BLUR);
    autoApprovalRuleDetailsBulr.setOperator(Constants.GREATER_THAN_OR_EQUAL_TO);
    autoApprovalRuleDetailsBulr.setValue(PREDICTION_BLUR_VALUES);
    autoApprovalRuleDetailsBulr.setValueType(Constants.INT);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsWatermark = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsWatermark.setKeyName(WATERMARK);
    autoApprovalRuleDetailsWatermark.setOperator(Constants.GREATER_THAN);
    autoApprovalRuleDetailsWatermark.setValue(PREDICTION_WATERMARK_VALUES);
    autoApprovalRuleDetailsWatermark.setValueType(Constants.INT);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsText = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsText.setKeyName(TEXT);
    autoApprovalRuleDetailsText.setOperator(Constants.EQUAL_TO);
    autoApprovalRuleDetailsText.setValue(PREDICTION_TEST_VALUES);
    autoApprovalRuleDetailsText.setValueType(Constants.INT);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsAdult = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsAdult.setKeyName(ADULT);
    autoApprovalRuleDetailsAdult.setOperator(Constants.LESS_THAN);
    autoApprovalRuleDetailsAdult.setValue(PREDICTION_ADULT_VALUES);
    autoApprovalRuleDetailsAdult.setValueType(Constants.INT);
    imageQcConfig = Arrays.asList(autoApprovalRuleDetailsBulr,
        autoApprovalRuleDetailsWatermark, autoApprovalRuleDetailsText, autoApprovalRuleDetailsAdult);
    ImageQcConfidenceDto imageQcConfidenceDtoBlur = new ImageQcConfidenceDto();
    imageQcConfidenceDtoBlur.setPredictionType(Constants.BLUR_PREDICTION);
    imageQcConfidenceDtoBlur.setDisplayName(BLUR);
    imageQcConfidenceDtoBlur.setConfidence(PREDICTION_BLUR_VALUES_1);
    ImageQcConfidenceDto imageQcConfidenceDtoWaterMark = new ImageQcConfidenceDto();
    imageQcConfidenceDtoWaterMark.setPredictionType(Constants.WATERMARK_PREDICTION);
    imageQcConfidenceDtoWaterMark.setDisplayName(WATERMARK);
    imageQcConfidenceDtoWaterMark.setConfidence(PREDICTION_WATERMARK_VALUES_1);
    ImageQcConfidenceDto imageQcConfidenceDtoText = new ImageQcConfidenceDto();
    imageQcConfidenceDtoText.setPredictionType(Constants.TEXT_PREDICTION);
    imageQcConfidenceDtoText.setDisplayName(TEXT);
    imageQcConfidenceDtoText.setConfidence(PREDICTION_TEST_VALUES);
    ImageQcConfidenceDto imageQcConfidenceDtoAdult = new ImageQcConfidenceDto();
    imageQcConfidenceDtoAdult.setPredictionType(Constants.ADULT_PREDICTION);
    imageQcConfidenceDtoAdult.setDisplayName(ADULT);
    imageQcConfidenceDtoAdult.setConfidence(PREDICTION_ADULT_VALUES_2);
    imageQcProcessingDto = new ImageQcProcessingDto();
    imageQcProcessingDto.setLocationPath(LOCATION_PATH);
    imageQcProcessingDto.setImageQcConfidenceDtoList(new ArrayList<>());
    imageQcProcessingDto.getImageQcConfidenceDtoList().add(imageQcConfidenceDtoBlur);
    imageQcProcessingDto.getImageQcConfidenceDtoList().add(imageQcConfidenceDtoWaterMark);
    imageQcProcessingDto.getImageQcConfidenceDtoList().add(imageQcConfidenceDtoText);
    imageQcProcessingDto.getImageQcConfidenceDtoList().add(imageQcConfidenceDtoAdult);
    imageQcData = new ArrayList<>();
    imageQcData.add(imageQcProcessingDto);
    autoApprovalsDetail = new AutoApprovalsDetailDto();
    autoApprovalsDetail.setUpdatedBy(UPDATED_BY + AMPHI_USERS_CONTAINS);
    autoApprovalsDetail.setStoreId(STORE_ID);
    autoApprovalsDetail.setProductCode(PRODUCT_CODE);
    autoApprovalsDetail.setImageQcConfidenceDetails(new ArrayList<>());
    autoQCDetailResponse = new AutoQCDetailResponse();
    autoQCDetailResponse.setIsOfficialStore(OFFICIAL_STORE);
    autoQCDetailResponse.setIsWhitelistSeller(WHITELIST_SELLER);
    autoQCDetailResponse.setIsWhitelistCompanyOfficer(WHITELIST_COMPANY_OFFICER);
    autoQCDetailResponse.setC1RejectionPercent(C1_REJECTION_PERCENT);
    autoQCDetailResponse.setSellerBadge(SELLER_BADGE);
    autoQCDetailResponse.setSellerCreatedProductCount180(SELLER_CREATED_PRODUCTS_COUNT_180);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsInternalUser = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsInternalUser.setKeyName(Constants.IS_EDITED_BY_INTERNAL_USER);
    autoApprovalRuleDetailsInternalUser.setOperator(Constants.CONTAINS);
    autoApprovalRuleDetailsInternalUser.setValue(AMPHI_USERS_CONTAINS);
    autoApprovalRuleDetailsInternalUser.setValueType(Constants.STRING);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsOfficialStore = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsOfficialStore.setKeyName(AutoQCConstants.IS_OFFICIAL_STORE);
    autoApprovalRuleDetailsOfficialStore.setOperator(NULL);
    autoApprovalRuleDetailsOfficialStore.setValue(TRUE);
    autoApprovalRuleDetailsOfficialStore.setValueType(Constants.BOOLEAN);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsIsWhiteListSeller = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsIsWhiteListSeller.setKeyName(AutoQCConstants.IS_WHITELIST_SELLER);
    autoApprovalRuleDetailsIsWhiteListSeller.setOperator(NULL);
    autoApprovalRuleDetailsIsWhiteListSeller.setValue(TRUE);
    autoApprovalRuleDetailsIsWhiteListSeller.setValueType(Constants.BOOLEAN);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsIsWhiteListCompany = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsIsWhiteListCompany.setKeyName(AutoQCConstants.IS_WHITELIST_COMPANY_OFFICER);
    autoApprovalRuleDetailsIsWhiteListCompany.setOperator(NULL);
    autoApprovalRuleDetailsIsWhiteListCompany.setValue(TRUE);
    autoApprovalRuleDetailsIsWhiteListCompany.setValueType(Constants.BOOLEAN);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsSellerBadge = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsSellerBadge.setKeyName(AutoQCConstants.SELLER_BADGE);
    autoApprovalRuleDetailsSellerBadge.setOperator(NULL);
    autoApprovalRuleDetailsSellerBadge.setValue(BADGE_VALUE);
    autoApprovalRuleDetailsSellerBadge.setValueType(Constants.LIST_STRING);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsProductCreated180 = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsProductCreated180.setKeyName(AutoQCConstants.SELLER_CREATED_PRODUCT_COUNT180);
    autoApprovalRuleDetailsProductCreated180.setOperator(Constants.GREATER_THAN_OR_EQUAL_TO);
    autoApprovalRuleDetailsProductCreated180.setValue(SELLER_CREATED_PRODUCTS_COUNT_180_VALUE);
    autoApprovalRuleDetailsProductCreated180.setValueType(Constants.INT);
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsC1RejectionPercent = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsC1RejectionPercent.setKeyName(AutoQCConstants.C1_REJECTION_PERCENT);
    autoApprovalRuleDetailsC1RejectionPercent.setOperator(Constants.LESS_THAN_OR_EQUAL_TO);
    autoApprovalRuleDetailsC1RejectionPercent.setValue(C1_REJECTION_PERCENT_VALUE);
    autoApprovalRuleDetailsC1RejectionPercent.setValueType(Constants.DOUBLE);
    autoApprovalRulesDto = new AutoApprovalRulesDto();
    autoApprovalRulesDto.setRuleName(APPLIED_RULE_CONFIG);
    autoApprovalRulesDto.setRuleConfig(Arrays.asList(autoApprovalRuleDetailsInternalUser,
        autoApprovalRuleDetailsOfficialStore, autoApprovalRuleDetailsIsWhiteListSeller,
        autoApprovalRuleDetailsIsWhiteListCompany, autoApprovalRuleDetailsSellerBadge,
        autoApprovalRuleDetailsProductCreated180, autoApprovalRuleDetailsC1RejectionPercent));
    autoApprovalRulesDto.setImageQcConfig(new ArrayList<>());
    autoApprovalRulesMap = new LinkedHashMap<>();
    autoApprovalRulesMap.put(APPLIED_RULE_CONFIG, autoApprovalRulesDto);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void toAutoApprovalRulesDtoListTest() throws Exception {
    when(objectMapper.readValue((String) any(),
        Mockito.eq(AutoApprovalRuleDetailsDto[].class))).thenReturn(autoApprovalRuleDetailsDtos);
    Map<String, AutoApprovalRulesDto> autoApprovalRulesMap =
        autoApprovalUtilBean.toAutoApprovalRulesDtoList(autoApprovalRuleList);
    verify(objectMapper, times(4)).readValue((String) any(),
        Mockito.eq(AutoApprovalRuleDetailsDto[].class));
    Assertions.assertTrue(autoApprovalRulesMap.keySet().contains(EDITED_BY_AMPHI_USERS));
    Assertions.assertTrue(autoApprovalRulesMap.keySet().contains(OFFICIAL_SELLERS));
  }

  @Test
  public void toAutoApprovalRulesDtoListEmptyAutoApprovalRulesTest() throws Exception {
    Map<String, AutoApprovalRulesDto> autoApprovalRulesMap = autoApprovalUtilBean.toAutoApprovalRulesDtoList(null);
    Assertions.assertTrue(autoApprovalRulesMap.isEmpty());
  }

  @Test
  public void verifyImageQcRulesAllPassTest(){
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertTrue(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesFailBlurTest(){
    imageQcConfig.get(0).setValue(PREDICTION_BLUR_VALUES_2);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesFailWaterMarkTest(){
    imageQcConfig.get(1).setValue(PREDICTION_WATERMARK_VALUES_2);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesFailTextTest(){
    imageQcConfig.get(2).setValue(PREDICTION_TEST_VALUES_1);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesFailAdultTest(){
    imageQcConfig.get(3).setValue(PREDICTION_ADULT_VALUES_2);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesFailAdultChangeOperationTest(){
    imageQcConfig.get(3).setOperator(Constants.NOT_EQUALS_TO);
    imageQcConfig.get(3).setValue(PREDICTION_ADULT_VALUES_2);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesFailAdultWithLessThanEqualToTest(){
    imageQcConfig.get(3).setOperator(Constants.LESS_THAN_OR_EQUAL_TO);
    imageQcConfig.get(3).setValue(PREDICTION_ADULT_VALUES_1);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }


  @Test
  public void verifyImageQcRulesTestImageQcDataNull(){
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(null, imageQcConfig);
    Assertions.assertTrue(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesTestImageQcConfigNull(){
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, null);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesTestTypeCastingError(){
    this.imageQcConfig.get(0).setValue(NOTES);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesUnknownImageConfidencDataTest(){
    ImageQcConfidenceDto imageQcConfidenceDto = new ImageQcConfidenceDto();
    imageQcConfidenceDto.setDisplayName(NOTES);
    this.imageQcData.get(0).getImageQcConfidenceDtoList().add(imageQcConfidenceDto);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesEachImageConfidenceDataContainsNULL(){
    this.imageQcData.get(0).getImageQcConfidenceDtoList().add(null);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void verifyImageQcRulesTestCheckFewMoreConditions(){
    this.imageQcConfig.get(1).setOperator(Constants.NOT_EQUALS_TO);
    this.imageQcConfig.get(2).setOperator(Constants.LESS_THAN_OR_EQUAL_TO);
    this.imageQcConfig.get(3).setOperator(Constants.TEXT);
    boolean isEligibleAutoApproval = autoApprovalUtilBean.verifyImageQcRules(imageQcData, imageQcConfig);
    Assertions.assertFalse(isEligibleAutoApproval);
  }

  @Test
  public void covertToProductAutoApprovalCriteriaTest() throws Exception{
    when(objectMapper.writeValueAsString(any())).thenReturn(NOTES);
    autoApprovalUtilBean.covertToProductAutoApprovalCriteria(AutoApprovalType.CONTENT_AND_IMAGE,
        autoApprovalRulesDto, APPLIED_RULE_CONFIG, autoApprovalsDetail, autoQCDetailResponse);
    verify(objectMapper, times(3)).writeValueAsString(any());
  }

  @Test
  public void covertToProductHistoryRepositoryTest() throws Exception{
    when(objectMapper.writeValueAsString(any())).thenReturn(NOTES);
    autoApprovalUtilBean.covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse);
    verify(objectMapper).writeValueAsString(any());
  }

  @Test
  public void verifyAutoQcRulesTest() throws Exception {
    when(objectMapper.readValue(anyString(), Mockito.any(TypeReference.class))).thenReturn(Arrays.asList(SELLER_BADGE));
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    verify(objectMapper).readValue(anyString(), Mockito.any(TypeReference.class));
    Assertions.assertEquals(APPLIED_RULE_CONFIG, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesTest1() throws Exception {
    autoQCDetailResponse.setC1RejectionPercent(null);
    autoQCDetailResponse.setIsOfficialStore(false);
    autoQCDetailResponse.setIsWhitelistSeller(false);
    autoQCDetailResponse.setIsWhitelistCompanyOfficer(false);
    autoQCDetailResponse.setSellerBadge("None Merchant");
    autoQCDetailResponse.setSellerCreatedProductCount180(SELLER_CREATED_PRODUCTS_COUNT_180);
    autoApprovalsDetail.setUpdatedBy("System");
    AutoApprovalRuleDetailsDto autoApprovalRuleDetailsC1RejectionPercent = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsC1RejectionPercent.setKeyName(AutoQCConstants.C1_REJECTION_PERCENT);
    autoApprovalRuleDetailsC1RejectionPercent.setOperator(Constants.LESS_THAN_OR_EQUAL_TO);
    autoApprovalRuleDetailsC1RejectionPercent.setValue(C1_REJECTION_PERCENT_VALUE);
    autoApprovalRuleDetailsC1RejectionPercent.setValueType(Constants.DOUBLE);
    AutoApprovalRulesDto autoApprovalRulesDto1 = new AutoApprovalRulesDto();
    autoApprovalRulesDto1.setRuleConfig(Arrays.asList( autoApprovalRuleDetailsC1RejectionPercent));
    autoApprovalRulesMap.put("None Merchant", autoApprovalRulesDto1);
    when(objectMapper.readValue(anyString(), Mockito.any(TypeReference.class))).thenReturn(Arrays.asList("None Merchant"));
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
  }

  @Test
  public void verifyAutoQcRulesTypeCastErrorTest() throws Exception{
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setValueType(Constants.INT);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesWrongKeyNameTest() throws Exception{
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setKeyName(KEY_VALUE);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesStringCheckFailsTest() throws Exception{
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setOperator(Constants.NOT_EQUALS_TO);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesStringCheckFailDoesContainTest() throws Exception{
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setValue(NOTES);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesFailsTest() throws Exception{
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setOperator(Constants.EQUAL_TO);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesOfficialSellerFailsTest() throws Exception{
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(1).setValue(FALSE);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesSellerBadgeTest() throws Exception{
    when(objectMapper.readValue(anyString(), Mockito.any(TypeReference.class))).thenReturn(Arrays.asList(SELLER_NO_BADGE));
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    verify(objectMapper).readValue(anyString(), Mockito.any(TypeReference.class));
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesDifferntOperatorTest() throws Exception{
    when(objectMapper.readValue(anyString(), Mockito.any(TypeReference.class))).thenReturn(Arrays.asList(SELLER_BADGE));
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setOperator(Constants.EQUAL_TO);
    this.autoApprovalRulesMap.get(APPLIED_RULE_CONFIG).getRuleConfig().get(0).setValue(UPDATED_BY + AMPHI_USERS_CONTAINS);
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(autoQCDetailResponse,
        autoApprovalRulesMap, autoApprovalsDetail);
    verify(objectMapper).readValue(anyString(), Mockito.any(TypeReference.class));
    Assertions.assertEquals(APPLIED_RULE_CONFIG, appliedRuleConfig);
  }

  @Test
  public void verifyAutoQcRulesAutoQCDetailResponseNullTest() throws Exception{
    String appliedRuleConfig = autoApprovalUtilBean.verifyAutoQcRules(null,
        autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(StringUtils.EMPTY, appliedRuleConfig);
  }

  @Test
  public void getOldAutoQcDetailResponseTest() {
    AutoQCDetailResponse autoQCDetailResponse = new AutoQCDetailResponse();
    ChangedFieldDto changedFieldDto1 = new ChangedFieldDto("true", "false");
    ChangedFieldDto changedFieldDto2 = new ChangedFieldDto("2", "1");
    ChangedFieldDto changedFieldDto3 = new ChangedFieldDto("2.0", "1.0");
    ChangedFieldDto changedFieldDto4 = new ChangedFieldDto("Bp-Code2", "Bp-Code1");
    ChangedFieldDto changedFieldDto5 = new ChangedFieldDto("20-02-2022", "20-02-2022");
    ChangedFieldDto changedFieldDto6 = new ChangedFieldDto("null", "true");
    Map<String, ChangedFieldDto> changedFieldDtoMap =
        ImmutableMap.of("isOfficialStore", changedFieldDto1, "sellerCreatedProductCount", changedFieldDto2,
            "c1RejectionPercent", changedFieldDto3, "businessPartnerCode", changedFieldDto4, "activationDate",
            changedFieldDto5);
    changedFieldDtoMap = new HashMap<>(changedFieldDtoMap);
    changedFieldDtoMap.put("isWhitelistSeller", changedFieldDto6);

    AutoQCDetailResponse oldAutoQCDetailResponse =
        autoApprovalUtilBean.getOldAutoQcDetailResponse(autoQCDetailResponse, changedFieldDtoMap);

    Assertions.assertTrue(oldAutoQCDetailResponse.getIsOfficialStore());
    Assertions.assertEquals(2, oldAutoQCDetailResponse.getSellerCreatedProductCount().intValue());
    Assertions.assertEquals(2.0, oldAutoQCDetailResponse.getC1RejectionPercent().intValue(), 0);
    Assertions.assertEquals("Bp-Code2", oldAutoQCDetailResponse.getBusinessPartnerCode());
    Assertions.assertNull(oldAutoQCDetailResponse.getIsWhitelistSeller());
  }

  @Test
  public void getOldAutoQcDetailResponseEmptyChangedFieldsTest() {
    AutoQCDetailResponse autoQCDetailResponse = new AutoQCDetailResponse();

    AutoQCDetailResponse oldAutoQCDetailResponse =
        autoApprovalUtilBean.getOldAutoQcDetailResponse(autoQCDetailResponse, null);

    Assertions.assertNotNull(oldAutoQCDetailResponse);

  }

  @Test
  public void getOldAutoQcDetailResponseExceptionTest() {
    AutoQCDetailResponse autoQCDetailResponse = new AutoQCDetailResponse();
    ChangedFieldDto changedFieldDto1 = new ChangedFieldDto("true", "false");
    Map<String, ChangedFieldDto> changedFieldDtoMap =
        ImmutableMap.of("sellerCreatedProductCount", changedFieldDto1);

    AutoQCDetailResponse oldAutoQCDetailResponse =
        autoApprovalUtilBean.getOldAutoQcDetailResponse(autoQCDetailResponse, changedFieldDtoMap);

    Assertions.assertNotNull(oldAutoQCDetailResponse);

  }
}
