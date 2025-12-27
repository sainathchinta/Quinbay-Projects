package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.service.exception.ApiInvalidImageQcConfigException;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class AutoQcConfigServiceImplTest {
  @InjectMocks
  private AutoQcConfigServiceImpl autoQcConfigService;

  @Mock
  private AutoApprovalService autoApprovalService;

  @Mock
  private ProductImagePredictionService productImagePredictionService;

  @Captor
  private ArgumentCaptor<AutoApprovalRules> autoApprovalRulesArgumentCaptor;

  private String RULE_NAME = "OFFICIAL_SELLERS";
  private String STORE_ID = "10001";
  private String DISPLAY_NAME = "Adult";
  private AutoQcConfigRequest autoQcConfigRequest;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto_BLUR;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto_WATERMARK;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto_TEXT;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto_ADULT;
  private AutoApprovalRules autoApprovalRules;
  private ProductImagePrediction productImagePrediction;
  private List<AutoApprovalRuleDetailsDto> imageConfig;

  private Gson gson = new GsonBuilder().disableHtmlEscaping().create();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    autoApprovalRuleDetailsDto_BLUR = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDto_BLUR.setKeyName(Constants.BLUR);
    autoApprovalRuleDetailsDto_BLUR.setValue("58");
    autoApprovalRuleDetailsDto_BLUR.setValueType(Constants.INT);
    autoApprovalRuleDetailsDto_BLUR.setOperator(Constants.LESS_THAN_OR_EQUAL_TO);


    autoApprovalRuleDetailsDto_WATERMARK = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDto_WATERMARK.setKeyName(Constants.WATERMARK);
    autoApprovalRuleDetailsDto_WATERMARK.setValue("70");
    autoApprovalRuleDetailsDto_WATERMARK.setValueType(Constants.INT);
    autoApprovalRuleDetailsDto_WATERMARK.setOperator(Constants.LESS_THAN_OR_EQUAL_TO);

    autoApprovalRuleDetailsDto_TEXT = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDto_TEXT.setKeyName(Constants.TEXT);
    autoApprovalRuleDetailsDto_TEXT.setValue("0");
    autoApprovalRuleDetailsDto_TEXT.setValueType(Constants.INT);
    autoApprovalRuleDetailsDto_TEXT.setOperator(Constants.EQUAL_TO);

    autoApprovalRuleDetailsDto_ADULT = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDto_ADULT.setKeyName(Constants.ADULT);
    autoApprovalRuleDetailsDto_ADULT.setValue("45");
    autoApprovalRuleDetailsDto_ADULT.setValueType(Constants.INT);
    autoApprovalRuleDetailsDto_ADULT.setOperator(Constants.LESS_THAN_OR_EQUAL_TO);
    productImagePrediction = new ProductImagePrediction();
    productImagePrediction.setConfidenceThreshold(50);
    productImagePrediction.setPredictionType(Constants.ADULT_PREDICTION);
    productImagePrediction.setDisplayName(DISPLAY_NAME);
    autoApprovalRules = new AutoApprovalRules();
    autoApprovalRules.setRuleName(RULE_NAME);
    autoApprovalRules.setImageQcConfig(Arrays
        .asList(autoApprovalRuleDetailsDto_BLUR, autoApprovalRuleDetailsDto_WATERMARK, autoApprovalRuleDetailsDto_TEXT,
            autoApprovalRuleDetailsDto_ADULT).toString());

  }

  @Test
  public void updateTest() throws Exception {
    imageConfig = new ArrayList<>(Arrays
        .asList(autoApprovalRuleDetailsDto_BLUR, autoApprovalRuleDetailsDto_WATERMARK, autoApprovalRuleDetailsDto_TEXT,
            autoApprovalRuleDetailsDto_ADULT));
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setImageConfig(imageConfig);
    autoQcConfigRequest.setRuleEnabled(null);
    Mockito.when(
        productImagePredictionService.findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(Mockito.anyString()))
        .thenReturn(Arrays.asList(productImagePrediction));
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(autoApprovalRules);
    autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, false);
    Mockito.verify(productImagePredictionService).findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(STORE_ID);
    Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalService).save(autoApprovalRulesArgumentCaptor.capture());
    Assertions.assertEquals(RULE_NAME, autoApprovalRulesArgumentCaptor.getValue().getRuleName());
    Assertions.assertEquals(gson.toJson(imageConfig), autoApprovalRulesArgumentCaptor.getValue().getImageQcConfig());
  }

  @Test
  public void updateRuleEnabledFalseTest() throws Exception {
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setRuleEnabled(Boolean.FALSE);
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(autoApprovalRules);
    autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, false);
    Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalService).save(autoApprovalRulesArgumentCaptor.capture());
    Assertions.assertEquals(RULE_NAME, autoApprovalRulesArgumentCaptor.getValue().getRuleName());
    Assertions.assertEquals(true, autoApprovalRulesArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void updateRuleEnabledTrueTest() throws Exception {
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setRuleEnabled(Boolean.TRUE);
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(autoApprovalRules);
    autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, false);
    Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalService).save(autoApprovalRulesArgumentCaptor.capture());
    Assertions.assertEquals(RULE_NAME, autoApprovalRulesArgumentCaptor.getValue().getRuleName());
    Assertions.assertEquals(false, autoApprovalRulesArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void updateInvalidImageQcConfigTest() throws Exception {
    autoApprovalRuleDetailsDto_ADULT.setValue("87");
    imageConfig = new ArrayList<>(Arrays
        .asList(autoApprovalRuleDetailsDto_BLUR, autoApprovalRuleDetailsDto_WATERMARK, autoApprovalRuleDetailsDto_TEXT,
            autoApprovalRuleDetailsDto_ADULT));
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setImageConfig(imageConfig);
    autoQcConfigRequest.setRuleEnabled(null);
    Mockito.when(
        productImagePredictionService.findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(Mockito.anyString()))
        .thenReturn(Arrays.asList(productImagePrediction));
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(STORE_ID, RULE_NAME)).thenReturn(autoApprovalRules);
    try {
      autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, false);
      Mockito.verify(productImagePredictionService)
          .findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(STORE_ID);
      Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    } catch (ApiInvalidImageQcConfigException e) {
      Assertions.assertEquals(ApiErrorCode.IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD, e.getErrorCode());
    }
  }

  @Test
  public void updateNeedRevisonConfigTest() throws Exception {
    imageConfig = new ArrayList<>(Arrays
        .asList(autoApprovalRuleDetailsDto_BLUR, autoApprovalRuleDetailsDto_WATERMARK, autoApprovalRuleDetailsDto_TEXT,
            autoApprovalRuleDetailsDto_ADULT));
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setImageConfig(imageConfig);
    autoQcConfigRequest.setRuleEnabled(null);
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(autoApprovalRules);
    autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, true);
    Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalService).save(autoApprovalRulesArgumentCaptor.capture());
    Assertions.assertEquals(RULE_NAME, autoApprovalRulesArgumentCaptor.getValue().getRuleName());
    Assertions.assertEquals(gson.toJson(imageConfig), autoApprovalRulesArgumentCaptor.getValue().getNeedRevisionConfig());
  }

  @Test
  public void updateRuleEnabledFalseNeedRevisonConfigTest() throws Exception {
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setRuleEnabled(Boolean.FALSE);
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(autoApprovalRules);
    autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, true);
    Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalService).save(autoApprovalRulesArgumentCaptor.capture());
    Assertions.assertEquals(RULE_NAME, autoApprovalRulesArgumentCaptor.getValue().getRuleName());
    Assertions.assertFalse(autoApprovalRulesArgumentCaptor.getValue().isNeedRevisionEnabled());
  }

  @Test
  public void updateRuleEnabledTrueNeedRevisionConfigTest() throws Exception {
    autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setRuleEnabled(Boolean.TRUE);
    Mockito.when(autoApprovalService.findByStoreIdAndRuleName(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(autoApprovalRules);
    autoQcConfigService.update(RULE_NAME, STORE_ID, autoQcConfigRequest, true);
    Mockito.verify(autoApprovalService).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalService).save(autoApprovalRulesArgumentCaptor.capture());
    Assertions.assertEquals(RULE_NAME, autoApprovalRulesArgumentCaptor.getValue().getRuleName());
    Assertions.assertTrue(autoApprovalRulesArgumentCaptor.getValue().isNeedRevisionEnabled());
  }

}
