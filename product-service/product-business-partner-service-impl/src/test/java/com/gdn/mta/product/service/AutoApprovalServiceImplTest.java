package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.enums.ProductCreationType;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.context.ApplicationContext;

import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.repository.AutoApprovalRulesRepository;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

public class AutoApprovalServiceImplTest {

  @InjectMocks
  private  AutoApprovalServiceImpl autoApprovalService;

  @Mock
  private AutoApprovalRulesRepository autoApprovalRulesRepository;

  @Mock
  private ProductAutoApprovalCriteriaService productAutoApprovalCriteriaService;

  @Mock
  private ProductService productService;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Mock
  private AutoApprovalUtil autoApprovalUtil;

  private static final String MERCHANT_CODE = "merchantCode";
  private static final String C1_CATEGORY_CODE = "c1CategoryCode";
  private static final boolean OFFICIAL_STORE = true;
  private static final boolean WHITELIST_SELLER = true;
  private static final boolean WHITELIST_COMPANY_OFFICER = true;
  private static final Float C1_REJECTION_PERCENT = 10.0f;
  private static final String SELLER_BADGE = "GOLD";
  private static final Integer SELLER_CREATED_PRODUCTS_COUNT_180 = 6;
  private static final String STORE_ID = "STORE_ID";
  private static final String APPLIED_RULE_CONFIG = "ruleConfig";
  private static final String PRODUCT_CODE = "productCode";
  private static final String RULE_NAME = "WHITELISTED_SELLERS";
  private static final String SELLER_OFFICIAL_RULE_NAME = "OFFICIAL_SELLER";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);

  private static final String DEFAULT_STORE_ID = "10001";
  private static final Integer SEQUENCE_NUMBER = 0;
  private static final String RULE_CONFIG =
      "[{\"keyName\":\"is_edited_by_internal_user\",\"value\":\"@gdn-commerce.com\",\"valueType\":\"string\",\"operator\":\"contains\"}]";
  private static final String IMAGE_QC_CONFIG =
      "[{\"keyName\":\"blur\",\"value\":\"101\",\"valueType\":\"int\",\"operator\":\"<=\"},{\"keyName\":\"watermark\",\"value\":\"101\",\"valueType\":\"int\",\"operator\":\"<=\"},{\"keyName\":\"text\",\"value\":\"0\",\"valueType\":\"int\",\"operator\":\"==\"},{\"keyName\":\"Adult\",\"value\":\"50\",\"valueType\":\"int\",\"operator\":\"<=\"}]";
  private static final String DEFAULT_CREATED_BY = "test_user_create";
  private static final String DEFAULT_UPDATED_BY = "test_user_update";
  private static final Long DEFAULT_VERSION = 53535L;


  private AutoApprovalsDetailDto autoApprovalsDetail;
  private AutoQCDetailResponse autoQCDetailResponse;
  private AutoApprovalRulesDto autoApprovalRulesDto;
  private List<AutoApprovalRules> autoApprovalRules;
  Map<String, AutoApprovalRulesDto> autoApprovalRulesMap;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    autoApprovalsDetail = new AutoApprovalsDetailDto();
    autoApprovalsDetail.setC1CategoryCode(C1_CATEGORY_CODE);
    autoApprovalsDetail.setMerchantCode(MERCHANT_CODE);
    autoApprovalsDetail.setStoreId(STORE_ID);
    autoApprovalsDetail.setProductCode(PRODUCT_CODE);
    autoApprovalsDetail.setImageQcConfidenceDetails(new ArrayList<>());
    autoApprovalsDetail.setPostLive(true);
    autoQCDetailResponse = new AutoQCDetailResponse();
    autoQCDetailResponse.setIsOfficialStore(OFFICIAL_STORE);
    autoQCDetailResponse.setIsWhitelistSeller(WHITELIST_SELLER);
    autoQCDetailResponse.setIsWhitelistCompanyOfficer(WHITELIST_COMPANY_OFFICER);
    autoQCDetailResponse.setC1RejectionPercent(C1_REJECTION_PERCENT);
    autoQCDetailResponse.setSellerBadge(SELLER_BADGE);
    autoQCDetailResponse.setSellerCreatedProductCount180(SELLER_CREATED_PRODUCTS_COUNT_180);
    autoApprovalRulesDto = new AutoApprovalRulesDto();
    autoApprovalRulesDto.setRuleName(SELLER_OFFICIAL_RULE_NAME);
    autoApprovalRulesDto.setSequenceNumber(1);
    autoApprovalRulesDto.setImageQcConfig(new ArrayList<>());
    autoApprovalRulesMap = new LinkedHashMap<>();
    autoApprovalRulesMap.put(SELLER_OFFICIAL_RULE_NAME, autoApprovalRulesDto);
    autoApprovalRules = new ArrayList<>();
    Mockito.when(applicationContext.getBean(AutoApprovalService.class))
        .thenReturn(autoApprovalService);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(autoApprovalRulesRepository);
    Mockito.verifyNoMoreInteractions(productAutoApprovalCriteriaService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productAnalyticsOutbound);
    Mockito.verifyNoMoreInteractions(autoApprovalUtil);
    Mockito.verifyNoMoreInteractions(applicationContext);
  }

  @Test
  public void verifyAutoApprovalRulesTest() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
        STORE_ID)).thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
        autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil.covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
        eq(autoApprovalsDetail), eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail,
        autoQCDetailResponse)).thenReturn(new ProductHistory());
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(autoApprovalUtil).verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    verify(autoApprovalUtil).verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig());
    verify(autoApprovalUtil).covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
        eq(autoApprovalsDetail), eq(autoQCDetailResponse));
    verify(autoApprovalUtil).covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse);
    verify(productAutoApprovalCriteriaService).saveProductAutoApprovalCriteria(any(ProductAutoApprovalCriteria.class));
    verify(productService).saveProductHistory(eq(PRODUCT_CODE), any(ProductHistory.class));
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.CONTENT_AND_IMAGE);
    Mockito.verify(applicationContext).getBean(AutoApprovalService.class);
  }

  @Test
  public void verifyAutoApprovalRules_forTrustedSellersTest() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
      STORE_ID)).thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
      autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
      autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil.covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
      eq(autoApprovalsDetail), eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail,
      autoQCDetailResponse)).thenReturn(new ProductHistory());
    autoApprovalsDetail.setRestrictedKeywordPresent(true);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
  }

  @Test
  public void verifyAutoApprovalRulesForceReviewTest() throws Exception {
    autoApprovalsDetail.setForceReview(true);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(AutoApprovalType.NA, autoApprovalType);
  }

  @Test
  public void verifyAutoApprovalRulesNotApplicableTest() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
        STORE_ID)).thenReturn(autoApprovalRules);
    autoApprovalsDetail.setRestrictedKeywordPresent(false);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
        autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil.covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
        eq(autoApprovalsDetail), eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail,
        autoQCDetailResponse)).thenReturn(new ProductHistory());
    autoApprovalsDetail.setRevised(true);
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(autoApprovalUtil).verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    verify(autoApprovalUtil).verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig());
    verify(autoApprovalUtil).covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
        eq(autoApprovalsDetail), eq(autoQCDetailResponse));
    verify(autoApprovalUtil).covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse);
    verify(productAutoApprovalCriteriaService).saveProductAutoApprovalCriteria(any(ProductAutoApprovalCriteria.class));
    verify(productService).saveProductHistory(eq(PRODUCT_CODE), any(ProductHistory.class));
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.CONTENT_AND_IMAGE);
    Mockito.verify(applicationContext).getBean(AutoApprovalService.class);
  }

  @Test
  public void verifyAutoApprovalRulesNotApplicableTest_forTrustedSellers() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
      STORE_ID)).thenReturn(autoApprovalRules);
    autoApprovalsDetail.setRestrictedKeywordPresent(true);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
      autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
      autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil.covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
      eq(autoApprovalsDetail), eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail,
      autoQCDetailResponse)).thenReturn(new ProductHistory());
    autoApprovalsDetail.setRevised(true);
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
  }

  @Test
  public void verifyAutoApprovalRulesNeedRevisionTest() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
        STORE_ID)).thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
        autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil.covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
        eq(autoApprovalsDetail), eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail,
        autoQCDetailResponse)).thenReturn(new ProductHistory());
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    autoApprovalsDetail.setRevised(true);
    autoApprovalsDetail.setPostLive(true);
    autoApprovalsDetail.setRestrictedKeywordPresent(false);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(autoApprovalUtil).verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    verify(autoApprovalUtil).verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig());
    verify(autoApprovalUtil).covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
        eq(autoApprovalsDetail), eq(autoQCDetailResponse));
    verify(autoApprovalUtil).covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse);
    verify(productAutoApprovalCriteriaService).saveProductAutoApprovalCriteria(any(ProductAutoApprovalCriteria.class));
    verify(productService).saveProductHistory(eq(PRODUCT_CODE), any(ProductHistory.class));
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.CONTENT_AND_IMAGE);
    Mockito.verify(applicationContext).getBean(AutoApprovalService.class);
  }

  @Test
  public void verifyAutoApprovalRulesNeedRevisionConvertedExcelTest() throws Exception{
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    autoApprovalsDetail.setRevised(true);
    autoApprovalsDetail.setPostLive(true);
    autoApprovalsDetail.setRestrictedKeywordPresent(false);
    autoApprovalsDetail.setProductCreationType(ProductCreationType.EXTERNAL_BULK_UPLOAD
        .getProductCreationType());
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(AutoApprovalType.NA, autoApprovalType);
  }

  @Test
  public void verifyAutoApprovalRulesNeedRevisionTest_forTrustedSellers() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
      STORE_ID)).thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
      autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
      autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil.covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(),
      eq(autoApprovalsDetail), eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail,
      autoQCDetailResponse)).thenReturn(new ProductHistory());
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    autoApprovalsDetail.setRevised(true);
    autoApprovalsDetail.setPostLive(true);
    autoApprovalsDetail.setRestrictedKeywordPresent(true);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
  }

  @Test
  public void verifyAutoApprovalRulesFailImageCheckTest() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
        STORE_ID)).thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
        autoApprovalsDetail)).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.FALSE);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(autoApprovalUtil).verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    verify(autoApprovalUtil).verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig());
    verify(productService)
        .updateAutoApprovalTypeByProductCode(AutoApprovalType.NOT_ELIGIBLE, autoApprovalsDetail.getProductCode());
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
    Mockito.verify(applicationContext).getBean(AutoApprovalService.class);
  }

  @Test
  public void verifyAutoApprovalRulesPreLive() throws Exception{
    autoApprovalsDetail.setPostLive(false);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
  }

  @Test
  public void verifyAutoApprovalRulesNeedRevisionPreLive() throws Exception{
    autoApprovalsDetail.setPostLive(false);
    autoApprovalsDetail.setRevised(true);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
  }

  @Test
  public void verifyAutoApprovalRulesAutoApprovalTypeNE() throws Exception{
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    autoApprovalsDetail.setPostLive(true);
    autoApprovalsDetail.setContentEditOnly(false);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
  }

  @Test
  public void verifyAutoApprovalRulesAutoApprovalTypeNEAndContentEdit() throws Exception {
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    autoApprovalsDetail.setPostLive(true);
    autoApprovalsDetail.setContentEditOnly(true);
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID))
        .thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail))
        .thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil
        .covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(), eq(autoApprovalsDetail),
            eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse))
        .thenReturn(new ProductHistory());
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(autoApprovalUtil).verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    verify(autoApprovalUtil).verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
        autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig());
    verify(autoApprovalUtil)
        .covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(), eq(autoApprovalsDetail),
            eq(autoQCDetailResponse));
    verify(autoApprovalUtil).covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse);
    verify(productAutoApprovalCriteriaService).saveProductAutoApprovalCriteria(any(ProductAutoApprovalCriteria.class));
    verify(productService).saveProductHistory(eq(PRODUCT_CODE), any(ProductHistory.class));
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.CONTENT_AND_IMAGE);
    Mockito.verify(applicationContext).getBean(AutoApprovalService.class);
  }

  @Test
  public void verifyAutoApprovalRulesAutoApprovalTypeNEAndContentEdit_forTrusted() throws Exception {
    autoApprovalsDetail.setAutoApprovalType(AutoApprovalType.NOT_ELIGIBLE);
    autoApprovalsDetail.setPostLive(true);
    autoApprovalsDetail.setContentEditOnly(true);
    autoApprovalsDetail.setRestrictedKeywordPresent(true);
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID))
      .thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail))
      .thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
      autoApprovalRulesMap.get(SELLER_OFFICIAL_RULE_NAME).getImageQcConfig())).thenReturn(Boolean.TRUE);
    when(autoApprovalUtil
      .covertToProductAutoApprovalCriteria(any(), eq(autoApprovalRulesDto), any(), eq(autoApprovalsDetail),
        eq(autoQCDetailResponse))).thenReturn(new ProductAutoApprovalCriteria());
    when(autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse))
      .thenReturn(new ProductHistory());
    autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
  }


  @Test
  public void verifyAutoApprovalRulesNoRuleTest() throws Exception{
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(
        STORE_ID)).thenReturn(autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap,
        autoApprovalsDetail)).thenReturn(null);
    AutoApprovalType autoApprovalType = autoApprovalService.verifyAutoApprovalRules(autoApprovalsDetail);
    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(autoApprovalUtil).verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    Assertions.assertEquals(autoApprovalType, AutoApprovalType.NA);
    Mockito.verify(applicationContext).getBean(AutoApprovalService.class);
  }

  private List<AutoApprovalRules> generateAutoApprovalRules() {
    AutoApprovalRules autoApprovalRules = new AutoApprovalRules();
    autoApprovalRules.setStoreId(DEFAULT_STORE_ID);
    autoApprovalRules.setCreatedBy(DEFAULT_CREATED_BY);
    autoApprovalRules.setUpdatedDate(new Date());
    autoApprovalRules.setUpdatedBy(DEFAULT_UPDATED_BY);
    autoApprovalRules.setVersion(DEFAULT_VERSION);
    autoApprovalRules.setCreatedDate(new Date());
    autoApprovalRules.setSequenceNumber(SEQUENCE_NUMBER);
    autoApprovalRules.setRuleName(RULE_NAME);
    autoApprovalRules.setRuleConfig(RULE_CONFIG);
    autoApprovalRules.setImageQcConfig(IMAGE_QC_CONFIG);
    autoApprovalRules.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    List<AutoApprovalRules> autoApprovalRulesList = new ArrayList<>();
    autoApprovalRulesList.add(autoApprovalRules);
    return autoApprovalRulesList;
  }

  @Test
  public void findAllAutoQcConfigRulesTest() throws Exception {
    Mockito.when(autoApprovalRulesRepository.findAll()).thenReturn(generateAutoApprovalRules());
    List<AutoApprovalRules> autoApprovalRulesList = autoApprovalService.findAllAutoQcConfigRules();
    Assertions.assertEquals(1, autoApprovalRulesList.size());
    Mockito.verify(autoApprovalRulesRepository).findAll();
  }

  @Test
  public void testFindByStoreIdAndRuleName() throws Exception {
    autoApprovalService.findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
    Mockito.verify(autoApprovalRulesRepository, AT_LEAST_ONE).findByStoreIdAndRuleName(STORE_ID, RULE_NAME);
  }

  @Test
  public void testSave() throws Exception {
    AutoApprovalRules autoApprovalRules = new AutoApprovalRules();
    autoApprovalService.save(autoApprovalRules);
    Mockito.verify(autoApprovalRulesRepository, AT_LEAST_ONE).save(autoApprovalRules);
  }

  @Test
  public void verifyAutoApprovalRulesForConfigChangeTest() throws Exception {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest =
        new AutoQcConfigChangeRequest(MERCHANT_CODE, C1_CATEGORY_CODE, true, new HashMap<>());
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(autoQCDetailResponse, oldAutoQCDetailResponse);
    oldAutoQCDetailResponse.setC1CreatedProductCount180(80);

    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID)).thenReturn(
        autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap())).thenReturn(oldAutoQCDetailResponse);
    when(autoApprovalUtil.verifyAutoQcRules(eq(oldAutoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(RULE_NAME);
    when(autoApprovalUtil.verifyAutoQcRules(eq(autoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(SELLER_OFFICIAL_RULE_NAME);

    boolean result = autoApprovalService.verifyAutoApprovalRulesForConfigChange(STORE_ID, autoQcConfigChangeRequest);

    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap());
    verify(autoApprovalUtil).verifyAutoQcRules(any(AutoQCDetailResponse.class),
        anyMap(), any(AutoApprovalsDetailDto.class));
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(applicationContext).getBean(AutoApprovalService.class);

    Assertions.assertTrue(result);
  }

  @Test
  public void verifyAutoApprovalRulesForConfigChangeNewConfigNullTest() throws Exception {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest =
        new AutoQcConfigChangeRequest(MERCHANT_CODE, C1_CATEGORY_CODE, true, new HashMap<>());
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(autoQCDetailResponse, oldAutoQCDetailResponse);
    oldAutoQCDetailResponse.setC1CreatedProductCount180(80);

    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID)).thenReturn(
        autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap())).thenReturn(oldAutoQCDetailResponse);
    when(autoApprovalUtil.verifyAutoQcRules(eq(oldAutoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(StringUtils.EMPTY);
    when(autoApprovalUtil.verifyAutoQcRules(eq(autoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(StringUtils.EMPTY);

    boolean result = autoApprovalService.verifyAutoApprovalRulesForConfigChange(STORE_ID, autoQcConfigChangeRequest);

    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap());
    verify(autoApprovalUtil).verifyAutoQcRules(any(AutoQCDetailResponse.class),
        anyMap(), any(AutoApprovalsDetailDto.class));
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(applicationContext).getBean(AutoApprovalService.class);

    Assertions.assertFalse(result);
  }

  @Test
  public void verifyAutoApprovalRulesForConfigChangeNewRuleWithLessPriorityTest() throws Exception {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest =
        new AutoQcConfigChangeRequest(MERCHANT_CODE, C1_CATEGORY_CODE, false, new HashMap<>());
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(autoQCDetailResponse, oldAutoQCDetailResponse);
    oldAutoQCDetailResponse.setC1CreatedProductCount180(80);
    AutoApprovalRulesDto autoApprovalRulesDto1 = new AutoApprovalRulesDto();
    autoApprovalRulesDto1.setRuleName(RULE_NAME);
    autoApprovalRulesDto1.setSequenceNumber(2);
    autoApprovalRulesDto1.setImageQcConfig(new ArrayList<>());
    autoApprovalRulesMap.put(RULE_NAME, autoApprovalRulesDto1);
    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID)).thenReturn(
        autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap())).thenReturn(oldAutoQCDetailResponse);
    when(autoApprovalUtil.verifyAutoQcRules(eq(oldAutoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(RULE_NAME);
    when(autoApprovalUtil.verifyAutoQcRules(eq(autoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(SELLER_OFFICIAL_RULE_NAME);

    boolean result = autoApprovalService.verifyAutoApprovalRulesForConfigChange(STORE_ID, autoQcConfigChangeRequest);

    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap());
    verify(autoApprovalUtil, times(2)).verifyAutoQcRules(any(AutoQCDetailResponse.class),
        anyMap(), any(AutoApprovalsDetailDto.class));
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(applicationContext).getBean(AutoApprovalService.class);

    Assertions.assertTrue(result);
  }

  @Test
  public void verifyAutoApprovalRulesForConfigChangeSameRuleTest() throws Exception {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest =
        new AutoQcConfigChangeRequest(MERCHANT_CODE, C1_CATEGORY_CODE, false, new HashMap<>());
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(autoQCDetailResponse, oldAutoQCDetailResponse);
    oldAutoQCDetailResponse.setC1CreatedProductCount180(80);

    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID)).thenReturn(
        autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap())).thenReturn(oldAutoQCDetailResponse);
    when(autoApprovalUtil.verifyAutoQcRules(eq(oldAutoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyAutoQcRules(eq(autoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(SELLER_OFFICIAL_RULE_NAME);

    autoApprovalService.verifyAutoApprovalRulesForConfigChange(STORE_ID, autoQcConfigChangeRequest);

    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap());
    verify(autoApprovalUtil, times(2)).verifyAutoQcRules(any(AutoQCDetailResponse.class),
        anyMap(), any(AutoApprovalsDetailDto.class));
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(applicationContext).getBean(AutoApprovalService.class);
  }

  @Test
  public void verifyAutoApprovalRulesForConfigChangeSecondRuleNullTest() throws Exception {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest =
        new AutoQcConfigChangeRequest(MERCHANT_CODE, C1_CATEGORY_CODE, false, new HashMap<>());
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(autoQCDetailResponse, oldAutoQCDetailResponse);
    oldAutoQCDetailResponse.setC1CreatedProductCount180(80);

    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID)).thenReturn(
        autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap())).thenReturn(oldAutoQCDetailResponse);
    when(autoApprovalUtil.verifyAutoQcRules(eq(oldAutoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(SELLER_OFFICIAL_RULE_NAME);
    when(autoApprovalUtil.verifyAutoQcRules(eq(autoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(StringUtils.EMPTY);

    boolean result = autoApprovalService.verifyAutoApprovalRulesForConfigChange(STORE_ID, autoQcConfigChangeRequest);

    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap());
    verify(autoApprovalUtil, times(2)).verifyAutoQcRules(any(AutoQCDetailResponse.class),
        anyMap(), any(AutoApprovalsDetailDto.class));
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(applicationContext).getBean(AutoApprovalService.class);

    Assertions.assertFalse(result);
  }

  @Test
  public void verifyAutoApprovalRulesForConfigChangeFirstRuleNullTest() throws Exception {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest =
        new AutoQcConfigChangeRequest(MERCHANT_CODE, C1_CATEGORY_CODE, false, new HashMap<>());
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(autoQCDetailResponse, oldAutoQCDetailResponse);
    oldAutoQCDetailResponse.setC1CreatedProductCount180(80);

    when(productAnalyticsOutbound.getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE)).thenReturn(autoQCDetailResponse);
    when(autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID)).thenReturn(
        autoApprovalRules);
    when(autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules)).thenReturn(autoApprovalRulesMap);
    when(autoApprovalUtil.getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap())).thenReturn(oldAutoQCDetailResponse);
    when(autoApprovalUtil.verifyAutoQcRules(eq(oldAutoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(StringUtils.EMPTY);
    when(autoApprovalUtil.verifyAutoQcRules(eq(autoQCDetailResponse), eq(autoApprovalRulesMap),
        any(AutoApprovalsDetailDto.class))).thenReturn(SELLER_OFFICIAL_RULE_NAME);

    boolean result = autoApprovalService.verifyAutoApprovalRulesForConfigChange(STORE_ID, autoQcConfigChangeRequest);

    verify(productAnalyticsOutbound).getAutoQCDetails(MERCHANT_CODE, C1_CATEGORY_CODE);
    verify(autoApprovalRulesRepository).findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(STORE_ID);
    verify(autoApprovalUtil).getOldAutoQcDetailResponse(eq(autoQCDetailResponse),
        anyMap());
    verify(autoApprovalUtil, times(2)).verifyAutoQcRules(any(AutoQCDetailResponse.class),
        anyMap(), any(AutoApprovalsDetailDto.class));
    verify(autoApprovalUtil).toAutoApprovalRulesDtoList(autoApprovalRules);
    verify(applicationContext).getBean(AutoApprovalService.class);

    Assertions.assertTrue(result);
  }
}
