package com.gdn.mta.product.service;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.enums.ProductCreationType;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.repository.AutoApprovalRulesRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true, rollbackFor = Exception.class)
public class AutoApprovalServiceImpl implements AutoApprovalService{

  @Autowired
  private AutoApprovalRulesRepository autoApprovalRulesRepository;

  @Autowired
  private ProductAutoApprovalCriteriaService productAutoApprovalCriteriaService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Autowired
  private AutoApprovalUtil autoApprovalUtil;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public AutoApprovalType verifyAutoApprovalRules(AutoApprovalsDetailDto autoApprovalsDetail) throws Exception {
    AutoApprovalType autoApprovalType = AutoApprovalType.NA;
    log.info("Product auto approval rules request : {} , productCode : {} ", autoApprovalsDetail,
        autoApprovalsDetail.getProductCode());
    if (autoApprovalsDetail.isRevised()) {
      if (!autoApprovalsDetail.isPostLive()) {
        return autoApprovalType;
      }
    } else if (!autoApprovalsDetail.isPostLive() || (!autoApprovalsDetail.isContentEditOnly() && AutoApprovalType.NOT_ELIGIBLE
        .equals(autoApprovalsDetail.getAutoApprovalType()))) {
      return autoApprovalType;
    }
    if(autoApprovalsDetail.isForceReview() || autoApprovalsDetail.isRestrictedKeywordPresent()) {
      return autoApprovalType;
    }
    if (ProductCreationType.EXTERNAL_BULK_UPLOAD.getProductCreationType()
        .equals(autoApprovalsDetail.getProductCreationType())) {
      log.info(
          "Auto approval skipped because creation type is EXTERNAL_BULK_UPLOAD for product code:"
              + " {}", autoApprovalsDetail.getProductCode());
      return autoApprovalType;
    }
    AutoQCDetailResponse autoQCDetailResponse = productAnalyticsOutbound
        .getAutoQCDetails(autoApprovalsDetail.getMerchantCode(), autoApprovalsDetail.getC1CategoryCode());
    List<AutoApprovalRules> autoApprovalRules =
        autoApprovalService().findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(autoApprovalsDetail.getStoreId());
    Map<String, AutoApprovalRulesDto> autoApprovalRulesMap =
        autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules);
    String appliedRuleConfig =
        autoApprovalUtil.verifyAutoQcRules(autoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    if (StringUtils.isNotEmpty(appliedRuleConfig)) {
      boolean isEligibleForAutoApproval = autoApprovalUtil
          .verifyImageQcRules(autoApprovalsDetail.getImageQcConfidenceDetails(),
              autoApprovalRulesMap.get(appliedRuleConfig).getImageQcConfig());
      if (isEligibleForAutoApproval) {
        autoApprovalType = AutoApprovalType.CONTENT_AND_IMAGE;
      } else {
        productService
            .updateAutoApprovalTypeByProductCode(AutoApprovalType.NOT_ELIGIBLE, autoApprovalsDetail.getProductCode());
      }
    }
    if (AutoApprovalType.CONTENT_AND_IMAGE.equals(autoApprovalType)) {
      productAutoApprovalCriteriaService.saveProductAutoApprovalCriteria(autoApprovalUtil
          .covertToProductAutoApprovalCriteria(autoApprovalType, autoApprovalRulesMap.get(appliedRuleConfig),
              appliedRuleConfig, autoApprovalsDetail, autoQCDetailResponse));
      productService.saveProductHistory(autoApprovalsDetail.getProductCode(),
          autoApprovalUtil.covertToProductHistoryRepository(autoApprovalsDetail, autoQCDetailResponse));
    }
    log.info("Auto approval response for productCode : {}, response : {} ", autoApprovalsDetail.getProductCode(),
        autoApprovalType);
    return autoApprovalType;
  }

  @Override
  public boolean verifyAutoApprovalRulesForConfigChange(String storeId,
      AutoQcConfigChangeRequest autoQcConfigChangeRequest) throws Exception {
    AutoQCDetailResponse newAutoQCDetailResponse =
        productAnalyticsOutbound.getAutoQCDetails(autoQcConfigChangeRequest.getSellerCode(),
            autoQcConfigChangeRequest.getCategoryCode());
    AutoQCDetailResponse oldAutoQCDetailResponse = autoApprovalUtil.getOldAutoQcDetailResponse(newAutoQCDetailResponse,
        autoQcConfigChangeRequest.getChangedFields());
    List<AutoApprovalRules> autoApprovalRules =
        autoApprovalService().findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(storeId);
    Map<String, AutoApprovalRulesDto> autoApprovalRulesMap =
        autoApprovalUtil.toAutoApprovalRulesDtoList(autoApprovalRules);
    return verifyChangeInAutoQcRuleConfig(autoQcConfigChangeRequest.isNewSeller(), newAutoQCDetailResponse,
        oldAutoQCDetailResponse, autoApprovalRulesMap);
  }

  private boolean verifyChangeInAutoQcRuleConfig(boolean isNewSeller, AutoQCDetailResponse newAutoQCDetailResponse,
      AutoQCDetailResponse oldAutoQCDetailResponse, Map<String, AutoApprovalRulesDto> autoApprovalRulesMap) {
    AutoApprovalsDetailDto autoApprovalsDetail = new AutoApprovalsDetailDto();
    autoApprovalsDetail.setUpdatedBy(Constants.SYSTEM);
    String newAppliedRuleConfig =
        autoApprovalUtil.verifyAutoQcRules(newAutoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    String oldAppliedRuleConfig = isNewSeller ?
        StringUtils.EMPTY :
        autoApprovalUtil.verifyAutoQcRules(oldAutoQCDetailResponse, autoApprovalRulesMap, autoApprovalsDetail);
    log.info("newAppliedRuleConfig : {} , oldAppliedRuleConfig : {} ", newAppliedRuleConfig, oldAppliedRuleConfig);
    return isConfigChanged(newAppliedRuleConfig, oldAppliedRuleConfig, autoApprovalRulesMap);
  }

  private boolean isConfigChanged(String newAppliedRuleConfig, String oldAppliedRuleConfig,
      Map<String, AutoApprovalRulesDto> autoApprovalRulesMap) {
    if (StringUtils.isBlank(oldAppliedRuleConfig) && StringUtils.isNotBlank(newAppliedRuleConfig)) {
      return true;
    } else if (StringUtils.isNotBlank(newAppliedRuleConfig)) {
      return autoApprovalRulesMap.get(newAppliedRuleConfig).getSequenceNumber() < autoApprovalRulesMap.get(
          oldAppliedRuleConfig).getSequenceNumber();
    }
    return false;
  }


  @Override
  @Cacheable(value = CacheKeys.AUTO_APPROVAL_RULES, key = "#storeId", unless =  "#result == null")
  public List<AutoApprovalRules> findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(String storeId) {
    return autoApprovalRulesRepository.findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(storeId);
  }

  @Override
  public List<AutoApprovalRules> findAllAutoQcConfigRules() {
    List<AutoApprovalRules> autoApprovalRulesList = autoApprovalRulesRepository.findAll();
    Collections.sort(autoApprovalRulesList, Comparator.comparing(AutoApprovalRules::getSequenceNumber));
    return autoApprovalRulesList;
  }

  @Override
  public AutoApprovalRules findByStoreIdAndRuleName(String storeId, String ruleName) {
    return autoApprovalRulesRepository.findByStoreIdAndRuleName(storeId, ruleName);
  }

  @Override
  public void save(AutoApprovalRules autoApprovalRules) {
    autoApprovalRulesRepository.save(autoApprovalRules);
  }

  private AutoApprovalService autoApprovalService() {
    return applicationContext.getBean(AutoApprovalService.class);
  }
}
