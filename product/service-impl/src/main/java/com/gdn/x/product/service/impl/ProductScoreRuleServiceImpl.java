package com.gdn.x.product.service.impl;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.CategoryProductScoreRuleRepository;
import com.gdn.x.product.dao.api.GlobalProductScoreRuleRepository;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.model.entity.CategoryProductScoreRule;
import com.gdn.x.product.model.entity.GlobalProductScoreRule;
import com.gdn.x.product.model.entity.RuleConfig;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;
import com.gdn.x.product.rest.web.model.response.RuleConfigResponse;
import com.gdn.x.product.service.api.ProductScoreRuleService;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductScoreRuleServiceImpl implements ProductScoreRuleService {

  @Autowired
  private GlobalProductScoreRuleRepository globalProductScoreRuleRepository;

  @Autowired
  private CategoryProductScoreRuleRepository categoryProductScoreRuleRepository;

  @Override
  @Cacheable(value = CacheNames.PRODUCT_SCORE_RULE_GLOBAL, key = "#storeId", unless = "#result == null")
  public Map<String, MaxScoreAndRuleConfigResponse> getProductScoreRulesGlobal(String storeId) throws Exception {
    List<GlobalProductScoreRule> globalProductScoreRules = getGlobalProductScoreRules(storeId);
    return getResponseFromGlobalRule(globalProductScoreRules);
  }

  @Override
  @Cacheable(value = CacheNames.PRODUCT_SCORE_RULE_CATEGORY, key = "#storeId + '-' + #categoryCode", unless = "#result == null")
  public Map<String, MaxScoreAndRuleConfigResponse> getProductScoreRulesForCategory(String storeId, String categoryCode) throws Exception {
    List<CategoryProductScoreRule> categoryProductScoreRules = getProductScoreRulesByCategoryCode(storeId, categoryCode);
    if (CollectionUtils.isNotEmpty(categoryProductScoreRules)) {
      return getResponseFromCategoryRule(categoryProductScoreRules);
    } else {
      List<GlobalProductScoreRule> globalProductScoreRules = getGlobalProductScoreRules(storeId);
      return getResponseFromGlobalRule(globalProductScoreRules);
    }
  }

  private Map<String, MaxScoreAndRuleConfigResponse> getResponseFromCategoryRule(
      List<CategoryProductScoreRule> categoryProductScoreRules) throws Exception {
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap = new HashMap<>();
    for (CategoryProductScoreRule categoryProductScoreRule : categoryProductScoreRules) {
      if (Objects.nonNull(categoryProductScoreRule.getRuleConfig())) {
        productScoreRuleDtoMap.put(categoryProductScoreRule.getRuleName(),
            new MaxScoreAndRuleConfigResponse(categoryProductScoreRule.getMaxScore(),
                toRuleConfigResponse(categoryProductScoreRule.getRuleConfig())));
      } else {
        productScoreRuleDtoMap.put(categoryProductScoreRule.getRuleName(),
            new MaxScoreAndRuleConfigResponse(categoryProductScoreRule.getMaxScore(), null));
      }

    }
    return productScoreRuleDtoMap;
  }

  private Map<String, MaxScoreAndRuleConfigResponse> getResponseFromGlobalRule(
      List<GlobalProductScoreRule> globalProductScoreRules) throws Exception {
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap = new HashMap<>();
    for (GlobalProductScoreRule globalProductScoreRule : globalProductScoreRules) {
      if (Objects.nonNull(globalProductScoreRule.getRuleConfig())) {
        productScoreRuleDtoMap.put(globalProductScoreRule.getRuleName(),
            new MaxScoreAndRuleConfigResponse(globalProductScoreRule.getMaxScore(),
                toRuleConfigResponse(globalProductScoreRule.getRuleConfig())));
      } else {
        productScoreRuleDtoMap.put(globalProductScoreRule.getRuleName(),
            new MaxScoreAndRuleConfigResponse(globalProductScoreRule.getMaxScore(), null));
      }
    }
    return productScoreRuleDtoMap;
  }

  private List<RuleConfigResponse> toRuleConfigResponse(List<RuleConfig> ruleConfigs) {
    return ruleConfigs.stream().map(
        ruleConfig -> new RuleConfigResponse(ruleConfig.getOperator(), ruleConfig.getValue(), ruleConfig.getScore()))
        .collect(Collectors.toList());
  }


  private List<GlobalProductScoreRule> getGlobalProductScoreRules(String storeId) {
    return globalProductScoreRuleRepository.findByStoreIdAndMarkForDeleteFalse(storeId);
  }

  private List<CategoryProductScoreRule> getProductScoreRulesByCategoryCode(String storeId, String categoryCode) {
    return categoryProductScoreRuleRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
  }

}
