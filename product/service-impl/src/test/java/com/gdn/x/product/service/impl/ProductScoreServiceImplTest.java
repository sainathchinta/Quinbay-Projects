package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.context.ApplicationContext;

import com.gdn.x.product.dao.api.CategoryProductScoreRuleRepository;
import com.gdn.x.product.dao.api.GlobalProductScoreRuleRepository;
import com.gdn.x.product.model.entity.CategoryProductScoreRule;
import com.gdn.x.product.model.entity.GlobalProductScoreRule;
import com.gdn.x.product.model.entity.RuleConfig;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;

public class ProductScoreServiceImplTest {
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String PRODUCT_TITLE = "PRODUCT_TITLE";
  private static final String MANDATORY = "MANDATORY";
  private static final String OPERATOR = "==";
  private static final int VALUE = 5;
  private static final int MAX_SCORE = 20;
  private static final String STORE_ID = "10001";

  private List<GlobalProductScoreRule> globalProductScoreRules;
  private List<CategoryProductScoreRule> categoryProductScoreRules;

  @InjectMocks
  private ProductScoreRuleServiceImpl productScoreRuleService;

  @Mock
  private GlobalProductScoreRuleRepository globalProductScoreRuleRepository;

  @Mock
  private CategoryProductScoreRuleRepository categoryProductScoreRuleRepository;

  @Mock
  private ApplicationContext applicationContext;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);

    RuleConfig ruleConfig = new RuleConfig(OPERATOR, VALUE, MAX_SCORE);
    GlobalProductScoreRule globalProductScoreRule =
        new GlobalProductScoreRule(PRODUCT_TITLE, MANDATORY, MAX_SCORE, Arrays.asList(ruleConfig));
    CategoryProductScoreRule categoryProductScoreRule = new CategoryProductScoreRule();
    categoryProductScoreRule.setCategoryCode(CATEGORY_CODE);
    categoryProductScoreRule.setRuleName(PRODUCT_TITLE);
    categoryProductScoreRule.setRuleType(MANDATORY);
    categoryProductScoreRule.setMaxScore(MAX_SCORE);
    categoryProductScoreRule.setRuleConfig(Arrays.asList(ruleConfig));

    globalProductScoreRules = new ArrayList<>();
    globalProductScoreRules.add(globalProductScoreRule);
    categoryProductScoreRules = new ArrayList<>();
    categoryProductScoreRules.add(categoryProductScoreRule);

    Mockito.when(applicationContext.getBean(ProductScoreRuleServiceImpl.class)).thenReturn(productScoreRuleService);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(globalProductScoreRuleRepository);
    Mockito.verifyNoMoreInteractions(categoryProductScoreRuleRepository);
  }

  @Test
  public void getProductScoreRulesCategoryGlobalTest() throws Exception {
    Mockito.when(categoryProductScoreRuleRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(globalProductScoreRuleRepository.findByStoreIdAndMarkForDeleteFalse(STORE_ID)).thenReturn(globalProductScoreRules);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap =
        productScoreRuleService.getProductScoreRulesForCategory(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryProductScoreRuleRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(globalProductScoreRuleRepository).findByStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(1, productScoreRuleDtoMap.size());
    Assertions.assertEquals(MAX_SCORE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getMaxScore());
    Assertions.assertEquals(VALUE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getRuleConfig().get(0).getValue());
  }

  @Test
  public void getProductScoreRulesCatgeoryTest() throws Exception {
    Mockito.when(categoryProductScoreRuleRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryProductScoreRules);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap =
        productScoreRuleService.getProductScoreRulesForCategory(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryProductScoreRuleRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Assertions.assertEquals(1, productScoreRuleDtoMap.size());
    Assertions.assertEquals(MAX_SCORE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getMaxScore());
    Assertions.assertEquals(VALUE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getRuleConfig().get(0).getValue());
  }

  @Test
  public void getProductScoreRulesNullTest() throws Exception {
    Mockito.when(globalProductScoreRuleRepository.findByStoreIdAndMarkForDeleteFalse(STORE_ID)).thenReturn(globalProductScoreRules);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap =
        productScoreRuleService.getProductScoreRulesGlobal(STORE_ID);
    Mockito.verify(globalProductScoreRuleRepository).findByStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(1, productScoreRuleDtoMap.size());
    Assertions.assertEquals(MAX_SCORE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getMaxScore());
    Assertions.assertEquals(VALUE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getRuleConfig().get(0).getValue());
  }

  @Test
  public void getProductScoreRulesRuleConfigNullTest() throws Exception {
    globalProductScoreRules.get(0).setRuleConfig(null);
    Mockito.when(categoryProductScoreRuleRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(globalProductScoreRuleRepository.findByStoreIdAndMarkForDeleteFalse(STORE_ID)).thenReturn(globalProductScoreRules);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap =
        productScoreRuleService.getProductScoreRulesForCategory(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryProductScoreRuleRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(globalProductScoreRuleRepository).findByStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(1, productScoreRuleDtoMap.size());
    Assertions.assertEquals(MAX_SCORE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getMaxScore());
  }

  @Test
  public void getProductScoreRulesCatgeoryRuleConfigNullTest() throws Exception {
    categoryProductScoreRules.get(0).setRuleConfig(null);
    Mockito.when(categoryProductScoreRuleRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryProductScoreRules);
    Map<String, MaxScoreAndRuleConfigResponse> productScoreRuleDtoMap =
        productScoreRuleService.getProductScoreRulesForCategory(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryProductScoreRuleRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    Assertions.assertEquals(1, productScoreRuleDtoMap.size());
    Assertions.assertEquals(MAX_SCORE, productScoreRuleDtoMap.get(PRODUCT_TITLE).getMaxScore());
  }
}
