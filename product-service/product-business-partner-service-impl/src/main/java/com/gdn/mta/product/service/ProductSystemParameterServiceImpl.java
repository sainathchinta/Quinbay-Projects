package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.repository.ProductSystemParameterRepository;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.pbp.property.MandatoryParameterHelper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductSystemParameterServiceImpl implements ProductSystemParameterService {

  @Value(value = "${system.parameter.switch.fetch}")
  private String switchVariables;

  @Value(value = "${system.parameter.text.fetch}")
  private String textVariables;

  @Value(value = "${system.parameter.max.stock.value}")
  private long maxStockValue;

  @Value(value = "${system.parameter.max.variants}")
  private long maxVariants;

  @Value(value = "${system.parameter.max.Assignee.limit}")
  private long maxAssigneeLimit;

  @Value(value = "${system.parameter.max.Auto.Assignment.productCount}")
  private long maxAutoAssignmentProductCount;

  @Value(value = "${system.parameter.bulk.estimation.interval.limit}")
  private long bulkEstimationIntervalLimit;

  @Value(value = "${system.parameter.bulk.listing.interval.limit}")
  private long bulkListingIntervalLimit;

  @Value("#{${canary.feature.switches}}")
  private Map<String, String> canaryFeatureSwitches;

  @Value(value = "${size.chart.feature.switch.phase1}")
  private String sizeChartFeatureSwitchPhase1;

  @Value(value = "${size.chart.feature.switch.phase2}")
  private String sizeChartFeatureSwitchPhase2;

  @Value(value = "${system.parameter.bopis.category.restriction.feature.switch}")
  private String bopisCategoryRestrictionFeatureSwitch;

  @Value(value = "${system.parameter.bopis.unsupported.merchant.types}")
  private String bopisUnsupportedMerchantTypes;

  @Value(value = "${system.parameter.bopis.cnc.restriction.feature.switch}")
  private String bopisCNCRestrictionFeatureSwitch;

  @Value("${validate.warehouse.deletion.eligible.sellers}")
  private String validateWarehouseDeletionEligibleSellers;

  @Value("${validate.warehouse.variant.deletion.enabled}")
  private String validateWarehouseVariantDeletionEnabled;

  @Autowired
  private ProductSystemParameterRepository productSystemParameterRepository;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Autowired
  private ApplicationContext applicationContext;

  @Override
  @Transactional(readOnly = false)
  @CacheEvict(value = CacheKeys.PRODUCT_SYSTEM_PARAMETER_SWITCHES, key = "#productSystemParameter.storeId")
  public void insert(ProductSystemParameter productSystemParameter) {
    validateProductSystemParameterRequest(productSystemParameter);
    productSystemParameterRepository.save(productSystemParameter);
  }

  private void validateProductSystemParameterRequest(ProductSystemParameter productSystemParameter) {
    checkArgument(StringUtils.isNotBlank(productSystemParameter.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSystemParameter.getVariable()),
        ErrorMessages.VARIABLE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSystemParameter.getValue()), ErrorMessages.VALUE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSystemParameter.getDescription()),
        ErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
  }

  @Override
  @Caching(evict = {
      @CacheEvict(cacheNames = CacheKeys.PRODUCT_SYSTEM_PARAMETER, key = "#productSystemParameter.variable"),
      @CacheEvict(cacheNames = CacheKeys.PRODUCT_SYSTEM_PARAMETER_SWITCHES, key = "#productSystemParameter.storeId")
  })
  @Transactional(readOnly = false)
  public void update(ProductSystemParameter productSystemParameter) {
    validateProductSystemParameterRequest(productSystemParameter);
    ProductSystemParameter response = productSystemParameterRepository
        .findByStoreIdAndVariable(productSystemParameter.getStoreId(), productSystemParameter.getVariable());
    checkState(Objects.nonNull(response), ErrorMessages.VALUE_NOT_FOUND);
    response.setValue(productSystemParameter.getValue());
    response.setShowOnUI(productSystemParameter.isShowOnUI());
    response.setDescription(productSystemParameter.getDescription());
    productSystemParameterRepository.save(response);
  }

  @Override
  @Caching(evict = {
      @CacheEvict(cacheNames = CacheKeys.PRODUCT_SYSTEM_PARAMETER, key = "#variable"),
      @CacheEvict(cacheNames = CacheKeys.PRODUCT_SYSTEM_PARAMETER_SWITCHES, key = "#storeId")
  })
  @Transactional(readOnly = false)
  public void delete(String storeId, String variable) {
    ProductSystemParameter response = findByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(response), ErrorMessages.VALUE_NOT_FOUND);
    productSystemParameterRepository.deleteById(response.getId());
  }

  @Override
  @Cacheable(value = CacheKeys.PRODUCT_SYSTEM_PARAMETER, key = "#variable", unless = "#result == null")
  public ProductSystemParameter findByStoreIdAndVariable(String storeId, String variable) {
    return this.productSystemParameterRepository.findByStoreIdAndVariable(storeId, variable);
  }

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  public Map<String, Object> findSwitchValuesWithCanaryAndNonCanary(String storeId) {

    ProductSystemParameterService productSystemParameterService =
        applicationContext.getBean(ProductSystemParameterService.class);
    Map<String, Object> variableMap = productSystemParameterService.findSwitchValues(storeId);
  // Override flags from Canary Feature switch Map
    for (String canaryFeatureSwitch : canaryFeatureSwitches.keySet()) {
      variableMap.put(canaryFeatureSwitch, canaryFeatureSwitches.get(canaryFeatureSwitch));
    }
    return variableMap;
  }

  @Override
  @Cacheable(value = CacheKeys.PRODUCT_SYSTEM_PARAMETER_SWITCHES, key = "#storeId", unless = "#result == null")
  public Map<String, Object> findSwitchValues (String storeId) {
    List<String> variableList = Arrays.stream(switchVariables.split(",", -1)).collect(Collectors.toList());
    List<String> textVariableList = Arrays.stream(textVariables.split(",", -1)).collect(Collectors.toList());
    List<String> combinedList = new ArrayList<>();
    combinedList.addAll(variableList);
    combinedList.addAll(textVariableList);
    List<ProductSystemParameter> productSystemParameterList =
        this.productSystemParameterRepository.findByStoreIdAndVariableIn(storeId, combinedList);
    Map<String, Object> variableMap = Optional.ofNullable(productSystemParameterList).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(productSystemParameter -> productSystemParameter.getVariable(),
            productSystemParameter -> variableList.contains(productSystemParameter.getVariable()) ?
                Boolean.valueOf(productSystemParameter.getValue()) :
                productSystemParameter.getValue()));
    variableMap.put(SystemParameterConstants.MAX_STOCK_LIMIT, maxStockValue);
    variableMap.put(SystemParameterConstants.MAX_VARIANTS, maxVariants);
    variableMap.put(SystemParameterConstants.MAX_ASSIGNEE_LIMIT, maxAssigneeLimit);
    variableMap.put(SystemParameterConstants.MAX_AUTO_ASSIGNMENT_PRODUCT_COUNT,
      maxAutoAssignmentProductCount);
    variableMap.put(SystemParameterConstants.BULK_LISTING_INTERVAL_LIMIT, bulkListingIntervalLimit);
    variableMap.put(SystemParameterConstants.BULK_ESTIMATION_INTERVAL_LIMIT,
      bulkEstimationIntervalLimit);
    variableMap.put(SystemParameterConstants.SIZE_CHART_FEATURE_SWITCH_PHASE_1,
        variableMap.getOrDefault(sizeChartFeatureSwitchPhase1, false));
    variableMap.put(SystemParameterConstants.SIZE_CHART_FEATURE_SWITCH_PHASE_2,
        variableMap.getOrDefault(sizeChartFeatureSwitchPhase2, false));
    variableMap.put(SystemParameterConstants.BOPIS_CATEGORY_RESTRICTION_FEATURE_SWITCH,
      Boolean.valueOf(variableMap.getOrDefault(bopisCategoryRestrictionFeatureSwitch, false).toString()));
    variableMap.put(SystemParameterConstants.BOPIS_CNC_RESTRICTION_FEATURE_SWITCH,
      Boolean.valueOf(variableMap.getOrDefault(bopisCNCRestrictionFeatureSwitch, false).toString()));
    variableMap.put(SystemParameterConstants.BOPIS_UNSUPPORTED_MERCHANT_TYPES,
      bopisUnsupportedMerchantTypes);
    variableMap.put(SystemParameterConstants.WAREHOUSE_STOCK_VALIDATION_SUPPORTED_MERCHANT_TYPES,
      validateWarehouseDeletionEligibleSellers);
    variableMap.put(SystemParameterConstants.VALIDATE_WAREHOUSE_VARIANT_DELETION_FEATURE_SWITCH,
      Boolean.valueOf(variableMap.getOrDefault(validateWarehouseVariantDeletionEnabled, false).toString()));
    variableMap.put(SystemParameterConstants.PRE_ORDER_FEATURE_SWITCH_FOR_UI, preOrderConfig.isPoQuotaFeatureSwitch());
    return variableMap;
  }

  @Override
  public List<ProductSystemParameter> findByStoreIdAndShowOnUITrue(String storeId) {
    return this.productSystemParameterRepository.findByStoreIdAndShowOnUIIsTrue(storeId);
  }
}
