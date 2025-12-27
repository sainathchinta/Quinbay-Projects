package com.gdn.mta.product.service.util;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;

import com.gdn.mta.product.service.AutoApprovalUtil;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalProductHistoryDto;
import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.CategoryPerformanceDto;
import com.gda.mta.product.dto.ChangedFieldDto;
import com.gda.mta.product.dto.ImageQcProcessingDto;
import com.gda.mta.product.dto.SellerTypeDto;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gda.mta.product.dto.response.ImageQcConfidenceDto;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class AutoApprovalUtilBean implements AutoApprovalUtil {

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public Map<String, AutoApprovalRulesDto> toAutoApprovalRulesDtoList(List<AutoApprovalRules> autoApprovalRules)
      throws Exception {
    Map<String, AutoApprovalRulesDto> autoApprovalRulesMap = new LinkedHashMap<>();
    for (AutoApprovalRules approvalRules : Optional.ofNullable(autoApprovalRules).orElse(new ArrayList<>())) {
      AutoApprovalRulesDto autoApprovalRulesDto = toAutoApprovalRulesDto(approvalRules);
      autoApprovalRulesMap.put(autoApprovalRulesDto.getRuleName(), autoApprovalRulesDto);
    }
    return autoApprovalRulesMap;
  }

  private AutoApprovalRulesDto toAutoApprovalRulesDto(AutoApprovalRules autoApprovalRule)
      throws Exception {
    AutoApprovalRulesDto autoApprovalRulesDto = new AutoApprovalRulesDto();
    autoApprovalRulesDto.setRuleName(autoApprovalRule.getRuleName());
    autoApprovalRulesDto.setAutoApprovalType(autoApprovalRule.getAutoApprovalType());
    List<AutoApprovalRuleDetailsDto> ruleConfig = Arrays.asList(objectMapper.readValue(
        autoApprovalRule.getRuleConfig(), AutoApprovalRuleDetailsDto[].class));
    List<AutoApprovalRuleDetailsDto> imageQcConfig = Arrays.asList(objectMapper.readValue(
        autoApprovalRule.getImageQcConfig(), AutoApprovalRuleDetailsDto[].class));
    autoApprovalRulesDto.setRuleConfig(ruleConfig);
    autoApprovalRulesDto.setImageQcConfig(imageQcConfig);
    autoApprovalRulesDto.setSequenceNumber(autoApprovalRule.getSequenceNumber());
    return autoApprovalRulesDto;
  }

  @Override
  public ProductAutoApprovalCriteria covertToProductAutoApprovalCriteria(AutoApprovalType autoApprovalType,
      AutoApprovalRulesDto autoApprovalRulesDto, String appliedRuleConfig,
      AutoApprovalsDetailDto autoApprovalsDetail, AutoQCDetailResponse autoQCDetailResponse)
      throws JsonProcessingException {
    ProductAutoApprovalCriteria productAutoApprovalCriteria = new ProductAutoApprovalCriteria();
    productAutoApprovalCriteria.setStoreId(autoApprovalsDetail.getStoreId());
    productAutoApprovalCriteria.setCreatedDate(new Date());
    productAutoApprovalCriteria.setUpdatedDate(new Date());
    productAutoApprovalCriteria.setMarkForDelete(false);
    productAutoApprovalCriteria.setProductCode(autoApprovalsDetail.getProductCode());
    productAutoApprovalCriteria.setRuleName(appliedRuleConfig);
    productAutoApprovalCriteria.setAutoApprovalResponse(objectMapper.writeValueAsString(autoQCDetailResponse));
    productAutoApprovalCriteria.setRuleCriteria(objectMapper.writeValueAsString(autoApprovalRulesDto.getRuleConfig()));
    productAutoApprovalCriteria.setImageQcCriteria(objectMapper.writeValueAsString(autoApprovalRulesDto.getImageQcConfig()));
    productAutoApprovalCriteria.setApprovalType(autoApprovalType);
    return productAutoApprovalCriteria;
  }

  @Override
  public ProductHistory covertToProductHistoryRepository(AutoApprovalsDetailDto autoApprovalsDetail,
      AutoQCDetailResponse autoQCDetailResponse)
      throws JsonProcessingException {
    ProductHistory productHistory = new ProductHistory();
    productHistory.setStoreId(autoApprovalsDetail.getStoreId());
    productHistory.setCreatedDate(new Date());
    productHistory.setUpdatedDate(new Date());
    productHistory.setMarkForDelete(false);
    productHistory.setState(Constants.STATE_5);
    productHistory.setDescription(Constants.ELIGIBLE_FOR_AUTO_APPROVAL);
    AutoApprovalProductHistoryDto autoApprovalProductHistory = new AutoApprovalProductHistoryDto();
    CategoryPerformanceDto categoryPerformance = new CategoryPerformanceDto();
    SellerTypeDto sellerType = new SellerTypeDto();
    categoryPerformance.setSellerBadge(autoQCDetailResponse.getSellerBadge());
    categoryPerformance.setC1RejectionPercent(autoQCDetailResponse.getC1RejectionPercent());
    categoryPerformance.setC1CreatedProductCount(autoQCDetailResponse.getC1CreatedProductCount());
    sellerType.setIsOfficialStore(autoQCDetailResponse.getIsOfficialStore());
    sellerType.setIsWhitelistCompanyOfficer(autoQCDetailResponse.getIsWhitelistCompanyOfficer());
    sellerType.setIsWhitelistSeller(autoQCDetailResponse.getIsWhitelistSeller());
    sellerType.setIsCompanyOfficial(autoApprovalsDetail.getUpdatedBy());
    autoApprovalProductHistory.setImageQcProcessing(autoApprovalsDetail.getImageQcConfidenceDetails());
    autoApprovalProductHistory.setCategoryPerformance(categoryPerformance);
    autoApprovalProductHistory.setSellerType(sellerType);
    productHistory.setNotes(objectMapper.writeValueAsString(autoApprovalProductHistory));
    return productHistory;
  }

  @Override
  public String verifyAutoQcRules(AutoQCDetailResponse autoQCDetailResponse,
      Map<String, AutoApprovalRulesDto> autoApprovalRulesMap, AutoApprovalsDetailDto autoApprovalsDetail) {
    return autoApprovalRulesMap.entrySet().stream()
        .map(Map.Entry::getValue)
        .filter(autoApprovalRulesDto -> Optional.ofNullable(autoApprovalRulesDto.getRuleConfig()).orElse(new ArrayList<>())
            .stream().allMatch(autoApprovalRule -> checkAutoQcRules(autoQCDetailResponse, autoApprovalRule, autoApprovalsDetail)))
        .findFirst()
        .map(AutoApprovalRulesDto::getRuleName)
        .orElseGet(String::new);
  }

  private boolean checkAutoQcRules(AutoQCDetailResponse autoQCDetailResponse,
      AutoApprovalRuleDetailsDto ruleConfig, AutoApprovalsDetailDto autoApprovalsDetail) {
    if (Objects.isNull(autoQCDetailResponse)) {
      return Boolean.FALSE;
    }
    Object value;
    value = AutoQcMapper.getValueByKey(autoQCDetailResponse, ruleConfig.getKeyName(), autoApprovalsDetail.getUpdatedBy());
    return compareValueWithRules(value, ruleConfig);
  }

  private boolean compareValueWithRules(Object value, AutoApprovalRuleDetailsDto ruleConfig) {
    boolean ruleComparisonResult = false;
    if (Objects.nonNull(value)) {
      try {
        if (ruleConfig.getValueType().equalsIgnoreCase(Constants.BOOLEAN)) {
          ruleComparisonResult = (value == Boolean.valueOf(ruleConfig.getValue()));
        } else if (ruleConfig.getValueType().equalsIgnoreCase(Constants.LIST_STRING)) {
          List<String> valuesList = objectMapper.readValue(convertToMapperList(ruleConfig.getValue()),
                  new TypeReference<List<String>>() {});
          ruleComparisonResult = valuesList.contains(value);
        } else if (ruleConfig.getValueType().equalsIgnoreCase(Constants.STRING)) {
          ruleComparisonResult = checkConditions(ruleConfig.getOperator(), ruleConfig.getValue(), (String) value);
        } else if (ruleConfig.getValueType().equalsIgnoreCase(Constants.INT)
            || ruleConfig.getValueType().equalsIgnoreCase(Constants.DOUBLE)) {
          ruleComparisonResult = checkConditions(ruleConfig.getOperator(),
              Double.parseDouble(ruleConfig.getValue()), Double.valueOf(value.toString()));
        }
      } catch (Exception e) {
        log.error("Error While TypeCasting For ruleName : {} ", ruleConfig.getKeyName(), e);
      }
    }
    return ruleComparisonResult;
  }

  private String convertToMapperList(String listString){
    listString = listString.replaceAll(Constants.COMMA, Constants.COMMA_WITH_SLASH);
    listString = listString.replace(Constants.OPEN_BRACKET, Constants.OPEN_BRACKET_WITH_SLASH);
    listString = listString.replace(Constants.CLOSED_BRACKET, Constants.CLOSED_BRACKET_WITH_SLASH);
    return listString;
  }

  @Override
  public boolean verifyImageQcRules(List<ImageQcProcessingDto> imageQcData,
      List<AutoApprovalRuleDetailsDto> imageQcConfig) {
    if (CollectionUtils.isEmpty(imageQcData)) {
      return true;
    }
    Map<String, AutoApprovalRuleDetailsDto> imageAutoApprovalRules =
        Optional.ofNullable(imageQcConfig).orElse(new ArrayList<>()).stream()
            .collect(Collectors.toMap(AutoApprovalRuleDetailsDto::getKeyName, Function.identity()));

    return imageQcData.stream()
        .map(ImageQcProcessingDto::getImageQcConfidenceDtoList)
        .allMatch(list -> list.stream()
            .allMatch(imageQcConfidenceDto -> this.checkImageQcConfidence(imageQcConfidenceDto, imageAutoApprovalRules)));
  }

  @Override
  public AutoQCDetailResponse getOldAutoQcDetailResponse(AutoQCDetailResponse newAutoQCDetailResponse,
      Map<String, ChangedFieldDto> changedFields) {
    AutoQCDetailResponse oldAutoQCDetailResponse = new AutoQCDetailResponse();
    BeanUtils.copyProperties(newAutoQCDetailResponse, oldAutoQCDetailResponse);
    if (!MapUtils.isEmpty(changedFields)) {
      for (Map.Entry<String, ChangedFieldDto> entry : changedFields.entrySet()) {
        try {
          Field field = oldAutoQCDetailResponse.getClass().getDeclaredField(entry.getKey());
          field.setAccessible(true);
          if (Constants.NULL.equalsIgnoreCase(entry.getValue().getOldValue())) {
            field.set(oldAutoQCDetailResponse, null);
          }
          else if (Boolean.class == field.getType()) {
            field.set(oldAutoQCDetailResponse, Boolean.valueOf(entry.getValue().getOldValue()));
          } else if (Integer.class == field.getType()) {
            field.set(oldAutoQCDetailResponse, Integer.parseInt(entry.getValue().getOldValue()));
          } else if (Float.class == field.getType()) {
            field.set(oldAutoQCDetailResponse, Float.parseFloat(entry.getValue().getOldValue()));
          } else if (String.class == field.getType()) {
            field.set(oldAutoQCDetailResponse, entry.getValue().getOldValue());
          }
        } catch (Exception e) {
          log.warn("Error while accessing field in AutoQCDetailResponse. field : {} ", entry.getKey(), e);
        }
      }
    }
    return oldAutoQCDetailResponse;
  }

  private boolean checkImageQcConfidence(ImageQcConfidenceDto imageQcConfidenceDetails,
      Map<String, AutoApprovalRuleDetailsDto> imageAutoApprovalRules) {
    if (Objects.isNull(imageQcConfidenceDetails)) {
      return false;
    }
    AutoApprovalRuleDetailsDto imageAutoApprovalRule;
    imageAutoApprovalRule = imageAutoApprovalRules.getOrDefault(imageQcConfidenceDetails.getDisplayName(), null);
    if (Objects.nonNull(imageAutoApprovalRule)) {
      try {
        return checkConditions(imageAutoApprovalRule.getOperator(),
            Double.parseDouble(imageAutoApprovalRule.getValue()), Double.parseDouble(imageQcConfidenceDetails.getConfidence()));
      }catch (Exception e){
        log.error("Error While TypeCasting For ruleName : {} : ", imageAutoApprovalRule.getKeyName(),e);
      }
    }
    return false;
  }

  private boolean checkConditions(String operator, double referenceValue, double value) {
    boolean isConditionValid;
    switch (operator) {
      case Constants.GREATER_THAN_OR_EQUAL_TO:
        isConditionValid = (value >= referenceValue);
        break;
      case Constants.GREATER_THAN:
        isConditionValid = (value > referenceValue);
        break;
      case Constants.LESS_THAN_OR_EQUAL_TO:
        isConditionValid=  (value <= referenceValue);
        break;
      case Constants.LESS_THAN:
        isConditionValid = (value < referenceValue);
        break;
      case Constants.EQUAL_TO:
        isConditionValid = (value == referenceValue);
        break;
      case Constants.NOT_EQUALS_TO:
        isConditionValid = (value != referenceValue);
        break;
      default:
        isConditionValid = false;
        break;
    }
    return isConditionValid;
  }

  private boolean checkConditions(String operator, String referenceValue, String value) {
    boolean isConditionValid;
    switch (operator) {
      case Constants.EQUAL_TO:
        isConditionValid = (referenceValue.equals(value));
        break;
      case Constants.CONTAINS:
        isConditionValid = (value.contains(referenceValue));
        break;
      default:
        isConditionValid = false;
        break;
    }
    return isConditionValid;
  }


}
