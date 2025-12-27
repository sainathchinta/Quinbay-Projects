package com.gdn.mta.product.service;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.service.exception.ApiInvalidImageQcConfigException;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true, rollbackFor = Exception.class)
public class AutoQcConfigServiceImpl implements AutoQcConfigService {

  @Autowired
  private AutoApprovalService autoApprovalService;

  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  private Gson gson = new GsonBuilder().disableHtmlEscaping().create();

  @Override
  @CacheEvict(value = CacheKeys.AUTO_APPROVAL_RULES, key = "#storeId")
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(String ruleName, String storeId, AutoQcConfigRequest autoQcConfigRequest,
      boolean isNeedRevisionConfig) {
    AutoApprovalRules autoApprovalRules = autoApprovalService.findByStoreIdAndRuleName(storeId, ruleName);
    if (Objects.isNull(autoQcConfigRequest.getRuleEnabled())) {
      validateAutoApprovalImageConfig(storeId, autoQcConfigRequest, isNeedRevisionConfig);
      setImageConfig(autoApprovalRules, autoQcConfigRequest.getImageConfig(), isNeedRevisionConfig);
    } else {
      enableOrDisableRule(autoApprovalRules, isNeedRevisionConfig, autoQcConfigRequest.getRuleEnabled());
    }
    autoApprovalService.save(autoApprovalRules);
  }

  private void validateAutoApprovalImageConfig(String storeId, AutoQcConfigRequest autoQcConfigRequest,
      boolean isNeedRevisionConfig) {
    if (!isNeedRevisionConfig) {
      List<ProductImagePrediction> productImagePredictionList =
          productImagePredictionService.findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(storeId);
      List<AutoApprovalRuleDetailsDto> imageConfigList = autoQcConfigRequest.getImageConfig();
      HashMap<String, Integer> imageConfigMap = new HashMap<>();
      for (AutoApprovalRuleDetailsDto imageConfig : imageConfigList) {
        imageConfigMap.put(imageConfig.getKeyName(), Integer.parseInt(imageConfig.getValue()));
      }
      for (ProductImagePrediction productImagePrediction : productImagePredictionList) {
        if (imageConfigMap.get(productImagePrediction.getDisplayName()) >= productImagePrediction
            .getConfidenceThreshold()) {
          throw new ApiInvalidImageQcConfigException(
              productImagePrediction.getDisplayName().concat(Constants.IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD),
              ApiErrorCode.IMAGE_QC_CONFIG_VALUE_GT_CONFIDENCE_THRESHOLD);
        }
      }
    }
  }

  private void setImageConfig(AutoApprovalRules autoApprovalRules, List<AutoApprovalRuleDetailsDto> imageConfigList,
      boolean isNeedRevisionConfig) {
    if (isNeedRevisionConfig) {
      autoApprovalRules.setNeedRevisionConfig(gson.toJson(imageConfigList));
    } else {
      autoApprovalRules.setImageQcConfig(gson.toJson(imageConfigList));
    }
  }

  private void enableOrDisableRule(AutoApprovalRules autoApprovalRules, boolean isNeedRevisionConfig, boolean flag) {
    if (isNeedRevisionConfig) {
      autoApprovalRules.setNeedRevisionEnabled(flag);
    } else {
      autoApprovalRules.setMarkForDelete(!flag);
    }
  }


}