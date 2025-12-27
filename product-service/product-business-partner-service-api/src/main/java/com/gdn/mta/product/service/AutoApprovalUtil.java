package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.ChangedFieldDto;
import com.gda.mta.product.dto.ImageQcProcessingDto;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

public interface AutoApprovalUtil {

  /**
   *
   * @param autoApprovalRules
   * @return
   * @throws Exception
   */
  Map<String, AutoApprovalRulesDto> toAutoApprovalRulesDtoList(List<AutoApprovalRules> autoApprovalRules) throws Exception;

  /**
   *
   * @param autoApprovalType
   * @param autoApprovalRulesDto
   * @param appliedRuleConfig
   * @param autoApprovalsDetail
   * @param autoQCDetailResponse
   * @return
   * @throws JsonProcessingException
   */
  ProductAutoApprovalCriteria covertToProductAutoApprovalCriteria(AutoApprovalType autoApprovalType,
      AutoApprovalRulesDto autoApprovalRulesDto, String appliedRuleConfig,
      AutoApprovalsDetailDto autoApprovalsDetail, AutoQCDetailResponse autoQCDetailResponse) throws JsonProcessingException;

  /**
   *
   * @param autoApprovalsDetail
   * @param autoQCDetailResponse
   * @return
   * @throws JsonProcessingException
   */
  ProductHistory covertToProductHistoryRepository(AutoApprovalsDetailDto autoApprovalsDetail,
      AutoQCDetailResponse autoQCDetailResponse) throws JsonProcessingException;

  /**
   *
   * @param autoQCDetailResponse
   * @param autoApprovalRulesMap
   * @param autoApprovalsDetail
   * @return
   */
  String verifyAutoQcRules(AutoQCDetailResponse autoQCDetailResponse,
      Map<String, AutoApprovalRulesDto> autoApprovalRulesMap, AutoApprovalsDetailDto autoApprovalsDetail);

  /**
   *
   * @param imageQcData
   * @param imageQcConfig
   * @return
   */
  boolean verifyImageQcRules(List<ImageQcProcessingDto> imageQcData, List<AutoApprovalRuleDetailsDto> imageQcConfig);

  /**
   *
   * @param newAutoQCDetailResponse
   * @param changedFields
   * @return
   */
  AutoQCDetailResponse getOldAutoQcDetailResponse(AutoQCDetailResponse newAutoQCDetailResponse,
      Map<String, ChangedFieldDto> changedFields);
}
