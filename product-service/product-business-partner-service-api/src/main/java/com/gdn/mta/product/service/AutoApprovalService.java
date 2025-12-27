package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.enums.AutoApprovalType;

public interface AutoApprovalService {

  /**
   *
   * @param autoApprovalsDetailDto
   * @return
   * @throws Exception
   */
  AutoApprovalType verifyAutoApprovalRules(AutoApprovalsDetailDto autoApprovalsDetailDto) throws Exception;

  /**
   * verify if there is positive change in auto qc config
   * @param storeId
   * @param autoQcConfigChangeRequest
   * @return
   * @throws Exception
   */
  boolean verifyAutoApprovalRulesForConfigChange(String storeId,  AutoQcConfigChangeRequest autoQcConfigChangeRequest)
      throws Exception;


  /**
   * Get Auto approval rules
   *
   * @param storeId
   * @return
   */
  List<AutoApprovalRules> findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(String storeId);

  /**
   * Get All Auto approval rules
   *
   * @return
   */
  List<AutoApprovalRules> findAllAutoQcConfigRules();

  /**
   * @param storeId
   * @param ruleName
   * @return
   */
  AutoApprovalRules findByStoreIdAndRuleName(String storeId, String ruleName);

  /**
   *
   * @param autoApprovalRules
   */
  void save(AutoApprovalRules autoApprovalRules);

}
