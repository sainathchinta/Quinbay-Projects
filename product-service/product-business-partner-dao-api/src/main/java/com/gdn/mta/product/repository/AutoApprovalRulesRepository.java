package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.AutoApprovalRules;

public interface AutoApprovalRulesRepository
    extends JpaRepository<AutoApprovalRules, String> {

  /**
   *
   * @param storeId
   * @return
   */
  List<AutoApprovalRules> findByStoreIdAndMarkForDeleteFalseOrderBySequenceNumber(String storeId);

  /**
   * @param ruleName
   * @return
   */
  AutoApprovalRules findByStoreIdAndRuleName(String storeId, String ruleName);

}
