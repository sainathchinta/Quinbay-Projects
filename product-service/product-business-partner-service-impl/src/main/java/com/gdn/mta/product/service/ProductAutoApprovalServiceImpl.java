package com.gdn.mta.product.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;
import com.gdn.mta.product.repository.ProductAutoApprovalCriteriaRepository;

@Service
public class ProductAutoApprovalServiceImpl implements ProductAutoApprovalCriteriaService{

  @Autowired
  private ProductAutoApprovalCriteriaRepository productAutoApprovalCriteriaRepository;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveProductAutoApprovalCriteria(ProductAutoApprovalCriteria productAutoApprovalCriteria){
    this.productAutoApprovalCriteriaRepository.save(productAutoApprovalCriteria);
  }

  @Override
  public void deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(String storeId, String productCode){
    this.productAutoApprovalCriteriaRepository.deleteByStoreIdAndProductCode(storeId, productCode);
  }

}
