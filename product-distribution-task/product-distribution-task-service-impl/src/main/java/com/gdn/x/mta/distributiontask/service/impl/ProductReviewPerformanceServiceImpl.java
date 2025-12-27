package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductReviewPerformanceRepository;
import com.gdn.x.mta.distributiontask.model.ProductReviewPerformance;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewPerformanceService;

@Service
@Transactional(readOnly = true)
public class ProductReviewPerformanceServiceImpl implements ProductReviewPerformanceService {

  @Autowired
  private ProductReviewPerformanceRepository productReviewPerformanceRepository;

  private void generatePerformance(ProductReviewPerformance productReviewPerformance, WorkflowState workflowState,
      Date startDate, Date endDate) throws Exception {
    Long duration = (endDate.getTime() - startDate.getTime()) / (60 * 1000);
    if (WorkflowState.IN_REVIEW.equals(workflowState)) {
      Long distributionTime = productReviewPerformance.getDistributionTime() + duration;
      Integer distributionCounter = productReviewPerformance.getDistributionCounter() + 1;
      productReviewPerformance.setDistributionTime(distributionTime);
      productReviewPerformance.setDistributionCounter(distributionCounter);
    } else if (WorkflowState.PASSED.equals(workflowState)) {
      Long qcTime = productReviewPerformance.getQcTime() + duration;
      Integer qcCounter = productReviewPerformance.getQcCounter() + 1;
      productReviewPerformance.setQcTime(qcTime);
      productReviewPerformance.setQcCounter(qcCounter);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void calculatePerformance(String productCode, String productName, String categoryCode,
      WorkflowState workflowState, Date startDate, Date endDate) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductReviewPerformance productReviewPerformance =
        this.productReviewPerformanceRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productReviewPerformance == null) {
      productReviewPerformance = new ProductReviewPerformance();
      productReviewPerformance.setProductCode(productCode);
      productReviewPerformance.setProductName(productName);
      productReviewPerformance.setCategoryCode(categoryCode);
      generatePerformance(productReviewPerformance, workflowState, startDate, endDate);
    } else {
      generatePerformance(productReviewPerformance, workflowState, startDate, endDate);
    }
    this.productReviewPerformanceRepository.saveAndFlush(productReviewPerformance);
  }

}
