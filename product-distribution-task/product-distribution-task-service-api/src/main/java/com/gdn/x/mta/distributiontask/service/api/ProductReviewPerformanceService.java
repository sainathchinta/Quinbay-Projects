package com.gdn.x.mta.distributiontask.service.api;

import java.util.Date;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

public interface ProductReviewPerformanceService {

  void calculatePerformance(String productCode, String productName, String categoryCode, WorkflowState workflowState,
      Date startDate, Date endDate) throws Exception;

}
