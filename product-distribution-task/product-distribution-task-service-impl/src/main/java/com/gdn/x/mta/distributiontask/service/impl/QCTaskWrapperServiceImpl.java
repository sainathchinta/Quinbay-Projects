package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.QCTaskService;
import com.gdn.x.mta.distributiontask.service.api.QCTaskWrapperService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class QCTaskWrapperServiceImpl implements QCTaskWrapperService {

  @Autowired
  private QCTaskService qcTaskService;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Override
  public void qcRejectProduct(Product product, boolean isAssignedToVendor, String reason,
      WorkflowState state) throws Exception {
    this.qcTaskService.qcRejectProduct(product, isAssignedToVendor, reason, state);
    if (isAssignedToVendor) {
      this.solrVendorCollectionService
          .clearReviewerDetailsAndUpdateState(product.getProductCode(), state);
    } else {
      this.solrVendorCollectionService
          .updateStateOnSolr(product.getProductCode(), WorkflowState.UNASSIGNED);
    }
  }
}
