package com.gdn.partners.pcu.internal.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.client.feign.ProductWorkflowFeign;
import com.gdn.partners.pcu.internal.service.ProductWorkflowService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;

/**
 * Created by govind on 11/01/2019 AD.
 */
@Service
public class ProductWorkflowServiceImpl implements ProductWorkflowService{

  @Autowired
  private ProductWorkflowFeign productWorkflowFeign;

  @Override
  public void returnForCorrection(
      ProductReturnForCorrectionRequest productReturnForCorrectionRequest) {
    GdnBaseRestResponse response =
        this.productWorkflowFeign.returnForCorrection(productReturnForCorrectionRequest);
    ResponseHelper.validateResponse(response);
  }
}
