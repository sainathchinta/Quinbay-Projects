package com.gdn.x.mta.distributiontask.dao.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.x.mta.distributiontask.dao.api.ProductWorkflowRepository;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.model.Constants;

@Repository
public class ProductWorkflowRepositoryBean implements ProductWorkflowRepository {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public ProductWorkflowStatusResponse getWorkflowStatus(String productCode) throws Exception {
    GdnRestSingleResponse<ProductWorkflowStatusResponse> response =
        this.pbpFeign.getProductWorkflowStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productCode);
    if (!response.isSuccess()) {
      throw new IllegalStateException(response.getErrorMessage());
    }
    return response.getValue();
  }

}
