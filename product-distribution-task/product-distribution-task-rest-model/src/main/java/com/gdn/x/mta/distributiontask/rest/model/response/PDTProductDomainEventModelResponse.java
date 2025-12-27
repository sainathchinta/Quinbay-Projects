package com.gdn.x.mta.distributiontask.rest.model.response;

import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;

/**
 * Created by virajjasani on 02/11/16.
 */
public class PDTProductDomainEventModelResponse extends BaseResponse {

  private PDTProductDomainEventModel pdtProductDomainEventModel;

  public PDTProductDomainEventModelResponse() {
    // no implementation
  }

  public PDTProductDomainEventModelResponse(
      PDTProductDomainEventModel pdtProductDomainEventModel) {
    this.pdtProductDomainEventModel = pdtProductDomainEventModel;
  }

  public PDTProductDomainEventModel getPdtProductDomainEventModel() {
    return pdtProductDomainEventModel;
  }

  public void setPdtProductDomainEventModel(
      PDTProductDomainEventModel pdtProductDomainEventModel) {
    this.pdtProductDomainEventModel = pdtProductDomainEventModel;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("PDTProductDomainEventModelResponse{");
    sb.append("pdtProductDomainEventModel=").append(pdtProductDomainEventModel);
    sb.append('}');
    return sb.toString();
  }
}
