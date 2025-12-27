package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;

public interface ReportProductService {

  /**
   *
   * @param reportProductRequest
   */
  void addReportProduct(ReportProductRequest reportProductRequest);

}
