package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.ProductCreationRequest;

public interface TrackerService {

  void sendTracker(String event, String attrtype, String attrname, String attrvalue, String customfield);

  void trackProductCreationFailure(String requestId, ProductCreationRequest productRequest,
      String errorMessage);
}
