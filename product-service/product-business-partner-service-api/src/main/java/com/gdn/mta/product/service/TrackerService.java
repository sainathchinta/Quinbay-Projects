package com.gdn.mta.product.service;

import com.gda.mta.product.dto.ProductCreationRequest;

public interface TrackerService {

  void trackProductCreationFailure(
      String requestId, String flowType, ProductCreationRequest productRequest, String errorMessage);
}
