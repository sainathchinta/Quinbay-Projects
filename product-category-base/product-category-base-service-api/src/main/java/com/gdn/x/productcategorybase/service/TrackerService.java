package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public interface TrackerService {

  void trackProductCreationFailure(String requestId, ProductRequest productRequest, String errorMessage);
}
