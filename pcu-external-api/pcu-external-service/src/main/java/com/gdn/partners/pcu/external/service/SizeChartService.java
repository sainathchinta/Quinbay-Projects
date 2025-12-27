package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.ProductSizeChartUpdateWebRequest;

public interface SizeChartService {

  /**
   * update product size chart code
   * @param sizeChartCode
   * @param productSizeChartUpdateWebRequest
   */
  void updateProductSizeChart(String sizeChartCode, ProductSizeChartUpdateWebRequest productSizeChartUpdateWebRequest);

}
