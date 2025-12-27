package com.gdn.partners.pcu.external.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.service.SizeChartService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.ProductSizeChartUpdateWebRequest;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SizeChartServiceImpl implements SizeChartService {

  @Autowired
  private XProductFeign xProductFeign;

  @Override
  public void updateProductSizeChart(String sizeChartCode,
      ProductSizeChartUpdateWebRequest productSizeChartUpdateWebRequest) {
    GdnBaseRestResponse response = xProductFeign.updateProductSizeChart(sizeChartCode,
        RequestHelper.toProductSizeChartUpdateRequest(productSizeChartUpdateWebRequest));
    ResponseHelper.validateResponse(response);
  }
}
