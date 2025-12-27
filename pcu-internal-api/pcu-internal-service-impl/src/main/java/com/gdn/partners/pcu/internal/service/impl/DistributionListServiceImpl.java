package com.gdn.partners.pcu.internal.service.impl;

import java.util.HashMap;
import java.util.Map;

import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.service.DistributionListService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;

@Service
public class DistributionListServiceImpl implements DistributionListService {

  @Autowired
  private PDTFeign pdtFeign;

  @Override
  public Page<DistributionProductWebResponse> getSummaryByMultipleFilter(int page, int size,
      DistributionFilterWebRequest distributionFilterWebRequest) {
    GdnRestListResponse<DistributionProductResponse> response = pdtFeign
        .getSummaryByMultipleFilter(page, size, distributionFilterWebRequest.getSortBy(),
            RequestHelper.toDistributionTaskMultipleFilterRequest(distributionFilterWebRequest));
    ResponseHelper.validateResponse(response);
    Map<String, ProfileResponse> profileResponseMap = new HashMap<>();
    return new PageImpl<>(ResponseHelper
        .fromDistributionProductResponseToDistributionProductWebResponse(response.getContent(), profileResponseMap),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void saveProductVendorMapping(ProductVendorWebRequest productVendorWebRequest) {
    GdnBaseRestResponse response =
        pdtFeign.saveProductDistributionTask(RequestHelper.toProductDistributionTaskRequest(productVendorWebRequest));
    ResponseHelper.validateResponse(response);
  }
}
