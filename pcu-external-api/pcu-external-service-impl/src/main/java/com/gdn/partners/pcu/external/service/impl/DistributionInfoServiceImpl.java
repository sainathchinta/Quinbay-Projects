package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.service.DistributionInfoService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DistributionInfoServiceImpl implements DistributionInfoService {

  private final PCBFeign pcbFeign;

  private final PBPFeign pbpFeign;

  @Override
  public Page<DistributionInfoPerSkuResponse> getDistributionInfo(String productCode,
      boolean needDistributionInfoResponse, int page, int size) {
    GdnRestListResponse<DistributionInfoPerSkuResponse> response =
        pcbFeign.getDistributionInfo(productCode, needDistributionInfoResponse, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateDistributionInfo(String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest) {
    GdnBaseRestResponse response =
        pbpFeign.updateDistributionInfo(productCode, distributionInfoUpdateRequest);
    ResponseHelper.validateResponse(response);
  }
}
