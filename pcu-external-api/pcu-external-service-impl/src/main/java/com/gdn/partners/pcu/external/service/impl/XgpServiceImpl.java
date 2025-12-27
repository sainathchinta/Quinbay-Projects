package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.XgpFeign;
import com.gdn.partners.pcu.external.service.XgpService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class XgpServiceImpl implements XgpService {
  private static final String CLIENT_HOST = "pcu-external-api";

  @Autowired
  private XgpFeign xgpFeign;

  @Override
  public void scaleActiveProductNewImages(XgpImageScaleRequest xgpImageScaleRequest) {
    log.info("Scaling active product new images with request: {} ", xgpImageScaleRequest);
    GdnBaseRestResponse response =
        xgpFeign.scaleActiveProductNewImages(CLIENT_HOST, xgpImageScaleRequest);
    ResponseHelper.validateResponse(response);
  }
}
