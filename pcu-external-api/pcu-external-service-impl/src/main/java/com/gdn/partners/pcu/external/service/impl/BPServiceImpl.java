package com.gdn.partners.pcu.external.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.BPServiceFeign;
import com.gdn.partners.pcu.external.service.BPService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.x.bpservice.dto.request.BusinessPartnerHistoryRequest;

/**
 * Created by govind on 12/12/2018 AD.
 */
@Service
public class BPServiceImpl implements BPService{

  @Autowired
  private BPServiceFeign bpServiceFeign;

  @Override
  public void saveBusinessPartnerHistory(BusinessPartnerHistoryRequest request) {
    GdnBaseRestResponse restResponse = bpServiceFeign.saveBusinessPartnerHistory(request);
    ResponseHelper.validateResponse(restResponse);
  }

}
