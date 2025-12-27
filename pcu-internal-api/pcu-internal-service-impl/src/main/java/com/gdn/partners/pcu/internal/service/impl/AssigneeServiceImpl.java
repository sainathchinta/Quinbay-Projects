package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.service.AssigneeService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class AssigneeServiceImpl implements AssigneeService {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public List<String> getAssigneesByFilterRequestAndActivatedAndViewableFlag(ReviewProductsFilterRequest request, boolean activated,
      boolean viewable) {
    SummaryFilterRequest summaryFilterRequest = RequestHelper.getSummaryFilterRequest(request);
    GdnRestListResponse<AssigneeResponse> response =
        pbpFeign.getAssigneesByFilterRequestAndActivatedAndViewableFlag(summaryFilterRequest, activated, viewable);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toAssigneeResponse(response.getContent());
  }
}
