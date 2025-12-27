package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.service.LookupService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.response.LookupWebResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;

@Service
public class LookupServiceImpl implements LookupService {

  private static final String LOOKUP_GROUP = "DANGEROUS_GOODS_LEVEL";

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public List<LookupWebResponse> getDangerousGoodsLevel() {
    GdnRestListResponse<LookupResponse> response = this.pcbFeign.getLookupByLookupGroup(LOOKUP_GROUP);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toLookupWebResponseList(response.getContent());
  }
}
