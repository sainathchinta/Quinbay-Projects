package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.service.PBPOutboundService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PBPOutboundServiceImpl implements PBPOutboundService {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public String getProductSkuByProductCode(String productCode) {
    GdnRestSimpleResponse<ProductSkuResponseList> response = pbpFeign.getProductSkuByProductCode(productCode);
    ResponseHelper.validateResponse(response);
    return Optional.ofNullable(response.getValue().getProductSkuList()).orElse(new ArrayList<>()).stream().findFirst()
        .orElse(StringUtils.EMPTY);
  }
}
