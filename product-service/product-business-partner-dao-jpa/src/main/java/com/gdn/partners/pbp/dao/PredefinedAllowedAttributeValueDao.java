package com.gdn.partners.pbp.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

@Component
public class PredefinedAllowedAttributeValueDao implements IPredefinedAllowedAttributeValueDao {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(
      String requestId, GdnRestListRequest listRequest, String attributeId, String value) throws Exception {
    return pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), requestId, GdnMandatoryRequestParameterUtil.getUsername(),
        Long.valueOf(listRequest.getPage()).intValue(), Long.valueOf(listRequest.getSize()).intValue(), attributeId,
        value);
  }

  @Override
  public GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndMatchAttributeCodeAndValue(
      String requestId, String attributeCode, String value) throws Exception {
    return pcbFeign.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), requestId, GdnMandatoryRequestParameterUtil.getUsername(),
        attributeCode, value, false);
  }

}
