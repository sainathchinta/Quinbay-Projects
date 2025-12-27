package com.gdn.mta.bulk.repository.pcb;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;

@Repository
public class ProductAttributeRepositoryBean implements ProductAttributeRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public List<AttributeResponse> getAttributeDetailByAttributeCodes(String requestId, String username,
      List<String> attributeCodes) throws Exception {
    AttributeCodesRequest request = new AttributeCodesRequest();
    request.setAttributeCodes(attributeCodes);
    GdnRestListResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetailByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
            request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<AttributeResponse> getAttributeDetailByAttributeCodes(String requestId, String username,
      List<String> attributeCodes, boolean fetchOnlyBasicAttributeDetails) throws Exception {
    AttributeCodesRequest request = new AttributeCodesRequest();
    request.setAttributeCodes(attributeCodes);
    GdnRestListResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetailByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            fetchOnlyBasicAttributeDetails, request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, response.getErrorMessage());
    }
    return response.getContent();
  }
}
