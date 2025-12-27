package com.gdn.mta.bulk.repository;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

@Repository
public class AttributeRepositoryBean implements AttributeRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public AttributeResponse findOne(String storeId, String id) throws Exception {
    GdnRestSingleResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetailByAttributeId(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), id, false);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> findByStoreIdAndAttributeIdAndMarkForDeleteFalse(
      String requestId, GdnRestListRequest listRequest, String attributeId, String value) throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
            GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), 0, Integer.MAX_VALUE, attributeId, value);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public AttributeValueResponse addNewAttribute(String requestId, String value, String attributeCode) throws Exception {
    MasterAttributeAddRequest request = new MasterAttributeAddRequest();
    request.setValue(value);
    request.setSequence(0);
    GdnRestSingleResponse<AttributeValueResponse> response =
        pcbFeign.addMasterAttributeValue(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            attributeCode, request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
    return response.getValue();
  }
}
