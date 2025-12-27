package com.gdn.mta.product.repository;

import java.util.ArrayList;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;

@Repository
public class AttributeRepositoryBean implements AttributeRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public Attribute findOne(String id) throws Exception {
    Attribute attribute = null;
    AttributeResponse response = pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            id,false).getValue();
    if (response == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "[attributeId:" + id + " not found]");
    }
    attribute = new Attribute();
    if (response.getAllowedAttributeValues() == null) {
      response.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueResponse>());
    }
    if (response.getPredefinedAllowedAttributeValues() == null) {
      response.setPredefinedAllowedAttributeValues(new ArrayList<PredefinedAllowedAttributeValueResponse>());
    }
    BeanUtils.copyProperties(response, attribute, "allowedAttributeValues", "predefinedAllowedAttributeValues");
    for (AllowedAttributeValueResponse allowedAttributeValueResponse : response.getAllowedAttributeValues()) {
      AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
      BeanUtils.copyProperties(allowedAttributeValueResponse, allowedAttributeValue);
      attribute.getAllowedAttributeValues().add(allowedAttributeValue);
    }
    for (PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse : response
        .getPredefinedAllowedAttributeValues()) {
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
      BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValue);
      attribute.getPredefinedAllowedAttributeValues().add(predefinedAllowedAttributeValue);
    }
    return attribute;
  }

  @Override
  public AttributeResponse findDetailById(String attributeId) throws Exception {
    GdnRestSingleResponse<AttributeResponse> response =
        this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            attributeId, false);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    AttributeResponse attributeResponse = response.getValue();
    return attributeResponse;
  }

}
