package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.DimensionMappingWrapperService;
import com.gdn.x.productcategorybase.service.MasterAttributeService;
import com.gdn.x.productcategorybase.util.ValidationUtil;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.Set;

@Service
public class DimensionMappingWrapperServiceBean implements DimensionMappingWrapperService {

  @Autowired
  private MasterAttributeService masterAttributeService;

  @Autowired
  private DimensionMappingService dimensionMappingService;

  @Override
  public void updateDimensionMapping(String storeId, String attributeCode,
    DimensionMappingUpdateRequest dimensionMappingUpdateRequest) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(attributeCode),
      ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
    Set<String> uniqueDimensionIds = new HashSet<>();
    ValidationUtil.validateDimensionMappingUpdateRequest(
      dimensionMappingUpdateRequest.getAddedDimensionMapping(), uniqueDimensionIds);
    ValidationUtil.validateDimensionMappingUpdateRequest(
      dimensionMappingUpdateRequest.getUpdateDimensionMapping(), uniqueDimensionIds);
    ValidationUtil.validateDimensionMappingUpdateRequest(
      dimensionMappingUpdateRequest.getDeletedDimensionMapping(), uniqueDimensionIds);
    Attribute attribute = masterAttributeService.findDetailByAttributeCode(attributeCode);
    ValidationUtil.checkParameter(attribute.isSizeAttribute(),
      ErrorMessage.ATTRIBUTE_IS_NOT_SIZE_ATTRIBUTE);
    dimensionMappingService.updateDimensionMapping(storeId, attribute,
      dimensionMappingUpdateRequest);
  }
}
