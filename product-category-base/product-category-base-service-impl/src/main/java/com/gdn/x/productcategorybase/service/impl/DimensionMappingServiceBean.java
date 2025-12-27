package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.repository.DimensionMappingRepository;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.DimensionService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import com.gdn.x.productcategorybase.util.ValidationUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
public class DimensionMappingServiceBean implements DimensionMappingService {

  @Autowired
  private DimensionMappingRepository dimensionMappingRepository;

  @Autowired
  private DimensionService dimensionService;

  @Override
  public Page<DimensionMappingResponse> fetchDimensionMapping(String attributeCode, String storeId,
      Pageable pageable) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK);
    ValidationUtil.checkParameter(StringUtils.isNotEmpty(attributeCode),
        ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
    Page<DimensionMapping> response =
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId,
            attributeCode, pageable);
    ValidationUtil.checkParameter(Objects.nonNull(response),
        ErrorMessage.DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE_ERROR_CODE.getMessage(),
        ErrorMessage.DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE.getMessage());
    return ConverterUtil.toDimensionMappingResponse(response);
  }

  @Override
  @Transactional
  public void save(DimensionMapping dimensionMapping) {
    DimensionMapping existingDimensionMapping =
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
            dimensionMapping.getStoreId(), dimensionMapping.getAttributeCode(),
            dimensionMapping.getDimension());
    ValidationUtil.checkParameter(Objects.isNull(existingDimensionMapping),
        ErrorMessage.DIMENSION_MAPPING_ALREADY_EXIST_WITH_ATTRIBUTE_CODE);
    dimensionMappingRepository.save(dimensionMapping);
  }

  @Override
  @Transactional
  public void updateDimensionMapping(String storeId, Attribute attribute,
    DimensionMappingUpdateRequest dimensionMappingUpdateRequest) {
    List<DimensionMapping> resultList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty((dimensionMappingUpdateRequest.getAddedDimensionMapping()))) {
      createNewDimensionMappingFromRequest(storeId, attribute, resultList,
        dimensionMappingUpdateRequest.getAddedDimensionMapping());
    }
    if (CollectionUtils.isNotEmpty((dimensionMappingUpdateRequest.getUpdateDimensionMapping()))) {
      updateDimensionMappingFromRequest(storeId, false, attribute.getAttributeCode(),
        dimensionMappingUpdateRequest.getUpdateDimensionMapping(), resultList);
    }
    if (CollectionUtils.isNotEmpty((dimensionMappingUpdateRequest.getDeletedDimensionMapping()))) {
      updateDimensionMappingFromRequest(storeId, true, attribute.getAttributeCode(),
        dimensionMappingUpdateRequest.getDeletedDimensionMapping(), resultList);
    }
    dimensionMappingRepository.saveAll(resultList);
  }

  private void createNewDimensionMappingFromRequest(String storeId, Attribute attribute,
    List<DimensionMapping> resultSet, List<DimensionMappingRequest> dimensionMappingRequests) {
    for (DimensionMappingRequest request : dimensionMappingRequests) {
      Dimension dimension = dimensionService.findById(storeId, request.getDimensionId());
      ValidationUtil.checkParameter(Objects.nonNull(dimension),
        ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE);
      DimensionMapping existingDimensionMapping =
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
          storeId, attribute.getAttributeCode(), dimension);
      ValidationUtil.checkParameter(Objects.isNull(existingDimensionMapping),
        ErrorMessage.DIMENSION_MAPPING_ALREADY_EXIST_FOR_ATTRIBUTE_CODE);
      DimensionMapping dimensionMapping =
        ConverterUtil.getDimensionMapping(attribute.getId(), attribute, request);
      dimensionMapping.setDimension(dimension);
      resultSet.add(dimensionMapping);
    }
  }

  private void updateDimensionMappingFromRequest(String storeId, boolean markForDelete,
    String attributeCode, List<DimensionMappingRequest> dimensionMappingRequests,
    List<DimensionMapping> resultSet) {
    for (DimensionMappingRequest request : dimensionMappingRequests) {
      Dimension dimension = dimensionService.findById(storeId, request.getDimensionId());
      ValidationUtil.checkParameter(Objects.nonNull(dimension),
        ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE);
      DimensionMapping dimensionMapping =
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
          storeId, attributeCode, dimension);
      ValidationUtil.checkParameter(Objects.nonNull(dimensionMapping),
        ErrorMessage.DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE);
      dimensionMapping.setMarkForDelete(markForDelete);
      dimensionMapping.setMandatory(request.isMandatory());
      resultSet.add(dimensionMapping);
    }
  }

  @Override
  public List<DimensionMapping> fetchDimensionMappingForAttribute(String storeId,
    String attributeCode) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK);
    ValidationUtil.checkParameter(StringUtils.isNotEmpty(attributeCode),
      ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
    return new ArrayList<>(
      dimensionMappingRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId,
        attributeCode));
  }

}
