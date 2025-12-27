package com.gdn.x.productcategorybase.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionFilterRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionResponse;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.repository.DimensionRepository;
import com.gdn.x.productcategorybase.service.DimensionService;
import com.gdn.x.productcategorybase.util.ValidationUtil;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
@Service
public class DimensionServiceBean implements DimensionService {

  @Autowired
  private DimensionRepository dimensionRepository;

  private static final String PREFIX_DIMENSION_CODE = "DIM";

  private static final String PADDING_STRING = "0";
  private static final int DIMENSION_CODE_SIZE = 6;

  @Override
  public DimensionResponse fetchDimensionDetails(String storeId, String dimensionCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(dimensionCode),
      ErrorMessage.DIMENSION_CODE_MUST_NOT_BE_BLANK.getMessage());
    Dimension dimension =
      dimensionRepository.findByStoreIdAndMarkForDeleteAndDimensionCode(storeId, false,
        dimensionCode);
    ValidationUtil.checkParameter(Objects.nonNull(dimension),
      ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE.getMessage());
    DimensionResponse dimensionResponse = new DimensionResponse();
    BeanUtils.copyProperties(dimension, dimensionResponse);
    return dimensionResponse;
  }

  @Override
  @Transactional
  public void save(String storeId, DimensionRequest request) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    Dimension existingDimension =
        dimensionRepository.findByStoreIdAndNameIgnoreCase(storeId, request.getName());
    ValidationUtil.checkParameter(Objects.isNull(existingDimension),
        ErrorMessage.DIMENSION_ALREADY_EXISTS_ERROR_CODE.getMessage(),
        ErrorMessage.DIMENSION_ALREADY_EXISTS_ERROR_MESSAGE.getMessage());
    Dimension dimension = convertRequestToDimension(request);
    dimension.setStoreId(storeId);
    if (Objects.nonNull(request.getDescription())) {
      dimension.setDescriptionSearch(new String(request.getDescription()));
    }
    dimensionRepository.save(dimension);
  }

  public Dimension convertRequestToDimension(DimensionRequest dimensionRequest) {
    return Dimension.builder().name(dimensionRequest.getName())
        .nameEnglish(dimensionRequest.getNameEnglish())
        .description(dimensionRequest.getDescription())
        .descriptionEnglish(dimensionRequest.getDescriptionEnglish())
        .example(dimensionRequest.getExample())
        .dsAttributeName(dimensionRequest.getDsAttributeName()).dimensionCode(
            new StringBuilder(PREFIX_DIMENSION_CODE).append(Constants.HYPHEN).append(
                StringUtils.leftPad(Long.toString(
                        this.dimensionRepository.getSequenceByAttributeCode(PREFIX_DIMENSION_CODE)),
                    DIMENSION_CODE_SIZE, PADDING_STRING)).toString())
        .dimensionType(AttributeType.DESCRIPTIVE_ATTRIBUTE).build();
  }

  @Override
  public Page<DimensionResponse> filter(String storeId, DimensionFilterRequest request,
      Pageable pageable) {
    Page<Dimension> dimensions =
        dimensionRepository.findByStoreIdAndKeywordAndMarkForDeleteFalseOrderByName(storeId,
            request.getKeyword(), request.getSortedBy(), request.getSortDirection(), pageable);
    List<DimensionResponse> dimensionReponseList =
        convertDimensionListToResponseList(dimensions.getContent());
    return new PageImpl<>(dimensionReponseList, pageable, dimensions.getTotalElements());
  }

  private List<DimensionResponse> convertDimensionListToResponseList(List<Dimension> dimensions) {
    return dimensions.stream()
        .map(this::convertDimensionToResponse)
        .collect(Collectors.toList());
  }

  private DimensionResponse convertDimensionToResponse(Dimension dimension) {
    DimensionResponse response =  DimensionResponse.builder()
        .name(dimension.getName())
        .dimensionCode(dimension.getDimensionCode())
        .description(dimension.getDescription())
        .build();
    response.setId(dimension.getId());
    return response;
  }

  @Override
  @Transactional
  public void edit(String storeId, DimensionRequest dimensionRequest) {
    Dimension existingDimension =
        dimensionRepository.findByStoreIdAndMarkForDeleteAndDimensionCode(storeId, Boolean.FALSE,
            dimensionRequest.getDimensionCode());
    ValidationUtil.checkParameter(Objects.nonNull(existingDimension),
        ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE_ERROR_CODE.getMessage(),
        ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE.getMessage());
    updateDimensionFromRequest(existingDimension, dimensionRequest);
    dimensionRepository.saveAndFlush(existingDimension);
  }

  private void updateDimensionFromRequest(Dimension existingDimension,
      DimensionRequest dimensionRequest) {
    existingDimension.setNameEnglish(dimensionRequest.getNameEnglish());
    existingDimension.setDescription(dimensionRequest.getDescription());
    existingDimension.setDescriptionEnglish(dimensionRequest.getDescriptionEnglish());
    existingDimension.setDescriptionSearch(new String(dimensionRequest.getDescription()));
    existingDimension.setExample(dimensionRequest.getExample());
    existingDimension.setDsAttributeName(dimensionRequest.getDsAttributeName());
  }

  @Override
  public Dimension findById(String storeId, String id) {
    return dimensionRepository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  @Override
  public DimensionResponse findByName(String storeId, String name) {
    Dimension dimension = dimensionRepository.findByStoreIdAndNameIgnoreCase(storeId, name);
    if(Objects.isNull(dimension)){
      return null;
    }
    return convertDimensionToResponse(dimension);
  }
}
