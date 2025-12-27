package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.DimensionService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.util.ValidationUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeFilter;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeTypeDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.MasterAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryAttributeRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.MasterAttributeService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

@Service
@Transactional(readOnly = true)
public class MasterAttributeServiceBean implements MasterAttributeService {

  private static final String NOT_FOUND_PREDFINED_ATTRIBUTE_VALUE_WITH_ATTRIBUTE_CODE =
      "not found predefined attribute value with attribute code: {} ";
  private static final String NOT_FOUND_ALLOWED_ATTRIBUTE_VALUE_WITH_ATTRIBUTE_CODE =
      "not found allowed attribute value with attribute code: {} ";
  private static final Logger LOG = LoggerFactory.getLogger(MasterAttributeServiceBean.class);
  private static final String NOT_FOUND_ATTRIBUTE_WITH_ATTRIBUTE_CODE = "Not found attribute with attribute code ";
  private static final String EXISTING_ALLOWED_ATTRIBUTE_VALUE = "Attribute value already exist";
  protected static final String INVALID_TYPE = "PREDEFINED Attribute must be in Basic "
      + "View";
  private static final String ATTRIBUTE_VALUE_NOT_FOUND_WITH_ID =
      "Attribute Value not found with id : ";
  private static final String ATTRIBUTE_VALUE_ALREADY_EXISTS_WITH_VALUE =
      "Attribute Value already exists with value(s) : ";
  protected static final String INVALID_ATT_ID = "Attribute with the given attribute ID not found ";

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private AllowedAttributeValueService allowedAttributeValueService;

  @Autowired
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private CategoryAttributeRepository categoryAttributeRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherServiceBean;

  @Autowired
  private DimensionService dimensionService;

  @Autowired
  private DimensionMappingService dimensionMappingService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Override
  public Attribute findDetailByAttributeCode(String attributeCode) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(attributeCode),
      ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK.getMessage());
    Attribute attribute =
        this.attributeRepository.findByStoreIdAndAttributeCode(GdnMandatoryParameterUtil.getStoreId(), attributeCode);
    GdnPreconditions.checkArgument(Objects.nonNull(attribute),
        MasterAttributeServiceBean.NOT_FOUND_ATTRIBUTE_WITH_ATTRIBUTE_CODE + attributeCode);
    return attribute;
  }


  @Override
  public Page<AttributeValueDTO> getAttributeValuesByAttributeCode(String storeId, String attributeCode,
      Pageable pageable, Boolean getAllValues, boolean concatenateValueWithValueType) throws Exception {
    AttributeTypeDTO attributeTypeDTO = attributeRepository.getAttributeTypeInfoByAttributeCode(storeId, attributeCode);
    Page<AttributeValueDTO> attributeValueDTOPage = null;
    if (Objects.nonNull(attributeTypeDTO)) {
      if (attributeTypeDTO.getAttributeType().equals(AttributeType.PREDEFINED_ATTRIBUTE)
          || attributeTypeDTO.getAttributeType().equals(AttributeType.PREDEFINED_MULTIVALUE)) {
        Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues;
        if (getAllValues) {
          predefinedAllowedAttributeValues = new PageImpl<>(getPredefinedAllowedAttributeValueFromAttribute(storeId, attributeTypeDTO));
        } else {
          predefinedAllowedAttributeValues = this.predefinedAllowedAttributeValueService
              .findByStoreIdAndAttributeId(storeId, attributeTypeDTO.getAttributeId(), pageable,
                  attributeTypeDTO.getAttributeSortType());
        }
        attributeValueDTOPage =
            this.getAttributeValueDTOListForPredefinedAttribute(predefinedAllowedAttributeValues, attributeCode,
                pageable, attributeTypeDTO.isMandatory());
      } else if (attributeTypeDTO.getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE)) {
        Page<AllowedAttributeValue> allowedAttributeValues;
        if (getAllValues) {
          allowedAttributeValues = new PageImpl<>(getAllowedAttributeValueFromAttribute(storeId, attributeTypeDTO));
        } else {
          allowedAttributeValues = this.allowedAttributeValueService
              .findByStoreIdAndAttributeId(storeId, attributeTypeDTO.getAttributeId(), pageable,
                  attributeTypeDTO.getAttributeSortType());
        }
        attributeValueDTOPage =
            this.getAttributeValueDTOListForDefineningAttribute(allowedAttributeValues, attributeCode, pageable,
                concatenateValueWithValueType);
      }
    }
    return attributeValueDTOPage;
  }

  private List<AllowedAttributeValue> getAllowedAttributeValueFromAttribute(String storeId,
      AttributeTypeDTO attributeTypeDTO) {
    Attribute requiredAttribute =
        this.attributeService.findDetailByStoreIdAndId(storeId, attributeTypeDTO.getAttributeId(), false);
    if (AttributeSortType.MANUAL.equals(attributeTypeDTO.getAttributeSortType()) || AttributeSortType.CUSTOM
        .equals(attributeTypeDTO.getAttributeSortType())) {
      requiredAttribute.getAllowedAttributeValues().sort(Comparator.comparing(AllowedAttributeValue::getSequence));
    } else if (attributeTypeDTO.getAttributeSortType().equals(AttributeSortType.ALPHABETICAL)) {
      requiredAttribute.getAllowedAttributeValues().sort(Comparator.comparing(AllowedAttributeValue::getValue));
    }
    return requiredAttribute.getAllowedAttributeValues().stream()
        .filter(predefinedAllowedAttributeValue -> !predefinedAllowedAttributeValue.isMarkForDelete())
        .collect(Collectors.toList());
  }

  private List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValueFromAttribute(String storeId,
      AttributeTypeDTO attributeTypeDTO) {
    Attribute requiredAttribute =
        this.attributeService.findDetailByStoreIdAndId(storeId, attributeTypeDTO.getAttributeId(), false);
    if (attributeTypeDTO.getAttributeSortType().equals(AttributeSortType.MANUAL) || attributeTypeDTO
        .getAttributeSortType().equals(AttributeSortType.CUSTOM)) {
      requiredAttribute.getPredefinedAllowedAttributeValues().sort(
          Comparator.comparing(PredefinedAllowedAttributeValue::getSequence));
    } else if (attributeTypeDTO.getAttributeSortType().equals(AttributeSortType.ALPHABETICAL)) {
      requiredAttribute.getPredefinedAllowedAttributeValues().sort(
          Comparator.comparing(PredefinedAllowedAttributeValue::getValue));
    }
    return requiredAttribute.getPredefinedAllowedAttributeValues().stream()
        .filter(predefinedAllowedAttributeValue -> !predefinedAllowedAttributeValue.isMarkForDelete())
        .collect(Collectors.toList());
  }

  @Override
  @Transactional(readOnly = false)
  public String insertMasterAttribute(Attribute attribute,
      List<DimensionMappingRequest> dimensionMappingRequest, List<String> valueTypeList)
      throws Exception {
    if(attribute.isDsExtraction()){
      Attribute existingAttribute =
          attributeRepository.findByStoreIdAndDsAttributeNameAndMarkForDeleteFalse(
              attribute.getStoreId(), attribute.getDsAttributeName());
      ValidationUtil.checkParameter(Objects.isNull(existingAttribute),
          ErrorMessage.DUPLICATE_DS_ATTRIBUTE_NAME_ERROR_CODE.getMessage(),
          String.format(ErrorMessage.DUPLICATE_DS_ATTRIBUTE_NAME.getMessage(),
              attribute.getDsAttributeName()));
    }
    if (Objects.nonNull(attribute.getDescription())) {
      attribute.setDescriptionSearch(new String(attribute.getDescription()));
    }
    validateValueTypes(attribute, valueTypeList);
    String attributeId = attributeService.save(attribute);
    if (attribute.isSizeAttribute()) {
      ValidationUtil.checkParameter(validateDimensionMapping(dimensionMappingRequest),
          ErrorMessage.DIMENSION_MAPPING_CANNOT_BE_EMPTY);
      saveDimensionMapping(attributeId, attribute, dimensionMappingRequest);
    }
    return attributeId;
  }

  private void validateValueTypes(Attribute attribute, List<String> valueTypeList)
      throws Exception {
    if (attribute.isValueTypeAttribute()) {
      ValidationUtil.checkParameter(CollectionUtils.isNotEmpty(valueTypeList),
          ErrorMessage.VALUE_TYPES_MUST_NOT_BE_EMPTY);
      SystemParameter systemParameter =
          systemParameterService.findByStoreIdAndVariable(attribute.getStoreId(),
              Constants.VALUE_TYPES_CONFIG);
      String systemParameterValueTypes = systemParameter.getValue();
      ValidationUtil.checkParameter(StringUtils.isNotBlank(systemParameterValueTypes),
          ErrorMessage.VALUE_TYPE_CONFIG_IS_EMPTY);
      Set<String> valueTypeConfigSet =
          Arrays.stream(systemParameterValueTypes.split(Constants.COMMA))
              .collect(Collectors.toSet());
      ValidationUtil.checkParameter(valueTypeConfigSet.containsAll(new HashSet<>(valueTypeList)),
          ErrorMessage.VALUE_TYPE_PROVIDED_IS_WRONG);
      String valueTypeString = objectMapper.writeValueAsString(valueTypeList);
      attribute.setValueTypes(valueTypeString);
    }
  }

  private void saveDimensionMapping(String attributeId, Attribute attribute,
      List<DimensionMappingRequest> dimensionMappingRequests) {
      for (DimensionMappingRequest request : dimensionMappingRequests) {
        DimensionMapping dimensionMapping =
            ConverterUtil.getDimensionMapping(attributeId, attribute, request);
        Dimension dimension =
            dimensionService.findById(attribute.getStoreId(), request.getDimensionId());
        ValidationUtil.checkParameter(Objects.nonNull(dimension),
            ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE);
        dimensionMapping.setDimension(dimension);
        dimensionMappingService.save(dimensionMapping);
      }
  }

  private boolean validateDimensionMapping(List<DimensionMappingRequest> dimensionMappingRequests) {
    return CollectionUtils.isNotEmpty(dimensionMappingRequests) && StringUtils.isNotBlank(
        dimensionMappingRequests.stream().findFirst().map(DimensionMappingRequest::getDimensionId)
            .orElse(StringUtils.EMPTY));
  }

  @Override
  @Transactional(readOnly = false)
  public Attribute updateMasterAttribute(Attribute attribute, List<String> updatedValueTypes)
      throws Exception {
    validateMasterAttributeData(attribute);
    if(Objects.nonNull(attribute.getDescription())) {
      attribute.setDescriptionSearch(new String(attribute.getDescription()));
    }
    Attribute savedAttribute =
        this.attributeRepository.findByStoreIdAndIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
    List<String> updatedFields = new ArrayList<>();
    if (Objects.nonNull(savedAttribute)) {
      if (savedAttribute.isDsExtraction() != attribute.isDsExtraction()) {
        updatedFields.add(Constants.DS_EXTRACTION);
      }
      BeanUtils.copyProperties(attribute, savedAttribute, "name", "attributeType", "sortType",
          "predefinedAllowedAttributeValues", "allowedAttributeValues", "createdDate", "variantCreation",
          "screeningMandatory", "variantCreatingUI", "sizeAttribute", "valueTypes", "attributeImageUrl");
      if(attribute.isSizeAttribute() || savedAttribute.isSizeAttribute()) {
        updateSizeAttribute(attribute, savedAttribute, updatedValueTypes);
      }
      savedAttribute = this.attributeRepository.saveAndFlush(savedAttribute);
      List<String> categoryList = categoryAttributeRepository
          .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(attribute.getStoreId(), attribute.getId());
      if (CollectionUtils.isNotEmpty(categoryList)) {
        applicationCacheServiceBean.clearCategoryDetails(attribute.getStoreId(), categoryList);
      }
      attributeService.evictAttributeCache(attribute.getStoreId(), attribute.getId(), attribute.getAttributeCode());
    } else {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, INVALID_ATT_ID);
    }
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(savedAttribute);
    attributeDomainEventModel.setNewAttribute(false);
    attributeDomainEventModel.setValueUpdate(false);
    attributeDomainEventModel.setUpdatedFields(updatedFields);
    domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(attributeDomainEventModel);
    return savedAttribute;
  }

  private void updateSizeAttribute(Attribute updatedAttribute, Attribute existingAttribute,
      List<String> updatedValueTypes) throws Exception {
    updatedValueTypes = new ArrayList<>(new HashSet<>(updatedValueTypes));
    if (!existingAttribute.isSizeAttribute()) {
      //to convert an attribute to size attribute if it wasn't initially
      existingAttribute.setSizeAttribute(updatedAttribute.isSizeAttribute());
      existingAttribute.setValueTypeAttribute(updatedAttribute.isSizeAttribute());
    } else {
      //to avoid turning off the size attribute once it's turned on
      ValidationUtil.checkParameter(updatedAttribute.isSizeAttribute(),
          ErrorMessage.SIZE_ATTRIBUTE_TYPE_CANNOT_BE_CHANGED);
    }
    existingAttribute.setAttributeImageUrl(updatedAttribute.getAttributeImageUrl());
    existingAttribute.setValueTypeAttribute(true);
    List<String> existingValueTypes = new ArrayList<>();
    if (StringUtils.isNotBlank(existingAttribute.getValueTypes())) {
      existingValueTypes = objectMapper.readValue(existingAttribute.getValueTypes(),
          new TypeReference<List<String>>() {});
    }
    //to check if updated valueTypes has all existing valueTypes to avoid unmapping
    ValidationUtil.checkParameter(new HashSet<>(updatedValueTypes).containsAll(existingValueTypes),
        ErrorMessage.VALUE_TYPES_CANNOT_BE_UNMAPPED);
    validateValueTypes(existingAttribute, updatedValueTypes);
  }

  private void validateMasterAttributeData(Attribute attribute) throws ApplicationException {
    GdnPreconditions.checkArgument((StringUtils.isNotEmpty(attribute.getUpdatedBy())),
        ErrorCategory.VALIDATION.getMessage());
  }

  private Page<AttributeValueDTO> getAttributeValueDTOListForPredefinedAttribute(
      Page<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues, String attributeCode, Pageable pageable, boolean mandatory) {
    List<AttributeValueDTO> attributeValueDTOS = new ArrayList<>();
    if (Objects.nonNull(predefinedAllowedAttributeValues)) {
      for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : predefinedAllowedAttributeValues) {
        if (mandatory && Objects.nonNull(predefinedAllowedAttributeValue) && StringUtils
            .isNotBlank(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode())
            && predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode().contains(Constants.DEFAULT)) {
          continue;
        }
        AttributeValueDTO attributeValueDTO = new AttributeValueDTO();
        BeanUtils.copyProperties(predefinedAllowedAttributeValue, attributeValueDTO);
        attributeValueDTOS.add(attributeValueDTO);
      }
    } else {
      LOG.info(NOT_FOUND_PREDFINED_ATTRIBUTE_VALUE_WITH_ATTRIBUTE_CODE, attributeCode);
    }
    return new PageImpl<AttributeValueDTO>(attributeValueDTOS, pageable,
        predefinedAllowedAttributeValues.getTotalElements());
  }

  private Page<AttributeValueDTO> getAttributeValueDTOListForDefineningAttribute(
      Page<AllowedAttributeValue> allowedAttributeValues, String attributeCode, Pageable pageable,
      boolean concatenateValueWithValueType) {
    List<AttributeValueDTO> attributeValueDTOS = new ArrayList<>();
    if (Objects.nonNull(allowedAttributeValues)) {
      for (AllowedAttributeValue allowedAttributeValue : allowedAttributeValues) {
        AttributeValueDTO attributeValueDTO = new AttributeValueDTO();
        BeanUtils.copyProperties(allowedAttributeValue, attributeValueDTO);
        concatenateAttributeValueWithValueType(concatenateValueWithValueType, attributeValueDTO);

        attributeValueDTOS.add(attributeValueDTO);
      }
    } else {
      LOG.info(NOT_FOUND_ALLOWED_ATTRIBUTE_VALUE_WITH_ATTRIBUTE_CODE, attributeCode);
    }
    return new PageImpl<AttributeValueDTO>(attributeValueDTOS, pageable, allowedAttributeValues.getTotalElements());
  }

  private void concatenateAttributeValueWithValueType(boolean concatenateValueWithValueType,
      AttributeValueDTO attributeValueDTO) {
    if (CommonUtil.isAttributeEligibleForConcatenation(valueTypeAdditionForDefiningAttributes,
        concatenateValueWithValueType, attributeValueDTO)) {
      attributeValueDTO.setValue(
          attributeValueDTO.getValueType().concat(sizeChartValueTypeDelimiter).concat(attributeValueDTO.getValue()));
    }
  }

  @Transactional(readOnly = false)
  @Override
  public void updateAttributeValues(String storeId, String attributeCode,
      MasterAttributeUpdateDTO masterAttributeUpdateDTO) throws Exception{
    Attribute attribute = findDetailByAttributeCode(attributeCode);
    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      Hibernate.initialize(attribute.getAllowedAttributeValues());
      updateAllowedAttributeValues(storeId, attribute, masterAttributeUpdateDTO);
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())
        || AttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType())) {
      Hibernate.initialize(attribute.getPredefinedAllowedAttributeValues());
      updatePredefinedAllowedAttributeValues(storeId, attribute, masterAttributeUpdateDTO);
    }
    attribute.setSortType(masterAttributeUpdateDTO.getSortType());
    attribute.setUpdatedBy(masterAttributeUpdateDTO.getUpdatedBy());
    attribute.setUpdatedDate(masterAttributeUpdateDTO.getUpdatedDate());
    attributeService.update(attribute);
    attributeService.evictAttributeCache(storeId, attribute.getId(), attribute.getAttributeCode());
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    attributeDomainEventModel.setNewAttribute(false);
    attributeDomainEventModel.setValueUpdate(true);
    domainEventPublisherServiceBean.publishMasterAttributeInfoEvent(attributeDomainEventModel);
  }

  @Transactional(readOnly = false)
  @Override
  public AttributeValueResponse addAttributeValue(String storeId, String attributeCode,
      AttributeValueUpdateDTO attributeValueUpdateDTO, String createdBy, Date createdDate) throws Exception {
    Attribute attribute = findDetailByAttributeCode(attributeCode);
    AttributeValueResponse attributeValueResponse = null;
    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      AllowedAttributeValue existingAllowedAttributeValue = allowedAttributeValueService
          .findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(storeId, attribute,
              attributeValueUpdateDTO.getValue());
      GdnPreconditions.checkArgument(Objects.isNull(existingAllowedAttributeValue),
          MasterAttributeServiceBean.EXISTING_ALLOWED_ATTRIBUTE_VALUE + attributeValueUpdateDTO.getValue());
      AllowedAttributeValue allowedAttributeValue =
          getInActiveDefiningAttributeValueIfExists(storeId, attribute, attributeValueUpdateDTO);
      if (Objects.isNull(allowedAttributeValue)) {
        allowedAttributeValue =
            addAllowedAttributeValue(storeId, attribute, attributeValueUpdateDTO, createdBy, createdDate);
      }
      attributeValueResponse = convertToAttributeValueResponse(allowedAttributeValue);
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())) {
      PredefinedAllowedAttributeValue existingPredefinedAllowedAttributeValue = predefinedAllowedAttributeValueService.
          findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(storeId, attribute,
              attributeValueUpdateDTO.getValue());
      GdnPreconditions.checkArgument(Objects.isNull(existingPredefinedAllowedAttributeValue),
          MasterAttributeServiceBean.EXISTING_ALLOWED_ATTRIBUTE_VALUE + attributeValueUpdateDTO.getValue());
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
          getInActivePredefiningAttributeValueIfExists(storeId, attribute, attributeValueUpdateDTO);
      if (Objects.isNull(predefinedAllowedAttributeValue)) {
        predefinedAllowedAttributeValue =
            addPredefinedAllowedAttributeValue(storeId, attribute, attributeValueUpdateDTO, createdBy, createdDate);
      }
      attributeValueResponse = convertToAttributeValueResponse(predefinedAllowedAttributeValue);
    }
    attribute.setUpdatedBy(createdBy);
    attribute.setUpdatedDate(createdDate);
    attributeService.update(attribute);
    attributeService.evictAttributeCache(storeId, attribute.getId(), attribute.getAttributeCode());
    return attributeValueResponse;
  }

  /**
   * Check for inactive defining attribute value i.e MFD = true
   *
   * @param storeId
   * @param attribute
   * @param attributeValueUpdateDTO
   * @return
   * @throws Exception
   */
  private AllowedAttributeValue getInActiveDefiningAttributeValueIfExists(String storeId, Attribute attribute,
      AttributeValueUpdateDTO attributeValueUpdateDTO) throws Exception {
    AllowedAttributeValue existingAllowedAttributeValue = allowedAttributeValueService
        .findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(storeId, attribute, attributeValueUpdateDTO.getValue());
    if (Objects.isNull(existingAllowedAttributeValue)) {
      return null;
    }
    existingAllowedAttributeValue.setMarkForDelete(false);
    existingAllowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
    allowedAttributeValueService.update(existingAllowedAttributeValue);
    return existingAllowedAttributeValue;
  }

  /**
   * Check for inactive predefined attribute value i.e MFD = true
   *
   * @param storeId
   * @param attribute
   * @param attributeValueUpdateDTO
   * @return
   * @throws Exception
   */
  private PredefinedAllowedAttributeValue getInActivePredefiningAttributeValueIfExists(String storeId,
      Attribute attribute, AttributeValueUpdateDTO attributeValueUpdateDTO) throws Exception {
    PredefinedAllowedAttributeValue existingPredefinedAllowedAttributeValue = predefinedAllowedAttributeValueService.
        findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(storeId, attribute, attributeValueUpdateDTO.getValue());
    if (Objects.isNull(existingPredefinedAllowedAttributeValue)) {
      return null;
    }
    existingPredefinedAllowedAttributeValue.setMarkForDelete(false);
    existingPredefinedAllowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
    predefinedAllowedAttributeValueService.update(existingPredefinedAllowedAttributeValue);
    return existingPredefinedAllowedAttributeValue;
  }

  /**
   * Converts AllowedAttributeValue to AttributeValueResponse
   * @param allowedAttributeValue
   * @return AttributeValueResponse
   */
  private AttributeValueResponse convertToAttributeValueResponse(
      AllowedAttributeValue allowedAttributeValue) {
    AttributeValueResponse attributeValueResponse = new AttributeValueResponse();
    attributeValueResponse.setId(allowedAttributeValue.getId());
    attributeValueResponse.setValue(allowedAttributeValue.getValue());
    attributeValueResponse
        .setAllowedAttributeCode(allowedAttributeValue.getAllowedAttributeCode());
    return attributeValueResponse;
  }

  /**
   * Converts PredefinedAllowedAttributeValue to AttributeValueResponse
   * @param predefinedAllowedAttributeValue
   * @return AttributeValueResponse
   */
  private AttributeValueResponse convertToAttributeValueResponse(
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue) {
    AttributeValueResponse attributeValueResponse = new AttributeValueResponse();
    attributeValueResponse.setId(predefinedAllowedAttributeValue.getId());
    attributeValueResponse.setValue(predefinedAllowedAttributeValue.getValue());
    attributeValueResponse.setValueEn(predefinedAllowedAttributeValue.getValueEn());
    attributeValueResponse
        .setPredefinedAllowedAttributeCode(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode());
    return attributeValueResponse;
  }

  /**
   * Update AllowedAttributeValues
   * @param storeId
   * @param attribute
   * @param masterAttributeUpdateDTO
   * @throws Exception
   */
  private void updateAllowedAttributeValues(String storeId, Attribute attribute,
      MasterAttributeUpdateDTO masterAttributeUpdateDTO) throws Exception {
    Map<String, AllowedAttributeValue> existingAllowedAttributeValues = attribute.getAllowedAttributeValues().stream()
        .collect(Collectors.toMap(value -> value.getId(), value -> value));
    Map<String, List<String>> valueToValueTypeMap = attribute.getAllowedAttributeValues().stream()
        .collect(Collectors.groupingBy(AllowedAttributeValue::getValue, Collectors.mapping(
            allowedValue -> Optional.ofNullable(allowedValue.getValueType())
                .orElseGet(() -> StringUtils.EMPTY),
            Collectors.filtering(valueType -> !valueType.isEmpty(), Collectors.toList()))));
    Map<String, AllowedAttributeValue> updatedAllowedAttributeValues = new HashMap<>();
    for (AttributeValueUpdateDTO attributeValueUpdateDTO :
        masterAttributeUpdateDTO.getAttributeValues()) {
      updateAllowedAttributeAccordingToSort(masterAttributeUpdateDTO.getSortType(),
          attributeValueUpdateDTO, attribute, valueToValueTypeMap, masterAttributeUpdateDTO.getUpdatedBy(),
          masterAttributeUpdateDTO.getUpdatedDate(), existingAllowedAttributeValues,
          updatedAllowedAttributeValues);
    }
    Map<String, AllowedAttributeValue> existingInActiveAttributeValueMap = new HashMap<>();
    validateAddAttributeValuesAndUpdateInActiveValuesMap(storeId, attribute, masterAttributeUpdateDTO,
        existingInActiveAttributeValueMap, updatedAllowedAttributeValues);
    for (AttributeValueUpdateDTO attributeValueUpdateDTO : masterAttributeUpdateDTO.getAddedAttributeValues()) {
      if (!(attribute.isSizeAttribute() || attribute.isValueTypeAttribute())) {
        attributeValueUpdateDTO.setValueType(null);
      }
      AllowedAttributeValue existingInActiveAttributeValue =
          existingInActiveAttributeValueMap.get(attributeValueUpdateDTO.getValue());
      if (Objects.isNull(existingInActiveAttributeValue)) {
        addAllowedAttributeValue(storeId, attribute, attributeValueUpdateDTO,
            masterAttributeUpdateDTO.getUpdatedBy(), masterAttributeUpdateDTO.getUpdatedDate());
      }
    }
    for (AttributeValueUpdateDTO attributeValueUpdateDTO : masterAttributeUpdateDTO.getDeletedAttributeValues()) {
      deleteAllowedAttributeValue(attributeValueUpdateDTO, masterAttributeUpdateDTO.getUpdatedBy(),
          masterAttributeUpdateDTO.getUpdatedDate(), existingAllowedAttributeValues, updatedAllowedAttributeValues);
    }
    attribute.getAllowedAttributeValues()
        .removeIf(attributeValue -> Objects.isNull(updatedAllowedAttributeValues.get(attributeValue.getId())));
  }

  /**
   * update existing value and value type for allowed attribute values
   * @param sortType
   * @param attributeValueUpdateDTO
   * @param attribute
   * @param valueToValueTypeMap
   * @param updatedBy
   * @param UpdatedDate
   * @param existingAllowedAttributeValues
   * @param updatedAllowedAttributeValues
   * @throws Exception
   */

  private void updateAllowedAttributeAccordingToSort(AttributeSortType sortType,
      AttributeValueUpdateDTO attributeValueUpdateDTO, Attribute attribute,
      Map<String, List<String>> valueToValueTypeMap, String updatedBy, Date UpdatedDate,
      Map<String, AllowedAttributeValue> existingAllowedAttributeValues,
      Map<String, AllowedAttributeValue> updatedAllowedAttributeValues) throws Exception {
    AllowedAttributeValue allowedAttributeValue = existingAllowedAttributeValues.get(attributeValueUpdateDTO.getId());
    if (Objects.isNull(allowedAttributeValue)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          ATTRIBUTE_VALUE_NOT_FOUND_WITH_ID + attributeValueUpdateDTO.getId());
    }
    if (attribute.isSizeAttribute() || attribute.isValueTypeAttribute()) {
      if (StringUtils.isBlank(allowedAttributeValue.getValueType())) {
        List<String> valueType = valueToValueTypeMap.get(attributeValueUpdateDTO.getValue());
        ValidationUtil.checkParameter(!valueType.contains(attributeValueUpdateDTO.getValueType()),
            ErrorMessage.VALUE_WITH_VALUE_TYPE_ALREADY_EXISTS_ERROR_CODE.getMessage(),
            ErrorMessage.VALUE_WITH_VALUE_TYPE_ALREADY_EXISTS.getMessage());
        allowedAttributeValue.setValueType(attributeValueUpdateDTO.getValueType());
      }
    } else {
      allowedAttributeValue.setValueType(null);
    }
    if (AttributeSortType.MANUAL.equals(sortType) || AttributeSortType.CUSTOM.equals(sortType)) {
      allowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
    }
    allowedAttributeValue.setUpdatedBy(updatedBy);
    allowedAttributeValue.setUpdatedDate(UpdatedDate);
    updatedAllowedAttributeValues.put(allowedAttributeValue.getId(), allowedAttributeValue);
  }

  /**
   *
   * @param storeId
   * @param attribute
   * @param masterAttributeUpdateDTO
   * @param existingInActiveAttributeValueMap
   */
  private void validateAddAttributeValuesAndUpdateInActiveValuesMap(String storeId, Attribute attribute,
      MasterAttributeUpdateDTO masterAttributeUpdateDTO,
      Map<String, AllowedAttributeValue> existingInActiveAttributeValueMap,
      Map<String, AllowedAttributeValue> updatedAllowedAttributeValues) {
    List<String> duplicateAttributeValues = new ArrayList<>();
    List<AttributeValueUpdateDTO> addedAttributeValues =
        new ArrayList<>(new HashSet<>(masterAttributeUpdateDTO.getAddedAttributeValues()));
    for (AttributeValueUpdateDTO attributeValueUpdateDTO : addedAttributeValues) {
      List<AllowedAttributeValue> existingAllowedAttributeValues =
          attribute.getAllowedAttributeValues().stream().filter(
                  attributeValue -> attributeValue.getValue().equals(attributeValueUpdateDTO.getValue())
                      && ((StringUtils.isBlank(attributeValue.getValueType()) && StringUtils.isBlank(
                      attributeValueUpdateDTO.getValueType())) || Objects.equals(
                      attributeValue.getValueType(), attributeValueUpdateDTO.getValueType())))
              .collect(Collectors.toList());
      for (AllowedAttributeValue allowedAttributeValue : existingAllowedAttributeValues) {
        if (allowedAttributeValue.isMarkForDelete()) {
          allowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
          allowedAttributeValue.setUpdatedDate(masterAttributeUpdateDTO.getUpdatedDate());
          allowedAttributeValue.setUpdatedBy(masterAttributeUpdateDTO.getUpdatedBy());
          allowedAttributeValue.setMarkForDelete(false);
          updatedAllowedAttributeValues.put(allowedAttributeValue.getId(), allowedAttributeValue);
          existingInActiveAttributeValueMap.put(attributeValueUpdateDTO.getValue(), allowedAttributeValue);
        } else {
          duplicateAttributeValues.add(allowedAttributeValue.getValue());
        }
      }
    }
    if(CollectionUtils.isNotEmpty(duplicateAttributeValues)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ATTRIBUTE_VALUE_ALREADY_EXISTS_WITH_VALUE + duplicateAttributeValues.toString());
    }
  }

  /**
   * Add new allowedAttributeValue
   * @param storeId
   * @param attribute
   * @param attributeValueUpdateDTO
   * @param updatedBy
   * @param updatedDate
   * @throws Exception
   */
  private AllowedAttributeValue addAllowedAttributeValue(String storeId, Attribute attribute,
    AttributeValueUpdateDTO attributeValueUpdateDTO, String updatedBy, Date updatedDate) throws Exception{
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setStoreId(storeId);
    allowedAttributeValue.setAttribute(attribute);
    allowedAttributeValue.setValue(attributeValueUpdateDTO.getValue());
    allowedAttributeValue.setValueType(attributeValueUpdateDTO.getValueType());
    allowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
    allowedAttributeValue.setAllowedAttributeCode(
        attribute.getAttributeCode() + "-" + allowedAttributeValueService.getSequence(attribute.getAttributeCode()));
    allowedAttributeValue.setCreatedBy(updatedBy);
    allowedAttributeValue.setUpdatedBy(updatedBy);
    allowedAttributeValue.setUpdatedDate(updatedDate);
    allowedAttributeValue.setCreatedDate(updatedDate);
    allowedAttributeValueService.save(allowedAttributeValue);
    return allowedAttributeValue;
  }

  /**
   *
   * @param attributeValueUpdateDTO
   * @param updatedBy
   * @param updatedDate
   * @param existingAllowedAttributeValues
   * @param updatedAllowedAttributeValues
   * @throws Exception
   */
  private void deleteAllowedAttributeValue(AttributeValueUpdateDTO attributeValueUpdateDTO, String updatedBy,
      Date updatedDate, Map<String, AllowedAttributeValue> existingAllowedAttributeValues,
      Map<String, AllowedAttributeValue> updatedAllowedAttributeValues) throws Exception {
    AllowedAttributeValue allowedAttributeValue = existingAllowedAttributeValues.get(attributeValueUpdateDTO.getId());
    if (Objects.isNull(allowedAttributeValue)){
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          ATTRIBUTE_VALUE_NOT_FOUND_WITH_ID + attributeValueUpdateDTO.getId());
    }
    allowedAttributeValue.setUpdatedBy(updatedBy);
    allowedAttributeValue.setUpdatedDate(updatedDate);
    allowedAttributeValue.setMarkForDelete(true);
    updatedAllowedAttributeValues.put(allowedAttributeValue.getId(), allowedAttributeValue);
  }

  /**
   * Update PredefinedAllowedAttributeValues
   * @param storeId
   * @param attribute
   * @param masterAttributeUpdateDTO
   * @throws Exception
   */
  private void updatePredefinedAllowedAttributeValues(String storeId, Attribute attribute,
      MasterAttributeUpdateDTO masterAttributeUpdateDTO) throws Exception{
    masterAttributeUpdateDTO.getAddedAttributeValues()
        .removeIf(attributeValueUpdateDTO -> Constants.HYPHEN.equals(attributeValueUpdateDTO.getValue()));
    masterAttributeUpdateDTO.getDeletedAttributeValues()
        .removeIf(attributeValueUpdateDTO -> Constants.HYPHEN.equals(attributeValueUpdateDTO.getValue()));
    Map<String, PredefinedAllowedAttributeValue> existingPredefinedAttributeValues =
        attribute.getPredefinedAllowedAttributeValues().stream()
            .collect(Collectors.toMap(value -> value.getId(), value -> value));
    Map<String, PredefinedAllowedAttributeValue> updatedPredefinedAllowedAttributeValues = new HashMap();
    if (AttributeSortType.MANUAL.equals(masterAttributeUpdateDTO.getSortType()) || AttributeSortType.CUSTOM
        .equals(masterAttributeUpdateDTO.getSortType())) {
      for (AttributeValueUpdateDTO attributeValueUpdateDTO : masterAttributeUpdateDTO.getAttributeValues()) {
        updateSequenceForPredefinedAllowedAttributeValue(attributeValueUpdateDTO,
            masterAttributeUpdateDTO.getUpdatedBy(), masterAttributeUpdateDTO.getUpdatedDate(),
            existingPredefinedAttributeValues, updatedPredefinedAllowedAttributeValues);
      }
    }
    Map<String, PredefinedAllowedAttributeValue> existingInActiveAttributeValueMap = new HashMap<>();
    validateAddPredefinedAttributeValuesAndUpdateInActiveValuesMap(storeId, attribute, masterAttributeUpdateDTO,
        existingInActiveAttributeValueMap, updatedPredefinedAllowedAttributeValues);
    for (AttributeValueUpdateDTO attributeValueUpdateDTO : masterAttributeUpdateDTO.getAddedAttributeValues()) {
      PredefinedAllowedAttributeValue existingInActiveAttributeValue =
          existingInActiveAttributeValueMap.get(attributeValueUpdateDTO.getValue());
      if (Objects.isNull(existingInActiveAttributeValue)) {
        addPredefinedAllowedAttributeValue(storeId, attribute, attributeValueUpdateDTO,
            masterAttributeUpdateDTO.getUpdatedBy(), masterAttributeUpdateDTO.getUpdatedDate());
      }
    }
    for (AttributeValueUpdateDTO attributeValueUpdateDTO : masterAttributeUpdateDTO.getDeletedAttributeValues()) {
      deletePredefinedAllowedAttributeValue(attributeValueUpdateDTO, masterAttributeUpdateDTO.getUpdatedBy(),
          masterAttributeUpdateDTO.getUpdatedDate(), existingPredefinedAttributeValues,
          updatedPredefinedAllowedAttributeValues);
    }
    attribute.getPredefinedAllowedAttributeValues().removeIf(
        attributeValue -> Objects.isNull(updatedPredefinedAllowedAttributeValues.get(attributeValue.getId())));
  }

  /**
   * @param attributeValueUpdateDTO
   * @param updatedBy
   * @param UpdatedDate
   * @param existingPredefinedAttributeValues
   * @param updatedPredefinedAllowedAttributeValues
   * @throws Exception
   */
  private void updateSequenceForPredefinedAllowedAttributeValue(AttributeValueUpdateDTO attributeValueUpdateDTO,
      String updatedBy, Date UpdatedDate,
      Map<String, PredefinedAllowedAttributeValue> existingPredefinedAttributeValues,
      Map<String, PredefinedAllowedAttributeValue> updatedPredefinedAllowedAttributeValues) throws Exception {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        existingPredefinedAttributeValues.get(attributeValueUpdateDTO.getId());
    if (Objects.isNull(predefinedAllowedAttributeValue)){
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          ATTRIBUTE_VALUE_NOT_FOUND_WITH_ID + attributeValueUpdateDTO.getId());
    }
    predefinedAllowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
    predefinedAllowedAttributeValue.setUpdatedBy(updatedBy);
    predefinedAllowedAttributeValue.setUpdatedDate(UpdatedDate);
    updatedPredefinedAllowedAttributeValues
        .put(predefinedAllowedAttributeValue.getId(), predefinedAllowedAttributeValue);
  }

  /**
   *
   * @param storeId
   * @param attribute
   * @param masterAttributeUpdateDTO
   * @param existingInActiveAttributeValueMap
   * @param updatedPredefinedAllowedAttributeValues
   */
  private void validateAddPredefinedAttributeValuesAndUpdateInActiveValuesMap(String storeId, Attribute attribute,
      MasterAttributeUpdateDTO masterAttributeUpdateDTO,
      Map<String, PredefinedAllowedAttributeValue> existingInActiveAttributeValueMap,
      Map<String, PredefinedAllowedAttributeValue> updatedPredefinedAllowedAttributeValues) {
    List<String> duplicateAttributeValues = new ArrayList<>();
    for (AttributeValueUpdateDTO attributeValueUpdateDTO : masterAttributeUpdateDTO.getAddedAttributeValues()) {
      List<PredefinedAllowedAttributeValue> existingAllowedAttributeValues =
          attribute.getPredefinedAllowedAttributeValues().stream()
              .filter(attributeValue -> attributeValue.getValue().equals(attributeValueUpdateDTO.getValue()))
              .collect(Collectors.toList());
      for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : existingAllowedAttributeValues) {
        if (predefinedAllowedAttributeValue.isMarkForDelete()) {
          predefinedAllowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
          predefinedAllowedAttributeValue.setUpdatedDate(masterAttributeUpdateDTO.getUpdatedDate());
          predefinedAllowedAttributeValue.setUpdatedBy(masterAttributeUpdateDTO.getUpdatedBy());
          predefinedAllowedAttributeValue.setMarkForDelete(false);
          updatedPredefinedAllowedAttributeValues
              .put(predefinedAllowedAttributeValue.getId(), predefinedAllowedAttributeValue);
          existingInActiveAttributeValueMap.put(attributeValueUpdateDTO.getValue(), predefinedAllowedAttributeValue);
        } else {
          duplicateAttributeValues.add(attributeValueUpdateDTO.getValue());
        }
      }
    }
    if (CollectionUtils.isNotEmpty(duplicateAttributeValues)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ATTRIBUTE_VALUE_ALREADY_EXISTS_WITH_VALUE + duplicateAttributeValues.toString());
    }
  }

  /**
   * Add new predefinedAllowedAttributeValue
   * @param storeId
   * @param attribute
   * @param attributeValueUpdateDTO
   * @param updatedBy
   * @param updatedDate
   * @throws Exception
   */
  private PredefinedAllowedAttributeValue addPredefinedAllowedAttributeValue(String storeId, Attribute attribute,
    AttributeValueUpdateDTO attributeValueUpdateDTO, String updatedBy, Date updatedDate) throws Exception {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setStoreId(storeId);
    predefinedAllowedAttributeValue.setAttribute(attribute);
    predefinedAllowedAttributeValue.setValue(attributeValueUpdateDTO.getValue());
    predefinedAllowedAttributeValue.setValueEn(attributeValueUpdateDTO.getValueEn());
    predefinedAllowedAttributeValue.setSequence(attributeValueUpdateDTO.getSequence());
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(attribute.getAttributeCode() + "-" +
        predefinedAllowedAttributeValueService.getSequence(attribute.getAttributeCode()));
    predefinedAllowedAttributeValue.setCreatedBy(updatedBy);
    predefinedAllowedAttributeValue.setUpdatedBy(updatedBy);
    predefinedAllowedAttributeValue.setUpdatedDate(updatedDate);
    predefinedAllowedAttributeValue.setCreatedDate(updatedDate);
    predefinedAllowedAttributeValueService.save(predefinedAllowedAttributeValue);
    return predefinedAllowedAttributeValue;
  }

  /**
   *
   * @param attributeValueUpdateDTO
   * @param updatedBy
   * @param updatedDate
   * @param existingPredefinedAttributeValues
   * @param updatedPredefinedAllowedAttributeValues
   * @throws Exception
   */
  private void deletePredefinedAllowedAttributeValue(AttributeValueUpdateDTO attributeValueUpdateDTO, String updatedBy,
      Date updatedDate, Map<String, PredefinedAllowedAttributeValue> existingPredefinedAttributeValues,
      Map<String, PredefinedAllowedAttributeValue> updatedPredefinedAllowedAttributeValues) throws Exception {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        existingPredefinedAttributeValues.get(attributeValueUpdateDTO.getId());
    if (Objects.isNull(predefinedAllowedAttributeValue)){
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          ATTRIBUTE_VALUE_NOT_FOUND_WITH_ID + attributeValueUpdateDTO.getId());
    }
    predefinedAllowedAttributeValue.setUpdatedBy(updatedBy);
    predefinedAllowedAttributeValue.setUpdatedDate(updatedDate);
    predefinedAllowedAttributeValue.setMarkForDelete(true);
    updatedPredefinedAllowedAttributeValues
        .put(predefinedAllowedAttributeValue.getId(), predefinedAllowedAttributeValue);
  }

  @Override
  public Page<Attribute> getAttributeByAttributeFilter(String storeId,
      AttributeFilter attributeFilter, Pageable pageable) {
    Page<Attribute> response = null;
    AttributeType attributeType = AttributeType.valueOf(attributeFilter.getAttributeType());
    response = this.attributeRepository
        .findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(storeId,
            attributeType, attributeFilter.getName(), attributeFilter.getSortedBy(),
            attributeFilter.getSortDirection(), pageable, attributeFilter.getSizeAttribute());
    return response;
  }
}
