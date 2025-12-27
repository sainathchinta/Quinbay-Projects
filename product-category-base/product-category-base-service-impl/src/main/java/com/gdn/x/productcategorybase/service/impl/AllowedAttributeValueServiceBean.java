package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueDto;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.repository.AllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

@Service
@Transactional(readOnly = true)
public class AllowedAttributeValueServiceBean implements AllowedAttributeValueService {

  @Autowired
  private AllowedAttributeValueRepository repository;
  
  @Autowired
  private PredefinedAllowedAttributeValueRepository predefAllowedAttrValueRepository;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;
  
  private static final String ATTRIBUTE_CODE_NOT_NULL = "Attribute code must not be null";

  @Override
  public void delete(String id) throws Exception {
    // do nothing
  }

  @Override
  public AllowedAttributeValue findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<AllowedAttributeValue> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public List<AllowedAttributeValue> findByStoreIdAndAttributeAndValue(String storeId, Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValue(storeId, attribute, value);
  }

  @Override
  public AllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(String storeId,
      Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(storeId, attribute, value);
  }

  @Override
  public AllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(String storeId,
      Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(storeId, attribute, value);
  }

  @Override
  public AllowedAttributeValue findByStoreIdAndId(String storeId, String id) {
    return this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  /**
   * use prefix 0 for product-code 1 for category-code 2 for attribute-code
   */
  @Override
  public String getSequence(String attributeCode) {
    return StringUtils.leftPad("" + this.repository.getSequenceByAttributeCode(attributeCode), 5, "0");
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteAllowedAttributeValue(String storeId, String id) throws Exception {
    AllowedAttributeValue savedAllowedAttributeValue =
        this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
    if (savedAllowedAttributeValue == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Can not perform delete on un exist data : " + id);
    }

    savedAllowedAttributeValue.setMarkForDelete(true);
    this.update(savedAllowedAttributeValue);
  }

  @Override
  public String save(AllowedAttributeValue allowedAttributeValue) throws Exception {
    return ServiceBeanHelper.saveEntity(allowedAttributeValue, this.repository);
  }

  @Override
  public void update(AllowedAttributeValue allowedAttributeValue) throws Exception {
    ServiceBeanHelper.updateEntity(allowedAttributeValue, this.repository);
  }

  @Override
  public List<AllowedAttributeValueDtoResponse> findAllowedPredefiningAndDefiningAttributeValue(
      List<AllowedAttributeValueDtoRequest> request) {
    List<String> predefinedValues = new ArrayList<>();
    List<String> definingValues = new ArrayList<>();
    List<String> predefinedCodes = new ArrayList<>();
    List<String> definingCodes = new ArrayList<>();
    List<AllowedAttributeValueDtoRequest> definingAttributeRequest = new ArrayList<>();
    ConverterUtil.groupByAttributeType(request, predefinedValues, predefinedCodes, definingValues, definingCodes,
        definingAttributeRequest);
    List<AllowedAttributeValueDtoResponse> result = new ArrayList<>();
    getPredefinedAllowedAttributeValues(predefinedCodes, predefinedValues, result);
    getAllowedAttributeValues(definingAttributeRequest, definingCodes, definingValues, result);
    return result;
  }

  private void getPredefinedAllowedAttributeValues(List<String> predefinedCodes, List<String> predefinedValues,
      List<AllowedAttributeValueDtoResponse> result) {
    List<PredefinedAllowedAttributeValue> predefinedResponse =
        predefAllowedAttrValueRepository.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(predefinedCodes,
            predefinedValues);
    if (CollectionUtils.isNotEmpty(predefinedResponse)) {
      result.addAll(ConverterUtil.populatePredefinedAttribute(predefinedResponse));
    }
  }

  private void getAllowedAttributeValues(List<AllowedAttributeValueDtoRequest> definingAttributeRequest,
      List<String> definingCodes, List<String> definingValues, List<AllowedAttributeValueDtoResponse> result) {
    List<String> sanitizedDefiningAttributeValues =
        ConverterUtil.getAttributeValueFromValueAndValueType(definingValues, sizeChartValueTypeDelimiter);
    List<AllowedAttributeValue> sanitizedDefiningResponse =
        repository.findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(definingCodes,
            sanitizedDefiningAttributeValues);

    List<AllowedAttributeValue> nonSizeChartAllowedAttributeValue = new ArrayList<>();
    List<AllowedAttributeValue> sizeChartAllowedAttributeValue = new ArrayList<>();
    ConverterUtil.groupBySizeChartAttribute(sanitizedDefiningResponse, sizeChartAllowedAttributeValue, nonSizeChartAllowedAttributeValue);

    if (CollectionUtils.isNotEmpty(nonSizeChartAllowedAttributeValue)) {
      result.addAll(ConverterUtil.populateAllowedAttributeNonSizeChartValue(nonSizeChartAllowedAttributeValue));
    }

    Map<String, Map<String, Map<String, AllowedAttributeValue>>> sanitizedAttributeCodeAndValueAndValueTypeMapFromDb =
        ConverterUtil.getAttributeCodeAndValueAndValueTypeMapFromDb(sizeChartAllowedAttributeValue);
    if (MapUtils.isNotEmpty(sanitizedAttributeCodeAndValueAndValueTypeMapFromDb)) {
      result.addAll(ConverterUtil.populateAllowedAttributeSizeChartValue(definingAttributeRequest,
          sanitizedAttributeCodeAndValueAndValueTypeMapFromDb, sizeChartValueTypeDelimiter));
    }
  }

  public List<AllowedAttributeValue> findByStoreIdAndIds(String storeId, Set<String> ids) {
    return this.repository.findByStoreIdAndIdInAndMarkForDeleteFalse(storeId, ids);
  }

  @Override
  public Page<AllowedAttributeValue> findByStoreIdAndAttributeId(String storeId, String attributeId, Pageable pageable,
      AttributeSortType sortType) {
    Page<AllowedAttributeValue> allowedAttributeValues = null;
    if (AttributeSortType.MANUAL.equals(sortType) || AttributeSortType.CUSTOM.equals(sortType)) {
      allowedAttributeValues = this.repository
          .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(storeId, attributeId, pageable);
    } else if (AttributeSortType.ALPHABETICAL.equals(sortType)) {
      allowedAttributeValues = this.repository
          .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(storeId, attributeId, pageable);
    }
    return allowedAttributeValues;
  }

  @Override
  public Page<AttributeOptionDTO> getAttributeOptionsByAttributeCodeAndKeyword(String attributeCode,
      String keyword, Pageable pageable) throws Exception {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(attributeCode), ATTRIBUTE_CODE_NOT_NULL);
      keyword = Optional.ofNullable(keyword).orElse("");
      Page<AttributeOptionDTO> attributeOptions =
          this.repository.getDefiningAttributeOptionByAttributeCodeAndKeyword(attributeCode,
              keyword.toLowerCase(), pageable);
      if (CollectionUtils.isEmpty(attributeOptions.getContent())) {
        attributeOptions = this.repository.getPredefineAttributeOptionByAttributeCodeAndKeyword(
            attributeCode, keyword.toLowerCase(),pageable);
      }
      return attributeOptions;
  }

  @Override
  @Cacheable(value = CacheNames.ALLOWED_ATTRIBUTE_VALUES_CACHE, key = "#storeId+'_'+ #attributeId", unless = "#result == null")
  public List<AllowedAttributeValue> getAllowedAttributeValuesByStoreIdAndAttributeId(String storeId, String attributeId) {
    return repository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(storeId, attributeId);
  }

  @Override
  public AllowedAttributeValue getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(String storeId,
      String attributeId, String value) {
    return repository.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(storeId, attributeId, value);
  }

  @Override
  public List<AllowedAttributeValue> getAllowedAttributeValuesByStoreIdAndIds(String storeId, Set<String> ids) {
    return this.repository.findByStoreIdAndIdIn(storeId, ids);
  }

  @Override
  public AllowedAttributeValue findByStoreIdAndAllowedAttributeCode(String storeId, String allowedAttributeCode) {
    return this.repository.findByStoreIdAndAllowedAttributeCode(storeId, allowedAttributeCode);
  }

  @Override
  public List<AllowedAttributeValue> findByStoreIdAndAttributeAndValueOrderByMarkForDelete(String storeId,
      Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(storeId, attribute, value);
  }
}
