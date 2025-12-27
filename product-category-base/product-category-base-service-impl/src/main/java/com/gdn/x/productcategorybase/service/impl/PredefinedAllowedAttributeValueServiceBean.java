package com.gdn.x.productcategorybase.service.impl;


import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.brand.BrandService;

@Service
@Transactional(readOnly = true)
public class PredefinedAllowedAttributeValueServiceBean implements PredefinedAllowedAttributeValueService {

  @Autowired
  private PredefinedAllowedAttributeValueRepository repository;

  @Autowired
  @Lazy
  private AttributeService attributeService;

  @Autowired
  @Lazy
  private BrandService brandServiceBean;

  @Value("${allow.rejected.brand.in.attributes}")
  private boolean allowRejectedBrandInAttributes;

  @Override
  @Transactional(readOnly = false)
  public void deactivated(String predefinedAllowedAttributeValueId, String storeId) throws Exception {
    this.markForDeletePredefinedAllowedAttributeValue(storeId, predefinedAllowedAttributeValueId);
  }

  @Override
  public void delete(String id) throws Exception {
    // TODO Auto-generated method stub

  }

  @Override
  public PredefinedAllowedAttributeValue findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<PredefinedAllowedAttributeValue> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeAndValue(String storeId, Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValue(storeId, attribute, value);
  }

  @Override
  public PredefinedAllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(String storeId,
      Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(storeId, attribute, value);
  }

  @Override
  public PredefinedAllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(String storeId,
      Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(storeId, attribute, value);
  }

  @Override
  public Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
      String storeId, String value, String attributeId, Pageable pageable) {
    return this.repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(storeId, attributeId,
        value, pageable);
  }

  @Override
  public List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(
      String storeId, String value, String attributeId) {
    return this.repository.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(storeId, attributeId, value);
  }

  @Override
  public PredefinedAllowedAttributeValue findByStoreIdAndId(String storeId, String id) {
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
  public void markForDeletePredefinedAllowedAttributeValue(String storeId,
      List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues) throws Exception {
    for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : predefinedAllowedAttributeValues) {
      this.markForDeletePredefinedAllowedAttributeValue(storeId, predefinedAllowedAttributeValue.getId());
    }

  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeletePredefinedAllowedAttributeValue(String storeId, String id) throws Exception {
    PredefinedAllowedAttributeValue savedPredefinedAllowedAttributeValue =
        this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
    if (savedPredefinedAllowedAttributeValue == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Can not perform delete on un exist data : " + id);
    }

    savedPredefinedAllowedAttributeValue.setMarkForDelete(true);
    this.update(savedPredefinedAllowedAttributeValue);
  }

  @Override
  @Transactional(readOnly = false)
  public String save(PredefinedAllowedAttributeValue entity) throws Exception {
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveWithGeneratedCode(String storeId, String attributeId,
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue) throws Exception {
    Attribute attr = this.attributeService.findById(storeId, attributeId);
    if (attr != null) {
      Page<PredefinedAllowedAttributeValue> predefinedList =
          this.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(storeId,
              predefinedAllowedAttributeValue.getValue(), attr.getId(), PageRequest.of(0, 1));
      if ((predefinedList.getContent() == null) || predefinedList.getContent().isEmpty()) {
        predefinedAllowedAttributeValue.setStoreId(storeId);
        if (StringUtils.isBlank(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode())) {
          predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(
              attr.getAttributeCode() + "-" + this.getSequence(attr.getAttributeCode()));
        }
        predefinedAllowedAttributeValue.setAttribute(attr);
        this.save(predefinedAllowedAttributeValue);
      }
    } else {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform save on un exist data : " + attributeId);
    }

  }

  @Override
  @Transactional(readOnly = false)
  public void update(PredefinedAllowedAttributeValue entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);

  }

  @Override
  public PredefinedAllowedAttributeValue findByStoreIdAndPredefinedAllowedAttributeCode(String storeId,
      String predefinedAllowedAttributeCode) throws Exception {
    return this.repository.findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(storeId,
        predefinedAllowedAttributeCode);
  }

  @Override
  public PredefinedAllowedAttributeValue findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
      String storeId, String attributeCode, String value, boolean fetchByPredefinedAttributeCode) {
    if (fetchByPredefinedAttributeCode) {
      return this.repository.findByStoreIdAndAttributeAttributeCodeAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(
          storeId, attributeCode, value);
    } else {
      return this.repository.findTopByStoreIdAndAttributeAttributeCodeAndValueAndMarkForDeleteFalseOrderByUpdatedDateDesc(
          storeId, attributeCode, value);
    }
  }

  @Override
  public Page<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String storeId, String value,
      String businessPartnerCode, Pageable pageable, boolean isSearch, boolean isExternal)
      throws Exception {
    return this.brandServiceBean.getBrandSuggestions(storeId, value, businessPartnerCode,
        pageable, isSearch, isExternal);
  }

  @Override
  public Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeId(String storeId, String attributeId,
      Pageable pageable, AttributeSortType sortType) throws Exception {
    if (AttributeSortType.MANUAL.equals(sortType) || AttributeSortType.CUSTOM.equals(sortType)) {
      return this.repository
          .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(storeId, attributeId, pageable);
    } else if (AttributeSortType.ALPHABETICAL.equals(sortType)) {
      return this.repository
          .findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(storeId, attributeId, pageable);
    }
    return null;
  }

  public List<PredefinedAllowedAttributeValue> findByStoreIdAndPredefinedAllowedAttributeCodes(String storeId,
      Set<String> predefinedAllowedAttributeCodes) {
    return this.repository.findByStoreIdAndPredefinedAllowedAttributeCodeInAndMarkForDeleteFalse(storeId,
        predefinedAllowedAttributeCodes);
  }

  @Override
  public void add(PredefinedAllowedAttributeValue predefinedAllowedAttributeValue) {
    this.repository.save(predefinedAllowedAttributeValue);
  }

  @Override
  public PredefinedAllowedAttributeValue generatePredefinedAllowedAttributeValue(BrandWip brandWip,
      Attribute attribute) {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(brandWip.getBrandName());
    predefinedAllowedAttributeValue.setCreatedBy(brandWip.getCreatedBy());
    predefinedAllowedAttributeValue.setCreatedDate(brandWip.getCreatedDate());
    predefinedAllowedAttributeValue.setMarkForDelete(false);
    predefinedAllowedAttributeValue.setStoreId(brandWip.getStoreId());
    predefinedAllowedAttributeValue.setUpdatedBy(brandWip.getUpdatedBy());
    predefinedAllowedAttributeValue.setUpdatedDate(brandWip.getUpdatedDate());
    predefinedAllowedAttributeValue.setAttribute(attribute);
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(brandWip.getBrandRequestCode());
    predefinedAllowedAttributeValue.setVersion(brandWip.getVersion());
    return predefinedAllowedAttributeValue;
  }

  @Override
  public List<PredefinedAllowedAttributeValue> findByStoreIdAndIds(String storeId, Set<String> ids) {
    if (allowRejectedBrandInAttributes) {
      return Optional.ofNullable(repository.findByStoreIdAndIdIn(storeId, ids)).orElse(new ArrayList<>())
          .stream().filter(predefinedAllowedAttributeValue -> !predefinedAllowedAttributeValue.isMarkForDelete()
              || Constants.BRAND.equalsIgnoreCase(predefinedAllowedAttributeValue.getAttribute().getName()))
          .collect(Collectors.toList());
      } else {
      return this.repository.findByStoreIdAndIdInAndMarkForDeleteFalse(storeId, ids);
    }
  }

  @Override
  public List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
      String storeId, String attributeId, String value) {
    return repository.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(storeId, attributeId, value);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updatePredefinedAllowedAttributeCodeForApprovedBrand(String storeId,
      String predefinedAllowedAttributeCode, Brand brand) throws ApplicationException {
    PredefinedAllowedAttributeValue response = repository
        .findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(storeId, predefinedAllowedAttributeCode);
    if(Objects.isNull(response)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform save on un exist data : " + predefinedAllowedAttributeCode);
    }
    response.setPredefinedAllowedAttributeCode(brand.getBrandCode());
    response.setValue(brand.getBrandName());
    repository.save(response);
    Hibernate.initialize(response.getAttribute());
    attributeService
        .evictAttributeCache(storeId, response.getAttribute().getId(), response.getAttribute().getAttributeCode());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updatePredefinedAllowedAttributeCodeForRejectedBrand(String storeId, String brandRequestCode)
      throws ApplicationException {
    PredefinedAllowedAttributeValue response =
        repository.findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(storeId, brandRequestCode);
    if (Objects.isNull(response)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform save on un exist data : " + brandRequestCode);
    }
    response.setMarkForDelete(true);
    repository.save(response);
    Hibernate.initialize(response.getAttribute());
    attributeService
        .evictAttributeCache(storeId, response.getAttribute().getId(), response.getAttribute().getAttributeCode());
  }

  @Override
  @Cacheable(value = CacheNames.PREDEFINED_ALLOWED_ATTRIBUTE_VALUES_CACHE, key = "#storeId+'_'+ #attributeId", unless = "#result == null")
  public List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
      String storeId, String attributeId) {
    return repository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(storeId, attributeId);
  }

  @Override
  public List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValuesByStoreIdAndIds(
      String storeId, Set<String> ids) {
    return this.repository.findByStoreIdAndIdIn(storeId, ids);
  }

  @Override
  public List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeAndValueOrderByMarkForDelete(String storeId,
      Attribute attribute, String value) {
    return this.repository.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(storeId, attribute, value);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public PredefinedAllowedAttributeValue addPredefinedAllowedAttributeValue(String storeId, Attribute attribute,
      String updatedBy, String value, Integer sequence, String valueEn) {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setStoreId(storeId);
    predefinedAllowedAttributeValue.setAttribute(attribute);
    predefinedAllowedAttributeValue.setValue(value);
    predefinedAllowedAttributeValue.setValueEn(valueEn);
    predefinedAllowedAttributeValue.setSequence(sequence);
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(
        attribute.getAttributeCode() + Constants.HYPHEN + this.getSequence(attribute.getAttributeCode()));
    predefinedAllowedAttributeValue.setCreatedBy(updatedBy);
    predefinedAllowedAttributeValue.setUpdatedBy(updatedBy);
    return repository.save(predefinedAllowedAttributeValue);
  }

  @Override
  public List<PredefinedAllowedAttributeValue> getAttributeIdAndValueLikeIgnoreCase(
    String storeId, String attributeId, String value) {
    return repository.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
      storeId, attributeId, value);
  }
}
