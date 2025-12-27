package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.dto.request.AttributeAndValueByTypeRequest;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeBasicDetailDTO;
import com.gdn.x.productcategorybase.dto.AttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.config.ApplicationConfig;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class AttributeServiceBean implements AttributeService {

  private static final String NOT_FOUND_PRODUCT_ATTRIBUTE_WITH_ID = "not found product attribute with id ";

  private static final String NOT_FOUND_ATTRIBUTE_WITH_ID = "not found attribute with id ";


  @Autowired
  private ApplicationConfig applicationConfig;

  @Autowired
  private AttributeRepository repository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private AllowedAttributeValueService allowedAttributeValueService;

  @Autowired
  @Lazy
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductAttributeValueService productAttributeValueService;

  @Autowired
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Autowired
  private CategoryAttributeRepository categoryAttributeRepository;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  @Lazy
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Autowired
  @Lazy
  private CategoryServiceBean categoryServiceBean;

  @Autowired
  @Lazy
  private ProductAttributeService productAttributeService;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${ds.attribute.extraction.optimisation.enabled}")
  private boolean dsAttributeExtractionOptimisationEnabled;

  @Value("${ignore.ds.attribute}")
  private boolean ignoreDsAttribute;

  @Override
  @Transactional(readOnly = false)
  public void delete(String id) throws Exception {

    // TODO: Open this comment for working on PX-329 Attribute
    /*
     * savedAttribute = repository.getOne(id); if(savedAttribute == null) { throw new
     * ApplicationException(ErrorCategory.DATA_NOT_FOUND,
     * "Can not perform delete on un exist data : "+ id); } savedAttribute.setMarkForDelete(true);
     * repository.saveAndFlush(savedAttribute);
     */
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteAllowedAttributeValue(String storeId, String allowedAttributeValueId) throws Exception {
    AllowedAttributeValue allowedAttributeValue =
        this.allowedAttributeValueService.findByStoreIdAndId(storeId, allowedAttributeValueId);

    Hibernate.initialize(allowedAttributeValue.getAttribute());
    Long countProductItemAttribute =
        productItemAttributeRepository.countByAttribute_IdAndValueAndMarkForDeleteFalse(
            allowedAttributeValue.getAttribute().getId(), allowedAttributeValue.getValue());

    if (countProductItemAttribute > 0) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, "This value is already used by other product");
    } else {
      this.allowedAttributeValueService.markForDeleteAllowedAttributeValue(storeId, allowedAttributeValueId);
      if (this.applicationConfig.isRegenerateWhenDeleteAllowedAttribute()) {
        this.productService.deleteProductItemsByProductAttributeValues(storeId,
            this.productAttributeValueService.findByStoreIdAndAllowedAttributeValue(storeId, allowedAttributeValue));
      }
    }
    evictAttributeCache(storeId, allowedAttributeValue.getAttribute().getId(),
        allowedAttributeValue.getAttribute().getAttributeCode());
  }

  @Override
  @Transactional(readOnly = false)
  public void deletePredefinedAllowedAttributeValue(String storeId, String predefinedAllowedAttributeValueId)
      throws Exception {
    this.predefinedAllowedAttributeValueService.markForDeletePredefinedAllowedAttributeValue(storeId,
        predefinedAllowedAttributeValueId);
  }

  @Override
  public List<Attribute> findByAttributeCode(String storeId, String attributeCode) {
    return this.repository.findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(storeId,
        attributeCode);
  }

  @Override
  public Page<Attribute> findByAttributeCode(String storeId, String attributeCode, Pageable pageable) {
    return this.repository.findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(storeId,
        attributeCode, pageable);
  }

  @Override
  public List<Attribute> findByAttributeType(String storeId, AttributeType attributeType) {
    return this.repository.findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(storeId, attributeType);
  }

  @Override
  public Page<Attribute> findByAttributeType(String storeId, AttributeType attributeType, Pageable pageable) {
    return this.repository.findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(storeId, attributeType, pageable);
  }

  @Override
  public Attribute findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Attribute findById(String storeId, String id) {
    return this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  @Override
  public List<Attribute> findByAttributeIds(String storeId, List<String> attributeIdList) {
    return this.repository.findByIdInAndStoreIdAndMarkForDeleteFalse(attributeIdList, storeId);
  }

  @Override
  public Page<Attribute> findByNameLikeIgnoreCase(String storeId, String name, Pageable pageable) {
    return this.repository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(storeId, name, pageable);
  }

  @Override
  public Page<Attribute> findByNameStartingWith(String storeId, String name, Pageable pageable) {
    return this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(storeId, name, pageable);
  }

  @Override
  public List<Attribute> findBySearchAbleFalse(String storeId) {
    return this.repository.findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(storeId);
  }

  @Override
  public Page<Attribute> findBySearchAbleFalse(String storeId, Pageable pageable) {
    return this.repository.findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public List<Attribute> findBySearchAbleTrue(String storeId) {
    return this.repository.findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(storeId);
  }

  @Override
  public Page<Attribute> findBySearchAbleTrue(String storeId, Pageable pageable) {
    return this.repository.findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public Page<Attribute> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public Attribute findDetailByStoreIdAndId(String storeId, String id,
      boolean needCategoryAttributes) {
    Attribute attribute = getAttributeService().getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(storeId, id);
    attribute.setAllowedAttributeValues(new ArrayList<>());
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      attribute.setAllowedAttributeValues(
      allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeId(storeId, attribute.getId()));
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())
        || AttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType())) {
      attribute.setPredefinedAllowedAttributeValues(
      predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
          storeId, attribute.getId()));
      removeDefaultAttributeValue(attribute);
    }
    if(needCategoryAttributes) {
      attribute.setCategoryAttributes(categoryAttributeRepository.findByStoreIdAndAttribute(storeId, attribute));
    }
    return attribute;
  }

  @Override
  public AttributeResponse findDetailByStoreIdAndIdAndValue(String storeId, String id, String value,
    boolean ignoreCaseFetch) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(id), ErrorMessage.ATTRIBUTE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(value), ErrorMessage.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY.getMessage());
    Attribute attribute = getAttributeService().getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(storeId, id);
    attribute.setAllowedAttributeValues(new ArrayList<>());
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());

    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      AllowedAttributeValue allowedAttributeValue =
          allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(storeId,
              attribute.getId(), value);
      if (Objects.nonNull(allowedAttributeValue)) {
        attribute.getAllowedAttributeValues().add(allowedAttributeValue);
      }
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())) {
      List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues;
      if (ignoreCaseFetch) {
        predefinedAllowedAttributeValues =
          predefinedAllowedAttributeValueService.getAttributeIdAndValueLikeIgnoreCase(
            storeId, attribute.getId(), value);
      } else {
        predefinedAllowedAttributeValues =
          predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
            storeId, attribute.getId(), value);
      }
      if (CollectionUtils.isNotEmpty(predefinedAllowedAttributeValues)) {
        attribute.getPredefinedAllowedAttributeValues().addAll(predefinedAllowedAttributeValues);
      }
    }
    return ConverterUtil.toAttributeValueResponse(attribute);
  }


  @Override
  public Attribute findDetailByStoreIdAndAttributeCode(String storeId, String attributeCode) {
    Attribute attribute = getAttributeService().getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
        storeId, attributeCode);
    attribute.setAllowedAttributeValues(new ArrayList<>());
    attribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      attribute.setAllowedAttributeValues(
          allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndAttributeId(storeId, attribute.getId()));
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())) {
      attribute.setPredefinedAllowedAttributeValues(
          predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
              storeId, attribute.getId()));
      removeDefaultAttributeValue(attribute);
    }
    else if (AttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType())) {
      attribute.setPredefinedAllowedAttributeValues(
        predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
          storeId, attribute.getId()));
    }
    return attribute;
  }

  private void removeDefaultAttributeValue(Attribute attribute) {
    if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType()) && attribute.isMandatory()
        && CollectionUtils.isNotEmpty(attribute.getPredefinedAllowedAttributeValues())) {
      attribute.getPredefinedAllowedAttributeValues().removeIf(
          predefinedAllowedAttributeValue -> Objects.nonNull(predefinedAllowedAttributeValue) && StringUtils
              .isNotBlank(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode())
              && predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode().contains(Constants.DEFAULT));
    }
  }

  private Attribute generateAttributeValueCode(Attribute entity) {
    for (AllowedAttributeValue allowedAttributeValue : entity.getAllowedAttributeValues()) {
      allowedAttributeValue.setAllowedAttributeCode(
          entity.getAttributeCode() + "-" + this.allowedAttributeValueService.getSequence(entity.getAttributeCode()));
    }

    for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : entity
        .getPredefinedAllowedAttributeValues()) {
      if (Constants.HYPHEN.equals(predefinedAllowedAttributeValue.getValue())) {
        predefinedAllowedAttributeValue
            .setPredefinedAllowedAttributeCode(entity.getAttributeCode() + Constants.HYPHEN + Constants.DEFAULT);
      } else {
        predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(
            entity.getAttributeCode() + Constants.HYPHEN + this.predefinedAllowedAttributeValueService
                .getSequence(entity.getAttributeCode()));
      }
    }

    return entity;
  }

  /**
   * use prefix 0 for product-code 1 for category-code 2 for attribute-code
   */
  @Override
  public String getSequence(String attributeCode) {
    return StringUtils.leftPad("" + this.repository.getSequenceByAttributeCode(attributeCode), 6, "0");
  }

  @Override
  @Transactional(readOnly = false)
  public void regenerateAllowedAttributeValue(String storeId, Attribute oldAttribute, Attribute newAttribute)
      throws Exception {
    List<String> allowedAttributeValueIds = new ArrayList<String>();
    List<String> predefinedAllowedAttributeValueIds = new ArrayList<String>();
    for (AllowedAttributeValue oldAllowedAttributeValue : oldAttribute.getAllowedAttributeValues()) {
      if (!oldAllowedAttributeValue.isMarkForDelete()) {
        boolean same = false;
        for (AllowedAttributeValue newAllowedAttributeValue : newAttribute.getAllowedAttributeValues()) {
          if ((newAllowedAttributeValue.getId() != null)
              && newAllowedAttributeValue.getId().equals(oldAllowedAttributeValue.getId())) {
            same = true;
          }
        }
        if (!same) {
          allowedAttributeValueIds.add(oldAllowedAttributeValue.getId());
        }
      }
    }
    for (AllowedAttributeValue newAllowedAttributeValue : newAttribute.getAllowedAttributeValues()) {
      if (newAllowedAttributeValue.getId() == null) {
        oldAttribute.getAllowedAttributeValues().add(newAllowedAttributeValue);
      }
    }

    for (PredefinedAllowedAttributeValue oldPredefinedAllowedAttributeValue : oldAttribute
        .getPredefinedAllowedAttributeValues()) {
      if (!oldPredefinedAllowedAttributeValue.isMarkForDelete()) {
        boolean same = false;
        for (PredefinedAllowedAttributeValue newPredefinedAllowedAttributeValue : newAttribute
            .getPredefinedAllowedAttributeValues()) {
          if ((newPredefinedAllowedAttributeValue.getId() != null)
              && newPredefinedAllowedAttributeValue.getId().equals(oldPredefinedAllowedAttributeValue.getId())) {
            same = true;
          }
        }
        if (!same) {
          predefinedAllowedAttributeValueIds.add(oldPredefinedAllowedAttributeValue.getId());
        }
      }
    }

    for (PredefinedAllowedAttributeValue newPredefinedAllowedAttributeValue : newAttribute
        .getPredefinedAllowedAttributeValues()) {
      if (newPredefinedAllowedAttributeValue.getId() == null) {
        oldAttribute.getPredefinedAllowedAttributeValues().add(newPredefinedAllowedAttributeValue);
      }
    }
    this.update(oldAttribute);

    for (String allowedAttributeValueId : allowedAttributeValueIds) {
      this.deleteAllowedAttributeValue(storeId, allowedAttributeValueId);
    }

    for (String predefinedAllowedAttributeValueId : predefinedAllowedAttributeValueIds) {
      this.deletePredefinedAllowedAttributeValue(storeId, predefinedAllowedAttributeValueId);
    }
    this.evictAttributeCache(storeId, oldAttribute.getId(), oldAttribute.getAttributeCode());
  }

  @Override
  public List<Attribute> findByName(String storeId, String name) throws Exception {
    return this.repository.findByStoreIdAndNameAndMarkForDeleteFalse(storeId, name);
  }

  @Override
  @Transactional(readOnly = false)
  public String save(Attribute entity) throws Exception {
    if (StringUtils.isEmpty(entity.getAttributeCode())) {
      String prefixCode = StringUtils.left(entity.getName(), 2).toUpperCase();
      entity.setAttributeCode(prefixCode + "-2" + this.getSequence(prefixCode));
    }
    this.generateAttributeValueCode(entity);
    String attributeId = ServiceBeanHelper.saveEntity(entity, this.repository);
    this.evictAttributeCache(entity.getStoreId(), attributeId, entity.getAttributeCode());
    AttributeDomainEventModel attributeDomainEventModel =
        ConverterUtil.getMasterAttributeDomainEventModel(entity);
    attributeDomainEventModel.setId(attributeId);
    attributeDomainEventModel.setNewAttribute(true);
    attributeDomainEventModel.setValueUpdate(false);
    if (Boolean.TRUE.equals(entity.isDsExtraction())) {
      attributeDomainEventModel.setUpdatedFields(List.of(Constants.DS_EXTRACTION));
    }
    domainEventPublisherService.publishMasterAttributeInfoEvent(attributeDomainEventModel);
    return attributeId;
  }

  @Override
  @Transactional(readOnly = false)
  public void update(Attribute entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
    final List<String> categoryList = categoryAttributeRepository
        .findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
            GdnMandatoryParameterUtil.getStoreId(), entity.getId());
    applicationCacheServiceBean.clearCategoryDetails(GdnMandatoryParameterUtil.getStoreId(), categoryList);
  }

  @Override
  public CategoryAttributeDetailDTO getAttributeDetailByCategoryCode(String categoryCode,
      boolean concatenateValueWithValueType) throws Exception {
    try{
      List<Object[]> attrList = getAttributeList(categoryCode);
      CategoryAttributeDetailDTO categoryAttribute = new CategoryAttributeDetailDTO("", "", new ArrayList<AttributeDetailDTO>());
      AttributeDetailDTO attributeDetail = new AttributeDetailDTO("", "", "", new ArrayList<String>());
      for(Object[] data : attrList){
        categoryAttribute.setCategoryCode(String.valueOf(data[0]));
        categoryAttribute.setName(String.valueOf(data[1]));
        if (Objects.nonNull(data[12])) {
          categoryAttribute.setEnglishName(String.valueOf(data[12]));
        }
        if(! attributeDetail.getAttributeCode().equals(String.valueOf(data[2]))
            && attributeDetail.getAttributeCode() != ""){
          categoryAttribute.getAttributes().add(attributeDetail);
          attributeDetail = new AttributeDetailDTO("", "", "", new ArrayList<String>());
        }
        attributeDetail.setAttributeCode(String.valueOf(data[2]));
        attributeDetail.setAttributeType(String.valueOf(data[3]));
        attributeDetail.setName(String.valueOf(data[4]));
        attributeDetail.setSkuValue((Boolean)data[5]);
        attributeDetail.setBasicView((Boolean)data[6]);

        if(! StringUtils.isEmpty(String.valueOf(data[7]).trim())){
          String value = String.valueOf(data[7]);
          String valueType = null;
          if(data.length>14) {
            valueType = String.valueOf(data[14]);
          }
          String attributeType = String.valueOf(data[3]);
          value = CommonUtil.getAttributeValueBasedOnSizeChart(concatenateValueWithValueType,
              valueTypeAdditionForDefiningAttributes, value, valueType, attributeType, sizeChartValueTypeDelimiter);
          attributeDetail.getOptions().add(value);
        } else if(! StringUtils.isEmpty(String.valueOf(data[8]).trim())){
          attributeDetail.getOptions().add(String.valueOf(data[8]));
        }
        attributeDetail.setVariantCreation((Boolean)data[9]);
        attributeDetail.setMandatory((Boolean)data[10]);
        attributeDetail.setVariantCreatingUi((boolean)(data[11]));
        if (Objects.nonNull(data[13])) {
          attributeDetail.setEnglishName(String.valueOf(data[13]));
        }
      }
      categoryAttribute.getAttributes().add(attributeDetail);

      return categoryAttribute;
    } catch(Exception e){
      throw new Exception("Service error:" + e.getMessage());
    }
  }

  private List<Object[]> getAttributeList(String categoryCode) {
    List<Object[]> attrList;
    if (ignoreDsAttribute) {
      attrList = this.repository.getAttributeDetailByCategoryCodeIgnoreDs(categoryCode);
    } else {
      attrList = this.repository.getAttributeDetailByCategoryCode(categoryCode);
    }
    return attrList;
  }

  @Override
  public List<Attribute> findDetailByStoreIdAndAttributeCodes(List<String> attributeCodes,
      boolean fetchOnlyBasicAttributeDetails) {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    List<Attribute> attributes =
        this.repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(storeId, attributeCodes);
    if(!CollectionUtils.isEmpty(attributes) && !fetchOnlyBasicAttributeDetails) {
      for(Attribute attribute : attributes) {
        Hibernate.initialize(attribute.getAllowedAttributeValues());
        Hibernate.initialize(attribute.getPredefinedAllowedAttributeValues());
      }
    }
    return attributes;
  }

  @Override
  public List<Attribute> findDetailByStoreIdAndAttributeCodeList(String storeId, List<String> attributeCodes) {
    List<Attribute> attributes =
        this.repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(storeId, attributeCodes);
    return attributes;
  }

  @Override
  public List<Attribute> findAttributesByStoreIdAndAttributeCodeList(String storeId, List<String> attributeCodes) {
    List<Attribute> attributes =
        this.repository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(storeId, attributeCodes);
    if(!dsAttributeExtractionOptimisationEnabled) {
      if (CollectionUtils.isNotEmpty(attributes)) {
        for (Attribute attribute : attributes) {
          Hibernate.initialize(attribute.getPredefinedAllowedAttributeValues());
        }
      }
    }
    return attributes;
  }

  @Override
  public List<AttributeSummaryDTO> getAttributeDetailByCategoryCodeWithoutOptions(String storeId,
      String categoryCode) throws Exception {
    if (StringUtils.isNotEmpty(categoryCode)) {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), "StoreId must not be Empty");
      List<AttributeSummaryDTO> attrList =
          this.repository.getAttributeDetailByCategoryCodeWithoutOption(storeId, categoryCode);
      return attrList;
    }
    return null;
  }

  @Override
  public List<AttributeBasicDetailDTO> getAttributeBasicDetailByCategoryCode(String storeId, String categoryCode) {
    if (StringUtils.isNotEmpty(categoryCode)) {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
          String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY));
      return this.repository.getAttributeBasicDetailByCategoryCode(storeId, categoryCode);
    }
    return new ArrayList<>();
  }

  @Override
  public void evictAttributeCache(String storeId, String attributeId, String attributeCode) {
    applicationCacheServiceBean.evictAttributeCacheByStoreIdAndAttributeId(storeId, attributeId);
    applicationCacheServiceBean.evictAttributeCacheByStoreIdAndAttributeId(storeId, attributeCode);
    applicationCacheServiceBean.evictAllowedAttributeValuesCacheByStoreIdAndAttributeId(storeId, attributeId);
    applicationCacheServiceBean.evictPredefinedAllowedAttributeValuesCacheByStoreIdAndAttributeId(storeId, attributeId);
  }

  private AttributeService getAttributeService() {
    return applicationContext.getBean(AttributeService.class);
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.ATTRIBUTE_CACHE, key = "#storeId+'_'+ #attributeId", unless = "#result == null")
  public Attribute getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(String storeId, String attributeId) {
    Attribute attribute = this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, attributeId);
    GdnPreconditions.checkArgument(Objects.nonNull(attribute),
        AttributeServiceBean.NOT_FOUND_ATTRIBUTE_WITH_ID + attributeId);
    Attribute clonedAttribute = new Attribute();
    BeanUtils.copyProperties(attribute, clonedAttribute,
        "allowedAttributeValues", "predefinedAllowedAttributeValues", "categoryAttributes");
    return clonedAttribute;
  }

  @Override
  @Transactional(readOnly = true)
  public List<Attribute> getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(String storeId, List<String> attributeIds) {
    List<Attribute> attributes = this.repository.findByIdInAndStoreIdAndMarkForDeleteFalse(attributeIds, storeId);
    attributeIds.removeAll(attributes.stream().map(Attribute::getId).collect(Collectors.toList()));
    GdnPreconditions.checkArgument(CollectionUtils.isEmpty(attributeIds),
        AttributeServiceBean.NOT_FOUND_ATTRIBUTE_WITH_ID + attributeIds);
    return attributes;
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.ATTRIBUTE_CACHE, key = "#storeId+'_'+ #attributeCode", unless = "#result == null")
  public Attribute getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(String storeId, String attributeCode) {
    Attribute attribute = this.repository.findByStoreIdAndAttributeCode(storeId, attributeCode);
    GdnPreconditions.checkArgument(Objects.nonNull(attribute),
        AttributeServiceBean.NOT_FOUND_ATTRIBUTE_WITH_ID + attributeCode);
    Attribute clonedAttribute = new Attribute();
    BeanUtils.copyProperties(attribute, clonedAttribute,
        "allowedAttributeValues", "predefinedAllowedAttributeValues", "categoryAttributes");
    return clonedAttribute;
  }

  @Override
  @Transactional(readOnly = true)
  public List<Attribute> findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(String storeId, String name, AttributeType attributeType) {
    log.info("Fetching the attribute by store ID :{}, name : {} and attribute type : {}", storeId, name, attributeType);
    List<Attribute> attributes = this.repository
        .findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(storeId,
            attributeType, name, name);
    if (CollectionUtils.isNotEmpty(attributes)) {
      List<Attribute> clonedAttributes = new ArrayList<>();
      for (Attribute attribute : attributes) {
        Attribute clonedAttribute = new Attribute();
        BeanUtils
            .copyProperties(attribute, clonedAttribute, "allowedAttributeValues", "predefinedAllowedAttributeValues",
                "categoryAttributes");
        clonedAttribute.setAllowedAttributeValues(new ArrayList<>());
        clonedAttribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
        if (AttributeType.DEFINING_ATTRIBUTE.equals(clonedAttribute.getAttributeType())) {
          clonedAttribute.setAllowedAttributeValues(allowedAttributeValueService
              .getAllowedAttributeValuesByStoreIdAndAttributeId(storeId, clonedAttribute.getId()));
        } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(clonedAttribute.getAttributeType())) {
          clonedAttribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueService
              .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(storeId, clonedAttribute.getId()));
          removeDefaultAttributeValue(clonedAttribute);
        }
        clonedAttributes.add(clonedAttribute);
      }
      return clonedAttributes;
    }
    return new ArrayList<>();
  }

  @Override
  public Attribute findAttributeByCodeOrNameAndValue(String storeId, String attributeCode, String name, String value,
      String attributeType) {
    log.info("Finding attribute with attributeCode : {} , name : {}, value : {} , attributeType : {}", attributeCode,
        name, value, attributeType);
    Attribute attribute = null;
    try {
      attribute =
          getAttributeService().getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, attributeCode);
    } catch (ApplicationRuntimeException e) {
      log.warn("Attribute not found with attribute code : {} ", attributeCode);
    }
    if (Objects.nonNull(attribute)) {
      attribute = setAttribute(attribute, storeId);
      return attribute;
    }
    List<Attribute> attributes =
        findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(storeId, name, AttributeType.valueOf(attributeType));
    if (CollectionUtils.isEmpty(attributes)) {
      log.warn("Attribute not found with attribute name : {}, type :{}", name, AttributeType.valueOf(attributeType));
      return null;
    }

    if (attributes.size() == 1 || AttributeType.valueOf(attributeType).equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
      return attributes.get(0);
    }

    for (Attribute attribute1 : attributes) {
      if (AttributeType.valueOf(attributeType).equals(AttributeType.PREDEFINED_ATTRIBUTE)) {
        List<PredefinedAllowedAttributeValue> values = predefinedAllowedAttributeValueService
            .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(storeId, attribute1, value);
        if (CollectionUtils.isNotEmpty(values)) {
          return attribute1;
        }
      } else if (AttributeType.valueOf(attributeType).equals(AttributeType.DEFINING_ATTRIBUTE)) {
        List<AllowedAttributeValue> values = allowedAttributeValueService
            .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(storeId, attribute1, value);
        if (CollectionUtils.isNotEmpty(values)) {
          return attribute1;
        }
      }
    }
    return null;
  }

  private Attribute setAttribute(Attribute attribute , String storeId) {
    Attribute clonedAttribute = new Attribute();
    BeanUtils.copyProperties(attribute, clonedAttribute, "allowedAttributeValues", "predefinedAllowedAttributeValues",
        "categoryAttributes");
    clonedAttribute.setAllowedAttributeValues(new ArrayList<>());
    clonedAttribute.setPredefinedAllowedAttributeValues(new ArrayList<>());
    if (AttributeType.DEFINING_ATTRIBUTE.equals(clonedAttribute.getAttributeType())) {
      clonedAttribute.setAllowedAttributeValues(allowedAttributeValueService
          .getAllowedAttributeValuesByStoreIdAndAttributeId(storeId, clonedAttribute.getId()));
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(clonedAttribute.getAttributeType())) {
      clonedAttribute.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueService
          .getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(storeId, clonedAttribute.getId()));
      removeDefaultAttributeValue(clonedAttribute);
    }
    return clonedAttribute;
  }

  @Override
  public String findProductAttributeValuesByProductCodeAndAttributeCode(String storeId, String productCode,
      String attributeCode) {
    Product product = productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    Attribute attribute = repository.findByStoreIdAndAttributeCode(storeId, attributeCode);
    ProductAttribute productAttribute =
        productAttributeRepository.findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(storeId,
            product.getId(), attribute.getId());
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    if (Objects.nonNull(productAttribute)) {
      productAttributeMap.put(productAttribute.getId(), productAttribute);
      productAttributeService.getProductAttributeValues(storeId, productAttributeMap);
      List<ProductAttributeValue> attributeValueList =
          productAttributeMap.get(productAttribute.getId()).getProductAttributeValues();
      attributeValueList =
          attributeValueList.stream().filter(productAttributeValue -> !productAttributeValue.isMarkForDelete())
              .collect(Collectors.toList());
      return ConverterUtil.getProductAttributeValuesFromProductAttributeList(attributeValueList);
    } else {
      return StringUtils.EMPTY;
    }
  }

  @Override
  public List<String> findCategoryCodesByAttributeCode(String storeId, String attributeCode) {
    return categoryServiceBean.findCategoryCodesByAttributeCode(storeId, attributeCode);
  }

  @Override
  @Transactional(readOnly = true, propagation = Propagation.REQUIRED)
  public Map<AttributeAndValueByTypeRequest, Object> getAttributeAndValueMap(String storeId,
    Set<String> allowedAttributeValuesIds, Set<String> predefinedAllowedAttributeValuesIds,
    Set<String> predefinedAllowedAttributeValuesCodes, List<String> attributesIds,
    boolean productSuitabilityFeatureEnabled, String productCode) {

    Map<AttributeAndValueByTypeRequest, Object> attributeAndValueMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(attributesIds)) {
      attributeAndValueMap.putAll(this.findByAttributeIds(storeId, attributesIds).stream().collect(
        Collectors.toMap(
          attributeValue -> new AttributeAndValueByTypeRequest(attributeValue.getId(),
            AttributeAndValueByTypeRequest.type.ATTRIBUTE), Function.identity(), (a, b) -> a)));
    }
    if (CollectionUtils.isNotEmpty(allowedAttributeValuesIds)) {
      attributeAndValueMap.putAll(
        this.allowedAttributeValueService.findByStoreIdAndIds(storeId, allowedAttributeValuesIds)
          .stream().collect(Collectors.toMap(
            allowedAttributeValue -> new AttributeAndValueByTypeRequest(allowedAttributeValue.getId(),
              AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE), Function.identity(),
            (a, b) -> a)));
    }
    if (CollectionUtils.isNotEmpty(predefinedAllowedAttributeValuesIds)) {
      attributeAndValueMap.putAll(
        this.predefinedAllowedAttributeValueService.findByStoreIdAndIds(storeId,
          predefinedAllowedAttributeValuesIds).stream().collect(Collectors.toMap(
          predefinedAllowedAttributeValue -> new AttributeAndValueByTypeRequest(
            predefinedAllowedAttributeValue.getId(),
            AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID),
          Function.identity(), (a, b) -> a)));
    }
    if (CollectionUtils.isNotEmpty(predefinedAllowedAttributeValuesCodes)) {
      attributeAndValueMap.putAll(
        this.predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCodes(
          storeId, predefinedAllowedAttributeValuesCodes).stream().collect(Collectors.toMap(
          predefinedAllowedAttributeValue -> new AttributeAndValueByTypeRequest(
            predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode(),
            AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_CODE),
          Function.identity(), (a, b) -> a)));
    }
    this.filterSellerHiddenAttributes(attributeAndValueMap, productSuitabilityFeatureEnabled, productCode);

    return attributeAndValueMap;
  }

  @Override
  public void filterSellerHiddenAttributes(
    Map<AttributeAndValueByTypeRequest, Object> attributeAndValueMap,
    boolean productSuitabilityFeatureEnabled, String productCode) {
    if (!productSuitabilityFeatureEnabled) {
      return;
    }
    Iterator<Map.Entry<AttributeAndValueByTypeRequest, Object>> iterator =
      attributeAndValueMap.entrySet().iterator();
    while (iterator.hasNext()) {
      Map.Entry<AttributeAndValueByTypeRequest, Object> entry = iterator.next();
      Object value = entry.getValue();

      try {
        Attribute attribute = null;
        if (value instanceof PredefinedAllowedAttributeValue) {
          attribute = ((PredefinedAllowedAttributeValue) value).getAttribute();
        } else if (value instanceof Attribute) {
          attribute = (Attribute) value;
        }
        if (Objects.nonNull(attribute) && attribute.isHideForSeller()) {
          iterator.remove();
        }
      } catch (Exception e) {
        log.error("Exception in filterSellerHiddenAttributes for product code : {}",
          e.getMessage());
      }
    }
  }
}
