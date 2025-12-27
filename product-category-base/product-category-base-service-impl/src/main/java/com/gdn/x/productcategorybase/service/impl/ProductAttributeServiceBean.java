package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductAttributeServiceBean implements ProductAttributeService {

  @Autowired
  private ProductAttributeRepository repository;

  @Autowired
  private ProductAttributeValueService productAttributeValueService;

  @Autowired
  @Lazy
  private AttributeService attributeService;

  @Autowired
  private AllowedAttributeValueService allowedAttributeValueService;

  @Autowired
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  public ProductAttributeServiceBean() {}

  @Override
  public void delete(String id) throws Exception {
    // do nothing
  }

  @Override
  public ProductAttribute findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<ProductAttribute> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public ProductAttribute findByStoreIdAndId(String storeId, String id) {
    return this.repository.findByIdAndMarkForDeleteFalse(id);
  }

  @Override
  public String save(ProductAttribute entity) throws Exception {
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  public void update(ProductAttribute entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

  @Override
  @Cacheable(value = CacheNames.PRODUCT_ATTRIBUTES_CACHE, key = "#storeId +'_'+ #productId", unless = "#result == null")
  public List<ProductAttribute> getProductAttributesByStoreIdAndProductIdCached(String storeId, String productId) {
    List<ProductAttribute> productAttributes = getProductAttributesByStoreIdAndProductId(storeId, productId);
    Map<String, ProductAttribute> productAttributeIdProductAttributeMap = new HashMap<>();
    Map<String, List<ProductAttribute>> attributeIdProductAttributesMap = new HashMap<>();
    for (ProductAttribute productAttribute : productAttributes) {
      productAttribute.setProductAttributeValues(new ArrayList<>());
      productAttributeIdProductAttributeMap.put(productAttribute.getId(), productAttribute);
      if(!attributeIdProductAttributesMap.containsKey(productAttribute.getAttributeId())) {
        attributeIdProductAttributesMap.put(productAttribute.getAttributeId(), new ArrayList<>());
      }
      attributeIdProductAttributesMap.get(productAttribute.getAttributeId()).add(productAttribute);
    }
    List<Attribute> attributes = attributeService.getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
        storeId, new ArrayList<>(attributeIdProductAttributesMap.keySet()));
    for (Attribute attribute : attributes) {
      attributeIdProductAttributesMap.get(attribute.getId()).forEach(
          productAttribute -> productAttribute.setAttribute(attribute));
    }
    setProductAttributeValues(storeId, productAttributeIdProductAttributeMap);
    return productAttributes;
  }

  private void setProductAttributeValues(String storeId, Map<String, ProductAttribute> productAttributeIdProductAttributeMap) {
    List<ProductAttributeValue> productAttributeValues =
        productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
            storeId, productAttributeIdProductAttributeMap.keySet());
    Map<String, List<ProductAttributeValue>> allowedAttributeIdProductAttributeValueMap = new HashMap<>();
    Map<String, List<ProductAttributeValue>> predefinedAllowedAttributeIdProductAttributeValueMap = new HashMap<>();
    for (ProductAttributeValue productAttributeValue : productAttributeValues) {
      productAttributeIdProductAttributeMap.get(productAttributeValue.getProductAttributeId())
          .getProductAttributeValues().add(productAttributeValue);
      if(Objects.nonNull(productAttributeValue.getAllowedAttributeValueId())) {
        if(!allowedAttributeIdProductAttributeValueMap.containsKey(productAttributeValue.getAllowedAttributeValueId())) {
          allowedAttributeIdProductAttributeValueMap.put(productAttributeValue.getAllowedAttributeValueId(), new ArrayList<>());
        }
        allowedAttributeIdProductAttributeValueMap.get(productAttributeValue.getAllowedAttributeValueId())
            .add(productAttributeValue);
      }
      if(Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValueId())) {
        if(!predefinedAllowedAttributeIdProductAttributeValueMap.containsKey(
            productAttributeValue.getPredefinedAllowedAttributeValueId())) {
          predefinedAllowedAttributeIdProductAttributeValueMap.put(
              productAttributeValue.getPredefinedAllowedAttributeValueId(), new ArrayList<>());
        }
        predefinedAllowedAttributeIdProductAttributeValueMap.get(productAttributeValue.getPredefinedAllowedAttributeValueId())
            .add(productAttributeValue);
      }
    }
    if(CollectionUtils.isNotEmpty(allowedAttributeIdProductAttributeValueMap.keySet())) {
      List<AllowedAttributeValue> allowedAttributeValues =
          allowedAttributeValueService.getAllowedAttributeValuesByStoreIdAndIds(
              storeId, allowedAttributeIdProductAttributeValueMap.keySet());
      for (AllowedAttributeValue allowedAttributeValue : allowedAttributeValues) {
        allowedAttributeIdProductAttributeValueMap.get(allowedAttributeValue.getId())
            .forEach(productAttributeValue -> productAttributeValue.setAllowedAttributeValue(allowedAttributeValue));
      }
    }
    if(CollectionUtils.isNotEmpty(predefinedAllowedAttributeIdProductAttributeValueMap.keySet())) {
      List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
          predefinedAllowedAttributeValueService.getPredefinedAllowedAttributeValuesByStoreIdAndIds(
              storeId, predefinedAllowedAttributeIdProductAttributeValueMap.keySet());
      for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : predefinedAllowedAttributeValues) {
        predefinedAllowedAttributeIdProductAttributeValueMap.get(predefinedAllowedAttributeValue.getId())
            .forEach(productAttributeValue -> productAttributeValue.setPredefinedAllowedAttributeValue(
                predefinedAllowedAttributeValue));
      }
    }
  }

  @Override
  public List<ProductAttribute> getProductAttributesByStoreIdAndProductId(String storeId, String productId) {
    List<ProductAttribute> productAttributes = repository.findByStoreIdAndProductId(storeId, productId);
    List<ProductAttribute> clonedProductAttributes = new ArrayList<>();
    for (ProductAttribute productAttribute : productAttributes) {
      ProductAttribute clonedProductAttribute = new ProductAttribute();
      BeanUtils.copyProperties(productAttribute, clonedProductAttribute,
          "product", "attribute", "productAttributeValues");
      clonedProductAttributes.add(clonedProductAttribute);
    }
    return clonedProductAttributes;
  }

  @Override
  public void updateExtractedValueInProductAttribute(String storeId, String id) {
    List<ProductAttribute> productAttributes =
      getProductAttributesByStoreIdAndProductIdCached(storeId, id);
    productAttributes.forEach(productAttribute -> productAttribute.setExtractedValue(false));
    repository.saveAll(productAttributes);
  }

  @Override
  public Map<String, ProductAttribute> getProductAttributeValues(String storeId,
      Map<String, ProductAttribute> productAttributeMap) {
    setProductAttributeValues(storeId, productAttributeMap);
    return productAttributeMap;
  }

  @Override
  public List<ProductAttribute> saveMissedProductAttributes(
      List<ProductAttribute> productAttributes) {
    return repository.saveAll(productAttributes);
  }

  @Override
  public void deleteByProductAttributeIds(List<String> productAttributeIds) {
    repository.deleteAllById(productAttributeIds);
  }

}
